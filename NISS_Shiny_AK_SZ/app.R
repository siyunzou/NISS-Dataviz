#loading packages
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(sp)
library(broom) 
library(rgeos)
library(mapproj)
library(plotly)
library(shiny)


#Import hexbins
hex <- geojson_read("us_states_hexgrid.geojson", what = "sp")

#Reformat the 'google_name' field
hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#Fortify the data to create a data format output
hex_fortify <- tidy(hex, region = "google_name") 

# read education data 
College <- read_csv('coldata_cleaned.csv')
College$black <- as.numeric(College$black)
College$asian <- as.numeric(College$asian)
HighSchool <- read_csv('hsdata_cleaned.csv')
HighSchool$black <- as.numeric(HighSchool$black)
HighSchool$asian <- as.numeric(HighSchool$asian)

College_US <- read_csv('coldata_UScleaned.csv')
HighSchool_US <- read_csv('hsdata_UScleaned.csv')

hex_fortify_col <- hex_fortify %>%
    left_join(. , College, by=c("id"="State")) 

hex_fortify_hs <- hex_fortify %>%
    left_join(. , HighSchool, by=c("id"="State"))

#Define centers of hexbins
centers <- cbind.data.frame(data.frame(gCentroid(hex, byid = TRUE), id = hex@data$iso3166_2))


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
    height = "auto",
    width = "auto",
    # Application title
    titlePanel(HTML(paste("Comparing Educational Attainment", "Across Racial Groups (2019)", sep = "<br/>"))),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(HTML("<p><b>App Introduction:</b>"), 
        HTML("<ul class=\"a\">
        <li> Select \"Educational Attainment\" and \"Race\" from the 
        dropdown menus below to update the 
        map and standard error distribution </li>
        <li>Use the slider to adjust the percentile range displayed on the map&#8224; </li>
        <li>Move your cursor over the plots for more information</li>
        <p>&#8224;<i>Note: States with missing (NA) data or those that are 
             outside the percentile range will appear gray</p></i>
             </ul>"), 
            selectInput("degree", "Educational Attainment:", 
                        c("Bachelor's Degree or Higher", 
                          "High School Degree or Higher*")), 
            selectInput("race", "Race:", 
                        c("Total**" = "total", 
                          "White" = "white", 
                          "Black" = "black", 
                          "Hispanic" = "hispanic", 
                          "Asian" = "asian", 
                          "Two or more races" = "two or more race")), 
            sliderInput("percentile",
                        "Percentile Range:",
                        min = 0,
                        max = 100,
                        value = c(0, 60)), 
        HTML(paste0("<b>US Overall Stats (", textOutput("race.text", inline = TRUE),"):</b> 
        <ul class=\"a\">
             <li>Percent Achieved: ", textOutput("mean.text", inline = TRUE), "%</li> ",
                    "<li>Standard Error: ", textOutput("se.text", inline = TRUE), "</li> 
                    </ul>")),
        
        HTML("<b> More Information:</b>
             <br> <p><a href=\"https://nces.ed.gov/programs/digest/d20/tables/dt20_104.85.asp\">NCES Source Data</a></p>
             <i>*High school completion includes through equivalency programs 
             such as a GED program.</i>
             <br><i>**Total includes all racial/ethnic groups, including those not listed in this data.</i>
")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot"),
            #plotlyOutput("boxPlot"), 
            width = 8
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    datasetInput <- reactive({
        if (input$degree == "Bachelor's Degree or Higher"){
            dataset <- hex_fortify_col
        }
        else if (input$degree == "High School Degree or Higher*"){
            dataset <- hex_fortify_hs
        }
        return(dataset)
    })
    
    datasetInput2 <- reactive({
        if (input$degree == "Bachelor's Degree or Higher"){
            dataset2 <- College
        }
        else if (input$degree == "High School Degree or Higher*"){
            dataset2 <- HighSchool
        }
        return(dataset2)
    })
    
    datasetInput3 <- reactive({
        if (input$degree == "Bachelor's Degree or Higher"){
            dataset3 <- College_US
        }
        else if (input$degree == "High School Degree or Higher*"){
            dataset3 <- HighSchool_US
        }
        return(dataset3)
    })
    
    observeEvent(input$dimension,{ 
        
    output$race.text <- renderText({input$race})
    output$mean.text <- renderText({paste0(" ", datasetInput3()[[input$race]])})
    error.var <- reactive({paste(input$race, "standard error")})
    output$se.text <- renderText({paste0(" ", datasetInput3()[[error.var()]])})
    
    output$distPlot <- renderPlotly({
        subplot(ggplotly(ggplot() + 
                             geom_polygon(data = datasetInput(), aes(fill = datasetInput()[[input$race]], x = long, y = lat, 
                                                                     group = group, 
                                                                     text = paste0("<b>",id, "</b>", 
                                                                                   "<br>Percentage (", input$race, "): ", 
                                                                                   datasetInput()[[input$race]], "%",
                                                                                   "<br>Standard error: ", 
                                                                                   datasetInput()[[paste(input$race, "standard error")]], 
                                                                                   "<br>Difference from US (%): ", 
                                                                                   round(datasetInput3()[[input$race]] - 
                                                                                       datasetInput()[[input$race]], 1))), 
                                          size = 0, alpha = 0.9, color = "#f7f7f7", width = (0.95*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2])) + 
                             theme_void() +
                             scale_fill_gradient(low = "white", high = "purple", #muted purple hexcode: #9467bd
                                                 name = "Percent (%)", 
                                                 limits = c(input$percentile[1], input$percentile[2])) + 
                             ggtitle(paste("Percent with", input$degree, "in the United States")), 
                         tooltip = "text", height = 700) %>% #********************************************************
                    #https://stackoverflow.com/questions/44324783/dynamically-adjust-height-and-or-width-of-shiny-plotly-output-based-on-window-si
                    plotly::layout(xaxis = list(title = "", 
                                                zeroline = FALSE, 
                                                showline = FALSE,
                                                showticklabels = FALSE, 
                                                showgrid = FALSE), 
                                   yaxis = list(title = "", 
                                                zeroline = FALSE, 
                                                showline = FALSE,
                                                showticklabels = FALSE, 
                                                showgrid = FALSE), 
                                   hoverlabel = list(bgcolor = "white")
                    ) %>% 
                    add_annotations(x = centers$x, y = centers$y, text = centers$id, showarrow = FALSE), 
                plot_ly(x = datasetInput2()[[error.var()]], type="box", 
                        hoverinfo = "text", 
                        hovertext = paste0("<b>", datasetInput2()$State, "</b>", "<br>Standard error: ", 
                                          datasetInput2()[[error.var()]]), 
                        jitter = datasetInput2()[[error.var()]], 
                        pointpos = .1, boxpoints = 'all', name = " ") ,
                nrows = 2, heights = c(0.85, 0.13)) %>% layout(annotations = 
                                                                   list(text = "Standard Error Distribution", 
                                                                        x = .175, 
                                                                        y = .12, 
                                                                        xref = "paper", 
                                                                        yref = "paper", 
                                                                        xanchor = "center", 
                                                                        yanchor = "bottom", 
                                                                        showarrow = FALSE))
    })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
