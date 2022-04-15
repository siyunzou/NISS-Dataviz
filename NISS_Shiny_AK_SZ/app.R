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
College <- read_csv('coldata_cleaned.csv')[-1,]
College$black <- as.numeric(College$black)
College$asian <- as.numeric(College$asian)
HighSchool <- read_csv('hsdata_cleaned.csv')[-1,]
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
    
    # Application title
    titlePanel(HTML(paste("Comparing Educational Attainment", "Across Racial Groups (2019)", sep = "<br/>"))),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(HTML("<b>App Introduction:</b>"), 
        HTML("<ul class=\"a\">
        <li> Select \"Educational Attainment\" and \"Race\" from the 
        dropdown menus below to update the 
        map and standard error distribution. </li>
        <li>Use the slider to adjust the percentile range displayed on the map**</li>
        <li>Move your cursor over the plots for more information</li>
        <i>**Note: Both states with missing data (NA) or that are 
             outside the percentile range will appear grey</i>
             </ul>"), 
            selectInput("degree", "Educational Attainment:", 
                        c("Bachelor's Degree or Higher", 
                          "High School Degree or Higher*")), 
            selectInput("race", "Race:", 
                        c("Total*" = "total", 
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
        HTML("<b>US Overall stats:</b> 
             <br>Percent Achieved:"), 
        
        textOutput("mean.text"), 
        
        HTML("<b> More Information:</b>
             <br> <p><a href=\"https://nces.ed.gov/programs/digest/d20/tables/dt20_104.85.asp/\">NCES Source Data Here</a></p>
             <i>*High school completion includes through equivalency programs 
             such as a GED program.</i>
             <br><i>*Total includes racial/ethnic groups not shown separately.</i>
             <br><i>*States that have missing (NA) in data: Montana, Vermont, Wyoming </i>
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
server <- function(input, output) {
    datasetInput <- reactive({
        if (input$degree == "Bachelor's Degree or Higher"){
            dataset <- hex_fortify_col
        }
        else if (input$degree == "High School Degree or Higher"){
            dataset <- hex_fortify_hs
        }
        return(dataset)
    })
    
    datasetInput2 <- reactive({
        if (input$degree == "Bachelor's Degree or Higher"){
            dataset <- College
        }
        else if (input$degree == "High School Degree or Higher"){
            dataset <- HighSchool
        }
        return(dataset)
    })
    
    datasetInput3 <- reactive({
        if (input$degree == "Bachelor's Degree or Higher"){
            dataset <- College_US
        }
        else if (input$degree == "High School Degree or Higher"){
            dataset <- HighSchool_US
        }
        return(dataset)
    })
    
    mean.text <- reactive({paste0(" ", datasetInput3()[[input$race]])})
    
    error.var <- reactive({paste(input$race, "standard error")})
    
    output$distPlot <- renderPlotly({
        subplot(ggplotly(ggplot() + 
                             geom_polygon(data = datasetInput(), aes(fill = datasetInput()[[input$race]], x = long, y = lat, 
                                                                     group = group, 
                                                                     text = paste0(id, 
                                                                                   "<br>Percentage (", input$race, "): ", 
                                                                                   datasetInput()[[input$race]], "%",
                                                                                   "<br>Standard error: ", 
                                                                                   datasetInput()[[paste(input$race, "standard error")]])), 
                                          size = 0, alpha = 0.9, color = "#f7f7f7") + 
                             theme_void() +
                             scale_fill_gradient(low = "white", high = "purple", #muted purple hexcode: #9467bd
                                                 name = "Percent (%)", 
                                                 limits = c(input$percentile[1], input$percentile[2])) + 
                             ggtitle(paste("Percent with", tolower(input$degree), "in the United States")), 
                         tooltip = "text", height = 700) %>% 
                    plotly::layout(xaxis = list(title = "", 
                                                zeroline = FALSE, 
                                                showline = FALSE,
                                                showticklabels = FALSE, 
                                                showgrid = FALSE), 
                                   yaxis = list(title = "", 
                                                zeroline = FALSE, 
                                                showline = FALSE,
                                                showticklabels = FALSE, 
                                                showgrid = FALSE)#, height = 700
                    ) %>% 
                    add_annotations(x = centers$x, y = centers$y, text = centers$id, showarrow = FALSE), 
                plot_ly(x = datasetInput2()[[error.var()]], type="box", 
                        hoverinfo = "text", 
                        hovertext = paste(datasetInput2()$State,"<br>Standard Error:", 
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
