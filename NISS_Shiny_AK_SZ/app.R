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
hex <- geojson_read("../NISS-Dataviz/data/us_states_hexgrid.geojson", what = "sp")

#Reformat the 'google_name' field
hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#Fortify the data to create a data format output
hex_fortify <- tidy(hex, region = "google_name") 

# read education data 
College <- read_csv('../NISS-Dataviz/data/coldata_cleaned.csv')
College$black <- as.numeric(College$black)
College$asian <- as.numeric(College$asian)
HighSchool <- read_csv('../NISS-Dataviz/data/hsdata_cleaned.csv')
HighSchool$black <- as.numeric(HighSchool$black)
HighSchool$asian <- as.numeric(HighSchool$asian)

hex_fortify_col <- hex_fortify %>%
    left_join(. , College, by=c("id"="State")) 

hex_fortify_hs <- hex_fortify %>%
    left_join(. , HighSchool, by=c("id"="State"))

#Define centers of hexbins
centers <- cbind.data.frame(data.frame(gCentroid(hex, byid = TRUE), id = hex@data$iso3166_2))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NISS Placeholder title"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("degree", "Educational Attainment:", 
                        c("Bachelor's Degree or Higher", 
                          "High School Degree or Higher")), 
            selectInput("race", "Race:", 
                        c("Total" = "total", 
                          "White" = "white", 
                          "Black" = "black", 
                          "Hispanic" = "hispanic", 
                          "Asian" = "asian", 
                          "Two or more races" = "two or more race")), 
            sliderInput("percentile",
                        "Percentile Range:",
                        min = 0,
                        max = 100,
                        value = c(0, 60))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabPanel("Plot"),
            fluidRow(
                column(8, plotlyOutput("distPlot")),
                column(12, plotlyOutput("boxPlot"))
            )
           , 
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
    
    output$distPlot <- renderPlotly({
        map <- ggplotly(ggplot() + 
            geom_polygon(data = datasetInput(), aes(fill = datasetInput()[[input$race]], x = long, y = lat, 
                                                     group = group, 
                                                     text = paste0("State: ", id, 
                                                                  "<br>Percentage (", "total", "): ", 
                                                                  datasetInput()[[input$race]], "%",
                                                                  "<br>Standard error: ", 
                                                                  datasetInput()[[paste(input$race, "standard error")]])), 
                         size = 0, alpha = 0.9, color = "#f7f7f7") + 
            theme_void() +
            scale_fill_gradient(low = "white", high = "purple", 
                                name = "Percent (%)", 
                                limits = c(input$percentile[1], input$percentile[2])) + 
            ggtitle(paste("Percent with", tolower(input$degree), "in the United States in 2019")), 
            tooltip = "text") %>% 
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
                           height = 400) %>% 
        add_annotations(x = centers$x, y = centers$y, text = centers$id, showarrow = F)
    })
}

# boxplot 
plot_ly(y=hs_data$`total standard error`, type="box", text=~hs_data$State, 
        name='hs total standard error', jitter = hs_data$`total standard error`, 
        pointpos = .1, boxpoints = 'all')

# Run the application 
shinyApp(ui = ui, server = server)
