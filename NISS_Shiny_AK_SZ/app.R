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
edu_data <- read_csv('104.85_errors_cleaned.csv')

sapply(edu_data, class)

hex_fortify <- hex_fortify %>%
    left_join(. , edu_data, by=c("id"="State")) 

#Define centers of hexbins
centers <- cbind.data.frame(data.frame(gCentroid(hex, byid = TRUE), id = hex@data$iso3166_2))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NISS Placeholder title"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("percentile",
                        "Percentile Range:",
                        min = 0,
                        max = 100,
                        value = c(0, 60))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"), 
           width = 8
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        ggplotly(ggplot() + 
            geom_polygon(data = hex_fortify, aes(fill = col_total, x = long, y = lat, group = group), 
                         size = 0, alpha = 0.9, color = "#f7f7f7") + 
            theme_void() +
            scale_fill_gradient(low = "white", high = "purple", 
                                name = "Percent Acheived Degree", 
                                limits = c(input$percentile[1], input$percentile[2])) + 
            ggtitle( "Percent with bachelor's or higher degree in the United States in 2019"), 
            tooltip = "col_total") %>% 
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
                           height = 700) %>% 
        add_annotations(x = centers$x, y = centers$y, text = centers$id, showarrow = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
