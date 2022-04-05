#Load the packages
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(sp)
library(broom) 
library(rgeos)
library(mapproj)
library(stringr)
library(plotly)
library(stringr)

#Import hexbins
hex <- geojson_read("../NISS-Dataviz/data/us_states_hexgrid.geojson", what = "sp")

#Reformat the 'google_name' field
hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#Fortify the data to create a data format output
hex_fortify <- tidy(hex, region = "google_name") 

#Plot the hexbins
ggplot () +
  geom_polygon(data = hex_fortify, aes( x = long, y = lat, group = group), fill="#a1dab4", color="#f7f7f7") +
  geom_text () +
  theme_void () +
  coord_map ()

# read data 
edu_data <- read_csv('../NISS-Dataviz/data/104.85_cleaned.csv')

# clean parenthesis in errors 
edu_data <- edu_data %>%
  mutate(col_total_error = as.numeric(gsub("[()]", "", edu_data$col_total_error)))

sapply(edu_data, class)

hex_fortify <- hex_fortify %>%
  left_join(edu_data, by=c("id"="State")) 
#Check if the join was successful
view(hex_fortify)

#explore descriptive statistics 
mean(edu_data$col_total_error)
range(edu_data$col_total_error)

#Add labels 
centers <- cbind.data.frame(data.frame(gCentroid(hex, byid = TRUE), id = hex@data$iso3166_2))

##Making a map with continuous filling
map_errors <- ggplot() + 
  geom_polygon(data = hex_fortify, aes(fill = col_total_error, x = long, y = lat, group = group), 
               size = 0, alpha = 0.9, color = "#f7f7f7") +
  geom_text(data = centers, aes(x = x, y = y, label = id), 
            color = "#252525", size = 5) + #Add our labels
  theme_void() +
  scale_fill_gradient(low = "white", high = "#69b3a2", 
                      name = "Standard Errors", limits = c(0, 1.2)) +
  ggtitle( "Standard Errors")

ggplotly(map_errors, tooltip = "col_total_error")
