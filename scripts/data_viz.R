#Load the packages
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(sp)
library(broom) 
library(rgeos)

#Import hexbins
hex <- geojson_read("../data/us_states_hexgrid.geojson", what = "sp")

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
edu_data <- read_csv('../data/104.85_cleaned.csv')






