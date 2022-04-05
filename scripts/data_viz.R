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

sapply(edu_data, class)

hex_fortify <- hex_fortify %>%
  left_join(. , edu_data, by=c("id"="State")) 
#Check if the join was successful
view(hex_fortify)

#explore descriptive statistics 
mean(edu_data$col_total)
range(edu_data$col_total)

#Add labels 
centers <- cbind.data.frame(data.frame(gCentroid(hex, byid = TRUE), id = hex@data$iso3166_2))

hex_fortify$bin <- cut(hex_fortify$col_total, breaks = c(20, 30, 40, 50, 60), 
                        labels = c("<20", "20-30", "30-50", "50+"))
my_palette <- brewer.pal(n = 4, name = "Greens")

ggplot() + 
  geom_polygon(data = hex_fortify, aes(fill = bin, x = long, y = lat, group = group), 
               size = 0, alpha = 0.9, color = "#f7f7f7") +
  geom_text(data = centers, aes(x = x, y = y, label = id), 
            color = "#252525", size = 5) + #Add our labels
  theme_void() +
  scale_fill_manual(
    values = my_palette, 
    name ="Rates of bachelor's degree attainment among persons age 25 and over", #Add legend title 
    guide = guide_legend(keyheight = unit(4, units = "mm"), 
                         keywidth = unit(10, units = "mm"), 
                         direction = "horizontal", 
                         label.position = "bottom", 
                         title.position = "top", nrow = 1)
  ) +
  ggtitle( "Percent with bachelor's or higher degree in the United States in 2019" ) + #Add map title
  theme(legend.position = c(0.5, 0.9), #Choose legend positioning (horizontal, vertical)
    text = element_text(color = "black", face="bold"), #This is where we customize the legend text
    plot.background = element_rect(), #Choose the colour of the background behind the title
    panel.background = element_rect(), #This is the main background
    legend.background = element_rect(), #This is the legend background 
    plot.title = element_text(size=18, hjust=0.5, color = "black", face="bold"), #This we where we customize the title 
  )


##Making a map with continuous filling
map2 <- ggplot() + 
  geom_polygon(data = hex_fortify, aes(fill = col_total, x = long, y = lat, group = group), 
               size = 0, alpha = 0.9, color = "#f7f7f7") +
  geom_text(data = centers, aes(x = x, y = y, label = id), 
            color = "#252525", size = 5) + #Add our labels
  theme_void() +
  scale_fill_gradient(low = "white", high = "#74c476", 
                      name = "Percent with bachelor's or higher degree in the United States in 2019", limits = c(0, 60)) +
  ggtitle( "Percent with bachelor's or higher degree in the United States in 2019")


ggplotly(map2, tooltip = "col_total")

