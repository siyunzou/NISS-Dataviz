#Load the packages
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(sp)
library(broom) 
library(rgeos)
# read data 
edu_data <- read_csv('../data/104.85_cleaned.csv')
