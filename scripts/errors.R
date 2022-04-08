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

# read data 
edu_data <- read_csv("../data/104.85_errors_cleaned.csv")

# clean parenthesis in errors 
# edu_data <- edu_data %>%
#   mutate(col_total_error = as.numeric(gsub("[()]", "", edu_data$col_total_error)),
#          col_white_error = as.numeric(gsub("[()]", "", edu_data$col_white_error)),
#          col_black_error = as.numeric(gsub("[()]", "", edu_data$col_black_error)),
#          col_hisp_error = as.numeric(gsub("[()]", "", edu_data$col_black_error)),
#          col_asia_error = as.numeric(gsub("[()]", "", edu_data$col_asia_error)),
#          col_2ormore_error = as.numeric(gsub("[()]", "", edu_data$col_2ormore_error)),
#          hs_total_error = as.numeric(gsub("[()]", "", edu_data$hs_total_error)),
#          hs_white_error = as.numeric(gsub("[()]", "", edu_data$hs_white_error)),
#          hs_black_error = as.numeric(gsub("[()]", "", edu_data$hs_black_error)),
#          hs_hisp_error = as.numeric(gsub("[()]", "", edu_data$hs_black_error)),
#          hs_asia_error = as.numeric(gsub("[()]", "", edu_data$hs_asia_error)),
#          hs_2ormore_error = as.numeric(gsub("[()]", "", edu_data$hs_2ormore_error)))

sapply(edu_data, class)

#explore descriptive statistics 
mean(edu_data$col_total_error)
range(edu_data$col_total_error)

df <- edu_data %>%
  group_by(col_total_error) %>%
  summarise(n=n(), State)

fig <- plot_ly(
  type = 'histogram',
  x = df$col_total_error,
  hoverinfo = 'text',
  text = ~paste('</br>State(s):', df$State,
                '</br>error:', df$col_total_error),
)

fig <- df %>%
  plot_ly(
    type='scatter',
    mode = 'markers',
    y = ~ n,
    x = ~col_total_error,
    markers = list(size = ~ State, sizeref = 4000, sizemode = 'area'),
    text = ~State,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "error: %{x:.0%}<br>",
      "<extra></extra>"
    )
  )

# write.csv(edu_data, "../data/104.85_errors_cleaned.csv")








