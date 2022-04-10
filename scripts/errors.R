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
edu_data <- read_csv("../NISS-Dataviz/data/104.85_errors_cleaned.csv")
hs_data <- read_csv("../NISS-Dataviz/data/hsdata_cleaned.csv")
col_data <- read_csv("../NISS-Dataviz/data/coldata_cleaned.csv")

hs_data_US <- hs_data[1,]
col_data_US <- col_data[1,]

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

 
# df <- edu_data %>%
#   group_by(col_total_error) %>%
#   summarise(n=n(), State)

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

fig <- plot_ly(y=hs_data$`total standard error`, type="box", text=~hs_data$State, 
               name= ' ', 
               jitter = hs_data$`total standard error`, 
               pointpos = .1, boxpoints = 'all',
               hoverinfo = "text",
               hovertext = paste("standard error:", hs_data$`total standard error`,
                                 "<br> state:", hs_data$State)) 
               
summary(hs_data)  


# write.csv(edu_data, "../data/104.85_errors_cleaned.csv")

# splitting data into hs and college 
hs_data <- edu_data %>%
  select(-col_2ormore, -col_total, -col_total_error, - col_2ormore_error,
         -col_asia, -col_asia_error, -col_black, -col_black_error, -col_hisp,
         -col_hisp_error, -col_white, -col_white_error) %>%
  rename(total = hs_total,
         "total standard error" = hs_total_error,
         white = hs_white,
         "white standard error" = hs_white_error,
         black = hs_black,
         "black standard error" = hs_black_error,
         hispanic = hs_hisp,
         "hispanic standard error" = hs_hisp_error,
         asian = hs_asia,
         "asian standard error" = hs_asia_error,
         "two or more race" = hs_2ormore,
         "two or more race standard error" = hs_2ormore_error)


col_data <- edu_data %>%
  select(-hs_2ormore, -hs_total, -hs_total_error, -hs_2ormore_error,
         -hs_asia, -hs_asia_error, -hs_black, -hs_black_error, -hs_hisp,
         -hs_hisp_error, -hs_white, -hs_white_error) %>%
  rename(total = col_total,
         "total standard error" = col_total_error,
         white = col_white,
         "white standard error" = col_white_error,
         black = col_black,
         "black standard error" = col_black_error,
         hispanic = col_hisp,
         "hispanic standard error" = col_hisp_error,
         asian = col_asia,
         "asian standard error" = col_asia_error,
         "two or more race" = col_2ormore,
         "two or more race standard error" = col_2ormore_error)


write.csv(hs_data, "../NISS-Dataviz/data/hsdata_cleaned.csv")
write.csv(col_data, "../NISS-Dataviz/data/coldata_cleaned.csv")
write.csv(hs_data_US, "../NISS-Dataviz/NISS_Shiny_AK_SZ/hsdata_UScleaned.csv")
write.csv(col_data_US, "../NISS-Dataviz/NISS_Shiny_AK_SZ/coldata_UScleaned.csv")





