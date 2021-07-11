#library for tibble function
library(tidyverse)

# Defining Weekdays
Weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Creating File Names
output_file <- paste0(Weekdays, "Analysis.md")

# Create a list for each day with just the day name parameter
params <- lapply(Weekdays, FUN=function(x){list(day = x)})

# put into a data frame
reports <- tibble(output_file, params)

apply(reports, MARGIN = 1,
      FUN = function(x){
        rmarkdown::render(input = "ST558Project2.Rmd", 
                          output_file = x[[1]],
                          params = x[[2]])
      })
