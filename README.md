# Bike Sharing Project 

## Purpose

The purpose of this repo is to explore the [bike sharing data set](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset), create predictive
models, determine the best performing model (based on the residual mean square error), and automate Markdown reports.  

## Packages

The packages needed to run the R code are: 

```{r}
library(tidyverse)
library(caret)
library(grid)
library(gridExtra)
library(knitr)
```

## Links to each analysis by day

The analysis for:

* [Monday is available here](MondayAnalysis.md).
* [Tuesday is available here](TuesdayAnalysis.md).
* [Wednesday is available here](WednesdayAnalysis.md).
* [Thursday is available here](ThursdayAnalysis.md).
* [Friday is available here](FridayAnalysis.md).
* [Saturday is available here](SaturdayAnalysis.md).
* [Sunday is available here](SundayAnalysis.md).

## Automating Analysis Report Generation

Below is the code used to automatically generate the analysis reports for each day of the week.

```{r}
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
```
