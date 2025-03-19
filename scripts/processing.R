#''''''''''''''''''''''''''''''''''''''''''
#'
#' Processing Ninigret sediment placement data
#' @date 2024/06/25
#' @author Cooper Kimball-Rhines
#' 
#''''''''''''''''''''''''''''''''''''''''''

# First we need to merge all of the data sheets from different years into one
# based on their column names

#Load in useful libraries
library(tidyverse)
library(readxl)
library(openxlsx)

# Start master sheet with 2015
allSheets <- read.csv("data/2015.csv")

#Apply formatting fixes to get rid of extra rows and columns
allSheets <- allSheets |>
  filter(Zone != "") |>
  select(!starts_with("X")) |>
  mutate(year = "2015")

# Write a function to automate processing and joining of all sheets
combo <- function(year){
  
  # Load in sheet selected by year variable and edit out extra cells
  sheet <- read.csv(paste("data/", year, ".csv", sep="")) |>
    filter(Zone != "") |>
    select(!starts_with("X")) |>
    mutate(year = as.numeric(year))
  
  # Join selected sheet into master sheet by column
  allSheets <<- allSheets |>
    full_join(sheet)
  print(nrow(allSheets))
}
combo("2015")
# Now check which columns didn't properly join and go back and fix them in the csv

# Iterate over all of the years using lapply
sheetList <- c("2016", "2017", "2018", "2019", "2020")
lapply(sheetList, combo)

# Replace NAs with 0s
allSheets[is.na(allSheets)] <- 0
View(allSheets)

# Write processed sheet to a file
write.csv(x = allSheets, file = "data/processed.csv")
