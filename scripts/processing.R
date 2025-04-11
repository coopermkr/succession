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

# Load in a test sheet
nin2015 <- read_csv("data/ninigret/ninigret_2015.csv")

#Apply formatting fixes to get rid of extra rows and columns
nin2015 <- nin2015 |>
  filter(Zone != "") |>
  select(!starts_with("...")) |>
  mutate(year = "2015")

# pivot to join with other sheets
nin2015Long <- nin2015 |>
  # Note, we're carrying the bare and open water measurements into the species column
  pivot_longer(cols = !c("Zone", "Plot", "Date", "Direction of plot", "Notes", "year"),
               names_to = "species",
               values_to = "count")

# Write a function to automate processing and joining of all sheets
proc <- function(site, year) {
    sheet <- read_csv(paste("data/", site, "/", site, "_", year, ".csv", sep="")) |>
      select(!starts_with("...")) |>
      filter(Zone != "") |>
      mutate(year = year,
             site = site) |>
      
      # pivot to combine
      pivot_longer(cols = !c("Zone", "Plot", "Date", "Direction of plot", "Notes", 
                             "year", "site"),
                   names_to = "species",
                   values_to = "count")
}

# Iterate over each year.csv
nin <- map2(.x = "ninigret", .y = 2015:2023, .f = proc) |> list_rbind()


##### Quonnie
# Load in a test sheet
qTest <- read_csv("data/quonnie/quonnie_2024.csv", na = "0")
str(qTest)
#Apply formatting fixes to get rid of extra rows and columns
qTest <- qTest |>
  filter(Zone != "") |>
  select(!c(starts_with("..."), `Plot Elevation`)) |>
  mutate(year = "2018")

# pivot to join with other sheets
qTestLong <- qTest |>
  # Note, we're carrying the bare and open water measurements into the species column
  pivot_longer(cols = !c("Zone", "Plot", "Date", "Direction of plot", "Notes", "year"),
               names_to = "species",
               values_to = "count")

# Write a function to automate processing and joining of all sheets
proc <- function(site, year) {
  sheet <- read_csv(paste("data/", site, "/", site, "_", year, ".csv", sep="")) |>
    filter(Zone != "") |>
    select(!c(starts_with("..."), `Plot Elevation`)) |>
    mutate(year = year,
           site = site,
           `Gerardia maritima` = as.double(`Gerardia maritima`)) |>
    
    # pivot to combine
    pivot_longer(cols = !c("Zone", "Plot", "Date", "Direction of plot", "Notes", 
                           "year", "site"),
                 names_to = "species",
                 values_to = "count")
}

quo <- map2(.x = "quonnie", .y = 2018:2023, .f = proc) |> list_rbind()

# Combine files and write out
rbind(nin, quo) |>
  write_csv(file = "data/tlp.csv")
