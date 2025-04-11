#''''''''''''''''''''''''''''''''''''''''''
#'
#' Processing Ninigret sediment placement data
#' @date 2025-04-08
#' @author Cooper Kimball-Rhines
#' 
#''''''''''''''''''''''''''''''''''''''''''


# Load libraries
library(tidyverse)

test <- read_csv("data/nag/nag_2008.csv")

# The structure of the individual csv files is like this:
# site/site_year.csv
# So we need a function that takes the prefix and year as inputs, loads in the csv and combines the outputs
proc <- function(site, year) {
  sheet <- read_csv(paste("data/", site, "/", site, "_", year, ".csv", sep="")) |>
    select(`...1`, starts_with("T")) |>
    rename(species = `...1`) |>
    mutate(year = year,
           site = site) 
}

nag <- map2(.x = "nag", .y = 2015:2024, .f = proc) |> list_rbind()
cog <- map2(.x = "Coggeshall", .y = 2015:2024, .f = proc) |> list_rbind()

# Filter combined sheets
nagFilt <- nag |>
  na.omit() |>
  pivot_longer(cols = starts_with("T"),
               names_to = "Plot",
               values_to = "count")

cogFilt <- cog |>
  na.omit() |>
  pivot_longer(cols = starts_with("T"),
               names_to = "Plot",
               values_to = "count")

# Merge and create the sentinal csv
rbind(nagFilt, cogFilt) |>
  arrange(site, species, year) |>
  write_csv("data/sentinal.csv")


