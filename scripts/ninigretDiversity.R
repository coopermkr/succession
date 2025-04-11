#''''''''''''''''''''''''''''''''''''''''''
#'
#' Diversity of the Ninigret sediment placement site
#' @date 2024/07/02
#' @author Cooper Kimball-Rhines
#' 
#''''''''''''''''''''''''''''''''''''''''''

# Load libraries
library(vegan)
library(tidyverse)
library(labdsv)
library(Matrix)

# Read in data
ninigret <- read_csv("data/tlp.csv")

# Pivoting!
ninFilt <- ninigret |>
  # Filter out abiotic elements
  filter(!species %in% c("Bare", "Burrows", "Open water", "Unknown Grass", "Open Water",
                         "Thatch average of 5 measurements", "Thatch", "Unknown grass",
                         "Unknonwn grass", "Fiddler Crab Burrows"),
         # Filter out Quonnie plots
         site == "ninigret",
         # Filter out the plot that didn't receive treatment
         Plot != "I1-74",
         Plot != "I1-74*") |>
  # Select out unimportant columns
  select(!c("Date", "site", "Direction of plot")) |>
  # Replace NA counts with 0
  mutate(count = ifelse(is.na(count), 0, count))

#### Richness ####
# Change all cover values into 1s or 0s
ninRich <- ninFilt |>
  mutate(count = ifelse(count > 0, 1, 0)) |>
  # Group by year and subplot
  group_by(year, Plot, Zone) |>
  summarize(richness = sum(count))

head(ninRich)

# Viz
# Distributions of richness over year
ggplot(data= ninRich,
       mapping = aes(x = richness)) +
  geom_histogram() +
  facet_wrap(vars(year))

#### Diversity ####
# Note: Vegan requires data in wide format with each species in a column
ninWide <- ninFilt |>
  pivot_wider(names_from = species,
              values_from = count,
              values_fill = 0)

veganized <- ninWide |>
  #Calculate Shannon's
  mutate(shannon = diversity(ninWide[, -c(1:4)],
                             index = "shannon")) |>
  
  #Calculate Simpson's
  mutate(simpson = diversity(ninWide[, -c(1:4)],
                             index = "simpson")) |>
  #Calculate Pielou's Evenness (from Shannon's)
  mutate(evenness = shannon/log(specnumber(ninWide[, -c(1:4)]))) |>
  
  select(Zone, Plot, year, shannon, simpson, evenness)

# Graph distribution of diversity metrics
ggplot(data = veganized,
       mapping = aes(x = shannon,
                     color = as.factor(year), fill = as.factor(year))) +
  geom_histogram(position = "stack") +
  facet_wrap(vars(year))

# Second: Simpson
ggplot(data = veganized,
       mapping = aes(x = simpson, 
                     color = as.factor(year), fill = as.factor(year))) +
  geom_histogram(position = "stack") +
  facet_wrap(vars(year))

# Third: evenness
ggplot(data = veganized,
       mapping = aes(x = evenness, 
                     color = as.factor(year), fill = as.factor(year))) +
  geom_histogram(position = "stack") +
  facet_wrap(vars(year))

# Write diversity data to .csv
write_csv(x = veganized, file = "data/ninigret_diversity.csv")
