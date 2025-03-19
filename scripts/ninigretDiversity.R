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
ninigret <- read_csv("data/processed.csv") |>
  select(!'...1')

# Pivoting!
ninLong <- ninigret |>
  pivot_longer(cols = !c("Zone", "Plot", "Date", "year", 
                         "Direction.of.plot", "Notes", "Bare",
                         "Open.Water"),
               names_to = "species",
               values_to = "cover")

#### Richness ####
# Change all cover values into 1s or 0s
ninRich <- ninLong |>
  mutate(cover = ifelse(cover > 0, 1, 0)) |>
  # Group by year and subplot
  group_by(year, Plot, Zone) |>
  summarize(richness = sum(cover))

head(ninRich)

# Viz
# Distributions of richness over year
ggplot(data= ninRich,
       mapping = aes(x = richness)) +
  geom_histogram() +
  facet_wrap(vars(year))

#### Diversity ####
# Note: Vegan requires data in wide format with each species in a column
veganized <- ninigret |>
  #Calculate Shannon's
  mutate(shannon = diversity(ninigret[, -c(1:9)],
                             index = "shannon")) |>
  
  #Calculate Simpson's
  mutate(simpson = diversity(ninigret[, -c(1:9)],
                             index = "simpson")) |>
  #Calculate Pielou's Evenness (from Shannon's)
  mutate(evenness = shannon/log(specnumber(ninigret[, -c(1:9)]))) |>
  
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
write.csv(x = veganized, file = "data/ninigret_diversity.csv")