#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ecosystem State Comparison
#' @date 2025-04-11
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Load libraries
library(smacof)
library(tidyverse)

# Load in each data file and filter for years after treatment
tlpstart <- read_csv("data/tlp.csv") |>
  filter(year ==2018,
         Plot != "I1-74*") |>
  select(species, year, site, Plot, count, Zone) |>
  mutate(grp = paste(site, Zone, sep = "_"))

sentinalstart <- read_csv("data/sentinal.csv") |>
  filter(year ==2018) |>
  mutate(Zone = "Control",
         grp = paste(site, Zone, sep = "_"))

# Combine the datasets and filter out abiotic records
marshstart <- rbind(tlpstart, sentinalstart) |>
  filter(!species %in% c("Bare", "Open Water", "Open water", "Atriplex", "Atriplex unknown", "Burrows",
                         "Fiddler Crab Burrows", "Salicornia spp.", "Stone", "Sueda", "Thatch",
                         "Thatch average of 5 measurements", "Unknonwn grass", "Unknown Grass",
                         "Unknown grass", "Wrack")) |>
  arrange(species) |>
  mutate(count = replace_na(count, 0)) |>
  mutate(count = as.numeric(count))

# Pivot out species
startWide <- marshstart |>
  pivot_wider(names_from = species, 
              values_from = count,
              values_fill = 0) |>
  arrange(Plot)

#### Calculate Bray-Curtis Dissimilarity matrix
library(ecodist)
dist <- ecodist::bcdist(startWide[,-c(1:5)], rmzero = 0)
summary(dist) # No NAs- performs better than vegan

# IMPORTANT: Turn output into matrix
dmat <- as.matrix(dist)

# Set row and column names
rownames(dmat) <- startWide$Plot
colnames(dmat) <- startWide$Plot

# Pivot into a table
hm <- as_tibble(dmat, rownames = NA) |>
  rownames_to_column() |>
  pivot_longer(cols = -c(rowname),
               names_to = "colnames",
               values_to = "bc")

# Mapping key
key <- startWide |>
  select(grp, Plot) |>
  distinct() |>
  mutate(rowname = Plot, 
         colnames = Plot)

# Use the key to add group info to the bc table
hm <- hm |>
  left_join(key, by = "rowname", relationship = "many-to-many") |>
  select(-Plot, -colnames.y, -rowname) |>
  rename(rowgrp = grp,
         colnames = colnames.x) |>
  left_join(key, by = "colnames", relationship = "many-to-many") |>
  select(-Plot, -colnames, -rowname) |>
  rename(colgrp = grp)

# Make the plot for 2018
ggplot(data = hm,
       mapping = aes(x = as.factor(rowgrp), y = as.factor(colgrp))) +
  geom_tile(aes(fill = bc)) +
  scale_fill_viridis_c()

#### 2023 Heatmap
# Load in each data file and filter for years after treatment
tlpend <- read_csv("data/tlp.csv") |>
  filter(year ==2023,
         Plot != "I1-74*") |>
  select(species, year, site, Plot, count, Zone) |>
  mutate(grp = paste(site, Zone, sep = "_"))

sentinalend <- read_csv("data/sentinal.csv") |>
  filter(year ==2023) |>
  mutate(Zone = "Control",
         grp = paste(site, Zone, sep = "_"))

# Combine the datasets and filter out abiotic records
marshend <- rbind(tlpend, sentinalend) |>
  filter(!species %in% c("Bare", "Open Water", "Open water", "Atriplex", "Atriplex unknown", "Burrows",
                         "Fiddler Crab Burrows", "Salicornia spp.", "Stone", "Sueda", "Thatch",
                         "Thatch average of 5 measurements", "Unknonwn grass", "Unknown Grass",
                         "Unknown grass", "Wrack")) |>
  arrange(species) |>
  mutate(count = replace_na(count, 0)) |>
  mutate(count = as.numeric(count))

# Pivot out species
endWide <- marshend |>
  pivot_wider(names_from = species, 
              values_from = count,
              values_fill = 0) |>
  arrange(Plot)

#### Calculate Bray-Curtis Dissimilarity matrix
library(ecodist)
dist <- ecodist::bcdist(endWide[,-c(1:5)], rmzero = 0)
summary(dist) # No NAs- performs better than vegan

# IMPORTANT: Turn output into matrix
dmat <- as.matrix(dist)

# Set row and column names
rownames(dmat) <- endWide$Plot
colnames(dmat) <- endWide$Plot

# Pivot into a table
hmend <- as_tibble(dmat, rownames = NA) |>
  rownames_to_column() |>
  pivot_longer(cols = -c(rowname),
               names_to = "colnames",
               values_to = "bc")

# Mapping key
keyend <- endWide |>
  select(grp, Plot) |>
  distinct() |>
  mutate(rowname = Plot, 
         colnames = Plot)

# Use the key to add group info to the bc table
hmend <- hmend |>
  left_join(keyend, by = "rowname", relationship = "many-to-many") |>
  select(-Plot, -colnames.y, -rowname) |>
  rename(rowgrp = grp,
         colnames = colnames.x) |>
  left_join(keyend, by = "colnames", relationship = "many-to-many") |>
  select(-Plot, -colnames, -rowname) |>
  rename(colgrp = grp)

# Make the plot for 2018
ggplot(data = hmend,
       mapping = aes(x = as.factor(rowgrp), y = as.factor(colgrp))) +
  geom_tile(aes(fill = bc)) +
  scale_fill_viridis_c()

