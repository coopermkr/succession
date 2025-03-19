#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ninigret Ecological Trajectory
#' @date 2024-07-23
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Load libraries
#remotes::install_github("emf-creaf/ecotraj")
library(ecotraj)
library(smacof)
library(tidyverse)
library(vegan)

# Load in data and filter out treatment year
plotCounts <- read_csv("data/processed.csv") |>
  filter(year != 2017) |>
  filter(Plot = c(C1-00, C2-00, I2-74, I4-74))

# Calculate dissimilarity matrix using all of the species, open, and bare columns
dist <- vegdist(plotCounts[,-c(1:7)], method = "bray")
summary(dist)

# Store metadata (which isn't included in the matrix)
sites <- plotCounts$Plot
surveys <- plotCounts$year

# Graph PCA
trajectoryPCoA(dist, sites, surveys, survey.labels = T)

