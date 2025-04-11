#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ecological Trajectory
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

# Load in each data file and filter for years after treatment
tlp <- read_csv("data/tlp.csv") |>
  filter(year > 2017,
         year < 2024) |>
  select(species, year, site, Plot, count, Zone) |>
  mutate(grp = paste(site, Zone, sep = "_"))

sentinal <- read_csv("data/sentinal.csv") |>
  filter(year > 2017,
         year < 2024) |>
  mutate(Zone = "Control",
         grp = paste(site, Zone, sep = "_"))

# Combine the datasets and filter out abiotic records
allMarsh <- rbind(tlp, sentinal) |>
  filter(!species %in% c("Bare", "Open Water", "Open water", "Atriplex", "Atriplex unknown", "Burrows",
                         "Fiddler Crab Burrows", "Salicornia spp.", "Stone", "Sueda", "Thatch",
                         "Thatch average of 5 measurements", "Unknonwn grass", "Unknown Grass",
                         "Unknown grass", "Wrack")) |>
  arrange(species) |>
  mutate(count = replace_na(count, 0)) |>
  mutate(count = as.numeric(count))

# Pivot out species
marshWide <- allMarsh |>
  pivot_wider(names_from = species, 
              values_from = count,
              values_fill = 0) |>
  
  # Combine results for typos
  mutate(Ammophila_brevigulata = `Ammophila brevigulata` + `Ammophila breviligulata`,
         Aster_tenuifolium = `Aster tenuifolium` + `Aster tenuifolius`,
         Atriplex_cristata = `Atriplex  cristata` + `Atriplex  crsitata` + `Atriplex cristata`,
         Cakile_edentula = `Cakile edentula` + `Cakile endentula`,
         Chenopodium_glaucum = `Chenopodium glaucum` + `Chenopodium glocum`,
         Myrica_pennsylvanica = `Myrica pennsylvanica` + `Myrica pensylvanica`,
         Schoenoplectus_pungens = `Schoen-oplectus pungens` + `Schoenoplectus pungens`,
         Suaeda_linearis = `Suaeda linearis` + `Sueda linearis`) |>
  
  # And drop the typo columns
  select(!c(`Ammophila brevigulata`, `Ammophila breviligulata`,
            `Aster tenuifolium`, `Aster tenuifolius`,
            `Atriplex  cristata`, `Atriplex  crsitata`, `Atriplex cristata`,
            `Cakile edentula`, `Cakile endentula`,
            `Chenopodium glaucum`, `Chenopodium glocum`,
            `Myrica pennsylvanica`, `Myrica pensylvanica`,
            `Schoen-oplectus pungens`, `Schoenoplectus pungens`,
            `Suaeda linearis`, `Sueda linearis`)) |>
  
  # Drop columns that only have 0s
  select(!c(`Setaria spp.`, `Pluchea purpura-scens`, `Cyperus (umbrella sedge)`,
            `Agalinis maritima`))

# Calculate Bray-Curtis Dissimilarity matrix
library(ecodist)
dist <- ecodist::bcdist(marshWide[,-c(1:5)])
summary(dist) # No NAs- performs better than vegan

# Store metadata
sites <- marshWide$grp
surveys <- as.numeric(as.factor(marshWide$Plot))
time <- as.numeric(as.factor(marshWide$year))

# Define trajectories
traj <- defineTrajectories(dist, sites, surveys, time)

# Graph PCA
pca <- trajectoryPCoA(traj, traj.colors = c("black", "red", "blue", "green", "orange", "purple"))

# Trajectory metrics
trajectoryLengths(traj)
trajectoryVariability(traj)
trajectoryAngles(traj)

trajectoryConvergence(traj)
trajectoryConvergence(traj, symmetric = FALSE)

# Trajectory resemblances
tDist <- trajectoryDistances(traj)

# Plot trajectory similarity by MDS
simMDS <- smacof::mds(tDist)

mdsPoints <- data.frame(site = rownames(simMDS$conf[]),
                        x = simMDS$conf[,1],
                        y = simMDS$conf[,2])

ggplot(data = mdsPoints,
       mapping = aes(x = x, y = y, color = site)) +
  geom_point(size = 3) +
  theme_classic()
