#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ninigret Ecological Regime
#' @date 2024-07-23
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Load packages
devtools::install_github("MSPinillos/ecoregime")
library(ecoregime)
library(vegan)

# Example from github: https://mspinillos.github.io/ecoregime/

variables <- data.frame(EDR_data$EDR3$abundance)

d <- vegdist(variables[, -c(1:3)])

# Identify sites and states in d
sites <- variables$traj
states <- as.integer(variables$state)

# Compute RETRA-EDR
RT <- retra_edr(d = d, trajectories = sites, states = states,
                minSegs = 5)

# Plot representative trajectories
plot(x = RT, d = d, trajectories = sites, states = states, select_RT = "T4",
     traj.colors = "lightblue", RT.colors = "orange", sel.color = "darkgreen",
     link.lty = 1, asp = 1, main = "Representative trajectories - EDR")


