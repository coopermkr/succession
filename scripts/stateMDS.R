#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ecosystem State MDS
#' @date 2025-04-12
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Load libraries
library(smacof)
library(tidyverse)
library(grid)
library(gridExtra)

# Load in each data file and filter for years after treatment
tlpstart <- read_csv("data/tlp.csv") |>
  filter(year == 2018,
         !Plot %in% c("I1-74*", "0-00")) |>
  select(species, year, site, Plot, count, Zone) |>
  mutate(grp = paste(site, Zone, sep = "_"),
         id = paste(grp, Plot, sep = "_"))

sentinalstart <- read_csv("data/sentinal.csv") |>
  filter(year == 2018) |>
  mutate(Zone = "Control",
         grp = paste(site, Zone, sep = "_"),
         id = paste(grp, Plot, sep = "_"))

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
  arrange(id)

#### Calculate Bray-Curtis Dissimilarity matrix
library(ecodist)
dist <- ecodist::bcdist(startWide[,-c(1:6)], rmzero = 0)
summary(dist) # No NAs- performs better than vegan

# IMPORTANT: Turn output into matrix
dmat <- as.matrix(dist)

# Set row and column names
rownames(dmat) <- startWide$id
colnames(dmat) <- startWide$id

# Convert to MDS
dmds <- cmdscale(dmat, k = 2, eig = TRUE)

mdsPoints <- as.data.frame(dmds$points) |>
  mutate(id = rownames(dmds$points)) |>
  separate(col = id, into = c("Site", "Treat", "Plot"), sep = "_") |>
  mutate(Treatment = paste(Site, Treat, sep = "_")) |>
  rename(Axis1 = V1,
         Axis2 = V2) |>
  mutate(Groups = case_match(Treatment, "Coggeshall_Control" ~ "Sentinel", 
                             "nag_Control" ~ "Sentinel", 
                             "ninigret_Control" ~ "Drowning Control", 
                             "ninigret_Impact" ~ "Drowning Treated", 
                             "quonnie_East" ~ "Drowning Treated", 
                             "quonnie_West" ~ "Drowning Control"))


# Plot 2018
mds2018 <- mdsPoints |>
  ggplot(mapping = aes(x = Axis1, y = Axis2, color = Groups, shape = Groups)) +
  geom_point(size = 3) +
  ggpubr::theme_pubr(base_size = 16) +
  labs(title = "2018 (one year post-sediment)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(labels = c("Drowning Control", "Drowning Treated", "Sentinel"), 
                     values = c('#BD288B', '#28BD5A', '#2841BD'), name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL)



#### Plot end of study
# Load in each data file and filter for years after treatment
tlpend <- read_csv("data/tlp.csv") |>
  filter(year == 2023,
         !Plot %in% c("I1-74*", "0-00")) |>
  select(species, year, site, Plot, count, Zone) |>
  mutate(grp = paste(site, Zone, sep = "_"),
         id = paste(grp, Plot, sep = "_"))

sentinalend <- read_csv("data/sentinal.csv") |>
  filter(year == 2023) |>
  mutate(Zone = "Control",
         grp = paste(site, Zone, sep = "_"),
         id = paste(grp, Plot, sep = "_"))

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
  arrange(id)

#### Calculate Bray-Curtis Dissimilarity matrix
library(ecodist)
enddist <- ecodist::bcdist(endWide[,-c(1:6)], rmzero = 0)
summary(enddist) # No NAs- performs better than vegan

# IMPORTANT: Turn output into matrix
enddmat <- as.matrix(enddist)

# Set row and column names
rownames(enddmat) <- endWide$id
colnames(enddmat) <- endWide$id

# Convert to MDS
enddmds <- cmdscale(enddmat, k = 2, eig = TRUE)

siteMap <- setNames(unlist(c("Sentinel", "Sentinel", "Drowning Control", "Drowning Treated", "Drowning Treated", "Drowning Control")),
                    unlist(c("Coggeshall Control", "nag Control", "ninigret Control", "ninigret Impact", "quonnie East", "quonnie West")))

endmdsPoints <- as.data.frame(enddmds$points) |>
  mutate(id = rownames(enddmds$points)) |>
  separate(col = id, into = c("Site", "Treat", "Plot"), sep = "_") |>
  mutate(Treatment = paste(Site, Treat, sep = "_")) |>
  rename(Axis1 = V1,
         Axis2 = V2) |>
  mutate(Groups = case_match(Treatment, "Coggeshall_Control" ~ "Sentinel", 
                         "nag_Control" ~ "Sentinel", 
                         "ninigret_Control" ~ "Drowning Control", 
                         "ninigret_Impact" ~ "Drowning Treated", 
                         "quonnie_East" ~ "Drowning Treated", 
                         "quonnie_West" ~ "Drowning Control"))




# Plot 2023
mds2023 <- endmdsPoints |>
  ggplot(mapping = aes(x = Axis1, y = Axis2, color = Groups, shape = Groups)) +
  geom_point(size = 3) +
  ggpubr::theme_pubr(base_size = 16) +
  labs(title = "2023 (six years post-sediment)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(labels = c("Drowning Control", "Drowning Treated", "Sentinel"), 
                     values = c('#BD288B', '#28BD5A', '#2841BD'), name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL)


# Create panel plot
grid.arrange(mds2018, mds2023, ncol = 2, 
             top=textGrob("MDS Bray-Curtis Dissimilarity of Vegetation Structure",
                          gp=gpar(fontsize = 20),
                          just = "centre"))

# Save
png("out/vegetationMDS.png", width = 800, height = 600)
grid.arrange(mds2018, mds2023, ncol = 2, 
             top=textGrob("MDS Bray-Curtis Dissimilarity of Vegetation Structure",
                          gp=gpar(fontsize = 20),
                          just = "centre"))
dev.off()

