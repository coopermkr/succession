#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ninigret Diff-In-Diff
#' @date 2024-07-17
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#### Diff-In-Diff

# Load libraries
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(grid)

# Load and mutate data to indicate treatment groups
diversity <- read_csv("data/ninigret_diversity.csv") |>
  filter(Plot != "I1-74") |>
  mutate(treated = Zone == 'Impact' &
           year %in% c('2018', '2019', '2020'),
         st = Zone == "Impact",
         normShan = shannon/max(shannon)) # Normalize Shannon from 0-1
summary(diversity)

# Check paralell trends
parTrends <- diversity |>
  group_by(Zone, year) |>
  summarize(m = mean(normShan))

ggplot(data = parTrends,
       mapping = aes(x = year, y = m, group = Zone, color = Zone)) +
  geom_line(aes(linewidth = 1)) +
  theme_pubr() +
  labs(x = "Year",
       y = "Mean Normalized Shannon Diversity Index",
       title = "Mean Diversity of Zone Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

# According to Kenney's paper, sediment placemnt happened in winter of 2016-2017
# and plugs were planted in the growing season of 2017.
# I think it makes sense to filter out 2017 from the analysis as the year of the application

diversity <- diversity |>
  filter(year != 2017)

#### Make a figure
# Calculate effects on diversity for treated/untreated before/after treatment
didLine <- diversity |>
  group_by(st, year) |>
  summarize(m = mean(shannon))

didSeg <- diversity |>
  group_by(st, year > 2017) |>
  summarize(m = mean(shannon))


# Create a plot showing the points for each plot in the background
didPlot <- ggplot(diversity, mapping = aes(x = year,
                                y = shannon,
                                color = Zone, shape = Zone)) +
  geom_point(position = position_jitter(width = .3, seed = 10), size = 2) +
  geom_vline(aes(xintercept = 2017), linetype = 'dashed', linewidth = 0.5) +
  scale_color_manual(labels = c("Control", "Impact"), values = c('darkorange', 'darkgreen')) +
  scale_shape_manual(values = c(3, 1)) +
  theme_pubr() +
  labs(x = "Year",
       y = "Shannon Diversity Index") +
  theme(plot.title = element_text(hjust = 0.5))

# Add in lines with different values for each year
didPlotLines <- didPlot +
  geom_line(aes(x = year, y = m),
            data = didLine, linewidth = 1)

# And lines showing the overall averages before and after treatment
didPlotSegs <- didPlot +
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[1], yend = didSeg$m[1]),
               linewidth = 1.5, color = "darkorange", linetype = "dashed") +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = didSeg$m[2], yend = didSeg$m[2]),
               linewidth = 1.5, color = "darkorange", linetype = "dashed") +
  
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[3], yend = didSeg$m[3]),
               linewidth = 1.5, color = "darkgreen") +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = didSeg$m[4], yend = didSeg$m[4]),
               linewidth = 1.5, color = "darkgreen") +
  geom_text(x = 2017, y = 1.35, label = "Impact", color = "darkgreen") +
  geom_text(x = 2017, y = .9, label = "Control", color = "darkorange") +
  labs(title = "Unadjusted Means")

didPlotSegs

# Now norm based on changes in the control to actually show Diff-In-Diff
# Subtract the change in the control from the post-treatment Impact group
didAdjusted <- didPlot +
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[1], yend = didSeg$m[1]),
               linewidth = 1.5, color = "darkorange", linetype = "dashed") +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = (didSeg$m[2]-(didSeg$m[2]-didSeg$m[1])), 
                   yend = didSeg$m[2]-(didSeg$m[2]-didSeg$m[1])),
               linewidth = 1.5, color = "darkorange", linetype = "dashed") +
  
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[3], yend = didSeg$m[3]),
               linewidth = 1.5, color = "darkgreen") +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = (didSeg$m[4]-(didSeg$m[2]-didSeg$m[1])), 
                   yend = (didSeg$m[4]-(didSeg$m[2]-didSeg$m[1]))),
               linewidth = 1.5, color = "darkgreen") +
  geom_text(x = 2017, y = 1.35, label = "Impact", color = "darkgreen") +
  geom_text(x = 2017, y = .9, label = "Control", color = "darkorange") +
  labs(title = "Diff-In-Diff Adjusted Means")

didAdjusted

# Create panel plot
grid.arrange(didPlotSegs, didAdjusted, ncol = 2, 
             top=textGrob("Sediment Placement Increases Plot Diversity",
                          gp=gpar(fontsize = 20),
                          just = "centre"))

# Save as .png
png("out/shannonDID.png", width = 800, height = 600)
grid.arrange(didPlotSegs, didAdjusted, ncol = 2, 
             top=textGrob("Sediment Placement Increases Plot Diversity",
                          gp=gpar(fontsize = 20),
                          just = "centre"))
dev.off()
