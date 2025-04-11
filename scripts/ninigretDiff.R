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
library(did)
library(ggpubr)
library(gridExtra)
library(grid)
# additional comment
# Load and mutate data to indicate treatment groups
diversity <- read_csv("data/ninigret_diversity.csv") |>
  filter(Plot != "I1-74") |>
  mutate(treated = Zone == 'Impact' &
           year > 2017,
         st = Zone == "Impact",
         normShan = shannon/max(shannon)) # Normalize Shannon from 0-1
summary(diversity)

# Check parallel trends
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

# Now a real test:
# Estimate group-group time average treatment effects
# Note, this function has some weird object requirements so we have to mess with
# our data formats a bit to get it to work
ptShan <- diversity |>
  mutate(firstTreat = as.numeric(Zone == "Impact")*2018,
         # Coerce plot into a numeric ID so att_gt doesn't throw a fit
         idNum = as.numeric(as.factor(Plot))) |>
  dplyr::select(shannon, year, idNum, firstTreat, treated)

set.seed(8675309)

trendAssess <- att_gt(yname = "shannon",
                      tname = "year",
                      idname = "idNum",
                      gname = "firstTreat",
                      data = ptShan,
                      panel = TRUE,
                      bstrap = TRUE,
                      cband = TRUE)

summary(trendAssess)

# Plot- Looks similarly good
ptPlot <- ggdid(trendAssess, grtitle = "Year", group = NULL) +
  labs(title = "Parallel Trends Analysis",
       subtitle = NULL,
       tag = NULL,
       x = "Year",
       y = "Effect Size") +
  clean_theme() +
  theme_pubr(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(labels = c("Untreated", "Treated"), values = c('darkorange', 'darkgreen'))
  

png("out/parTrends.png", width = 800, height = 600)
ptPlot
dev.off()

#### Make a figure
# Calculate effects on diversity for treated/untreated before/after treatment
didEB <- diversity |>
  group_by(st, year) |>
  summarize(m = mean(shannon),
            n = n(),
            gse = sd(shannon)/sqrt(n()),
            stdev = sd(shannon))

## Boxplot
didErrorPlot <- ggplot(data = didEB,
                       mapping = aes(x = year, y = m, 
                                     color = st)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = m - gse,
                              ymax = m + gse)) +
  theme_pubr(base_size = 16) +
  labs(x = "Year",
       y = "Shannon Diversity Index",
       title = "Sediment Placement Plot Diversity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = 2017), linetype = 'dashed', linewidth = 0.5) +
  scale_color_manual(labels = c("Untreated", "Treated"), 
                     values = c('darkorange', 'darkgreen'), name = NULL)


png("out/didErrorbar.png", width = 800, height = 600)
didErrorPlot
dev.off()

# Recalculate grand means for before and after policy implementation
didSeg <- diversity |>
  # Calculate mean and sum of squares for the standard error weighted by sample size
  group_by(st, year > 2017) |>
  summarize(m = mean(shannon),
            n = n(),
            gse = sd(shannon)/sqrt(n()),
            stdev = sd(shannon))

# Create a nice base plot showing the points for each plot in the background
didPlot <- diversity |>
  ggplot(mapping = aes(x = year, y = shannon,
                       color = `Zone`, shape = `Zone`)) +
  geom_point(position = position_jitter(width = .5, seed = 10), size = 1, alpha = 0.9) +
  geom_vline(aes(xintercept = 2017), linetype = 'dashed', linewidth = 0.5) +
  scale_color_manual(labels = c("Untreated", "Treated"), 
                     values = c('darkorange', 'darkgreen'), name = NULL) +
  scale_shape_manual(labels = c("Untreated", "Treated"), 
                     values = c(0, 2), name = NULL) +
  theme_pubr() +
  labs(x = "Year",
       y = "Shannon Diversity Index") +
  theme(plot.title = element_text(hjust = 0.5))

# And lines showing the overall averages before and after treatment

didPlotSegs <- didPlot +
  # Unaffected Pretreatment
  geom_rect(xmin = 2015, xmax = 2017,
            ymin = didSeg$m[1] - didSeg$gse[1], 
            ymax = didSeg$m[1] + didSeg$gse[1],
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[1], yend = didSeg$m[1]),
               linewidth = 1, color = "darkorange", linetype = "dashed") +
  
  #Unaffected Posttreatment
  geom_rect(xmin = 2017, xmax = 2023,
            ymin = didSeg$m[2] - didSeg$gse[2], 
            ymax = didSeg$m[2] + didSeg$gse[2],
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = didSeg$m[2], yend = didSeg$m[2]),
               linewidth = 1, color = "darkorange", linetype = "dashed") +
  
  # Affected Pretreatment
  geom_rect(xmin = 2015, xmax = 2017,
            ymin = didSeg$m[3] - didSeg$gse[3], 
            ymax = didSeg$m[3] + didSeg$gse[3],
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[3], yend = didSeg$m[3]),
               linewidth = 1, color = "darkgreen", linetype = "dashed") +
  
  # Affected Posttreatment
  geom_rect(xmin = 2017, xmax = 2023,
            ymin = didSeg$m[4] - didSeg$gse[4], 
            ymax = didSeg$m[4] + didSeg$gse[4],
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = didSeg$m[4], yend = didSeg$m[4]),
               linewidth = 1, color = "darkgreen", linetype = "dashed") +
  
  # Add labels
  geom_text(x = 2017, y = 0.9, label = "Untreated", color = "darkorange") +
  geom_text(x = 2017, y = 1.3, label = "Treated", color = "darkgreen") +
  labs(title = "Unadjusted Means")

didPlotSegs

didAdjusted <- didPlot +
  # Unaffected Pretreatment
  geom_rect(xmin = 2015, xmax = 2017,
            ymin = didSeg$m[1] - didSeg$gse[1], 
            ymax = didSeg$m[1] + didSeg$gse[1],
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[1], yend = didSeg$m[1]),
               linewidth = 1, color = "darkorange", linetype = "dashed") +
  
  #Unaffected Posttreatment
  geom_rect(xmin = 2017, xmax = 2023,
            ymin = didSeg$m[2]-(didSeg$m[2]-didSeg$m[1]) - sqrt(didSeg$gse[1]^2 + 2*didSeg$gse[2]^2), 
            ymax = didSeg$m[2]-(didSeg$m[2]-didSeg$m[1]) + sqrt(didSeg$gse[1]^2 + 2*didSeg$gse[2]^2),
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = (didSeg$m[2]-(didSeg$m[2]-didSeg$m[1])), 
                   yend = didSeg$m[2]-(didSeg$m[2]-didSeg$m[1])),
               linewidth = 1, color = "darkorange", linetype = "dashed") +
  
  # Affected Pretreatment
  geom_rect(xmin = 2015, xmax = 2017,
            ymin = didSeg$m[3] - didSeg$gse[3], 
            ymax = didSeg$m[3] + didSeg$gse[3],
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = min(year), xend = 2017,
                   y = didSeg$m[3], yend = didSeg$m[3]),
               linewidth = 1, color = "darkgreen", linetype = "dashed") +
  
  # Affected Posttreatment
  geom_rect(xmin = 2017, xmax = 2023,
            ymin = didSeg$m[4]-(didSeg$m[2]-didSeg$m[1]) - sqrt(didSeg$gse[4]^2 + didSeg$gse[1]^2 + didSeg$gse[2]^2), 
            ymax = didSeg$m[4]-(didSeg$m[2]-didSeg$m[1]) + sqrt(didSeg$gse[4]^2 + didSeg$gse[1]^2 + didSeg$gse[2]^2),
            fill = "lightgrey", color = "lightgrey", alpha = 0.1) +
  
  geom_segment(aes(x = 2017, xend = max(year),
                   y = (didSeg$m[4]-(didSeg$m[2]-didSeg$m[1])), 
                   yend = (didSeg$m[4]-(didSeg$m[2]-didSeg$m[1]))),
               linewidth = 1, color = "darkgreen", linetype = "dashed") +
  
  # Add Labels
  geom_text(x = 2017, y = 0.95, label = "Untreated", color = "darkorange") +
  geom_text(x = 2017, y = 1.45, label = "Treated", color = "darkgreen") +
  labs(title = "Adjusted Means")

didAdjusted

# Create panel plot
grid.arrange(didPlotSegs, didAdjusted, ncol = 2, 
             top=textGrob("Sediment Placement and Planting Increases Diversity",
                          gp=gpar(fontsize = 16),
                          just = "centre"))

# Save as .png
png("out/shannonDID.png", width = 1000, height = 600)
grid.arrange(didPlotSegs, didAdjusted, ncol = 2, 
             top=textGrob("State Forest Diversity is Stable in a Backdrop of Decline",
                          gp=gpar(fontsize = 20),
                          just = "centre"))
dev.off()



#### Past DID Graph
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
