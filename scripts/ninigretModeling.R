#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ninigret Diversity Modeling
#' @date 2024-07-02
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#### Load libraries and data
library(tidyverse)
library(performance)
library(broom.mixed)
library(broom)
library(emmeans)
library(modelr)
library(lme4)
library(ggpubr)

diversity <- read_csv("data/ninigret_diversity.csv")
summary(diversity)

# According to Kenney's paper, sediment placemnt happened in winter of 2016-2017
# and plugs were planted in the growing season of 2017
# So I'm going to create an extra column based on year saying if it's before or after treatment

# Also, filter out 2017 so we're only looking at pre and post treatments
diversity <- diversity |>
  mutate(treatment = year) |>
  filter(year != 2017,
         Plot != "I1-74") #remove I1-74 because it did not receive treatment

diversity$treatment <- ifelse(diversity$treatment < 2017, "pre", "applied")

# Viz
ggplot(data = diversity,
       mapping = aes(x = treatment,
                     y = shannon,
                     color = Zone)) +
  geom_boxplot() +
  facet_wrap(vars(Zone))

#### Modeling
# I don't know if this should be an interaction effect or not! I keep going back and forth
lmeShan <- lmer(data = diversity,
                  shannon ~ Zone * treatment +
                    (1|Plot))
# Check model
lmeShan
check_predictions(lmeShan) |> plot()
check_autocorrelation(lmeShan)
check_heteroscedasticity(lmeShan) |> plot()
check_outliers(lmeShan) |> plot() #Plot I1-74 was causing a huge problem, now it's mostly fixed

# Query Model
summary(lmeShan)
broom.mixed::tidy(lmeShan)
effectsize::effectsize(lmeShan)

# This should really be done by Diff-in-diff but this looks promising!
anova(lmeShan)
confint(lmeShan)

# Make a figure
pal <- c("#34eb7a", "#2c8ad1")
my_comparisons <- list(c("Post", "Pre"), c("Impact", "Control"))

shanPlot <- ggplot(data = diversity,
                   mapping = aes(x = Zone,
                                 y = shannon,
                                 fill = treatment)) +
  geom_boxplot() +
  stat_compare_means(method = "anova", paired = TRUE, label.y = 2.13) +
  #stat_compare_means(comparisons = my_comparisons) +
  scale_fill_manual(values = pal) +
  labs(title = "Sediment Placement Effect \non Shannon Diversity") +
  xlab("Treatment Group") +
  ylab("Shannon Diversity Index") +
  theme_pubr(base_size = 16)
shanPlot # Keep
