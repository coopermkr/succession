#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' Ninigret Diff-In-Diff Modeling
#' @date 2024-07-17
#' @author Cooper Kimball-Rhines
#' 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#Load libraries
library(tidyverse)
library(modelsummary)
library(performance)
library(fixest)
library(ggpubr)
library(emmeans)
library(ggdist)
library(modelr)

# Load and mutate data to indicate treatment groups
diversity <- read_csv("data/ninigret_diversity.csv") |>
  filter(Plot != "I1-74*",
         Plot != "I1-74",
         year != 2017) |>
  mutate(treated = Zone == 'Impact' &
           year %in% c('2018', '2019', '2020'),
         st = Zone == "Impact",
         normShan = shannon/max(shannon), # Normalize Shannon from 0-1
         year = as.numeric(year - 2017))
summary(diversity)

ggplot(data = diversity,
       mapping = aes(x = normShan, color = Zone, fill = Zone)) +
  geom_histogram() + # Holy zero inflation
  theme_pubr() +
  labs(title = "Normalized Plot Shannon Diversity") +
  theme(plot.title = element_text(hjust=0.5))
# Leaving out the zeros doesn't make biological sense, but it does make a normal distribution


#Are data normally distributed?
shapiro.test(diversity$normShan)

shapiro.test(asinh(diversity$normShan))

shapiro.test(sqrt(diversity$normShan))
# No, and a simple transformation won't make them normal
# It makes sense to model using the normalized Shannon numbers and a beta distribution GLM

#### DID ####
# Build diff-in-diff model with Zone, year, and Plot set as fixed effects
ninNorm <- feols(fml = normShan ~ treated  | Zone + year + Plot,
                 data = diversity)
ninNorm
check_model(ninNorm)
fixest::r2(ninNorm)
msummary(ninNorm, stars = c('*' = .1, '**' = .05, '***' = .01))


# Logit transform to mimic beta regression

logitDiv <- diversity |>
  mutate(logitShan = car::logit(normShan, percents = FALSE))

logitDif <- feols(fml = logitShan ~ treated | year + Plot,
                  data = logitDiv)

logitDif
check_model(logitDif)
fixest::r2(logitDif)
tidy(logitDif)

# Summarize model stats
msummary(logitDif, stars = c('*' = .1, '**' = .05, '***' = .01))

# Logit transforming before modeling works nicely
# But now we have to double reverse to get back to normal Shannon's Index
boot::inv.logit(1.711)*max(diversity$shannon)
# Sediment placement increases Shannon Diversity by 1.846

emmeans(logitDif, specs = ~ treated) |>
  contrast("pairwise") |> confint() # We get the same info from msummary

# Generate new points
ninPre <- data_grid(data = diversity,
                     Plot = unique(Plot),
                     year = unique(year))

ninPost <- data_grid(data = diversity,
                     Plot = unique(Plot),
                     year = unique(year))

ninPred <- rbind(ninPre, ninPost)
# Generate Zone and treatment assignments
pred <- ninPred |>
  mutate(Zone = ifelse(Plot %in% c("C1-00",  "C1-30",  "C2-00",  "C2-30",  
                                   "C2-60",  "C2-90",  "C2-120", "C2-150", 
                                   "C3-00",  "C3-30",  "C3-60",  "C3-90",  
                                   "C4-00", "C4-30",  "C4-60",  "C4-120", 
                                   "C5-00",  "C5-30",  "C5-60",  "C5-90"),
                       "Control", "Impact"),
         treated = Zone == 'Impact' &
           year > 0)

# Predict points with model
pred <- augment(logitDif, interval = "confidence", newdata = pred) |>
  mutate(shanPredicted = boot::inv.logit(.fitted)*max(diversity$shannon))

# Plot the points with two trend lines
ggplot(data = pred, mapping = aes(x = as.factor(year), y = shanPredicted, color = Zone, group = Zone)) +
  geom_point(data = diversity, mapping = aes(x = as.factor(year), y = shannon, 
                                             color = Zone, shape = Zone),
             position = position_jitter(width = .3, seed = 10), size = 2) +
  scale_color_manual(labels = c("Control", "Impact"), values = c('darkorange', 'darkgreen')) +
  scale_fill_manual(labels = c("Control", "Impact"), values = c('darkorange', 'darkgreen')) +
  scale_shape_manual(values = c(3, 1)) +
  theme_pubr() +
  geom_line(stat = "smooth") +
  labs(x = "Year",
       y = "Shannon Diversity Index",
       title = "Shannon Diversity Before and After Sediment Placement") +
  theme(plot.title = element_text(hjust = 0.5))

# And one with box plots
ggplot(data = pred, mapping = aes(x = as.factor(year), y = shanPredicted, fill = Zone)) +
  geom_point(data = diversity, mapping = aes(x = as.factor(year), y = shannon, 
                                             color = Zone, shape = Zone),
             position = position_jitter(width = .3, seed = 10), size = 2) +
  scale_color_manual(labels = c("Control", "Impact"), values = c('darkorange', 'darkgreen')) +
  scale_fill_manual(labels = c("Control", "Impact"), values = c('darkorange', 'darkgreen')) +
  scale_shape_manual(values = c(3, 1)) +
  theme_pubr() +
  geom_boxplot() +
labs(x = "Year",
     y = "Shannon Diversity Index",
     title = "Shannon Diversity Before and After Sediment Placement") +
  theme(plot.title = element_text(hjust = 0.5))





difPlot <- ggplot(data = diversity,
       mapping = aes(x = year,
                     y = shannon,
                     color = Zone)) +
  geom_point()

difPred <- cbind(diversity, predict(logitDif)) |>
  mutate(predShan = boot::inv.logit(`predict(logitDif)`)*max(diversity$shannon))

summary(difPred)




#### GLM ####
# Build a zero inflated beta regression with random effects
# Replace 1s in normShan with 0.99999
diversity$normShan <- replace(diversity$normShan,
                              diversity$normShan>0.99999,
                              0.99999)

# Construct a zero-inflated beta model
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
ninZIB <- glmmTMB(data = diversity,
                  formula = normShan ~ Zone:year + year + (1|Plot),
                  ziformula = ~ 1, # add for zero inflation
                  family = beta_family(link = "logit"))
ninZIB

# Assess model
check_predictions(ninZIB) # Looks really good
check_residuals(ninZIB) |> plot() # Comes back normal
check_autocorrelation(ninZIB) # This only happens once plot gets added as RE
check_homogeneity(ninZIB) # Looks good

# Query ZIB model
tidy(ninZIB)
confint(ninZIB)
glance(ninZIB)
summary(ninZIB)
r2(ninZIB) # This is awful

msummary(ninZIB, stars = c('*' = .1, '**' = .05, '***' = .01))

# Visualize
visreg::visreg(ninZIB, "Zone", by="year", breaks = c(1, 2, 3))

emmeans(ninZIB,
        specs = ~ treated | year) |>
  contrast("pairwise") |>
  confint()


