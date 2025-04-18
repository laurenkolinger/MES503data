###########################
# Lab Assignment: LMER demonstration
# Author: Lauren Olinger
# Date: 17 April 2025
# Course: MES 504
###########################

# Setting Working Directory
# setwd("YOUR_WORKING_DIRECTORY")

# Load required libraries
library(lme4)       # Core package for fitting mixed models
library(lmerTest)   # Provides p-values in lmer summaries (using Satterthwaite's method)
library(broom.mixed)# For tidying model output into data frames
library(dplyr)      # For data manipulation
library(knitr)      # For creating formatted tables (kable)
library(ggplot2)    # For plotting
library(ggeffects)  # For plotting marginal effects
library(patchwork)  # For combining plots
library(emmeans)    # For estimated marginal means

#import data
coral_data <- read.csv("https://raw.githubusercontent.com/laurenkolinger/MES503data/main/504_week13/coral_growth.csv")

head(coral_data)

# convert chr to factor for the analysis 
coral_data$site <- factor(coral_data$site)
coral_data$colony <- factor(coral_data$colony)

# plot to explore 
ggplot(coral_data, aes(x= depth, y= growth, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # Site-specific trends

# plot to explore temperature vs growth 
ggplot(coral_data, aes(x= temperature, y= growth, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # Site-specific trends

# plot to explore light vs growth 
ggplot(coral_data, aes(x= light, y= growth, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # Site-specific trends

# mixed effect

# for comparing mixed effects (testing combinations of random/nested , etc) and for interpreting final model outputs. 
model1_reml <- lmer(growth ~ depth + temperature + light + (1|site), 
                    data = coral_data, 
                    REML = TRUE)

# for comparing fixed effects for model selection with anova() likelihood ratio tests
model1_ml <- lmer(growth ~ depth + temperature + light + (1|site), 
                    data = coral_data, 
                    REML = FALSE)

summary(model1_reml)

# selecting fixed effects 

f0 <- lmer(growth ~ 1 + (1|site), data = coral_data, REML = FALSE)

f1 <- lmer(growth ~ depth + (1|site), data = coral_data, REML = FALSE)

f2 <- lmer(growth ~ depth + temperature + (1|site), data = coral_data, REML = FALSE)

f3 <- lmer(growth ~ depth + temperature + light + (1|site), data = coral_data, REML = FALSE)

# likelihood ratio test 
anova(f0, f1, f2, f3) 

# best model is f3 

# using information criteria 
AIC(f0, f1, f2, f3) 
BIC(f0, f1, f2, f3)


# selecting random effects 

f1_reml <- lmer(growth ~ depth + temperature + light + (1|site), 
                data = coral_data, 
                REML = TRUE) 

f2_reml <- lmer(growth ~ depth + temperature + light + (1 + depth | site), 
                data = coral_data, 
                REML = TRUE) 

f3_reml <- lmer(growth ~ depth + temperature + light + (1 | site/colony), 
                data = coral_data, 
                REML = TRUE) 

# look at f2_reml 
summary(f2_reml)
# excluded f2 on the basis of convergence 

# Compare using AIC/BIC (REML fits)
AIC(f1_reml, f2_reml, f3_reml)
BIC(f1_reml, f2_reml, f3_reml)

summary(f3_reml)
# exclude f3 on the basis of information criteria.
# also exclude on the basis of amount of variation accounted for by colony:site in the summary output 

# compare lmer to lm with equivalent factors 
# use information criteria 

# the best model from the fixed effects selection with REML = F 
summary(f3) 

# the lm equivalent 
lm_f3 <- lm(growth ~ depth + temperature + light, data= coral_data)
summary(lm_f3)

# do the information criteria test (which is lower?)
AIC(f3, lm_f3)
BIC(f3, lm_f3)

# model with the random effect of site is better (lower AIC/BIC)

# Extract residuals
residuals <- residuals(f3)

# Plot residuals vs. fitted values
plot(fitted(f3), residuals, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, lty = 2)

# qq plot 
qqnorm(residuals)
qqline(residuals)

#for final interpretation, always use the REML model!!!
summary(f1_reml)

Growth decreased significantly with depth (β = -0.39 ± 0.048 SE, t(91.7) = -8.15, p < 0.001), while increasing significantly with higher temperature (β = 0.67 ± 0.14 SE, t(86.4) = 4.78, p < 0.001) and greater light intensity (β = 0.030 ± 0.0099 SE, t(90.7) = 2.99, p = 0.004). Substantial variation in baseline growth rates among sites was captured by the random intercepts (Table 1, σ_site = 2.36) compared to the residual variation (σ_residual = 0.78), highlighting the importance of the mixed model structure (Figure 2).


# Generate predictions for each predictor
pred_depth <- ggpredict(f1_reml, terms = "depth")
pred_temp <- ggpredict(f1_reml, terms = "temperature")
pred_light <- ggpredict(f1_reml, terms = "light")

# Create individual plots
plot_depth <- plot(pred_depth) + 
  labs(x = "Depth (m)", y = "Predicted Growth (mm/year)", title = NULL) + # Removed redundant title
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_temp <- plot(pred_temp) + 
  labs(x = "Temperature (°C)", y = NULL, title = NULL) + # Removed y-axis label and title
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), # Remove y-axis text
        axis.ticks.y = element_blank()) # Remove y-axis ticks

plot_light <- plot(pred_light) + 
  labs(x = "Light Intensity (relative)", y = NULL, title = NULL) + # Removed y-axis label and title
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), # Remove y-axis text
        axis.ticks.y = element_blank()) # Remove y-axis ticks

# Combine plots
combined_plot <- plot_depth + plot_temp + plot_light + 
  plot_layout(nrow = 1) 

print(combined_plot)

