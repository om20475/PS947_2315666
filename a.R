library(dplyr)
library(tidyverse)  # includes dplyr, ggplot2, tidyr
library(randomForest)
library(caret)
library(rpart)
library(glmnet)
library(ggplot2)
library(readxl)
install.packages(c("ggplot2", "pwr"))
library(pwr)
brain_body <-  read.csv(file.choose())
brain_body

# Step 1: Estimate Plausible Effect Size
effect_size <- 0.5

# Step 2: Simulate Data
set.seed(123)
n_species <- 100  
body_weight <- rnorm(n_species, mean = 0, sd = 1)
brain_weight <- effect_size * body_weight + rnorm(n_species, mean = 0, sd = 1)

# Plot the simulated data
simulated_data <- data.frame(body_weight, brain_weight)
ggplot(simulated_data, aes(x = body_weight, y = brain_weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Simulated Data", x = "Body Weight (Standardized)", y = "Brain Weight (Standardized)")

# Step 3: Power Analysis
alpha <- 0.05
power <- 0.9

# Two-sided t-test for a medium effect size
result <- pwr.t.test(d = effect_size, sig.level = alpha, power = power, type = "two.sample")

# Print the sample size needed for 90% power
cat("Sample size needed for 90% power:", ceiling(result$n))







df_agg <- big5data %>%
  group_by(big5) %>%
  summarise(avg_score = mean(score), sd_score = sd(score))



ggplot(df_agg, aes(x = big5, y = avg_score, fill = big5)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_score - sd_score, ymax = avg_score + sd_score), width = 0.2, position = position_dodge(0.9))+
  geom_pointrange(aes(ymin=avg_score - sd_score, ymax=avg_score + sd_score))+
  xlab("Big Five Personality Traits") + ylab("Average Score") +
  ggtitle("Average Scores for Big Five Personality Traits") +
  theme_minimal()