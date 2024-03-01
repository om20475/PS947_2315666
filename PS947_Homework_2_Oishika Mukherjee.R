#------------ Adding required libraries -----------#

library('dplyr')
library('ggplot2')
library('tidyverse')
library(car)
library(ggthemes)
library(RColorBrewer)
library(wesanderson)
library(lme4)


#-----1.1 Plotting ------#



#read in aifaces.csv#

aifaces <- read.csv(file.choose())
head(aifaces, 15)
aifaces$real_or_sim  <- factor(aifaces$real_or_sim)


#summarise the data to look at how accuracy depends on face-type
grouping <- group_by(aifaces, real_or_sim)
aifaces_summary <- summarise(grouping, mean_accuracy = mean(acc))
aifaces_summary

#plotting the summarized data with the help of barplot
ggplot(aifaces_summary, aes(real_or_sim , mean_accuracy, fill = real_or_sim)) + geom_col(position = "dodge") +
  xlab('Face type') + ylab('Mean accuracy') + labs(fill = "Real or Simulated")+
  theme_few() + scale_fill_few()+ggtitle("Accuracy VS FaceType")

# Explanation:- We can observe that The mean accuracy for real faces is 0.56. 
# This means that when the face type was real, the correct prediction 
# was made about 56% of the time. The mean accuracy for simulated faces is lower, 
# at around 0.28. This means that when the face type was simulated, 
# the correct prediction was made about 28% of the time.Therefore, it is evident 
# that accuracy of predictions is much higher when the face type is real compared
# to when it is simulated.



#----------------- 1.2 Fit a model---------#

# fitting binomial GLM function #
acc_model <- glm(acc ~ real_or_sim, data = aifaces, family = "binomial")
summary(acc_model)

# Explanation:- The "real_or_simsimulated" is the estimated difference in log-odds
# between “simulated” and “real”. From the "Estimate" coefficient of "real_or_simsimulated" 
# we can say that the log-odds of acc being 1 decreases by 1.1856 when going from "real" to 
# "simulated". Again, in our case, the p-value for "real_or_simsimulated" 
# is 0.00524, which is less than 0.05, indicating a statistically significant 
# difference in accuracy between real and simulated faces. The decrease in deviance values when 
# switching from the null model to our suggested model also suggests that "real_or_sim" 
# is useful for predicting acc.
# 
# In conclusion, the model suggests that there is a significant difference 
# in accuracy between real and simulated faces for this participant. The negative 
# coefficient for real_or_simsimulated suggests that the accuracy is lower for 
# simulated faces compared to real faces.


#----------------  1.3 A more complicated model --------------#

# Fit a new model with interaction
new_model <- glm(acc ~ real_or_sim * conf, data = aifaces, family = "binomial")

# Print the summary of the new model
summary(new_model)

# Explanation:- The revised model is predicting the acc (accuracy) based on the type of face
# (real_or_sim) and the confidence (conf), as well as their interaction (real_or_sim:conf).
# "real_or_simsimulated:conf" represents the combined effect of the face type (real_or_sim) 
# and confidence (conf) on the accuracy (acc).We have included the interaction term 
# to test if the effect of confidence on accuracy is different based on the face type 
# (real or simulated). The coefficient of the interaction term in the revised model is 
# negative (-0.07613), which suggests that the effect of confidence on accuracy decreases 
# when the face is simulated compared to when it is real. Similarly, in your case, the p-value for 
# "real_or_simsimulated:conf" is 0.00658, which is less than 0.05, which suggests that there is 
# statistically significant interaction between face type and confidence. As a conclusion, we 
# can say that for the given participant, the confidence has a lesser impact on the accuracy 
# while judging simulated faces compared to real faces.


# Plotting Confidence Vs accuracy
ggplot(aifaces, aes(x = conf, y = acc, color = real_or_sim)) +
  geom_point()+
  geom_smooth(method = "glm",  formula = y ~ x, method.args = list(family = "binomial"), fullrange = TRUE, se= FALSE) +
  labs(x = "Confidence", y = "Accuracy", color = "Face Type") +
  theme_minimal()


# Explanation:- We have plotted the graph to explore the relation/interaction between the confidence 
# and accuracy for simulated and real face types, which further corroborates the conlusion, we have
# drawn from the previous revised model. The stiff increase of the red line (real) suggests that as confidence 
# increases, the accuracy of identifying real faces also increases. The exact opposite phenomenon we 
# we can observe for simulated face.The sharp decline of the blue line (simulated) suggests that as the
# confidence increases, the accuracy decreases drastically while identifying simulated faces.


#----------- 1.4 Predicting-----------------#

# setting prediction conditions for real face with confidence rating of 60
predict_conds <- data.frame(real_or_sim = "real", conf = 60)
predicted_accuracy <- predict(new_model, newdata = predict_conds, type = "response")

# Print the predicted accuracy
print(predicted_accuracy)

# Based on our revised model, the person's predicted accuracy for a real
# face that they have given a confidence rating of 60 to will be 69.49%.

#==========================================================================#

#read in veg.csv#
veg <- read.csv(file.choose())
head(veg, 15)


#-----1.1 Plotting ------#
ggplot(veg, aes(x = condition, y = PercentDinersChooseVeg, fill = condition)) +
  geom_boxplot() +
  geom_point() +
  stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.40), size=3.5) +
  labs(title = "Percentage of Diners Choosing Vegetable Dishes",
       x = "Condition",
       y = "Percentage of Diners")

# Explanation:- From the boxplot we can get an overview of how the percentage of diners
# choosing vegetables is affected by condition. For "healthy" and "tasty" conditions we can
# observe the presence of outliers. Also, we can check that the median for all the three conditions
# are nearly same. The IQR(interquartile) range for the "Tasty" is greater than other two
# conditions. 


#--------------2.2 Specifying a model ---------------#
veg$recipe <- as.factor(veg$recipe)
veg$school <- as.factor(veg$school)

# Specifying the formula for the mixed effects model
model_formula <-  lmer(PercentDinersChooseVeg ~ condition + recipe + (1|school), data = veg)


# Explanation:-"Condition", represents the labeling condition of the vegetable 
# dish (‘basic’, ‘healthy’, ‘tasty’). It’s a fixed effect because these conditions
# are set by the researchers and are the same across all schools. The researchers 
# are interested in estimating the effect of these conditions on the likelihood of
# pupils choosing the vegetable dish.
# "Recipe" represents the specific vegetable dish served. We are considering
# "Recipe" as fixed effect because each recipe is served under each condition as mentioned in the
# problem statement. The researchers might be interested in estimating the effect of
# different recipes on the likelihood of pupils choosing the vegetable dish.
# 
# We are considering "School" as a random effect because the schools are considered 
# as a random sample from a larger population of schools. The researchers are not
# specifically interested in the effects of individual schools on the likelihood of
# pupils choosing the vegetable dish. Instead, they are mostly interested in 
# observing the variability between schools.


#--------------2.3 Fitting the model--------------#

summary(model_formula)

# Explanation:- The random effects section provides the estimated variances for the
# random intercepts for school and the residuals.school has a variance of 31.45 
# and a standard deviation of 5.608, indicating that there is considerable 
# variability in the percentage of diners choosing vegetables across schools.
# 
# The conditionhealthy coefficient (-1.2531) suggests that, on average, the 
# percentage of diners choosing vegetables decreases by about 1.2531 percentage 
# points under the ‘healthy’ condition compared to the ‘basic’ condition, holding 
# all else constant.
# 
# The conditiontasty coefficient (1.4151) suggests that, on average, the 
# percentage of diners choosing vegetables increases by about 1.4151 percentage 
# points under the ‘tasty’ condition compared to the ‘basic’ condition, holding 
# all else constant.
# 
# The recipe coefficients represent the difference in the percentage of diners 
# choosing vegetables for each recipe compared to the reference recipe (recipe 1),
# holding all else constant. For example, the recipe2 coefficient (0.9890) suggests
# that, on average, the percentage of diners choosing vegetables increases by 
# about 0.9890 percentage points for recipe 2 compared to recipe 1.
# 
# Based on our analysis, we can conclude that the condition of the food 
# (labeled as ‘basic’, ‘healthy’, or ‘tasty’) and the recipe affect 
# the percentage of diners choosing vegetables. We can also observe
# considerable variability across schools.




