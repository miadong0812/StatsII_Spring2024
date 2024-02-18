#Question 2:
#  set a seed for reproducibility when doing simulations
set.seed(123)

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(broom)      # For tidying up model outputs
library(lme4)       # For fitting generalized linear mixed models (if needed)
library(ggplot2)    # For plotting


# Load the dataset
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Explore the dataset (Optional)
View(climateSupport)
summary(climateSupport)
# Use the summary function and capture the output
summary_output <- capture.output(summary(climateSupport))


# Ensure that 'countries' and 'sanctions' are factors with the correct levels
climateSupport$countries <- as.factor(climateSupport$countries)
climateSupport$sanctions <- as.factor(climateSupport$sanctions)

# Fit the logistic regression model
model <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial())

# Summary of the model to examine coefficients and significance
summary(model)
#Visualization
# Assuming 'model' is our fitted logistic regression model
coefficients_df <- broom::tidy(model)
ggplot(coefficients_df, aes(x = term, y = estimate, fill = p.value < 0.05)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Effect Sizes of Predictors on Policy Support", y = "Coefficient Estimate", x = "") +
  scale_fill_manual(name = "Significance", values = c("TRUE" = "steelblue", "FALSE" = "grey"), labels = c("TRUE" = "Significant", "FALSE" = "Not Significant")) +
  theme_minimal()

# Generate a new dataset for predictions
new_data <- expand.grid(countries = levels(climateSupport$countries), sanctions = levels(climateSupport$sanctions))
new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")

# Plot
ggplot(new_data, aes(x = countries, y = predicted_prob, color = sanctions, group = sanctions)) + # Added 'group = sanctions' here
  geom_line() +
  geom_point() +
  labs(title = "Predicted Probability of Policy Support", y = "Predicted Probability", x = "Number of Participating Countries") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



#Question 2
#2(a)
# Calculate odds ratio for sanctions increase from 5% to 15% with nearly full participation
# This needs to match the model's coefficient names.
# First, we need to determine the coding used for the levels of sanctions
# For example, let's say that we have the following coding:
# None - 0, 5% - 1, 15% - 2, 20% - 3
# We need to calculate the predicted log odds for sanctions at 5% and 15%

# Data frame for prediction focusing on 160 of 192 countries participating
predict_data <- data.frame(
  countries = factor(rep("160 of 192", 2), levels = c("20 of 192", "80 of 192", "160 of 192")),
  sanctions = factor(c("5%", "15%"), levels = c("None", "5%", "15%", "20%"))
)

# Calculate predicted log odds for the sanctions levels
predicted_log_odds <- predict(model, newdata = predict_data, type = "link")

# The odds at 5% sanctions level
odds_5 <- exp(predicted_log_odds[1])

# The odds at 15% sanctions level
odds_15 <- exp(predicted_log_odds[2])

# Calculate the odds ratio
odds_ratio <- odds_15 / odds_5
print(paste("Odds Ratio for sanctions increase from 5% to 15%: ", odds_ratio))



#2(b)
# Create new data for prediction
new_data_80_none <- data.frame(countries = factor("80 of 192", levels = levels(climateSupport$countries)),
                               sanctions = factor("None", levels = levels(climateSupport$sanctions)))

# Predict probability
prob_support_80_none <- predict(model, newdata = new_data_80_none, type = "response")
prob_support_80_none


#2(c)
# Fit a model with interaction terms
interaction_model <- glm(choice ~ countries * sanctions, data = climateSupport, family = binomial())

# ANOVA to compare models
anova_test <- anova(model, interaction_model, test = "Chisq")
anova_test


