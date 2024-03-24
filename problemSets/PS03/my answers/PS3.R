#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
#1:
# Step 1:load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
# Step 2: Create a factor variable for GDP change outcome
# Assign "no change" to rows where GDP difference is 0
gdp_data$GDPcat[gdp_data$GDPWdiff == 0] <- "no change"
# Assign "positive" to rows where GDP difference is greater than 0
gdp_data$GDPcat[gdp_data$GDPWdiff > 0] <- "positive"
# Assign "negative" to rows where GDP difference is less than 0
gdp_data$GDPcat[gdp_data$GDPWdiff < 0] <- "negative"
# Convert the 'GDPcat' column to a factor and set "no change" as the reference level
gdp_data$GDPcat <- factor(gdp_data$GDPcat, levels = c("no change", "positive", "negative"))

# Step 3:Run unordered multinomial logistic regression
library(nnet)
unordered_logit <- multinom(GDPcat ~ REG + OIL, data = gdp_data)

# Display the summary of the multinomial logistic regression model
summary(unordered_logit)

#2:
#Step 1:  Prepare for ordered logistic regression.Adjust the levels of the factor variable to establish a specific sequence.
gdp_data$GDPcat <- factor(gdp_data$GDPcat, levels = c("negative", "no change", "positive"))

# Step 2:Run the ordered logistic regression
ordered_logit <- polr(GDPcat ~ REG + OIL, data = gdp_data)

# Display the summary of the ordered logistic regression model
summary(ordered_logit)


#####################
# Problem 2
#####################
#(a)
# Step1:load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Step 2:Fit the Poisson regression model
mex_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                   data = mexico_elections, family = poisson())

# Display the summary of the Poisson model
summary(mex_poisson)
#(b) plotting of Predicted PAN Visits vs. Marginality
# Create a new data frame for predictions
new_data <- data.frame(
  competitive.district = mean(mexico_elections$competitive.district, na.rm = TRUE), 
  marginality.06 = seq(min(mexico_elections$marginality.06, na.rm = TRUE), 
                       max(mexico_elections$marginality.06, na.rm = TRUE), length.out = 100),
  PAN.governor.06 = mean(mexico_elections$PAN.governor.06, na.rm = TRUE) 
)

# Predict the number of visits using the new data
new_data$predicted_visits <- predict(mex_poisson, newdata = new_data, type = "response")

# Plotting
library(ggplot2)
ggplot(new_data, aes(x = marginality.06, y = predicted_visits)) +
  geom_line(color = "blue") +
  labs(title = "Predicted PAN Visits vs. Marginality",
       x = "Marginality in 2006",
       y = "Predicted Number of PAN Visits") +
  theme_minimal()

#(c)
# Predict expected PAN visits for a competitive district with average marginality and a PAN governor.
predict(mex_poisson, newdata = data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1), type = "response")

