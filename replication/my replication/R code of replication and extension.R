library(dplyr)
library(reshape2)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("texreg")
library(texreg)
library(tidyr)
DAT <- read.csv2("/Users/miadong/Desktop/replication_data.csv")

DAT$aid <- apply(DAT[,c("aidUK","aidUS")], 1, mean)
## Citation for Replication Data
# -------------------------------------------------------------------------------------
# Becker, Bastian (2024). International Inequality and Demand for Redistribution in 
# the Global South. Harvard Dataverse, V1. DOI: 10.7910/DVN/EZ0UZK. URL: 
# https://doi.org/10.7910/DVN/EZ0UZK. Accessed: 2024-03-22.
# Keywords: inequality, redistribution, aid, development, public opinion.
# Data and analysis script available at Harvard Dataverse.
#
# Note: In this replication script, the colors of the plots have been modified from
# the original presentation for enhanced clarity and visual appeal.
# -------------------------------------------------------------------------------------

# Main text

#Figure 1
#Convert data on dependent variables into long format; compute and plot aggregate frequencies:
DAT_long <- melt(DAT, id.vars = c("respID"), 
                 measure.vars = c("incdiff", "aidUK", "aidUS"))

DAT_agg <- aggregate(cbind(respID) ~ variable + value, data = DAT_long, FUN = length)

ggplot(DAT_agg, aes(fill = variable, y = respID, x = value)) + 
  geom_bar(position = "dodge", stat = "identity", color = "white") + 
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                    labels = c("Inequality Acceptance", "Demand for Aid (UK)", "Demand for Aid (USA)")) + 
  scale_x_continuous(breaks = 1:5, 
                     labels = c("--", "-", "0", "+", "++")) +
  labs(fill = "") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  ylab("Count") + 
  xlab("") +
  theme_bw() + 
  theme(legend.position = "bottom")


#Figure 2, left and right
#gg1
gg1 <- ggplot(DAT, aes(x=guess_recode)) +
  geom_histogram(fill="#5DADE2", color="#2471A3", show.legend = FALSE) +  # Blue shades
  geom_smooth(aes(x=guess_recode, y=(incdiff-1)*48), 
              formula = y ~ poly(x,2), 
              color="#E74C3C",  # Contrasting red for the line
              lwd=1.5, 
              method="lm")+
  scale_x_continuous(trans="log", 
                     breaks=c(1,10,100,1000,10000,100000,1000000), 
                     labels=c("1","10","100","1k","10k","100k","1m")) +
  scale_y_continuous(name="Inequality Acceptance", 
                     breaks = 48*c(0,1,2,3,4),
                     labels=c("--","-","0","+","++")) + 
  xlab("Perceived Inequality") +  
  theme_bw()

# gg2
gg2 <- ggplot(DAT, aes(x=guess_recode)) +
  geom_histogram(fill="#5DADE2", color="#2471A3", show.legend = FALSE) +  # Green shades
  geom_smooth(aes(x=guess_recode, y=(aid-1)*48), 
              formula = y ~ poly(x,2), 
              color="#8E44AD",  # Contrasting purple for the line
              lwd=1.5, 
              method="lm")+
  scale_x_continuous(trans="log", 
                     breaks=c(1,10,100,1000,10000,100000,1000000), 
                     labels=c("1","10","100","1k","10k","100k","1m")) +
  scale_y_continuous(name="Demand for Aid", 
                     breaks = 48*c(0,1,2,3,4),
                     labels=c("--","-","0","+","++")) + 
  xlab("Perceived Inequality") +  
  theme_bw()

# Arranging both plots together
ggarrange(gg1, gg2)


#Table 1:Estimate main regression models:
mod01 <- lm(incdiff ~ ineqtreat, DAT)
mod02 <- lm(aid ~ ineqtreat, DAT)
mod03 <- lm(aidUK ~ ineqtreat, DAT)
mod04 <- lm(aidUS ~ ineqtreat, DAT)
mod05 <- lm(aidUK - aidUS ~ ineqtreat, DAT)

# Adjusted htmlreg call
html_output <- htmlreg(list(mod01, mod02, mod03, mod04, mod05),
                       digits = 3, 
                       caption.above = TRUE,
                       caption = "Average Treatment Effects (OLS)",
                       custom.model.names = c("Inequality Acceptance (1)",
                                              "Demand for aid (2)",
                                              "Aid (UK) (3)",
                                              "Aid (USA) (4)", 
                                              "Aid (UK) - Aid (USA) (5)"),
                       custom.coef.map = list(ineqtreat = "Treatment"),
                       stars = .05,
                       custom.note = c("Note: Ordinary least squares regression. 'Inequality Acceptance' indicates acceptance of income differences between Kenya and Western Europe; 'aid' indicates demand for international financial transfers. (*=0.05)"))

# Print the HTML output to the console
cat(html_output)

# Appendix
# Table 2:Estimate regression models with dichotomized dependent variables:
 mod01 <- lm(incdiff < 3 ~ ineqtreat, DAT)
 mod02 <- lm(aid > 3 ~ ineqtreat, DAT)
 mod03 <- lm(aidUK > 3 ~ ineqtreat, DAT)
 mod04 <- lm(aidUS > 3 ~ ineqtreat, DAT)
 mod05 <- lm(aidUK - aidUS > 0 ~ ineqtreat, DAT)
 
 htmlreg(list(mod01,mod02,mod03,mod04,mod05), 
         digits = 3,
         caption.above = T,
         caption="Average Treatment Effects (LPM)",
         custom.header = list("Inequality Opposition"=1,
                              "Demand for aid"=2,
                              "Aid (UK)"=3,
                              "Aid (USA)"=4, 
                              "Aid (UK) - Aid (USA)"=5),
         custom.model.names = c("(1)","(2)","(3)","(4)","(5)"),
         custom.coef.map = list("ineqtreat"="Treatment"), 
         stars = .05,
         custom.note = "\\item\\textit{Note:} Linear probability models (OLS). All dependent variables are dummy-coded. \\textit{Inequality Opposition} indicates opposition to income differences between Kenya and Western Europe; \\textit{Aid} indicates demand for international financial transfers; all dependent variables are dichotomized. (*=.05)")
 
 
# Table 3:Estimate regression models including an interaction term with guesses prior to treatment:
 mod01 <- lm(incdiff ~ ineqtreat * guess_recode, DAT)
 mod02 <- lm(aid ~ ineqtreat * guess_recode, DAT)
 mod03 <- lm(aidUK ~ ineqtreat * guess_recode, DAT)
 mod04 <- lm(aidUS ~ ineqtreat * guess_recode, DAT)
 mod05 <- lm(aidUK - aidUS ~ ineqtreat * guess_recode, DAT)
 
 htmlreg(list(mod01,mod02,mod03,mod04,mod05), 
         digits = 3, 
         caption.above = T,
         caption="Treatment Effects by Dosage (OLS)",
         custom.header = list("Inequality Acceptance"=1,
                              "Demand for aid"=2,
                              "Aid (UK)"=3,
                              "Aid (USA)"=4, 
                              "Aid (UK) - Aid (USA)"=5),
         custom.model.names = c("(1)","(2)","(3)","(4)","(5)"),
         stars = .05,
         custom.coef.map = list("ineqtreat"="Treatment",
                                "guess_recode"="Perceived Inequality (PI)",
                                "ineqtreat:guess_recode"="Treatment $\\times$ PI"),
         custom.note = "\\item\\textit{Note:} Ordinary least squares regression. \\textit{Perceived Inequality} indicates pretreatment perception of income differences between Kenya and Western Europe; \\textit{Inequality Acceptance} indicates acceptance to these income differences; \\textit{Aid} indicates demand for international financial transfers. (*=.05)")
 

 
 #Figure 3
 gg1<-ggplot(subset(melt(table(DAT$living, DAT$aid)), 
                    value > 0)) + 
   geom_smooth(data=DAT, 
               aes(x=living,
                   y=aid), 
               method="lm",
               color="darkgrey")+
   geom_point(aes(Var1, 
                  Var2, 
                  size = value, 
                  color = value)) +  
   scale_color_gradient(low = "blue", high = "red") + 
   scale_x_continuous(breaks = 1:5, 
                      name = "", 
                      labels=c("1-low","2","3","4","5-high")) + 
   scale_y_continuous(name = "Demand for aid",
                      breaks = 1:5, 
                      labels=c("--","-","0","+","++")) + 
   labs(size="Frequency",
        color="darkgrey") +
   theme_bw()
 
 gg2<-ggplot(subset(melt(table(DAT$relig, DAT$aid)),
                    value > 0)) + 
   geom_smooth(data=DAT, 
               aes(x=relig,
                   y=aid),
               method="lm",
               color="darkgrey")+
   geom_point(aes(Var1,
                  Var2,
                  size = value,
                  color = value)) +  
   scale_color_gradient(low = "blue", high = "red") + 
   scale_x_continuous(breaks = 1:5,
                      name = "",
                      labels=c("1-low","2","3","4","5-high")) + 
   scale_y_continuous(name = "",
                      breaks = 1:5, 
                      labels=c("--","-","0","+","++")) +
   labs(size="Frequency") +
   theme_bw()
 
 gg3<-ggplot(subset(melt(table(DAT$living, DAT$incdiff)), 
                    value > 0)) + 
   geom_smooth(data=DAT, 
               aes(x=living,
                   y=incdiff), 
               method="lm",
               color="darkgrey") +
   geom_point(aes(Var1, 
                  Var2, 
                  size = value,
                  color = value)) +  
   scale_color_gradient(low = "blue", high = "red") + 
   scale_x_continuous(breaks = 1:5, 
                      name = "Economic Standing", 
                      labels=c("1-low","2","3","4","5-high")) + 
   scale_y_continuous(name = "Inequality Acceptance",
                      breaks = 1:5, 
                      labels=c("--","-","0","+","++")) +
   labs(size="Frequency") +
   theme_bw()
 
 gg4<-ggplot(subset(melt(table(DAT$relig, DAT$incdiff)), 
                    value > 0)) + 
   geom_smooth(data=DAT, 
               aes(x=relig,
                   y=incdiff), 
               method="lm",
               color="darkgrey")+
   geom_point(aes(Var1, 
                  Var2, 
                  size = value,
                  color = value)) +  
   scale_color_gradient(low = "blue", high = "red") + 
   scale_x_continuous(breaks = 1:5, 
                      name = "Religiosity", 
                      labels=c("1-low","2","3","4","5-high")) + 
   scale_y_continuous(name = "",
                      breaks = 1:5, 
                      labels=c("--","-","0","+","++")) +
   labs(size="Frequency") +
   theme_bw()
 
 ggarrange(gg1,gg2,gg3,gg4,common.legend = T,legend="bottom") 
 
 
 #Table 4:Estimate regression models including interaction terms with respondent characteristics (economic standing, religiosity):
 mod01 <- lm(incdiff ~ ineqtreat * living, DAT)
 mod02 <- lm(aid ~ ineqtreat * living, DAT)
 mod03 <- lm(incdiff ~ ineqtreat * relig, DAT)
 mod04 <- lm(aid ~ ineqtreat * relig, DAT)
 
 htmlreg(list(mod01,mod03,mod02,mod04), 
         digits = 3, 
         caption.above = T,
         caption="Treatment Effect Heterogeneity (OLS)",
         custom.header = list("Inequality Acceptance"=1:2,
                              "Demand for aid"=3:4),
         custom.model.names = c("(1)","(2)","(3)","(4)"), 
         stars = .05,
         custom.coef.map = list("ineqtreat"="Treatment",
                                "living"="Economic Standing (ES)",
                                "relig"="Religiosity",
                                "ineqtreat:living"="Treatment $\\times$ ES",
                                "ineqtreat:relig"="Treatment $\\times$ Rel."),
         custom.note = "\\item\\textit{Note:} Ordinary least squares regression. \\textit{Inequality Acceptance} indicates acceptance of income differences between Kenya and Western Europe; \\textit{Aid} indicates demand for international financial transfers. (*=.05)")
 
 #Extensions
 # Extension 1: 
#table 2 to "glm" Binary Logistic Regression 
 
 # Set up the binary dependent variables as factors
 DAT$incdiff_binary <- factor(DAT$incdiff < 3)
 DAT$aid_binary <- factor(DAT$aid > 3)
 DAT$aidUK_binary <- factor(DAT$aidUK > 3)
 DAT$aidUS_binary <- factor(DAT$aidUS > 3)
 DAT$aid_diff_binary <- factor(DAT$aidUK - DAT$aidUS > 0)
 
 # Fit the glm models using these factors
 mod01_glm <- glm(incdiff_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod02_glm <- glm(aid_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod03_glm <- glm(aidUK_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod04_glm <- glm(aidUS_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod05_glm <- glm(aid_diff_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 
 # Load the 'stargazer' package for output (install if not already available)
 library(stargazer)
 
 
 #Create a table with stargazer (table 5)
 stargazer(mod01_glm, mod02_glm, mod03_glm, mod04_glm, mod05_glm,
           type = "latex",
           title = "Comparative Model Output",
           out = "glm_models_table.tex", # Save the table to a LaTeX file
           column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
           covariate.labels = c("Treatment", "Constant"),
           omit.stat = c("ll", "aic", "ser"), # Omit log-likelihood, AIC, and standard error of regression
           add.lines = list(
             c("Model", "Inequality Opposition", "Demand for aid", "Aid (UK)", "Aid (USA)", "Aid (UK) - Aid (USA)")
           ),
           notes = c("Note: GLM with binomial family. All dependent variables are dummy-coded. Inequality Opposition indicates opposition to income differences between Kenya and Western Europe; Aid indicates demand for international financial transfers. Significance levels: *p<0.1; **p<0.05; ***p<0.01"),
           notes.align = "l",
           star.cutoffs = c(0.1, 0.05, 0.01) # Define significance levels for stars
 )

 
 
 
 
 # Compare for two models ("lm" and "glm")
 
 # Converting to binary factors for logistic regression models
 DAT$incdiff_binary <- factor(DAT$incdiff < 3)
 DAT$aid_binary <- factor(DAT$aid > 3)
 DAT$aidUK_binary <- factor(DAT$aidUK > 3)
 DAT$aidUS_binary <- factor(DAT$aidUS > 3)
 DAT$aid_diff_binary <- factor(DAT$aidUK - DAT$aidUS > 0)
 
 # Fitting Linear Models
 mod01_lm <- lm(incdiff < 3 ~ ineqtreat, data=DAT)
 mod02_lm <- lm(aid > 3 ~ ineqtreat, data=DAT)
 mod03_lm <- lm(aidUK > 3 ~ ineqtreat, data=DAT)
 mod04_lm <- lm(aidUS > 3 ~ ineqtreat, data=DAT)
 mod05_lm <- lm(aidUK - aidUS > 0 ~ ineqtreat, data=DAT)
 
 # Fitting GLMs for Logistic Regression
 mod01_glm <- glm(incdiff_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod02_glm <- glm(aid_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod03_glm <- glm(aidUK_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod04_glm <- glm(aidUS_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 mod05_glm <- glm(aid_diff_binary ~ ineqtreat, family=binomial(link="logit"), data=DAT)
 # Load the 'stargazer' package
 library(stargazer)
 
 # Comparative Table for Linear Models
 stargazer(mod01_lm, mod02_lm, mod03_lm, mod04_lm, mod05_lm,
           type = "text", # Change to "latex" for LaTeX output
           title = "Comparative Model Output for Linear Models",
           out = "linear_models_table.tex", # Only relevant for LaTeX output
           column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
           covariate.labels = c("Treatment", "Constant"),
           omit.stat = c("ll", "aic", "ser"),
           notes = "Note: Linear models. All dependent variables are processed as continuous but represent binary outcomes. Significance levels: *p<0.1; **p<0.05; ***p<0.01")
 
 # Comparative Table for Logistic Regression Models
 stargazer(mod01_glm, mod02_glm, mod03_glm, mod04_glm, mod05_glm,
           type = "text", # Change to "latex" for LaTeX output
           title = "Comparative Model Output for Logistic Regression Models",
           out = "glm_models_table.tex", # Only relevant for LaTeX output
           column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
           covariate.labels = c("Treatment", "Constant"),
           omit.stat = c("ll", "aic", "ser"),
           notes = "Note: GLM with binomial family. All dependent variables are binary. Significance levels: *p<0.1; **p<0.05; ***p<0.01")

  # Generating Residuals vs Fitted plots for all linear models
 par(mfrow = c(3, 2)) # Adjusting plot area to display 5 plots
 
 plot(mod01_lm, which = 1, main = "Residuals vs Fitted for Model 1")
 plot(mod02_lm, which = 1, main = "Residuals vs Fitted for Model 2")
 plot(mod03_lm, which = 1, main = "Residuals vs Fitted for Model 3")
 plot(mod04_lm, which = 1, main = "Residuals vs Fitted for Model 4")
 plot(mod05_lm, which = 1, main = "Residuals vs Fitted for Model 5")
 
 # Resetting plot area
 par(mfrow = c(1, 1))
 
 library(pROC)
 
 
 # Plot ROC curves for all models
 par(mfrow = c(3, 2)) # Adjusting plot area
 
 # Model 1
 roc_response1 <- roc(response = as.numeric(DAT$incdiff_binary) - 1, predictor = fitted(mod01_glm))
 plot(roc_response1, main = "ROC Curve for Model 1")
 
 # Model 2
 roc_response2 <- roc(response = as.numeric(DAT$aid_binary) - 1, predictor = fitted(mod02_glm))
 plot(roc_response2, main = "ROC Curve for Model 2")
 
 # Model 3
 roc_response3 <- roc(response = as.numeric(DAT$aidUK_binary) - 1, predictor = fitted(mod03_glm))
 plot(roc_response3, main = "ROC Curve for Model 3")
 
 # Model 4
 roc_response4 <- roc(response = as.numeric(DAT$aidUS_binary) - 1, predictor = fitted(mod04_glm))
 plot(roc_response4, main = "ROC Curve for Model 4")
 
 # Model 5
 roc_response5 <- roc(response = as.numeric(DAT$aid_diff_binary) - 1, predictor = fitted(mod05_glm))
 plot(roc_response5, main = "ROC Curve for Model 5")
 
 # Resetting plot area
 par(mfrow = c(1, 1))
 
 

 # Calculating predicted probabilities for each model
 DAT$prob_mod01_glm <- predict(mod01_glm, type = "response")
 DAT$prob_mod02_glm <- predict(mod02_glm, type = "response")
 DAT$prob_mod03_glm <- predict(mod03_glm, type = "response")
 DAT$prob_mod04_glm <- predict(mod04_glm, type = "response")
 DAT$prob_mod05_glm <- predict(mod05_glm, type = "response")
 
 # Melting the data for ggplot2
 DAT_long <- DAT %>% 
   select(prob_mod01_glm, prob_mod02_glm, prob_mod03_glm, prob_mod04_glm, prob_mod05_glm) %>%
   pivot_longer(cols = everything(), names_to = "Model", values_to = "PredictedProbability")
 
 # Plotting
 ggplot(DAT_long, aes(x = PredictedProbability, fill = Model)) +
   geom_density(alpha = 0.5) +
   labs(title = "Predicted Probabilities Across Models",
        x = "Predicted Probability",
        y = "Density") +
   theme_minimal() +
   scale_fill_brewer(palette = "Set1") # Adjust palette as needed

 
 # Install necessary packages if not already installed
 if (!require("MASS")) install.packages("MASS")
 library(MASS)
 
# Extension 2:
#The new hypothesis: education level effect on acceptance of ineuqlity
 # Convert ineqtreat to a factor for logistic regression
 DAT$ineqtreat <- factor(DAT$ineqtreat)
 
 # Create a binary factor for incdiff
 DAT$incdiff_binary <- factor(DAT$incdiff < 3, labels = c("0", "1"))
 
 #  run a binary logistic regression with ineqtreat as the dependent variable
 # and incdiff_binary, edu_low, edu_high as independent variables
 mod01 <- glm(ineqtreat ~ incdiff_binary + edu_low + edu_high, 
              family = binomial, data = DAT)
 
 # Summary of the model to see coefficients
 summary(mod01)
 
 # Calculate odds ratios
 odds_ratios <- exp(coef(mod01))
 
 # Get the confidence intervals for the odds ratios
 conf_int <- exp(confint(mod01))
 
 # Display the odds ratios with confidence intervals
 cbind(odds_ratios, conf_int)
 
 