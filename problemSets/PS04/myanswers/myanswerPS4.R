# Install the 'eha' package
install.packages("eha")
# Install the 'survival' package
install.packages("survival")
# Load the 'eha' library to access the 'child' dataset, which contains information on child mortality influenced by the mother's background and the child's sex
library(eha)
# Load the 'survival' package
library(survival)

# Load the 'child' dataset from the 'eha' library
data("child", package = "eha")

# Apply the Cox Proportional Hazards model to assess survival, considering factors such as the mother's age and the child's sex
infantMortality <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
# To get a summary of the model's results:
summary(infantMortality)
