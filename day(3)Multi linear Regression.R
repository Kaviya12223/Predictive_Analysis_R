#30/11/2024
#ML Model : Multi linear Regression
#Topic : Predicting Health Insurance Charges Based on Demographic and Lifestyle Factors

install.package("psych")
library(psych)
d = read.csv(file.choose()) #insurance.csv in downloads
View(d)
head(d)
# Convert categorical variables to factors
d$sex <- as.factor(d$sex)
d$smoker <- as.factor(d$smoker)
d$region <- as.factor(d$region)

# Calculate correlation matrix for numerical variables
correlation_matrix <- cor(d[, c("age", "bmi", "children", "charges")])

print(correlation_matrix)

# Visualize the correlation matrix with pairs.panels
psych::pairs.panels(d[c("age", "bmi", "children", "charges")],
                    method = "pearson",
                    hist.col = "brown",
                    density = TRUE,
                    scale = TRUE,
                    cex.labels = 1.5, # Increase label size for better visibility
                    cex.cor = 2,      # Increase size of correlation coefficient
                    pch = 19,         # Use filled circle for points
                    lm = TRUE)        # Add linear regression lines

# Predictions with new data (example)
new_data <- data.frame(
  age = c(30, 40),
  sex = as.factor(c("male", "female")),
  bmi = c(28.0, 30.0),
  children = c(1, 2),
  smoker = as.factor(c("yes", "no")),
  region = as.factor(c("southeast", "northwest"))
)
predicted_charges <- predict(model, newdata = new_data)
print(predicted_charges)
