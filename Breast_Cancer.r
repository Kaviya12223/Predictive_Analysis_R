#Project - Breast Cancer Prediction
#Author - Kaviya.G 
#Date - 14/09/2024
#------------------------------------------------------------------------------------------------------------------------

# Install necessary packages
install.packages(c('class','gmodels', 'caret', 'lava', 'stringi', "ggplot2"))
install.packages("lattice")
install.packages('GGally')

# Load necessary libraries
library(ggplot2)
library(lattice)
library(stringi)
library(caret)
library(GGally)
library(class)
library(gmodels)

# Read and process data
a <- read.csv(file.choose(), stringsAsFactors = FALSE)
View(a)
a <- a[-1]
a$diagnosis <- factor(a$diagnosis, levels = c('B', 'M'), labels = c('Benign', 'Malignant'))

# Create a pie chart
diagnosis_counts <- table(a$diagnosis)
pie(diagnosis_counts, main = "Diagnosis Distribution", col = c("lightblue", "salmon"), labels = paste0(names(diagnosis_counts), ": ", diagnosis_counts))


# Proportions and summary
round(prop.table(table(a$diagnosis)) * 100, digits = 1)
summary(a[c("radius_mean", "area_mean", "smoothness_mean")])

# Normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
a_n <- as.data.frame(lapply(a[2:31], normalize))

# Create a histogram for a normalized feature
ggplot(a_n, aes(x = area_mean)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Normalized Area Mean", x = "Normalized Area Mean", y = "Frequency") +
  theme_minimal()

# Train-test split
a_train <- a_n[1:469, ]
a_test <- a_n[470:569, ]
a_train_labels <- a[1:469, 1]
a_test_labels <- a[470:569, 1]

# KNN algorithm
a_test_pred <- knn(train = a_train, test = a_test, cl = a_train_labels, k = 21)

# Cross Table
CrossTable(x = a_test_labels, y = a_test_pred, prop.chisq = FALSE)

# Confusion Matrix
aa <- table(a_test_labels, a_test_pred)

aa
confusionMatrix(data = a_test_pred, reference = a_test_labels)

# Pair plot for selected features
ggpairs(a[c("radius_mean", "area_mean", "smoothness_mean", "diagnosis")], aes(color = diagnosis))
