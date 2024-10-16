#NAIVE BAYES Classification 

# Step 1: Install and load necessary libraries
install.packages(c("ggplot2","gmodels", "e1071"))
library(gmodels)
library(class)
library(lattice)
library(ggplot2)
library(caret)# Load 'caret' package for train-test split
library(e1071)# e1071- Naive Bayes
library(caret)

# Step 2: Load the Iris dataset
data(iris)
head(iris)

# Step 3: Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)  # 70% training, 30% testing
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Step 4: Train the Naive Bayes model
model <- naiveBayes(Species ~ ., data = trainData)# Train Naive Bayes using all features to predict 'Species'

# Step 5: Make predictions on the test set
predictions <- predict(model, testData)

# Step 6: Evaluate the model
cm<-confusionMatrix(predictions, testData$Species)  # Confusion matrix and accuracy
cm
cm_table<-as.table(cm$table)
cm_table

# Step 7: Visualize the model's accuracy (Optional)
plot(predictions, col = "lightblue", main = "Predicted vs True Species in Test Data")
points(testData$Species, col = c("red","yellow", "black"), pch = 20)
legend("topright", legend = unique(iris$Species), col = c("red","yellow", "black"),
       pch = 20, title = "Species")

colnames(cm_table) <- c("Reference", "Prediction", "Freq")


accuracy <- sum(diag(cm$table)) / sum(cm$table)
nir <- 0.3333  # No Information Rate

df <- data.frame(Measure = c("Accuracy", "NIR"), Value = c(accuracy, nir))

ggplot(df, aes(x = Measure, y = Value, fill = Measure)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  labs(title = "Model Accuracy vs No Information Rate", y = "Proportion")
