# Loading the 'caret' package for unified model training and evaluation
library(caret)

# Loading 'ggplot2' for creating visually appealing plots
library(ggplot2)

# Loading 'dplyr' for data manipulation tasks using the tidyverse syntax
library(dplyr)

# Loading the 'tidyverse' package, which includes several packages like 'dplyr', for a cohesive data manipulation and visualization workflow
library(tidyverse)

# Loading 'naivebayes' for building and using Naive Bayes classifiers
library(naivebayes)

# Loading 'class' for k-Nearest Neighbors (k-NN) classification
library(class)

# Loading 'rpart' for constructing decision trees
library(rpart)

# Loading 'corrplot' for visualizing correlation matrices
library(corrplot)

#Used for data splitting and manipulation tasks.
#sample.split function from this library is used to split the dataset into training and testing sets.
library(caTools)

#Provides functions for statistical learning and support vector machines (SVMs) in particular. 
#In the context, it is used for training a Naive Bayes model.
library(e1071)
#Reading/Loading the csv file which has been seperated by ;
data <- read.csv('cardio_train.csv',header=TRUE,sep = ";")

#To get the summary of the the data
summary((data))

#To check the null values in the data
colSums(is.na(data))

#removing id which is an extra column and storing the data to data_1
data_1 <- subset(data, select = -id) 
str(data)

#age was in number changed to years
data_1$age <- data_1$age / 365.25
data_1$age <-round(data_1$age)


#Loading the categorical values to another variable to change its data type
categorical_columns <- c('gender', 'cholesterol', 'gluc', 'smoke', 'alco','active','cardio')
for(column in categorical_columns)
  data_1[[column]]<-as.factor((data_1[[column]]))
str(data_1)

#To provide a convenient way to visualize the correlation structure of the variables in data_1 through a heatmap
#numeric_data <- data_1[sapply(data_1, is.numeric)]

# Calculate correlation matrix
#cor_matrix <- cor(numeric_data)

# Create a data frame with the correlation matrix
data_2 <- data.frame(round(cor_matrix, digits = 2))

# Reshape the data for plotting
hm <- data_2 %>% 
  rownames_to_column() %>%
  gather(colname, value, -rowname)

# Plot the heatmap
my_colors <- colorRampPalette(c("cyan", "deeppink4"))
ggplot(hm, aes(x = rowname, y = colname, fill = value)) + 
  geom_tile() + 
  scale_fill_distiller(palette = "RdPu")

data_2 <- data.frame(round(cor(data_1),digits=2))
hm <- data_2 %>% 
  rownames_to_column() %>%
  gather(colname,value,-rowname)
head(hm,5)
my_colors<- colorRampPalette(c("cyan", "deeppink4"))
ggplot(hm, aes(x=rowname,y=colname, fill=value)) + geom_tile() + scale_fill_distiller(palette = "RdPu")

#Checking and removing the duplicate values
sum(duplicated(data_1))
data_1 <- data_1[!duplicated(data_1), ]
sum(duplicated(data_1))

#Loading column into label
label=c("Age","weight","height","ap_hi","ap_lo") 
label

# boxplot with names parameter for labels  and to see outliers
boxplot(data_1$age, data_1$weight, data_1$height, data_1$ap_hi, data_1$ap_lo, names=label)

#Removing the outliers
remove_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25)
  Q3 <- quantile(data[[column_name]], 0.75)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  print(lower_bound)
  print(upper_bound)
  # Replace outliers with NA or remove them
  data[[column_name]][data[[column_name]] < lower_bound | data[[column_name]] > upper_bound] <- mean(data[[column_name]])
  # Alternatively, to remove outliers completely:
  #data <- data[!(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound), ]
  
  return(data)
}


numeric_columns <- c('age', 'height','weight', 'ap_hi', 'ap_lo')
# Loop through each numeric column and remove outliers
for (column in numeric_columns) {
  data_1 <- remove_outliers(data_1, column)
}

#Boxplot after outliers replace them with mean values
boxplot(data_1$age, data_1$weight,data_1$height, data_1$ap_hi, data_1$ap_lo, names=label)
colSums(is.na(data_1))



ggplot(data_1, aes(x=age)) + geom_histogram(aes(y=..density..),binwidth=.5,
                                            colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")
ggplot(data_1, aes(x=height)) + geom_histogram(aes(y=..density..),binwidth=.5,
                                               colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")

ggplot(data_1, aes(x=weight)) + geom_histogram(aes(y=..density..),binwidth=.5,
                                            colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")
ggplot(data_1, aes(x=ap_hi)) + geom_histogram(aes(y=..density..),binwidth=.5,
                                              colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")
ggplot(data_1, aes(x=ap_lo)) + geom_histogram(aes(y=..density..),binwidth=.5,
                                              colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")

# Create a two-panel plot
plot_grid <- function(p1, p2, cols = 2) {
  require(gridExtra)
  grid.arrange(p1, p2, ncol = cols)
}

# Plot for Smoke use by gender
plot1 <- ggplot(data_1, aes(x = smoke, fill = factor(gender))) +
  geom_bar(position = "dodge") +  # Create a dodged bar plot
  labs(title = "Smoke use for gender") +  # Set the plot title
  theme_minimal() +  # Use a minimal theme for the plot
  scale_fill_manual(values = c("1" = "sea green", "2" = "red"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_x_discrete(labels = c("0" = "No Cardio", "1" = "Cardio"))


# Plot for Alcohol use by gender
plot2 <- ggplot(data_1, aes(x = alco, fill = factor(gender))) +
  geom_bar(position = "dodge") +  # Create a dodged bar plot
  labs(title = "Alcohol use for gender") +  # Set the plot title
  theme_minimal() +  # Use a minimal theme for the plot
  scale_fill_manual(values = c("1" = "sea green", "2" = "red"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_x_discrete(labels = c("0" = "No Alcohol", "1" = "Alcohol"))


# Display side-by-side plots

plot_grid(plot1, plot2)



# Create a cross-tabulation in R between Glucose and Cardio
cross_tab <- table(data_1$gluc, data_1$cardio)

# Convert cross-tabulation to a data frame
cross_tab_df <- as.data.frame.matrix(cross_tab)

# Rename columns and convert factor levels for better plotting
names(cross_tab_df) <- c("No_Cardio", "Cardio")
cross_tab_df$Glucose <- rownames(cross_tab_df)
cross_tab_df$Glucose <- factor(cross_tab_df$Glucose)

# Reshape the data for plotting
cross_tab_df_long <- reshape2::melt(cross_tab_df, id.vars = "Glucose")
custom_colors <- c("brown", "#fc8d62")

# Plotting grouped bar plot
ggplot(cross_tab_df_long, aes(x = Glucose, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relationship between Glucose and Cardio",
       x = "Glucose",
       y = "Count",
       fill = "Cardio") +
  scale_x_discrete(labels = c("1" = "normal", "2" = "above normal", "3"= "Well above normal"))+
  scale_fill_manual(values = custom_colors)
  theme_minimal()

#########################
# Create a cross-tabulation in R between cholesterol and Cardio
cross_tab <- table(data_1$cholesterol, data_1$cardio)
  
# Convert cross-tabulation to a data frame
cross_tab_df <- as.data.frame.matrix(cross_tab)
  
# Rename columns and convert factor levels for better plotting
names(cross_tab_df) <- c("No_Cardio", "Cardio")
cross_tab_df$cholesterol <- rownames(cross_tab_df)
cross_tab_df$cholesterol <- factor(cross_tab_df$cholesterol)
  
# Reshape the data for plotting
cross_tab_df_long <- reshape2::melt(cross_tab_df, id.vars = "cholesterol")
custom_colors <- c("brown", "#fc8d62")
  
# Plotting grouped bar plot
ggplot(cross_tab_df_long, aes(x = cholesterol, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relationship between cholesterol and Cardio",
       x = "cholesterol",
       y = "Count",
       fill = "Cardio") +
  scale_x_discrete(labels = c("1" = "normal", "2" = "above normal", "3"= "Well above normal"))+
  scale_fill_manual(values = custom_colors)
theme_minimal()

  
##################################################
#RandomForest model and Feature selection
library(randomForest)
# Set seed for reproducibility
set.seed(123)
# Splitting the data into training (75%) and testing (25%) datasets
sample_indices <- sample(nrow(data_1), 0.75 * nrow(data_1))  # 75% indices for training

# Training dataset (75%)
development_data <- data_1[sample_indices, ]
validation_data <- data_1[-sample_indices, ]

# Train random forest model
rf <- randomForest(cardio ~ ., data = development_data, ntree = 100)

# Make predictions on testing data
predictions_rf <- predict(rf, validation_data)

# Create confusion matrix
confusion_matrix_rf <- confusionMatrix(factor(predictions_rf),factor(validation_data $cardio), mode = 'everything')
print("Confusion Matrix and Performance Evaluation for Random Forest")
print(confusion_matrix_rf)

# Get feature importances
importances <- rf$importance
print(rownames(importances))

importances_df <- data.frame(Feature = rownames(importances), Importance = importances)

print(importances_df)

importances <- rf$importance
print(rownames(importances))
importances_df <- data.frame(
  Feature = rownames(importances),
  Importance = importances
)  

#Plots for Variable Importance and Feature Importance
varImpPlot(rf, sort = TRUE, main = "Variable Importance Plot")
ggplot(importances_df, aes(x = importances, y = Feature)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Feature Importances", x = "importances", y = "Feature") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
warnings()

######################################  
#Logistic Regression
# Set seed for reproducibility
set.seed(123)

# Create indices for training and validation sets
index_train_datasaet <- createDataPartition(data_1$cardio, p = 0.6, list = FALSE)
index_validation_datasaet <- createDataPartition(data_1$cardio[-index_train_datasaet], p = 0.5, list = FALSE)


training_data_logreg <- data_1[index_train_datasaet,]  # Training dataset (60%)

# Testing dataset (20%)
validation_data_logreg <- data_1[-index_train_datasaet,][index_validation_datasaet,]

# Validation dataset (20%)
testing_data_logreg <- data_1[-index_train_datasaet,][-index_validation_datasaet,]

# Separate features and target variable for training set
X_train_logreg <- training_data_logreg[, -ncol(training_data_logreg)]
Y_train_logreg <- training_data_logreg$cardio

# Separate features and target variable for validation set
X_validation_logreg <- validation_data_logreg[, -ncol(validation_data_logreg)]
Y_validation_logreg <- validation_data_logreg$cardio

# Separate features and target variable for testing set
X_test_logreg <- testing_data_logreg[, -ncol(testing_data_logreg)]
Y_test_logreg <- testing_data_logreg$cardio

# Model Training using Logistic Regression
model_logreg <- glm(cardio ~ ., data = training_data_logreg, family = binomial)
summary(model_logreg)

# Predictions on validation dataset
predictions_validation_logreg <- predict(model_logreg, newdata = validation_data_logreg, type = "response")
predicted_classes_validation_logreg <- ifelse(predictions_validation_logreg > 0.5, 1, 0)

# Model Evaluation on validation set
confusion_matrix_validation_logreg <- confusionMatrix(factor(predicted_classes_validation_logreg),factor(validation_data_logreg$cardio), mode = 'everything')

print("Confusion Matrix and Performance Evaluation for Logistic Regression Validation Set")
print(confusion_matrix_validation_logreg)

# Predictions on testing dataset
predictions_test_logreg <- predict(model_logreg, newdata = testing_data_logreg, type = "response")
predicted_classes_test_logreg <- ifelse(predictions_test_logreg > 0.5, 1, 0)

# Model Evaluation on testing set
confusion_matrix_test_logreg <- confusionMatrix(factor(predicted_classes_test_logreg),
                                                factor(testing_data_logreg$cardio), mode = 'everything')
print("Confusion Matrix and Performance Evaluation for Logistic Regression Testing Set")
print(confusion_matrix_test_logreg)




#############################################
#Naive bayes model
set.seed(123) # Set seed for reproducibility
training_Index_data <- sample.split(data_1$cardio, SplitRatio = 0.7)

feature_data <- data_1[training_Index_data ,] # Training dataset (70%)
target_data <- data_1[-training_Index_data ,] # Testing dataset (30%)
nb_X_feature_data<- feature_data[,-ncol(feature_data)]
nb_Y_feature_data <- feature_data$cardio
nb_X_target_data <- target_data[,-ncol(feature_data)]
nb_Y_target_data <- target_data$cardio

model_nb <- naive_bayes(x = nb_X_feature_data, y = nb_Y_feature_data)
summary(model_nb)
# Predict using the trained model
predicted_nb <- predict(model_nb, newdata = nb_X_target_data)

# Create confusion matrix and compute performance metrics
nb_confusion_matrix <- confusionMatrix(data = factor(predicted_nb), reference = factor(nb_Y_target_data),mode='everything')
print("Confusion Matrix and Performance Evaluation for Naive Bayes")
print(nb_confusion_matrix)



#############################################
#KnnSplitting data into training and test data
set.seed(123) # Set seed for reproducibility
sampled_indices <- sample(nrow(data_1), 0.8 * nrow(data_1))
knn_training_data <- data_1[sampled_indices,] # Training dataset (80%)
knn_testing_data <- data_1[-sampled_indices,] # Testing dataset (20%)

knn_x_training_data <- knn_training_data[,-ncol(knn_training_data)]
knn_y_training_data <- knn_training_data$cardio
knn_x_testing_data <- knn_testing_data[,-ncol(knn_testing_data)]
knn_y_testing_data <- knn_testing_data$cardio

# Train k-Nearest Neighbors model
k <- 8  # Set the number of neighbors
model_knn <- knn(train = knn_x_training_data, test = knn_x_testing_data, cl = knn_y_training_data, k = k)
summary(model_knn)

knn_confusion_matrix <- confusionMatrix(factor(model_knn), factor(knn_y_testing_data), mode='everything')
print("Confusion Matrix and Performance Evaluation for KNN")
print(knn_confusion_matrix)


#############################################

#decision tree
set.seed(123) # Set seed for reproducibility

# Determine the number of rows for training and testing data
total_rows <- nrow(data_1)
train_ratio <- 7.2 / 10  # 7.2:2.8 ratio for training and testing data
train_size <- round(train_ratio * total_rows)

# Shuffle the indices
indices <- sample(total_rows)

# Split the data into training and testing sets based on the ratio
train_indices <- indices[1:train_size]
test_indices <- indices[(train_size + 1):total_rows]

training_data <- data_1[train_indices, ] # Training dataset (7.2)
testing_data <- data_1[test_indices, ]   # Testing dataset (2.8)

# Assigning variables for modeling
a_train <- training_data[, -ncol(training_data)]
b_train <- training_data$cardio
a_test <- testing_data[, -ncol(testing_data)]
b_test <- testing_data$cardio

# Decision tree modeling
model_tree <- rpart(cardio ~ ., data = training_data, method = "class")
model_tree

# Make predictions
predicted_dtree <- predict(model_tree, a_test, type = "class")

# Create confusion matrix
tree_confusion_matrix <- confusionMatrix(predicted_dtree, b_test, mode = 'everything')
print("Confusion Matrix and Performance Evaluation for Decision Tree")
print(tree_confusion_matrix)


#################################
#SVM
set.seed(123)
training_index <- sample(c(TRUE, FALSE), nrow(data_1), replace=TRUE, prob=c(0.7,0.3))
model_traindata_svm <- subset(data_1, training_index == TRUE)
model_testdata_svm <- subset(data_1, training_index == FALSE)

# Training the SVM model
svm_model <- svm(cardio ~ ., data = model_traindata_svm, kernel = "linear")

# Making predictions on the test set
predictions <- predict(svm_model, model_testdata_svm)

svm_confusion_matrix <- confusionMatrix(factor(predictions), factor(model_testdata_svm$cardio))
print("Confusion Matrix and Performance Evaluation for SVM")
print(svm_confusion_matrix)


###########################3



#ROC Curve for all models

library(pROC)

roc_logreg <- roc(Y_validation_logreg, predictions_validation_logreg)
auc_logreg <- auc(roc_logreg)
plot(roc_logreg, main = "ROC Curves", col = "blue", lwd = 2, cex.main = 1.5, cex.lab = 1.5)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = paste("Logistic Regression (AUC =", round(auc_logreg, 3), ")"), col = "blue", lty = 1, cex = 0.8)

# Adding ROC curves for other models
roc_nb <- roc(nb_Y_target_data, as.numeric(predicted_nb))
auc_nb <- auc(roc_nb)
lines(roc_nb, col = "green", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", paste("Naive Bayes (AUC =", round(auc_nb, 3), ")")), col = c("blue", "green"), lty = 1, cex = 0.8)

roc_knn <- roc(knn_y_testing_data, as.numeric(model_knn))
auc_knn <- auc(roc_knn)
lines(roc_knn, col = "red", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Naive Bayes", paste("KNN (AUC =", round(auc_knn, 3), ")")), col = c("blue", "green", "red"), lty = 1, cex = 0.8)

roc_dtree <- roc(b_test, as.numeric(predicted_dtree))
auc_dtree <- auc(roc_dtree)
lines(roc_dtree, col = "purple", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Naive Bayes", "KNN", paste("Decision Tree (AUC =", round(auc_dtree, 3), ")")), col = c("blue", "green", "red", "purple"), lty = 1, cex = 0.8)

roc_rf <- roc(validation_data$cardio, as.numeric(predictions))
auc_rf <- auc(roc_rf)
lines(roc_rf, col = "orange", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Naive Bayes", "KNN", "Decision Tree", paste("Random Forest (AUC =", round(auc_rf, 3), ")")), col = c("blue", "green", "red", "purple", "orange"), lty = 1, cex = 0.8)

#ROC independently#####################

par(mfrow = c(2, 3))  # 2 rows and 3 columns for the 6 models

# ROC for Logistic Regression
roc_logreg <- roc(Y_validation_logreg, predictions_validation_logreg)
auc_logreg <- auc(roc_logreg)
plot(roc_logreg, main = "Logistic Regression", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc_logreg, 3)), col = "blue", lty = 1, cex = 0.8)

# ROC for Naive Bayes
roc_nb <- roc(nb_Y_target_data, as.numeric(predicted_nb))
auc_nb <- auc(roc_nb)
plot(roc_nb, main = "Naive Bayes", col = "green", lwd = 2)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc_nb, 3)), col = "green", lty = 1, cex = 0.8)

# ROC for k-Nearest Neighbors (KNN)
roc_knn <- roc(knn_y_testing_data, as.numeric(model_knn))
auc_knn <- auc(roc_knn)
plot(roc_knn, main = "KNN", col = "red", lwd = 2)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc_knn, 3)), col = "red", lty = 1, cex = 0.8)

# ROC for Decision Tree
roc_dtree <- roc(b_test, as.numeric(predicted_dtree))
auc_dtree <- auc(roc_dtree)
plot(roc_dtree, main = "Decision Tree", col = "purple", lwd = 2)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc_dtree, 3)), col = "purple", lty = 1, cex = 0.8)

# ROC for Random Forest
roc_rf <- roc(validation_data$cardio, as.numeric(predictions))
auc_rf <- auc(roc_rf)
plot(roc_rf, main = "Random Forest", col = "orange", lwd = 2)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc_rf, 3)), col = "orange", lty = 1, cex = 0.8)

# Reset the plotting parameters
par(mfrow = c(1, 1))

