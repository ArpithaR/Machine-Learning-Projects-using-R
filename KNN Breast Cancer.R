# KNN Classification of Breast Cancer cells as 'benign' or 'malignant'

# Include libraries
library(class)
library(gmodels)

# Read raw input data file
cancer_raw <- read.csv("breast-cancer-wisconsin.csv", stringsAsFactors = FALSE) [-1]
# Convert target variable to factor
cancer_raw$diagnosis <- factor(cancer_raw$diagnosis, levels=c("B","M"))

# Normalization function 
normalizeVal <- function(x) {
    n <- (x-min(x))/(max(x)-min(x))
}

# Normalize values in columns 2 to 31 to values between 0 and 1
cancer_n <- as.data.frame(lapply(cancer_raw[2:31], normalizeVal))

# Divide input data into train and test datasets in 80:20 ratio
cancer_train <- cancer_n[1:469, ]
cancer_test <- cancer_n[470:569, ]

cancer_train_target <- cancer_raw[1:469, 1]
cancer_test_target <- cancer_raw[470:569, 1]

# Train and classify using KNN 
cancer_pred <- knn(train=cancer_train, test=cancer_test, cl=cancer_train_target, k=15)

# Check performance of the model
CrossTable(x=cancer_test_target, y=cancer_pred, prop.chisq=FALSE)