#rload dataset
data<-iris
head(data)
tail(data)

# summarize the class distribution
percentage <- proportions(table(data$Species)) * 100 # Alternatif : prop.table
cbind(freq=table(data$Species), percentage=percentage)

#check levels of Species
levels(data$Species)

#check missing value
sum(is.na(data))

# summarize attribute distributions
summary(data)

# Create Data Partition
library(caret)
set.seed(123)
index <- createDataPartition(data$Species, p=0.80, list=FALSE)

# select 80% of the data for Training
training <- data[index,] # Data Frame [baris, kolom]
dim(training)

# use the remaining 20% of data to testing the models
testing <- data[-index,]

##### NAIVE BAYES
library(caTools)
library(e1071)
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Species ~ ., data = training)

# Predicting on training data'
y_pred <- predict(classifier_cl, newdata = training)
# Confusion Matrix
cm <- table(training$Species, y_pred)
cm
# Model Evaluation
confusionMatrix(cm)

# Predicting on test data'
y_pred2 <- predict(classifier_cl, newdata = testing)

# Confusion Matrix
cm2 <- table(testing$Species, y_pred2)
cm2

# Model Evaluation
confusionMatrix(cm2)
dim(testing)