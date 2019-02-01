install.packages("caret",dependencies = c("Depends", "Suggests"))
library (caret)
filename <- "Iris_Data.csv"
# load file
setwd("C:/Users/P7576/Documents/R")
dataset <- read.csv("Iris_Data.csv")

# 80% OF THE DATASET ARE USED FOR WHILE
validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)
#20% OF THE DATASET ARE USED FOR VALIDATION
validation <- dataset[-validation_index,]

#80% of the dataset for testing and training
dataset <- dataset[validation_index,]

#checking dimension of the dataset 
dim(dataset)

#list type for the attribute
sapply(dataset, class)

#checking for head
head(dataset)

#list the levels for the class
levels(dataset$Species)

#summarize the class distribution 
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage = percentage)

#summarize all attribute distribution
summary(dataset)

#split input and output
x<- dataset[, 1:4]
y <- dataset[,5]

#boxplot for each attribute 
par(mfrow = c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main = names(iris)[i])
}

#barplot for class breakdown
plot(y)

#scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

#box and whisker plots for each atrribute
featurePlot(x=x, y=y, plot= "box")

#density plots for each attribute by class values
scales <- list(x=list(relation = "free"), y = list(relation = "free"))
featurePlot(x=x, y=y, plot = "density", scales = scales)

# run algorithms using 10fold cross validation
control <- trainControl(method = "cv", number = 10)
metrix <- "Accuracy"

#Building 5 models with different algorithm 

#Linear Discrimant Analysis (LDA)
set.seed(7)
fit.lda <- train (Species~., data = dataset, method = "lda", metrix = metrix, trControl = control)

#Classification and regression Trees (CART) Nonlinear Algorithms
set.seed(7)
fit.cart <- train (Species~., data = dataset, method = "rpart", metrix = metrix, trControl = control)

#k-nearest neighbors(knn)
set.seed(7)
fit.knn <- train (Species~., data = dataset, method = "knn", metrix = metrix, trControl = control)

#support vector machine with linear kernel (svm)
set.seed(7)
fit.svm <- train (Species~., data = dataset, method = "svmRadial", metrix = metrix, trControl = control)

#Random Forest (rf)
set.seed(7)
fit.rf <- train (Species~., data = dataset, method = "rf", metrix = metrix, trControl = control)

#summarize accuracy of models
results <- resamples(list(lda=fit.lda,  svm = fit.svm, rf = fit.rf))
summary(results)

#compare accuracy of models
dotplot(results)

#summarize the best model
print(fit.rf)

#estimate the skill of random forest on the validation datatset
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions,validation$Species)