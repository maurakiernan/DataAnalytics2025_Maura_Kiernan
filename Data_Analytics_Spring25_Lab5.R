library(readr)
library(ggplot2)
library(e1071)
library(caret)

## read data
wine <- read.csv("C:\\Users\\kiernm\\Downloads\\wine\\wine.data")

colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavenoids" , "Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

dataset <- wine
dataset$class <- as.factor(dataset$class)

## column names
names(dataset)

## split train/test
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

x <- dataset[,2:4] 
y <- as.factor(as.matrix(dataset[,1]))

## feature boxplots
boxplot(X, main="wine features")

## class label distributions
plot(Y)

## feature-class plots
install.packages("ellipse")
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

##################################
## SVM models ##
#################################

## svm 1 - linear, alcohol and ash
ggplot(dataset, aes(x = Alcohol, y = Ash, colour = class)) +
  geom_point()

svr.mod0 <- svm(Alcohol ~ Ash, train)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

svr.pred <- cbind(test$class,svr.pred)

ggplot(svr.pred, aes(x = V1, y = svr.pred)) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- svm(log10(Alcohol) ~ log10(Ash), dataset, kernel="linear")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$Alcohol), pred=svr.pred)

ggplot(dataset, aes(x = log10(Alcohol), y = log10(Ash))) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- lm(log10(Alcohol) ~ log10(Ash), dataset)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$Alcohol), pred=svr.pred)

ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm")

# tune svm 1 - linear
tuned.svm <- tune.svm(class ~ Alcohol + Ash, data = train, kernel = 'linear',gamma = seq(1/2^nrow(dataset),1, .01), cost = 2^seq(-6, 4, 2))

tuned.svm

# svm 2 - radial, flavenoids and hue

ggplot(dataset, aes(x = Flavenoids, y = Hue, colour = class)) +
  geom_point()

svr.mod1 <- svm(Flavenoids ~ Hue, train)

summary(svr.mod1)

svr.pred1 <- predict(svr.mod1, test)

svr.pred1 <- cbind(test$class,svr.pred1)

ggplot(svr.pred1, aes(x = V1, y = svr.pred1)) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod1 <- svm(log10(Flavenoids) ~ log10(Hue), dataset, kernel="radial")

summary(svr.mod1)

svr.pred1 <- predict(svr.mod1, dataset)

svr.outs1 <- data.frame(real=log10(dataset$Flavenoids), pred=svr.pred1)

ggplot(dataset, aes(x = log10(Flavenoids), y = log10(Hue))) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod1 <- lm(log10(Flavenoids) ~ log10(Hue), dataset)

summary(svr.mod1)

svr.pred1 <- predict(svr.mod1, dataset)

svr.outs1 <- data.frame(real=log10(dataset$Flavenoids), pred=svr.pred1)

ggplot(svr.outs1, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm")

# tune svm 2 - radial
tuned.svm2 <- tune.svm(class ~ Hue + Flavenoids, data = train, kernel = 'radial',gamma = seq(1/2^nrow(dataset),1, .01), cost = 2^seq(-6, 4, 2))

tuned.svm2

#######################################
## kNN ##
#######################################

knn_train_x <- train[, c("Alcohol", "Ash")]
knn_test_x <- test[, c("Alcohol", "Ash")]

knn_train_y <- as.factor(train$class)
knn_test_y <- as.factor(test$class)

knn_test_y <- factor(knn_test_y, levels = levels(knn_train_y))

set.seed(123)
knn_pred <- knn(train = knn_train_x, test = knn_test_x, cl = knn_train_y, k = 5)

knn_pred <- as.factor(knn_pred)

############################################
## compare models ##
############################################
knn_cm <- confusionMatrix(knn_pred, knn_test_y)
print(knn_cm)

knn_precision <- knn_cm$byClass[, "Precision"]
knn_recall <- knn_cm$byClass[, "Recall"]
knn_f1 <- knn_cm$byClass[, "F1"]

svr.pred <- predict(svr.mod0, test) 
svr.pred <- as.factor(svr.pred)
svm_cm = as.matrix(table(Actual = test$class, Predicted = svr.pred))
print(cm)

cm <- table(Actual = test$class, Predicted = svr.pred)

levels_actual <- levels(test$class)
levels_predicted <- levels(svr.pred)
all_levels <- union(levels_actual, levels_predicted)

TP <- diag(cm)  
FP <- colSums(cm) - TP  
FN <- rowSums(cm) - TP  
TN <- sum(cm) - (TP + FP + FN)

precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN)) 
f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))

metrics_df <- data.frame(Class = names(TP), Precision = precision, Recall = recall, F1_Score = f1)

print(metrics_df)



