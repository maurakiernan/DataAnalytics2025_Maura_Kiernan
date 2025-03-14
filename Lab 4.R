wine <- read.csv("C:\\Users\\kiernm\\Downloads\\wine\\wine.data")

colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavenoids" , "Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

library(caret)
library(ggfortify)

wine.df <- wine
head(wine.df)

# creating another dataframe from iris dataset that contains the columns from 1 to 4
X <- wine.df[,2:14]
Y <- wine.df[,1]

## scatter plot of 2 variables colored by class
ggplot(X, aes(x = Ash, y = Magnesium, color = Y, fill = Y)) + geom_point() + 
  stat_ellipse(type = "t",geom = "polygon",alpha = 0.4)

## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## Variance/Covariance

var(X$Ash)
var(X$Magnesium)
cov(X$Ash,X$Magnesium)
cor(X$Ash,X$Magnesium)

## scatter plot of 2 variables
ggplot(X, aes(x = Ash, y = Magnesium)) + geom_point(color="blue")

var(X$Ash)
var(X$Magnesium)
cov(X$Ash,X$Magnesium)
cor(X$Ash,X$Magnesium)

## scatter plot of 2 variables
ggplot(X, aes(x = Ash, y = Magnesium)) + geom_point(color="blue")


####### PCA #######

wine$class <- cut(wine$class, br=c(0,58,129,177), labels = c("", 'adult', 'old'))

#principal_components <- princomp(X, score = TRUE)
#center data
X.scaled <- scale(X, center = TRUE)

principal_components <- princomp(X.scaled, score = TRUE)

summary(principal_components)

principal_components$loadings

#scores are new data set projected onto principal components
principal_components$scores

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
#evaluate how close arrows are to being parallel with principal components
#direction of arrow aligned with pc implied relationship
#length of arrow is strength of relationship

# loadings
principal_components$loadings

###########################################################
##Color Intensity, Alcohol, Proline, OD280/OD315 of diluted wines, Flavenoids, and Total Polyphenols contribute most
###########################################################

##Dropping Ash, Magnesium, Alcalinity of Ash, and Proanthocyanins
library(dplyr)
wine <- wine %>% select(-c(Ash, Magnesium, `Alcalinity of ash`, Proanthocyanins))

##########################################################
#Run PCA following removal
#########################################################
X <- wine[,2:10]
Y <- wine$class
X.scaled <- scale(X, center = TRUE)

principal_components <- princomp(X.scaled, score = TRUE)

summary(principal_components)

principal_components$loadings

#scores are new data set projected onto principal components
principal_components$scores

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
#evaluate how close arrows are to being parallel with principal components
#direction of arrow aligned with pc implied relationship
#length of arrow is strength of relationship

# loadings
principal_components$loadings
#####################################################################
#Classifier model, original data

wine$class <-as.factor(wine$class)

##normalize wine variables
wine_scaled <- as.data.frame(scale(wine[,-1]))
wine_scaled$class <- wine$class


##allocate data for test and training purposes
set.seed(123)
train_index <-createDataPartition(wine_scaled$class, p=.8, list = FALSE)
train_data <- wine_scaled[train_index, ]
test_data <- wine_scaled[-train_index, ]

#features and class label
train_X <- train_data[, -ncol(train_data)]
train_y <- train_data$class

test_X <- test_data[, -ncol(test_data)]
test_y <- test_data$class

#Train kNN model (k=5)
library(class)
knn_pred <- knn(train_X, test_X, train_y, cl = train_y, k = 20)
contingency.table <- table(knn_pred, test_y, dnn=list('predicted', 'actual'))

##contingency table
contingency.table

#evaluate accuracy
accuracy_wine <- sum(knn_pred == test_y) / length(test_y)
print(paste("Accuracy of original data set:", round(accuracy_wine * 100, 2), "%"))

###############################################
#classifier model, using PCA top 3

pca_model <- prcomp(wine_scaled[, -ncol(wine_scaled)], center = TRUE, scale.=TRUE)

#selecting only top three principal components
train_pca <- data.frame(pca_model$x[train_index, 1:3])
train_pca$class <- train_y

test_pca <- data.frame(pca_model$x[-train_index, 1:3])
test_pca$class <- test_y

#train kNN on PCA
knn_pca <- knn(train_pca[,-4], test_pca[,-4], train_pca$class, k=20)

#accuracy
accuracy_pca <- sum(knn_pca == test_pca$class) / length(test_pca$class)
print(paste("Accuracy of original data set:", round(accuracy_pca * 100, 2), "%"))

#contingency table pca

contingency.table.pca <- table(knn_pca, test_pca$class, dnn=list('predicted', 'actual'))

##contingency table
contingency.table.pca

Print("wine data set contingency table")
print(contingency.table)
print("wine pca contingency table")
print(contingency.table.pca)

###################################
##comparison

calculate_metrics <- function(conf_table) {
  classes <- colnames(conf_table)
  precision <- recall <- f1 <- numeric(length(classes))
  
  for (i in seq_along(classes)) {
    TP <- conf_table[i, i]
    FP <- sum(conf_table[i, ]) - TP
    FN <- sum(conf_table[, i]) - TP
    precision[i] <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    recall[i] <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    f1[i] <- ifelse((precision[i] + recall[i]) == 0, 0, 
                    2 * (precision[i] * recall[i]) / (precision[i] + recall[i]))
  }
  
  return(data.frame(Class = classes, Precision = precision, Recall = recall, F1_Score = f1))
}

# Calculate metrics for both models
metrics_original <- calculate_metrics(contingency.table)
metrics_pca <- calculate_metrics(contingency.table.pca)

# Print results
print("wine metrics")
print(metrics_original)

print("wine pca")
print(metrics_pca)

# Compare overall model performance
comparison <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score"),
  Original_Data = c(mean(metrics_original$Precision), mean(metrics_original$Recall), mean(metrics_original$F1_Score)),
  PCA_Data = c(mean(metrics_pca$Precision), mean(metrics_pca$Recall), mean(metrics_pca$F1_Score))
)

print("comparison of two models")
print(comparison)


