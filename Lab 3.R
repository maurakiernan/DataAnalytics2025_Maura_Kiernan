###################
##### Abalone #####
###################

# read dataset
abalone <- read.csv("C:\\Users\\kiernm\\Downloads\\abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

library(class)

##length knn model

knn.predicted <- knn(train = dataset[,2:5], test = dataset[,2:5], cl = dataset$length, k=65)

contingency.table <- table(knn.predicted, dataset$length, dnn=list('predicted', 'actual'))

contingency.table

#accuracy

sum(diag(contingency.table))/length(dataset$age.group)

k.list <-c(59,61,63,65,67,69,71)

accuracy.list <- c()

#training loop for k
for (k in k.list) {
  knn.predicted <- knn(train = dataset[,2:5], test = dataset[,2:5], cl = dataset$length, k=k)
  contingency.table <- table(knn.predicted, dataset$length, dnn=list('predicted','actual'))
  accuracy <- sum(diag(contingency.table))/length(dataset$length)
  accuracy.list <- c(accuracy.list, accuracy)
}

plot(k.list, accuracy.list, type = "b")

##age group knn model

knn.predicted <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k=65)
contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted', 'actual'))

contingency.table

#accuracy

sum(diag(contingency.table2))/length(dataset$age.group)

k.list <- c(59,61,63,65,67,69,71)

accuracy.list <- c()

#k training loop

for (k in k.list) {
  knn.predicted <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k=k)
  contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual') )
  contingency.table
  
  accuracy <- sum(diag(contingency.table))/length(dataset$age.group)
  accuracy.list <- c(accuracy.list,accuracy)
}

plot(k.list,accuracy.list, type = "b")

##multiple k value testing

#cluster 1?

k.list <- c(2,3,4,5)

wcss.list <- c()

for (k in k.list) {
  abalone.km <- kmeans(dataset[,2:4],centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list, wcss)
  
  #cluster and output
  
  assigned.clusters <- as.factor(abalone.km$cluster)
}

plot(k.list,wcss.list, type = "b")

#cluster 2?

k.list <- c(7,8,9,10)

wcss.list <- c()

for (k in k.list) {
  abalone.km <- kmeans(dataset[,2:4],centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list, wcss)
  
  #cluster and output
  
  assigned.clusters <- as.factor(abalone.km$cluster)
}

plot(k.list,wcss.list, type = "b")
