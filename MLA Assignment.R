#Packages
require(fma)
require(knitr)
require(Distance)
library('philentropy')
library(dendextend)
library(dplyr)
library(cluster)
library(devtools)
library(PCAmixdata)
library(MASS)
library('lda')
library(class)
library(ellipse)
library("flexclust")
library(ggplot2)
install.packages("gghighlight")
library(gghighlight)

## Reading in Data
heart_data <- read.csv('/Users/nathancarney/Documents/College/3rd Year/MLA/Assignment 1/heart_data.csv')

head(heart)
ncol(heart_data)

#Summary of Continuous variables

mean_Vector <- c()

sd_vector <- c()

for (i in 1:(ncol(heart_continuous))) {
  column_mean <- mean(heart_continuous[,i])
  mean_Vector <- c(mean_Vector, column_mean)
  sd_vector <- c(sd_vector, sd(heart_continuous[,i]))
  
}
sd_vector
mean_Vector
summaryVector <- rbind(mean_Vector, sd_vector)
summaryDF <- as.data.frame(summaryVector)
row.names(summaryDF) = c("Mean", "Standard Deviation")
columnNames <- colnames(heart_continuous)
colnames(summaryDF) = columnNames

## Separating the Data into scaled/unscaled continuous data sets and binary data sets

heart_continuous <- heart_data[, c(1,3,4,6,8,9)]
scale_heart_con <- scale(heart_continuous)

heart_binary <- heart_data[, c(2,5,7)]
scale_heart_bin <- scale(heart_binary)


## Function for identifying outliers, being defined as observations which are
## 3 standard deviations away from the column mean

identifyOutliers <- function(dataSet)
{
  i = 0
  dataColumns <- ncol(dataSet)
  dataRows <- nrow(dataSet)
  outliers <- vector()
  
  for (i in 1:dataColumns) {
    
    sampleColumn <- dataSet[,i]
    columnMean <- mean(sampleColumn)
    columnSd <- sd(sampleColumn)
    
    for (j in 1:dataRows) {
      
      observation <- dataSet[j,i]
      if(observation > (columnMean + 3*columnSd) | (observation < (columnMean - (3*columnSd))))
      {
        outliers <- c(outliers, j)
      }
    }
  }
  return(outliers)
}

## Creating new Data Sets using raw data minus outliers

outliers <- identifyOutliers(heart_continuous)

heart_data_minus_outliers <- heart_data[-outliers,]

heart_continuous_minus_outliers <- heart_data_minus_outliers[, c(1,3,4,6,8,9)]
scale_heart_con_minus_outliers <- scale(heart_continuous_minus_outliers)

heart_binary_minus_outliers <- heart_data_minus_outliers[, c(2,5,7)]
scale_heart_bin_minus_outliers <- scale(heart_binary_minus_outliers)



## 1. Principal Components Analysis - Raw Data, standardised

raw_heart_pca <- prcomp(heart_data[,1:9], scale. = TRUE)

summary(raw_heart_pca) ## This method's PC's explain less of the data when compared to 
# data without outliers

print(raw_heart_pca) # In PC1, the contrast between coefficients of the 
#linear combination is less pronounced

plot(heart_pca, main = "Heart Data", type = "l")
heart_pca_var_explain <- (heart_pca$sdev^2) / (sum(heart_pca$sdev^2))
plot(heart_pca_var_explain, type = "b", main = "Heart Data",
     xlab = "No. of components", ylab = "Proportion of correlation explained", xaxt = "n")
axis(1, at = 1:9)

newheart <- predict(heart_pca)

pairs(heart_data_minus_outliers[,1:9],col=heart_data_minus_outliers[,10]) ## plotting all 
#variables against each other, the colour of each observation is classified depending on
#whether or not that person has heart disease

pairs(newheart[,1:6],col=heart_data[,10]) 
#Plot of PC's against each other, with each observation being black or red, 
#depending on if they have heart disease or not

## 2. Principal Components Analysis - Data without outliers, standardised

heart_pca <- prcomp(heart_data_minus_outliers[,1:9], scale. = TRUE) ##Class not included as it is a dependent variable
                                                                    ## and changed result of PCA by a negligible amount
print(heart_pca)
summary(heart_pca)  ## Best to use 6 PC's, as covers 81.79% of the data

plot(heart_pca, main = "Heart Data", type = "l")
heart_pca_var_explain <- (heart_pca$sdev^2) / (sum(heart_pca$sdev^2))
plot(heart_pca_var_explain, type = "b", main = "Heart Data",
     xlab = "No. of components", ylab = "Proportion of correlation explained", xaxt = "n")
axis(1, at = 1:9)

newheart <- predict(heart_pca)

pairs(heart_data_minus_outliers[,1:9],col=heart_data_minus_outliers[,10]) ## plotting all 
#variables against each other, the colour of each observation is classified depending on
#whether or not that person has heart disease

pairs(newheart[,1:6],col=heart_data[,10]) 
#Plot of PC's against each other, with each observation being black or red, 
#depending on if they have heart disease or not

## HC 

## Functions for HC

binaryConverted = function(dataSet)
{
  i = 0
  binaryVariables = matrix(nrow = 263, ncol = 3)
  for(i in 1:263)
  {
    if(dataSet[i,2] == 1)
    {
      binaryVariables[i,1] = 0
    }
    else{
      binaryVariables[i,1] = 1
    }
    if(dataSet[i,5] == 1)
    {
      binaryVariables[i,2] = 0
    }
    else{
      binaryVariables[i,2] = 1
    }
    if(dataSet[i,7] == 1)
    {
      binaryVariables[i,3] = 0
    }
    else{
      binaryVariables[i,3] = 1
    }
    i = i+1
  }  
  return(binaryVariables)
}

cutOffLine = function(sampleCluster)
{
  abline(h = cut_off, lty = 2, col = 2)
}

visualizeClusters = function(numberOfClusters, sampleCluster2, cutOff)
{
  suppressPackageStartupMessages(library(dendextend))
  avg_dend_obj <- as.dendrogram(sampleCluster2)
  avg_col_dend <- color_branches(avg_dend_obj, h = cutOff)
  plot(avg_col_dend)
}

## Binary Data converted to 1's and 0's, without outliers
heart_bin <- binaryConverted(heart_data_minus_outliers)
head(heart_bin)
heart_factor1 <- factor(heart_bin[,1])
heart_factor2 <- factor(heart_bin[,2])
heart_factor3 <- factor(heart_bin[,3])
heart_fac <- cbind(c(heart_factor1, heart_factor2, heart_factor3 ))
class(mat)
heart_data_mixed <- cbind(heart_continuous_minus_outliers, heart_bin)
head(heart_data_mixed)
heart_bin_factor <- as.factor(heart_bin)
PCAmix(X.quanti = heart_data_mixed[1:6], X.quali = heart_bin_factor, ndim = 9)
class(heart_factor1)
head(heart_factor)
length(heart_factor)
heart_factor
heart_data_factor <- cbind()
binaryColumnNames <- colnames(heart_binary)
colnames(heart_bin) = binaryColumnNames
colnames(heart_bin_minus_outliers) = binaryColumnNames

## Binary Jaccard Distance  with complete linkage 
heart_binary_jac_dist <- distance(heart_bin, method = 'jaccard',use.row.names = TRUE, as.dist.obj = TRUE)
binary_jaccard_cluster <- hclust(heart_binary_jac_dist, method = 'complete')
plot(binary_jaccard_cluster)
h_bar <- mean(binary_jaccard_cluster$height)
standard_deviation <- 3*(sd(binary_jaccard_cluster$height))
cut_off <- h_bar + standard_deviation
cutOffLine(binary_jaccard_cluster)

## Binary 'binary' distance with complete linkage
heart_binary_distance <- dist(heart_bin, method = 'binary')
binary_binary_cluster <- hclust(heart_binary_distance, method = 'complete')
plot(binary_binary_cluster)
cutOffLine(binary_binary_cluster)
h_bar <- mean(binary_binary_cluster$height)
standard_deviation <- 3*(sd(binary_binary_cluster$height))
cut_off <- h_bar + standard_deviation
visualizeClusters(7, binary_binary_cluster, cut_off)

## Continuous Euclidean Distance

continuous_euclidean_dist <- dist(scale_heart_con_minus_outliers, method = 'euclidean')
(continuous_euclidean_dist)

#Continuous Complete Linkage + Visualisation
continuous_euclidean_complete_cluster <- hclust(continuous_euclidean_dist, method = "complete")
plot(continuous_euclidean_complete_cluster)

h_bar <- mean(continuous_euclidean_complete_cluster$height)
standard_deviation <- 3*(sd(continuous_euclidean_complete_cluster$height))
cut_off <- h_bar + standard_deviation
plot(continuous_euclidean_complete_cluster)
cutOffLine(continuous_euclidean_complete_cluster)

visualizeClusters( 7, continuous_euclidean_complete_cluster, cut_off)
cutOffLine(continuous_euclidean_complete_cluster)

rect.hclust(continuous_euclidean_complete_cluster, k = 7, border = 1)


#Continuous with Ward Linkage
continuous_euclidean_average_cluster <- hclust(continuous_euclidean_dist, method = "ward.D")
plot(continuous_euclidean_average_cluster)

h_bar <- mean(continuous_euclidean_average_cluster$height)
standard_deviation <- 3*(sd(continuous_euclidean_average_cluster$height))
cut_off <- h_bar + standard_deviation
plot(continuous_euclidean_average_cluster)
cutOffLine(continuous_euclidean_average_cluster)

visualizeClusters(6, continuous_euclidean_average_cluster, cut_off)
cutOffLine(continuous_euclidean_average_cluster)

rect.hclust(continuous_euclidean_average_cluster, k = 6, border = 1)



##  Binary Binary + Continuous Euclidean 

fused_distance <- (3/9 * heart_binary_distance) + (6/9 * continuous_euclidean_dist)

## fused Distance with Complete Linkage
cluster_3 <- hclust(fused_distance, method = "complete")
plot(cluster_3)

h_bar <- mean(cluster_3$height)
standard_deviation <- 3*(sd(cluster_3$height))
cut_off <- h_bar + standard_deviation
cutOffLine(cluster_3)

visualizeClusters(6, cluster_3, cut_off)
cutOffLine(cluster_3)

rect.hclust(cluster_3, k = 6, border = 1)

## Fused Distance with Ward Linkage, cant be used as distance method must be euclidean
#cluster_4 <- hclust(fused_distance, method = "ward.D")
#plot(cluster_4)

#h_bar <- mean(cluster_4$height)
#standard_deviation <- 3*(sd(cluster_4$height))
#cut_off <- h_bar + standard_deviation
#cutOffLine(cluster_4)

#visualizeClusters(5, cluster_4, cut_off)
#cutOffLine(cluster_4)

## Gower Distance

  gower_data <- cbind(scale_heart_con_minus_outliers, heart_binary_minus_outliers)
  
  gower_dist <- daisy(gower_data, metric = "gower", stand = TRUE)
  gower_dist_binary <- daisy(heart_bin, metric = "gower")
  
  # Gower Complete Linkage
  gower_cluster <- hclust(gower_dist)
  gower_cluster_binary  <- hclust(gower_dist_binary)
  plot(gower_cluster_binary)
  h_bar <- mean(gower_cluster_binary$height)
  standard_deviation <- 3*(sd(gower_cluster_binary$height))
  cut_off <- h_bar + standard_deviation
  cutOffLine(gower_cluster_binary)
  
  glower_dendogram <- as.dendrogram(gower_cluster)
  plot(glower_dendogram, horiz = FALSE, leaflab = "none")
  h_bar <- mean(gower_cluster$height)
  standard_deviation <- 3*(sd(gower_cluster$height))
  cut_off <- h_bar + standard_deviation
  visualizeClusters(8, gower_cluster, cut_off)
  cutOffLine(gower_cluster)
  
  #Gower Average Linkage
  
  gower_cluster_average <- hclust(gower_dist, method = "average")
  
  gower_dendogram <- as.dendrogram(gower_cluster_average)
  plot(gower_dendogram, horiz = FALSE, leaflab = "none")
  h_bar <- mean(gower_cluster_average$height)
  standard_deviation <- 3*(sd(gower_cluster_average$height))
  cut_off <- h_bar + standard_deviation
  visualizeClusters(8, gower_cluster_average, cut_off)
  cutOffLine(gower_cluster_average)
  
  rect.hclust(gower_cluster_average , k = 8, border = 1)
  
  ## Rand Index on Continuous CLusters
  
  cluster_1_label <- cutree(continuous_euclidean_complete_cluster, k=7)
  cluster_2_label <- cutree(continuous_euclidean_average_cluster, k =6)
  
  randIndex(cluster_1_label, cluster_2_label,correct = FALSE) # 78% agreement
  
## K Means Analysis 

WSS <- rep(0,15)

kmax <- 15
k <- 1:kmax

k_means_summary <- cbind(k, WSS) 
colnames(k_means_summary)

for (i in 1:kmax) {
  means_result <- sum(kmeans(scale_heart_con_minus_outliers, centers = i)$withinss)
  WSS[i] = means_result # Filling WSS Vector with results 
}

WSS
k_means_summary <- cbind(k, WSS) # Combining k and WSS vectors
k_means_summary

plot(k_means_summary, type = "l", xlab = "k", ylab = "WSS") 

k <- 6
cl2 <- kmeans(scale_heart_con_minus_outliers,centers = 6)
table(cl2$cluster)

cl2$centers 

cl2$withinss 

plot(scale_heart_con_minus_outliers, col = cl2$cluster)
points(cl2$centers, col=1:k, pch=8, cex=5)

cl2$centers[1,]

dist(cl2$centers)

g1 <- scale_heart_con_minus_outliers[which(cl2$cluster==1),]
ng1 <- cl2$size[1]
total1 <- sum(as.matrix(dist(rbind(g1, cl2$centers[1,])))[ng1+1,])
ave1 <- total1/ng1 

ave1

g2 <- scale_heart_con_minus_outliers[which(cl2$cluster==2),]
ng2 <- cl2$size[2]
total2 <- sum(as.matrix(dist(rbind(g2, cl2$centers[1,])))[ng2+1,])
ave2 <- total2/ng2
ave2
g3 <- scale_heart_con_minus_outliers[which(cl2$cluster==3),]
ng3 <- cl2$size[3]
total3 <- sum(as.matrix(dist(rbind(g3, cl2$centers[1,])))[ng3+1,])
ave3 <- total3/ng3

g4 <- scale_heart_con_minus_outliers[which(cl2$cluster==4),]
ng4 <- cl2$size[4]
total4 <- sum(as.matrix(dist(rbind(g4, cl2$centers[1,])))[ng4+1,])
ave4 <- total4/ng4

g5 <- scale_heart_con_minus_outliers[which(cl2$cluster==5),]
ng5 <- cl2$size[5]
total5 <- sum(as.matrix(dist(rbind(g5, cl2$centers[1,])))[ng5+1,])
ave5 <- total5/ng5

g6 <- scale_heart_con_minus_outliers[which(cl2$cluster==6),]
ng6 <- cl2$size[6]
total6 <- sum(as.matrix(dist(rbind(g6, cl2$centers[1,])))[ng6+1,])
ave6 <- total6/ng6

averages <- c(ave1, ave2, ave3, ave4, ave5, ave6)
averages

## KNN

#50/25/25 split
#split of class variable
?cbind
head(heart_data_minus_outliers)
kdata <- cbind(scale_heart_con_minus_outliers, heart_data_minus_outliers[,10] )
colnames(kdata) = c("Age", "RestBloodPressure", "SerumCholestoral", "MaxHeartRate", "Slope", "MajorVessels", "Class" )
class(kdata[,7])
length(kdata[,7])
colnames(kdata)[,7] <- "Class"
head(kdata)

kdata <- as.data.frame(kdata)


head(kdata)

kdata <- kdata[order(kdata$Class),]
kdata[148,]

?which
HDindex <- which(kdata[,7] == 2, arr.ind = TRUE)
HDindex
noHDindex <- which(kdata[,7] == 1, arr.ind = TRUE)
noHDindex
numberHD <- length(HDindex)
numberNoHD <- length(noHDindex)

HDindex <- as.data.frame(HDindex)
HDindex

noHDindex <- as.data.frame(noHDindex)
noHDindex

HDindex <- HDindex[-c(113:115),]
length(HDindex)
noHDindex <- noHDindex[-c(113:148),]
length(noHDindex)

kdata <- kdata[-c(225:227),]
nrow(kdata)

index_train <- c(1:56, 1:56 + 112)
index_test<- c(1:28 + 56, 1:28 + 56 + 112)
index_valid <- c(1:28 + 28 + 56, 1:28 + 28 + 56 + 112)

train <- kdata[index_train, 1:6]
test <- kdata[index_test, 1:6]
valid <- kdata[index_valid, 1:6]

nrow(train)
  
heart_knn_result <- knn(train, test, cl = kdata[index_train, 7], k = 6)
length(heart_knn_result)

class_agree <- table(heart_knn_result, kdata[index_test,7])
class_agree


kmax <- 50
k <- 1:kmax
p <- rep(0, kmax)
ntest <- nrow(test)
k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")
for(i in 1:kmax){
  heart_knn_result <- knn(train, test, cl = kdata[index_train, 7], k = i)
  class_agree <- table(heart_knn_result, kdata[index_test, 7])
  sum_agree <- sum(diag(class_agree))
  k_summary[i, 2] <- (ntest - sum_agree) / ntest
}
k_summary[1:15, ]

k_summary <- as.data.frame(k_summary)

kplot <- plot(k_summary[,1], k_summary[,2], main = "Misclassification rate for each k", xlab = "K", ylab = "Misclassification Rate")
symbols(x=12, y=0.1607143, circles=.75, add=T, inches=F, fg = 'Red')
identify(k_summary)
result <- knn(valid, test, cl = kdata[index_valid, 7], k=12)

result

class_agree_valid <- table(result, kdata[index_valid, 7])
class_agree_valid

sum_agree_valid <- sum(diag(class_agree_valid))
sum_agree_valid

(nrow(valid) - sum_agree) / nrow(valid)

install.packages('kknn')
?kknn

kknn(train, test, cl = kdata[index_test, 7], k=12)

data(kdata)
m <- dim(kdata)[1]
m
val <- sample(1:m, size = round(m/2), replace = FALSE, 
              prob = rep(1/m, m)) 
heart.learn <- kdata[-val,]
heart.valid <- kdata[val,]
heart.kknn <- kknn(Class~., heart.learn, heart.valid, distance = 1,
                  kernel = "triangular")
?kknn
heart.kknn$fitted.values
summary(heart.kknn)
fit <- fitted(heart.kknn)
table(heart.valid$Class, fit)
pcol <- as.character(as.numeric(heart.valid$Class))
pairs(heart.valid[1:6], pch = pcol, col = c("green3", "red")
      [(heart.valid$Class != fit)+1])

## LDA / QDA

LDAdata <- heart_data_minus_outliers
length(LDAdata)
colnames(LDAdata) = c("Age", "Sex", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "MaxHeartRate", "ExerciseInduced", "Slope", "MajorVessels", "Class")
head(LDAdata)
LDAdata <- LDAdata[order(LDAdata$Class),]
nrow(LDAdata)
head(LDAdata)
#80/20 split
head(lc)
LDAdata <- LDAdata[-c(116:148),]
nrow(LDAdata)
plot(LDAdata[,c(1:9)], col = as.factor(LDAdata[, 10]))
lines(ellipse(cov(LDAdata[c(1:115), c(1:9)]), centre = colMeans(LDAdata[c(1:115), c(1:9)]),level = c(0.5)))
lines(ellipse(cov(LDAdata[c(116:230), c(1:9)]), centre = colMeans(LDAdata[c(116:230), c(1:9)]), level = 0.5), col = 2)

htrain <- LDAdata[c(1:92, 1:92 + 115),]
htest <- LDAdata[c(1:23 + 92 , 1:23 + 115 + 92),]

ldasol <- lda(htrain[, c(1:9)], grouping = htrain[,10])

ldasol$prior

ldasol

ldaPredict <- predict(ldasol, htest[, c(1:9)])

7/22

# Cross Validation 

ldasol_cv <- lda(LDAdata[,c(1:9)], grouping = LDAdata[, 10], CV = TRUE)

plot(LDAdata[, c(1, 4)], col = as.factor(LDAdata[, 10]), pch = as.numeric(ldasol_cv$class))

#QDA 

qdasol <- qda(htrain[, c(1:9)], grouping = htrain[, 10])
predict(qdasol, htest[, c(1:9)])

16/22




