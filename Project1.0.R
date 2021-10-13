# -- -- -- SWA Group X 2021 Major Project -- -- -- #

# -- Libraries
library(plyr)
library(dplyr)
library(readxl)
library(mosaic)
library(ggplot2)
<<<<<<< Updated upstream
=======
library(rtweet)
library("tm")
library(SnowballC)
>>>>>>> Stashed changes

# -- Directory (Add your working directory here)

#Ryan
directory <- "K:/UNI/WSU/Semester 2/SWA/Group Project/SWA-21-GA"
setwd(directory)

#Byron
#directory<-"/Users/CollectiveX/Desktop/Repos/SWA-21-GA"
#setwd(directory)

# -- -- Question 8.1 -- -- -- #

# -- 8.1.1
tweets = read.csv("tweets.csv", as.is = TRUE)
count(tweets,source)
TSTable = table(tweets$source)

# -- 8.1.2
random = read.csv("random.csv", as.is = TRUE)
count(random,source)
RSTable = table(random$source)

# -- 8.1.3 (Create table)
AllSourceNames = c(intersect(names(TSTable), names(RSTable)),setdiff(names(TSTable), names(RSTable)),setdiff(names(RSTable), names(TSTable)))
TweetSourceVect = c(5,347,20,458,154,1,1,1,1,1,2,1,1,1,1,5,replicate(36,0))
TweetRandVect = c(7,306,11,264,195,replicate(11,0),4,1,2,1,2,1,1,1,15,1,1,2,2,1,3,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,2,1,1,149,5,4)

sourceTable <- matrix(c(TweetSourceVect,TweetRandVect), nrow=2, byrow=TRUE)
colnames(sourceTable) <- AllSourceNames
rownames(sourceTable) <- c("Tweets","Random")
print(sourceTable)

# -- 8.1.4 (Chi Squared Test for Independence)
chisq <- chisq.test(sourceTable)
chisq

# -- 8.1.5 (Bootstrap Distribution for Iphone)
sourceTable[7:8]
colSums(sourceTable)
rowSums(sourceTable)



# -- 8.1.6

# -- -- Question 8.2

# -- 8.2.7
# -- 8.2.8

## Normalizing all the tweets

norm.tweet.matrix = diag(1/sqrt(rowSums(tweet.matrix^2))) %*% tweet.matrix

## then create a distance matrix
D = dist(norm.tweet.matrix, method = "euclidean")^2/2

## perform MDS using 100 dimensions
mds.tweet.matrix <- cmdscale(D, k=100)

##For loop to work out how many clusters
n = 15
SSW = rep(0, n)
for (a in 1:n) {
  K = kmeans(mds.tweet.matrix, a, nstart = 20)
  SSW[a] = K$tot.withinss
}

## plot the results to find the elbow
plot(1:15, SSW, type = "b")

##From this we know that ~5-6 clusters is most likely the best way to cluster the data.

# -- 8.2.9

##Perform Kmeans with the matrix and with 6 clusters
K = kmeans(mds.tweet.matrix, 2, nstart = 20)

# -- 8.2.10

##Plot the graph using the clusters as colour references
plot(mds.tweet.matrix, col=K$cluster)

# -- 8.2.11

##Print a table of the cluster numbers so we know which cluster is largest
table(K$cluster)

##From here we can see Cluster 2 is the largest between the two

# -- -- Question 8.3

# -- 8.3.12
# -- 8.3.13
# -- 8.3.14

# -- -- Question 8.4

# -- 8.4.15
# -- 8.4.16
# -- 8.4.17
# -- 8.4.18
# -- 8.4.19