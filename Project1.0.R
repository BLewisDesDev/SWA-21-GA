# -- -- -- SWA Group X 2021 Major Project -- -- -- #

# -- Libraries
library(plyr)
library(dplyr)
library(readxl)
library(mosaic)
library(ggplot2)

# -- Directory (Add your working directory here)

#Name
#directory <- ""
#setwd(directory)

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
# -- 8.2.9
# -- 8.2.10
# -- 8.2.11

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
# testing omer

# Test the editor - Gidi