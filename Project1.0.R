# -- -- -- SWA Group XXXX Major Project -- -- -- #

# -- Libraries
library(plyr)
library(dplyr)
library(readxl)
library(mosaic) #
library(ggplot2)

# -- Directories

#Omer
#directory<-"C:"

#Amir
#directory<-"C:"

#
#directory<-"D:/R studio/Projects"

#Byron
directory<-"/Users/CollectiveX/Desktop/Repos/SWA-21-GA"
setwd(directory)

# -- -- Question 8.1 -- -- -- #

# -- 8.1.1
tweets = read.csv("tweets.csv", as.is = TRUE)
count(tweets,source)
TSTable = table(tweets$source)
TSDF = t(as.data.frame(TSTable))
rownames(TSDF) = c("Source Name","Frequency")
TSDF

# -- 8.1.2
random = read.csv("random.csv", as.is = TRUE)
count(random,source)
RSTable = table(random$source)
RSDF = t(as.data.frame(RSTable))
rownames(RSDF) = c("Source Name","Frequency")
RSDF

# -- 8.1.3 (Create table)
# Table with all unique sources
sourceTable <- matrix(c(1:104), nrow=2, byrow=TRUE)

colnames(sourceTable) <- union(names(TSTable), names(RSTable))
rownames(sourceTable) <- c("Tweets","Random")
print(sourceTable)

# Table with intersecting sources
sourceTable <- matrix(c(5,347,20,458,154,  7,306,11,264,195), nrow=2, byrow=TRUE)
colnames(sourceTable) <- intersect(names(TSTable), names(RSTable))
rownames(sourceTable) <- c("Tweets","Random")
print(sourceTable)

# -- 8.1.4
# -- 8.1.5
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

#Testing Commit
x = 2

#Byrons Branch
