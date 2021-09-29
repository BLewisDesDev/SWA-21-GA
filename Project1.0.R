# -- -- -- SWA Group XXXX Major Project -- -- -- #

# -- Libraries
library(plyr)
library(dplyr)
library(readxl)
library(mosaic) #not working for me?
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

# Table with intersecting sources (Not sure if we should include all the random sources or we should clean)

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