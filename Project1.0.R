# -- -- -- SWA Group X 2021 Major Project -- -- -- #

# -- Libraries
library(readxl)
library(ggplot2)

# -- Directory (Add your working directory here)

#Name
#directory <- ""
#setwd(directory)

#Byron
setwd("/Users/CollectiveX/Desktop/Repos/SWA-21-GA")

# -- -- Question 8.1 -- -- -- #

# -- 8.1.1
tweets = read.csv("tweets.csv", as.is = TRUE)
TSTable = table(tweets$source)

# -- 8.1.2
random = read.csv("random.csv", as.is = TRUE)
RSTable = table(random$source)

# -- 8.1.3 (Create table)
AllSourceNames = c(intersect(names(TSTable), names(RSTable)),setdiff(names(TSTable), names(RSTable)),setdiff(names(RSTable), names(TSTable)))
TweetSourceVect = c(5,347,20,458,154,1,1,1,1,1,2,1,1,1,1,5,replicate(36,0))
TweetRandVect = c(7,306,11,264,195,replicate(11,0),4,1,2,1,2,1,1,1,15,1,1,2,2,1,3,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,2,1,1,149,5,4)

sourceTable = matrix(c(TweetSourceVect,TweetRandVect), nrow=2, byrow=TRUE)
colnames(sourceTable) = AllSourceNames
rownames(sourceTable) = c("Tweets","Random")

print(sourceTable)
#rowSums(sourceTable)

intersect(names(TSTable), names(RSTable))

# I think this gives junk data cause there are so many sources that dont overlap.
# I think we should take only the sources that overlap
# Create another category called "other" and then compute. (Not what is asked but i think a better way)
#EG:

AbreviatedSources = c(intersect(names(TSTable), names(RSTable)), "other")
AbTweetSourceVect = c(5,347,20,458,154,16)
AbTweetRandVect = c(7,306,11,264,195,217)

AbSourceTable = matrix(c(AbTweetSourceVect,AbTweetRandVect), nrow=2, byrow=TRUE)
colnames(AbSourceTable) = AbreviatedSources
rownames(AbSourceTable) = c("Tweets","Random")

print(AbSourceTable)
#rowSums(AbSourceTable) 

# -- 8.1.4 (Chi Squared Test for Independence)
chisq.test(sourceTable, simulate.p.value = TRUE) #(Lots of counts of 1, simulate pvalues used to make test better)
chisq.test(AbSourceTable)

# -- 8.1.5 (Bootstrap Distribution for iPhone)
obs = 722
sampleSize = 2000
pHat = obs/sampleSize
complement = sampleSize-obs

b = 10000
boot.dist.iPhone = rep(0,b)
propVec = c(replicate(obs,TRUE),replicate(complement,FALSE))

for (i in 1:b) {
  bootsample = table(sample(propVec,replace=TRUE))
  boot.dist.iPhone[i] = bootsample[2]/sampleSize
}

hist(boot.dist.iPhone)

# -- 8.1.6 (95% Confidence Interval)
quantile(boot.dist.iPhone, c(0.005, 0.995))

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

#Working on this now...

# -- 8.4.15
# -- 8.4.16
# -- 8.4.17
# -- 8.4.18
# -- 8.4.19
