# -- -- -- SWA Group XXXX Major Project -- -- -- #

# -- Import Data and Libraries

library(plyr)
library(dplyr)
library(readxl)

# -- -- Question 8.1 -- -- -- #

# -- 8.1.1
tweets = read.csv("tweets.csv", as.is = TRUE)
count(tweets,source)

TweetSource <- table(tweets$source)
TweetSource

# -- 8.1.2
random = read.csv("random.csv", as.is = TRUE)
count(random,source)

RandSource <- table(random$source)
RandSource

# -- 8.1.3
#join(TweetSource, RandSource, by=NULL, type="full", match="all")

sourceTable <- matrix(c(1,2,3,4,5,6,7,8), nrow=2, byrow=TRUE)
colnames(sourceTable) <- c("iPhone", "Android","WebApp", "TweetDeck")
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
