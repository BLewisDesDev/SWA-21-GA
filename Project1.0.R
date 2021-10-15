# -- -- -- SWA Group X 2021 Major Project -- -- -- #

# -- Libraries
library(readxl)
library(ggplot2)
library(rtweet)
library("tm")
library(SnowballC)
library(rtweet)


# -- Directory (Add your working directory here)

#Ryan
directory <- "K:/UNI/WSU/Semester 2/SWA/Group Project/SWA-21-GA"
setwd(directory)

#Byron
setwd("/Users/CollectiveX/Desktop/Repos/SWA-21-GA")

# -- -- Question 8.1 -- -- -- #

# -- 8.1.1
app="SWA5430"                                
key= "fGP35vn7MIpD1Mwig2rQuKEcl"
secret= "66fedVhW2urfx95iJ5tVU6DmmOsxWDXz7p8uLIrjpOBVGxD4ma"       
access_token="1131346472-RPfQOGfRBjx3KauX5GMYqY21Qw46x0sPVXFAXgB" 
access_secret="T4iOcRZ3Cv3bz3WhX8Aulhr91HQu8GMLcamLBmGMJG6mh"     

#authenticate 
twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)

#search for some tweets about LilNasX = tweets
tweets =search_tweets('LilNasX', n = 1000, token = twitter_token, include_rts = FALSE, lang='en')
write_as_csv(tweets, 'tweets.csv')

#tweets containing 'the' = random
random =search_tweets('the', n = 1000, token = twitter_token, include_rts = FALSE, lang='en')
write_as_csv(random, 'random.csv')

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

tweet.text = (tweets$text[1:1000])

tweet.corpus = Corpus(VectorSource(tweet.text))
tweet.corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII'))

#ignore url
tweet.corpus = tm_map(tweet.corpus,content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+","",x)))
#remove username
tweet.corpus = tm_map(tweet.corpus,content_transformer(function(x) gsub("@\\w+","",x)))
tweet.corpus = tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, removePunctuation)
stopwordlist  = c(stopwords(), "thank", "also", "the", "this", "will", "there", "see", "put")
tweet.corpus = tm_map(tweet.corpus, removeWords, stopwordlist)
tweet.corpus = tm_map(tweet.corpus, stemDocument)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus,tolower)

tweet.tdm = TermDocumentMatrix(tweet.corpus)
tweet.wtdm = weightTfIdf(tweet.tdm)
tweet.matrix = t(as.matrix(tweet.wtdm)) #tweet.matrix is now in DTM form

#observe the dim of tweet.matrix 
dim(tweet.matrix)
tweet.matrix[1,1:10] #observe a column in tweet.matrix represents a term in tweets

#so each row represents a tweet
## remove empty tweets.
empties = which(rowSums(abs(tweet.matrix)) == 0)#In a DTM, 0 rowsum means that doc is empty
length(empties)

if(length(empties)!=0){
  tweet.matrix = tweet.matrix[-empties,] 
}

dim(tweet.matrix)
View(tweet.matrix)

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
resultsTable = table(K$cluster)
rownames(resultsTable) = c("Cluster 1", "Cluster 2")
View(resultsTable)

##From here we can see Cluster 1 is the largest between the two

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

