# -- -- -- SWA Group X 2021 Major Project -- -- -- #

# -- Install Libraries
install.packages("readxl")
install.packages("ggplot2")
install.packages("rtweet")
install.packages("igraph")
install.packages("SnowballC")

# -- Attach Libraries
library(readxl)
library(ggplot2)
library(rtweet)
library(igraph)
library(SnowballC)


# -- Directory (Add your working directory here)

#Byron
setwd("/Users/CollectiveX/Desktop/Repos/SWA-21-GA")

# -- -- Question 8.1 -- -- -- #

# -- 8.1.1

# -- OMERS Deets
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
#rowSums(AbSourceTable) #check if all tweets accounted for

# -- 8.1.4 (Chi Squared Test for Independence)
chisq.test(sourceTable, simulate.p.value = TRUE) #(Lots of counts of 1, simulate pvalues used to make test more accurate)
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




##### REDO #####


tweetSamp = 458/1000
tweetSamp
randSamp = 264/1000
randSamp

# -- 8.1.5 (Bootstrap Distribution for iPhone)
obs = 264
sampleSize = 1000
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


##### REDO #####


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

# -- -- Question 8.3

# -- 8.3.12
## In order to make words visible into the plots, WordCloud needs Term freqs in corpus
tweet.wtdm = weightTfIdf(tweet.tdm) ## tweet.tdm is the Term Freq by using weightTfIdf
T = as.matrix(tweet.wtdm) ## Makes the matrix from previous script
words = rowSums(T) ## rows are terms in TDM
## Execute the command and output the common words that LilNasX comments had
wordcloud(names(words),words, random.order = FALSE, min.words = 3)

# -- 8.3.13
dim(tweet.matrix)

## Taking the terms of documents existed more than 4 documents
frequent.words = which(apply(tweet.matrix > 0) > 4)
length(frequent.words) ## numbers of words have multiple appearance

term.matrix = tweet.matrix[,frequent.words]

dim(term.matrix) ## shows the output of term.matrix

## Cosine Distance Calculation
norm.term.matrix = term.matrix %*% diag(1/sqrt(colSums(term.matrix^2)))
colnames(norm.term.matrix) = colnames(term.matrix)
tdm = t(norm.term.matrix)
D= dist(tdm)^2/2
h=hclust(D, method = "complete")
plot(h) ## Output the dendrogram

# -- 8.3.14
## we know that the common thing about LilNasX comments are his supportive action through LGBTQ addressing he is GAY 
## and through his vulnerability to other guys, he has lovesickness through guys and the data describes itself,
## opening his expressions to his followers and his friends.

## naughty things also mentioned in the data

# -- -- Question 8.4

#app = "SWA2021B"
#key = "ODXHzs5gKRsmSjzIYgPtf7CcA"
#secret = "MMMX25niSKlRlP4hN1xPghzA2HmfdfE355DRgsONOlxicLjPlu"
#access_token = "1292601739447697408-nLsCLI09bfHWVeJJzZKhKun5DFn6a6"
#access_secret = "LpnI1Pac3a1DPSbQEVeeP7w4Kh10IsY8dkiy4OHTF647u"

#authenticate 
#twitter_token = create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)

# -- 8.4.15 
LilNas = lookup_users(c("LilNasX"), token = twitter_token)
names(LilNas)
LilNas$friends_count
LilNas$screen_name
LilNas$followers_count

FriendList = get_friends(LilNas$screen_name, token = twitter_token)
LilNasFriends = lookup_users(FriendList$user_id, token = twitter_token)

friendRank = order(LilNasFriends$followers_count, decreasing = TRUE)[1:20] 
topFriends = LilNasFriends[friendRank,] #details of top 10 friends
topFriends

# -- 8.4.16
TweetsPosted = topFriends$statuses_count

# -- 8.4.17
n = nrow(topFriends)

#create edge list for graph
el.Nas = cbind(rep(LilNas$screen_name, n), topFriends$screen_name)

#loops through friends list, checks following status and adds to a vector, and appends to edge list
for (i in 1:n) {
  for (j in i:n) {
    fr = lookup_friendships(topFriends$screen_name[j], topFriends$screen_name[i], parse = TRUE, token = twitter_token)
    fr
    
    if(fr$value[4] == TRUE) {
    el.Nas = rbind(el.Nas,c(fr$user[1], fr$value[3]))
    }
  
    if(fr$value[5] == TRUE) {
    el.Nas = rbind(el.Nas,c(fr$value[3], fr$user[1]))
    }
  }
}

# -- Build a Graph -- #
nodes = c(LilNas$screen_name,topFriends$screen_name)
sizes = c(LilNas$statuses_count, TweetsPosted)

node.size = setNames(sizes,nodes)

edges = graph.edgelist(el.Nas)
plot(edges, vertex.size = node.size/10000, edge.arrow.size=0.1)

#The graph has wildly different vertex sizes
#Smallest and largest values 15x different
max(sizes)
min(sizes)

# -- 8.4.18

betweenness(edges)
nodes[order(betweenness(edges), decreasing = TRUE)]

#Top 3 Central nodes
nodes[4]
nodes[7]
nodes[2]

# -- 8.4.19 Comments...
