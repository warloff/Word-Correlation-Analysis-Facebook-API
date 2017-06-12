# This file contains an analysis of word correlations in the comments section of a 
 # Donald Trump fan page. Due to the contraversial nature of the 2016 election, I chose to analyze comments on articles from
 # a Donald Trump for President Page. This code can be used as a template to pull comments from any facebook page and analyze
 # word correlations by changing the page and the term of interest. 

install.packages("Rfacebook")
install.packages("devtools")

library(Rfacebook)
library(devtools)
library(ggplot2)
library(scales)
library(grid)
library(igraph)
library(wordcloud)
library(tm)
library(qdap)
library(SnowballC)

require("Rfacebook")
# install_github("pablobarbera/Rfacebook/Rfacebook")

# Get into the Facebook API
fb_oauth <- fbOAuth(app_id="Enter App ID", app_secret="Enter App Secret",extended_permissions = FALSE)

token <- "Enter Token"
save(fb_oauth, file="fb_oauth")

load("fb_oauth")

#Pull page info
Trumpforpresidentpage <- getPage("133961323610549", fb_oauth, n = 100, since = NULL, until = NULL, feed = FALSE,
                     reactions = FALSE)
djid <- Trumpforpresidentpage$id

post <- as.data.frame("")
datalist = list()

# For loop pulls the comments out of the posts from the page 
j <- 1 
for (i in djid){
  post <- getPost(i, fb_oauth, n = 500, comments = TRUE, likes = TRUE,
                 n.likes = 1, n.comments = 200)
  datalist[[j]] <- post$comments
  j <- j+1 
}

big_data = do.call(rbind, datalist)

## build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(big_data$message))
big_data$message <- iconv(big_data$message, "latin1", "ASCII", sub="")
## convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
## remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
## remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
## remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# populate list of stopwords
myStopwords <- c(stopwords('english'), "available", "via","j","said","see",
                 "well","will","put","dont","really","can")
## remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
## keep a copy 
myCorpusCopy <- myCorpus


## stem words
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

idx <- which(dimnames(myTdm)$Terms == "liberals")
inspect(myTdm[idx+(0:5),101:110])


library(ggplot2)
## which words are associated with "liberal"?
libs <- findAssocs(myTdm, "liberals", 0.25)
libdf <- as.data.frame(libs)
libdf$word = rownames(libdf)

# Reset the `rownames` of your original data
rownames(libdf) = NULL
toi <- "liberals"

#https://stackoverflow.com/questions/19549280/plot-highly-correlated-words-against-a-specific-word-of-interest
ggplot(libdf, aes( y = word ) ) +
  geom_point(aes(x = liberals), data = libdf) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\"")) + ylab("Word") 

