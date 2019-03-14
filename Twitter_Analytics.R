

#Setup the environment


library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
#install.packages("bitops")
library(devtools)
#install_github('sentiment140', 'okugami79', force = T)

library(sentiment)

library(qdap)
library(dplyr)
library(streamgraph)
# Set directory and read data
setwd("C:/Users/thulasi/Documents/Great lakes/Web n SM analytics")

tweets.df <- read.csv("BrexitTweets.csv",stringsAsFactors = F)
tweets.df = tweets.df[,c(2,6)]
# Convert char date to correct date format
tweets.df$created <- as.Date(tweets.df$created, format= "%d-%m-%y")


#Cleaning the text data by removing links, tags and delimiters.
# Remove character string between < >
tweets.df$text <- genX(tweets.df$text, " <", ">")

# Creating a corpus doc with tweet text
CorpusDoc<- Corpus(VectorSource(tweets.df$text)) 

# Removing inserted images/links
cleanurls <- function(x) gsub("http[^[:space:]]*", "", x)  
CorpusDoc <- tm_map(CorpusDoc, content_transformer(cleanurls))
#Converting to lowercases
CorpusDoc <- tm_map(CorpusDoc, content_transformer(stri_trans_tolower))
#Removing special characters
CleanSC <- function(x) gsub("[[:punct:]]", " ", x)   
CorpusDoc <- tm_map(CorpusDoc, content_transformer(CleanSC))
#Remove stop words
CleanStopWords<- c((stopwords('english')),c("k","rt","ma","use", "used", "via", "amp", "a","i","u","s","t"))
CorpusDoc<- tm_map(CorpusDoc,removeWords , CleanStopWords)
CorpusDoc<- tm_map(CorpusDoc, stripWhitespace) 
#keep a copy of "myCorpus" for stem completion later  
CorpusDoc1<- CorpusDoc
#Stem words in the corpus 

CorpusDoc<-tm_map(CorpusDoc, stemDocument)
writeLines(strwrap(CorpusDoc[[250]]$content,60))

stemCompletion2 <- function(x,dictionary) {
  x <- unlist(strsplit(as.character(x)," "))
  x <- x[x !=""]
  x <- stemCompletion(x, dictionary = dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}


#####Stem Complete and Display the same tweet above with the completed and corrected text. 

CorpusDoc <- lapply(CorpusDoc, stemCompletion2, dictionary=CorpusDoc1)
CorpusDoc <- Corpus(VectorSource(CorpusDoc))
writeLines(strwrap(CorpusDoc[[250]]$content, 60))



#####Correcting mis-splet words

wordFreq <- function(corpus,word)
{
  results<- lapply(corpus,
                   function(x){ grep(as.character(x),pattern = paste0("\\<", word))})
  sum(unlist(results))
}

?cat
n.leav<- wordFreq(CorpusDoc1, "leav")
n.leave <- wordFreq(CorpusDoc1, "leave")
cat(n.leav, n.leave)


#####Used to replace words with the proper ones

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, content_transformer(gsub), pattern=oldword, replacement=newword)
}
CorpusDoc<- replaceWord(CorpusDoc, "leav", "leave")

tdmatrix<- TermDocumentMatrix(CorpusDoc, control= list(wordLengths= c(1, Inf)))
tdmatrix
idx <- which(dimnames(tdmatrix)$Terms %in% c("brexit", "eu"))
as.matrix(tdmatrix[idx,21:60])


#####Find the terms used most frequently

(freq.terms <- findFreqTerms(tdmatrix, lowfreq = 50))
term.freq <- rowSums(as.matrix(tdmatrix))
term.freq <- subset(term.freq, term.freq > 50)
df <- data.frame(term = names(term.freq), freq= term.freq)
df


#####plotting the graph of frequent terms

ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 


#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud

word.freq <-sort(rowSums(as.matrix(tdmatrix)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 100)

# Identify and plot word correlations. 
WordCorr <- apply_as_df(CorpusDoc[1:500], word_cor, word = "negotiating", r=.25)
plot(WordCorr)
WordCorr1 <- apply_as_df(CorpusDoc[1:500], word_cor, word = "vote", r=.25)
plot(WordCorr1)

WordCorr2 <- apply_as_df(CorpusDoc[1:500], word_cor, word = "referendum", r=.25)
plot(WordCorr2)
WordCorr3 <- apply_as_df(CorpusDoc[1:500], word_cor, word = "stay", r=.25)
plot(WordCorr3)

qheat(vect2df(WordCorr[[1]], "word", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()
qheat(vect2df(WordCorr1[[1]], "word", "cor"), values=TRUE, high="green",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()
qheat(vect2df(WordCorr2[[1]], "word", "cor"), values=TRUE, high="blue",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()


# Messages with word 
df <- data.frame(text=sapply(CorpusDoc, `[[`, "content"), stringsAsFactors=FALSE)
head(unique(df[grep("negotiating", df$text), ]), n=10)



##### Find association with a specific keyword in the tweets 

findAssocs(tdmatrix, "vote", 0.2)
findAssocs(tdmatrix, "wrong", 0.2)


##### Topic Modelling to identify latent/hidden topics using LDA technique

dtm <- as.DocumentTermMatrix(tdmatrix)

rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}
tweets.df
lda <- LDA(dtm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics<- topics(lda)
topics<- data.frame(date=(tweets.df$created), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")


#####Sentiment Analysis to identify positive/negative tweets

 


# pol = 
#   lapply(tweets.df$text, function(txt) {
#     # strip sentence enders so each tweet is analyzed as a sentence,
#     # and +'s which muck up regex
#     gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
#       # strip URLs
#       gsub(' http[^[:blank:]]+', '', .) %>%
#       # calculate polarity
#       polarity()
#   })
# tweets.df$emotionalValence = sapply(pol, function(x) x$all$polarity)
# 
# ggplot(tweets.df, aes(x = emotionalValence, y = tweets.df$retweetCount)) +
#   geom_point(position = 'jitter') +
#   geom_smooth()
# library(rJava)
# # Use qdap polarity function to detect sentiment
# sentiments <- polarity(tweets.df$text)
# 
# sentiments <- data.frame(sentiments$all$polarity)
# 
# sentiments[["polarity"]] <- cut(sentiments[[ "sentiments.all.polarity"]], c(-5,0.0,5), labels = c("negative","positive"))
# 
# table(sentiments$polarity)

#install.packages("tidytext")
library(tidytext)
corpus.sent <- tidy(tdmatrix)

senti <- corpus.sent %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

str(senti)
library(dplyr)

senti %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 10) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")



  


##### Stream Graph for sentiment by date


Data<-data.frame(senti)
colnames(Data)[1] <- "polarity"
Data$Date <- tweets.df$created
Data$text <- NULL
Data$Count <- 1

graphdata <- aggregate(Count ~ polarity + as.character.Date(Date),data=Data,FUN=length)
colnames(graphdata)[2] <- "Date"
str(graphdata)

##### StreamGraph


graphdata %>%
  streamgraph(polarity, Count, Date) %>%
  sg_axis_x(20) %>%
  sg_axis_x(1, "Date","%d-%b") %>%
  sg_legend(show=TRUE, label="Polarity: ")
