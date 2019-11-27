setwd("C:/Users/Dehong Tan/Desktop/NUS/EBA5002 Business Analytics Practice/Text Analytics/Day 2/mini project")

library(topicmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)  

my_stopwords <- c(stopwords("english"), "will", "also", "etc", "else", "can", "even", "within", "without", 
                  "well", "say", "year", "must", "need", "never", "now", "want", "still", 
                  "time", "therefore", "send", "today", "may", "many", "make", "whose",
                  "however", "get", "have", "just", "him", "hospital", "employe", "worker")

textdata <- read.delim("osha.txt", header=FALSE, sep="\t", quote = "", stringsAsFactors = FALSE)
View(textdata)
text <- textdata[, 2]
text

doclen <- sapply(text, function(x) length(strsplit(x, " ")[[1]]))
str(doclen)
table(doclen)
hist(doclen)

corpus <- VCorpus(VectorSource(text))

dtm <- DocumentTermMatrix(corpus, control = list (removeNumbers = TRUE,
                                                  tolower = TRUE,
                                                  stemming = TRUE,
                                                  stopwords = my_stopwords,
                                                  removePunctuation = TRUE))

dtm

tf <-sort(colSums(as.matrix(dtm)), decreasing=TRUE)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(tf), tf, max.words=60, scale=c(8, .04), colors=dark2)

#===========topic modeling=========

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] # remove docs with zero key words, no loss of information

lda_5_g <- LDA(dtm.new, 5, method="Gibbs")

raw.sum=apply(dtm,1,FUN=sum) #sum by raw each raw of the table
table=dtm[raw.sum!=0,]

terms(lda_5_g, 10)
logLik(lda_5_g)

lda_5_g@terms[1:10]
lda_5_g@beta[3, 1:10]

showcloud = function (m, i) {
  tt <- m@beta
  colnames(tt) <- m@terms
  top <- sort(tt[i, ], decreasing = TRUE)
  wordcloud(names(top[1:20]), 2^top[1:20],scale=c(8, .04),rot.per=0.3, colors=dark2)
}

showcloud(lda_5_g, 4)

t(topics(lda_5_g, 3))[1:10,]

which.max(tabulate(topics(lda_5_g)))
tabulate(topics(lda_5_g))

lda_5_g@gamma[1,]
barplot(lda_5_g@gamma[1,], names.arg=1:5, main="Topic distribution of Story 1")

#===========QN 3 and 4 FINDING OUT MORE RISKY OCCUPATIONS/MOST INJURED BODY PARTS=========

initCoreNLP(type="english_fast") # remove NER from coreNLP properties file as it is not required in this case

getNouns = function (x) {
  tok <- getToken(annotateString(x))
  lem <- unlist(tok[startsWith(tok[, "POS"], "N"), "lemma"])
}

getVerbs = function (x) {
  tok <- getToken(annotateString(x))
  lem <- unlist(tok[startsWith(tok[, "POS"], "V"), "lemma"])
}

lemmaN <- sapply(text, getNouns)
View(lemmaN)

my_stopwords <- c(stopwords("english"), "will", "also", "etc", "else", "can", "even", "within", "without", 
                  "well", "say", "year", "must", "need", "never", "now", "want", "still", 
                  "time", "therefore", "send", "today", "may", "many", "make", "whose",
                  "however", "get", "have", "just", "him", "hospital", "employee", "killed", "fall", "injured", "worker")


vector1 <- VectorSource(lemmaN)
corpus1 <- VCorpus(vector1)
dtm <- DocumentTermMatrix(corpus1, control = list (removeNumbers = TRUE,
                                                   tolower = TRUE,
                                                   stemming = FALSE,
                                                   stopwords = my_stopwords,
                                                   removePunctuation = TRUE))

freq <- colSums(as.matrix(dtm))
freq
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=60, rot.per=0.5, colors=dark2)

occupation = c(hyponyms("employee"), hyponyms("worker"))
occupation

bodyparts = c(hyponyms("external body part"), hyponyms("body part"))
bodyparts

findAssocs(dtm, bodyparts, 0.7) ## 0.7 is adjustable required minimum coefficient
findAssocs(dtm, occupation, 0.7) 












