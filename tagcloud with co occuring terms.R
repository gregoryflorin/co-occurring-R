
# Installer
#install.packages("tm")  # pour le text mining
#install.packages("SnowballC") # pour le text stemming
#install.packages("wordcloud") # générateur de word-cloud 
#install.packages("RColorBrewer") # Palettes de couleurs
#install.packages("dplyr") # Palettes de couleurs

# Loading packages

library("wordcloud")
library("RColorBrewer")
library("tm")
library("dplyr")

#Loading the file (semrush keyrod file)
keywords <- read.csv(file="C:/Users/greg/Downloads/matelas-phrase_fullsearch-fr.csv", header=TRUE, sep=";",encoding = "UTF-8")

#Filtering to remove noise

keywords <- filter(keywords, keywords$Search.Volume >300)


# Creating a corpus
dfCorpus = Corpus(VectorSource(keywords$Keyword)) 
dtm <- TermDocumentMatrix(dfCorpus)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)

words <- data.frame(word = names(v))

# Calculating score
for ( i in 1:nrow(words))
{
  keywordWithList <- filter(keywords, grepl(paste("^",words$word[i]," | ",words$word[i]," | ",words$word[i],"$",sep=""),keywords$Keyword))
  score <- as.integer(summarise(keywordWithList,searchVolume = sum(keywordWithList$Search.Volume))[1])
  
  words$score[i] <- score
}

# Je génère mon tagcloud
set.seed(1234)
wordcloud(words = words$word, freq = words$score, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))