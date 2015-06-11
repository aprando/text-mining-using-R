install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(SnowballC)
library(tm)

#doc1 <- "Stray cats are running all over the place. I see 10 a day!"
#doc2 <- "Cats are killers. They kill billions of animals a year."
#doc3 <- "The best food in Columbus, OH is the North Market."
#doc4 <- "Brand ABC is the best tasting cat food around, makes healthy and happy cats. Your cat will love it."
#doc5 <- "Buy Brand ABC cat food for your cat. Brand ABC makes healthy and happy cats."
#doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
#doc7 <- "I have nothing to say. In summary, I have told you nothing."

doc1 <- "MacBook Pro MD101BZ/A Intel Core i5 LED 13.3 4GB 500GB Apple"
doc2 <- "MacBook Air MD761BZ/B com Intel Core i5 13,3 4GB 256GB Flash Apple"
doc3 <- "Notebook Positivo com Intel Dual Core 2GB 500GB Tela LED 14 Windows 8.1"
doc4 <- "Notebook Ultrafino CCE U45B Intel Dual Core 4GB 500GB Tela LED 14 Windows 8.1 - Preto"
doc5 <- "Notebook Samsung ATIV Book 2 Intel Core i3 4GB 500GB LED 14 Windows 8.1 - Branco"
doc6 <- "Notebook Lenovo G40 Intel Core i5 4GB 1TB LED 14 Windows 8.1 - Prata"
doc7 <- "Chromebook Samsung Exynos 5 Dual Core 2GB 16GB LED 11,6'' Sistema Operacional Google Chrome"

#create list of documents
doc.list <- list(doc1, doc2, doc3, doc4, doc5, doc6, doc7)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))

# Create vector of documents
my.docs <- VectorSource(c(doc.list))
my.docs$Names <- c(names(doc.list))

# Create corpus
my.corpus <- Corpus(my.docs)

# Apply TM tecs
my.corpus <- tm_map(my.corpus, content_transformer(tolower))
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, removeWords, stopwords("portuguese"))
my.corpus <- tm_map(my.corpus, stemDocument)

# Create Term Document Matrix
my.tdm <- TermDocumentMatrix(my.corpus, control = list(wordLengths=c(5,100)))

# Calculate TFIDF
my.tfidf <- weightTfIdf(my.tdm)

# Create Document Term Matrix
my.dtm <- DocumentTermMatrix(my.corpus, control = list(minWordLength = 1))

# Find frequency terms with lowfreq eq 3
findFreqTerms(my.dtm, lowfreq=3)

# Find associations of term
findAssocs(my.dtm, 'cat', 0.05)

# calculate the frequency of words
m <- as.matrix(my.dtm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="miners")
myNames[k] <- "mining"
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)

