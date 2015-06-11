# init libs
libs <- c("plyr", "class", "rmongodb", "SnowballC", "NLP", "tm", "e1071")# "RTextTools",
lapply(libs, require, character.only = TRUE)

# Set options 
options(stringsAsFactors = FALSE)

# Init general vars
host <- "192.168.33.10:27017"
username <- ""
password <- ""
db <- "recsysdb"

# clean text
cleanCorpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords(kind ="en"))
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

# Create connectiondoc
createConnection <- function() {
  return(mongo.create(host=host, db=db, username=username, password=password))
}

# Find products by categories
findProductsByCategories <- function(categories){
  mongo <- createConnection()
  nmprod <- paste(db, "produto", sep=".")
  
  # TODO: increase internal limit of query.
  # Error: Overflow sort stage buffered data usage of 33556480 bytes exceeds internal limit of 33554432 bytes
  query <- paste("{\"categorias.nome\": { \"$in\": [", paste(shQuote(categories, type="cmd"), collapse=", "), "] }}")
  
  fields <- '{"_ids":1,"descricaoLonga":1, "categorias":1, "nome":1}'
  
  ids <- c()
  descriptions <- c()
  categories <- c()
  
  index <- 1
  cursorProdutos <- mongo.find(mongo, nmprod, query = query, fields = fields)
  while(mongo.cursor.next(cursorProdutos)) {
    bsonprod <- mongo.cursor.value(cursorProdutos)
    listprod <- mongo.bson.to.list(bsonprod)
    
    if(!is.null(listprod$descricaoLonga)){
      descriptions[index] = listprod$descricaoLonga
      if(!is.null(listprod$nome)) descriptions[index] <- paste(descriptions[index], listprod$nome)
      
      ids[index] = NA
      if(!is.null(listprod$'_id')) ids[index] <- listprod$'_id'
      
      categories[index] = NA
      if(!is.null(listprod$categorias)) categories[index] <- listprod$categorias[[3]]$nome
      
      listprod$descricaoLonga <- gsub("[.]", " ", listprod$descricaoLonga)
      listprod$descricaoLonga <- gsub("\\w+\\d\\w+", " ", listprod$descricaoLonga)
      
      index <- index + 1  	
    }
  }
  
  return(data.frame(ids, descriptions, categories, stringsAsFactors=FALSE))#, row.names=c("ids", "descriptions", "categories")
}

# build corpus
buildCorpus <- function(products) {
  #create list of documents
  doc.list <- products$descriptions
  names(doc.list) <- products$ids
  
  # Create vector of documents
  doc.vector <- VectorSource(c(doc.list))
  doc.vector$Names <- c(names(doc.list))
  
  # Create corpus
  doc.corpus <- Corpus(doc.vector)
  doc.corpus <- cleanCorpus(doc.corpus)
  
  return(doc.corpus)
}

# build term document matrix
buildTdm <- function(corpus) {
  doc.tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(4,20)))
  doc.tdm <- removeSparseTerms(doc.tdm, 0.9995)
  return(doc.tdm)
}

buildDtm <- function(corpus) {
  doc.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths=c(4,20)))
  doc.dtm <- removeSparseTerms(doc.dtm, 0.9995)
  return(doc.dtm)
}

calcTfIdf <- function(tdm){
  return(weightTfIdf(tdm))
}

categs <- c('Computers & Tablets')#'Video Games', 'TV & Home Theater', 'Musical Instruments')#
products <- findProductsByCategories(categs)

# Option 1: Creating tdm and tfidf using tm lib and training SVM using e1071
products.corpus <- buildCorpus(products)

products.tdm <- buildTdm(products.corpus)
products.tdm.matrix <- as.matrix(products.tdm)
products.dtm <- buildDtm(products.corpus)
products.dtm.matrix <- as.matrix(products.dtm)

products.tfidf <- calcTfIdf(products.dtm)

#SVM
SVM.Data <- as.data.frame(inspect(products.tfidf))
SVM.Class <- factor(products$categories)

## Learning from train data
SVM.model <- svm(SVM.Class ~ ., data = SVM.Data)

# Take a look on trained model
print(SVM.model)
summary(SVM.model)
plot(SVM.model, SVM.Data)

#Take a look on corpus
N <- 10
findFreqTerms(products.dtm, N)
head(sort(rowSums(products.tdm.matrix), decreasing=TRUE), N)
head(sort(rowSums(products.tdm.matrix)), 50)

inspect(products.corpus)[1:N]
products[products$ids == '3304243',]

mydata.df <- as.data.frame(inspect(products.dtm))
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward")
plot(fit) 

groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red")

## Index for sampling
index <- 1:nrow(SVM.Data)

## Simple random sampling
testindex <- sample(index, trunc(length(index)/4))

## Generate train and test sets
testset   <- SVM.Data[testindex,]
trainset  <- SVM.Data[-testindex,]



## Prediction using test data
SVM.pred  <- predict(SVM.model, testset.x,
                     decision.values = TRUE,
                     na.action = na.omit)

## Compute SVM confusion matrix
table(pred = SVM.pred, true = testset.y)

summary(SVM.model)
plot(SVM.tune)

# Option 2: Creating matrix and container using RTextTools
products.matrix <- create_matrix(products$descriptions, language = "english", removeNumbers = TRUE,  removePunctuation=TRUE, removeStopwords = TRUE, stemWords = TRUE, removeSparseTerms = .998, minWordLength = 3, weighting=weightTfIdf)
productsSize = length(products$ids);
products.container <- create_container(products.matrix, products$categories, trainSize=1:productsSize, virgin=FALSE)
products.model <- train_model(products.container, "SVM", kernel="linear", cost=1)
predictionData <- list("iphone rocks! I just love it!", "Tablet is essential in my life", "Macbook and Ipad.... Apple products are awayls useful!")
predMatrix <- create_matrix(predictionData, originalMatrix=products.matrix, language = "english", removeNumbers = TRUE,  removePunctuation=TRUE, removeStopwords = TRUE, stemWords = TRUE, removeSparseTerms = .998, minWordLength = 3, weighting=weightTfIdf)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
results