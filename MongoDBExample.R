install.packages("rmongodb");
install.packages("RWeka");
install.packages("rjson");
install.packages("ade4");
install.packages("iterators");
install.packages("foreach");
install.packages("cluster");

library(rmongodb)
library(RWeka)
library(rjson)
library(stats)
library(ade4)
library(iterators)
library(foreach)
library(cluster)

host <- "192.168.33.10:27017"
username <- ""
password <- ""
db <- "recsysdb"

print("               ### CONECTANDO no MONGODB ###")
cat(" # Host: ", host, "\n")
cat(" # User: ", username, "\n")
cat(" # Password: ", password, "\n")
cat(" # Database: ", db, "\n")

mongo <- mongo.create(host=host, db=db, username=username, password=password)

nmprod <- paste(db, "produto", sep=".")
nmrec <- paste(db, "recomendacao", sep=".")

print("\n               ### RODANDO algumas QUERYS ###")

#Example of count on all dataset
cat("# Count total da base: ", mongo.count(mongo, nmprod, mongo.bson.empty()), "\n")

#Example of count on all dataset
query <- '{"categorias.nome" : "Xbox One Games"}'
fields <- '{"_ids":1,"detalhes":1}'
cat("# Count de uma categoria: ", mongo.count(mongo, nmprod, query), "\n")

ids <- c()
genres <- c()
developers <- c()
onlines <- c()
esrb_categories <- c()

index <- 1
cursorProdutos <- mongo.find(mongo, nmprod, query, fields)
while(mongo.cursor.next(cursorProdutos)) {
	ids[index] = NA
	genres[index] = NA
	developers[index] = NA
	onlines[index] = NA
	esrb_categories[index] = NA
	
	bsonprod <- mongo.cursor.value(cursorProdutos)
    listprod <- mongo.bson.to.list(bsonprod)
	if(!is.null(listprod$`_id`)) ids[index] <- listprod$`_id`
	if(!is.null(listprod$detalhes$Genre)) genres[index] <- listprod$detalhes$Genre
	if(!is.null(listprod$detalhes$Developer)) developers[index] <- listprod$detalhes$Developer
	if(!is.null(listprod$detalhes$`Online Features`)) onlines[index] <- listprod$detalhes$`Online Features`
	if(!is.null(listprod$detalhes$`ESRB Category`)) esrb_categories[index] <- listprod$detalhes$`ESRB Category`
	
	index <- index + 1		
}

print("\n               #### VECTORS info ####")
cat("## ids size: ", length(ids), "\n") 
cat("## Genres size: ", length(genres), "\n") 
cat("## developers size: ", length(developers), "\n") 
cat("## onlines size: ", length(onlines), "\n") 
cat("## esrb_categories size: ", length(esrb_categories), "\n") 

prod_data <- data.frame(ids, genres, developers, onlines, esrb_categories)

print("\n               #### DATA FRAME info ####")
prod_data[,]
print("\n## Table online ##")
table(prod_data$onlines)


print("\n               #### KMEANS info ####")
kmdf <- data.frame(genres, esrb_categories, developers, onlines)
kmdf$genres <- ifelse(is.na(kmdf$genres), '0', kmdf$genres)
kmdf$esrb_categories <- ifelse(is.na(kmdf$esrb_categories), '0', kmdf$esrb_categories)
kmdf$developers <- ifelse(is.na(kmdf$developers), '0', kmdf$developers)
kmdf$onlines <- ifelse(is.na(kmdf$onlines), '0', kmdf$onlines)

clusters <- kmeans(kmdf, 5)
prod_data$cluster <- clusters$cluster
res <- prod_data[c("cluster", "ids")]

print(clusters)

print("\n               #### PLOTING ####")
png(filename="/Users/alanprando/Dropbox/Mestrado/EXPERIMENTO/Projeto/Plots/kmeans_clusplot.png")
clusplot(kmdf, clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
dev.off()

png(filename="/Users/alanprando/Dropbox/Mestrado/EXPERIMENTO/Projeto/Plots/kmeans_plot.png")
plot(kmdf, col = clusters$cluster)
dev.off()

print("\n               #### SAVING clusters on MONGODB ####")
it <- iter(res, by = "row")
x <- nextElem(it)
while(!is.null(x)){
	filter <- paste(c('{"_id":"',toString(x$ids), '"}'), collapse='')
	update <- paste(c('{"_id":"',toString(x$ids), '", "kmeansClusterGroup":"',toString(x$cluster), '"}'), collapse='')
	mongo.update(mongo, nmrec, filter, update,  flags=mongo.update.upsert)
	x <- nextElem(it)
}