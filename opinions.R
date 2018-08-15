library(h2o)
library(twitteR)
library(foreach)
library(syuzhet)
library(cluster)
library(dplyr)
library(devtools)
library(tm)
library(stringr)
library(plotly)
library(xlsx)
library(NbClust)
library(data.table)
library(plyr)
library(proxy)

h2o.init()

#Function to calculate manhattan distance between two vectors
ManhattanDist <- function(vec1, vec2){
      distance <- abs(vec1-vec2)
      distance <- sum(distance)
      return(distance)
}

#Function to scale the values such that they range from 0 to 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Given a topic containing a list of tweets, this function calculates the narratives within that topic
Part1 <- function(keyword, number,filename,file_count){

	df <- read.csv(paste0(file_count,".csv"))
	df <- df[1:number,]
	df <- df$text
	print("starting here")

	df <- as.data.table(df)
	names(df)<-c("Tweet")
	#deal with parsing errors in h2o
	df[,":="(Tweet=gsub("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘", "", Tweet)
	)]
	df[,":="(Tweet=gsub("  ", " ", Tweet))]
	words_remove = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
                    "there","all","we","one","the","a","an","of","or","in","for","by","on",
                    "but","is","in","a","not","with","as","was","if","they","are","this","and","it","have",
                    "from","at","my","be","by","not","that","to","from","com","org","like",
                    "likes","so","said","from","what","told","over","more","other",
                    "have","last","with","this","that","such","when",
                    "been","says","will","also","where","why","would",
                    "today", "in", "on", "you", "r", "d", "u", "hw", 
                    "wat", "oly", "s", "b", "ht", "rt", "p","the","th")

	print("1")
	#for preprocessing tweets
	clean.text = function(df,words_to_remove=words_remove){
  		myCorpus <- Corpus(VectorSource(df$Tweet)) 
  		remove_symbols <- function(x) gsub("[^[:graph:]]", " ",x)
  		myCorpus <- tm_map(myCorpus, content_transformer(remove_symbols))
  		#Function to Remove all punctuation except '#' & '@'
  		removeMostPunctuation<- function (x) {
    		x <- gsub("[[:punct:]]+", "", x)
    		x
  		}
  		# remove punctuation
  		myCorpus <- tm_map(myCorpus, removeMostPunctuation) 
  		# remove numbers
  		myCorpus <- tm_map(myCorpus, removeNumbers)
  		# remove URLs
  		removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  		myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  		#Remove '&amp' symbol in the text
  		remove <- function(x) gsub("&amp", "", x)
  		myCorpus <- tm_map(myCorpus, content_transformer(remove))
  		#Remove all RT's (if there is any)
  		removeHeader <- function(x) gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
  		myCorpus <- tm_map(myCorpus, content_transformer(removeHeader))
  		#removing symbols and stripping white spaces
  		clean_tweet <- function(x) gsub("^\\s+|\\s+$", "", x) 
  		myCorpus <- tm_map(myCorpus, content_transformer(clean_tweet))
  		myCorpus <- tm_map(myCorpus, stripWhitespace)
  		# convert to lower case
  		myCorpus <- tm_map(myCorpus, content_transformer(tolower)) 
  		# remove stopwords from corpus
  		myCorpus <- tm_map(myCorpus, removeWords, stopwords('en'))
  		myStopwords <- c(stopwords("english"), "rt", "via", keyword)
  		myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  		myCorpus <- tm_map(myCorpus, removeWords, words_remove)
  		#remove unecessary data files/functions
  		rm(myStopwords)
  		rm(removeMostPunctuation)
  		rm(removeURL)
  		# Saving the corpus elements into the original dataframe 
  		df$after <- get("content", myCorpus)
  		return(df)
	}

	#preprocessed tweets
	df <- clean.text(df)
	print("2")
	#for sentimnt analysis
	scoreSentiment = function(tab){
  		sentiment = get_sentiment( as.character(tab$after) , method="bing")
  		return(sentiment)
	}
	df$sentiment<-scoreSentiment(df)

	#change to h2o frame
	twdf<-as.h2o(x=df,destination_frame = 'dat')

	#change array of preprocessed tweets to sequence of words with a delimiter "NA" at the end of a tweet
	tokenize <- function(sentences, stop.words = words_remove) {
  		tokenized <- h2o.tokenize(sentences, "\\\\W+")
  		# convert to lower case
  		tokenized.lower <- h2o.tolower(tokenized)
  		# remove short words (less than 2 characters)
  		tokenized.lengths <- h2o.nchar(tokenized.lower)
  		tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  		# remove words that contain numbers
  		tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
  		# remove stop words
  		tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% words_remove),]
	}
	print("3")
	#sequence of preprocessed tweet words
	words <- tokenize(twdf$after)

	#Build word2vec model
	w2v.model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)

	#convert each tweet to a vector
	words.vec <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")

	#convert back to R frame from h2o frame
	words.vecR <-as.data.frame(words.vec)

	#add additional column of sentiment
	words.vecR$C101 <- df$sentiment

	#omit vectors with NA values to avoid error in kmeans algorithm
	bool_nonNA <-is.na(words.vecR)
	words.vecR2<-words.vecR[-which(bool_nonNA==TRUE),]
	df2 <- df[-which(bool_nonNA[,1]==TRUE),]
	
	#scale data for better fit
	words.vecR2 <- scale(words.vecR2)
	print("4")


	#no ofclusters that minimize witin sum of square distance
	nb <- NbClust(words.vecR2, diss=NULL,distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "silhouette")
	#nb <- NbClust(words.vecR2, diss=NULL,distance = "manhattan", min.nc=2, max.nc=15, method = "kmeans", index = "silhouette")
	print("5")
	k <- as.numeric(nb$Best.nc[1])  

	#build clustering model
	km2 <- kmeans(words.vecR2,centers=k)
	sil <- silhouette(km2$cluster,daisy(words.vecR2)^2)
	
	#Array of tweet and its corresponding cluster
	finaldf <- as.data.frame(cbind(km2$cluster,as.character(df2$Tweet),df2$sentiment))
	names(finaldf) <- c("cl_num","tweet","sentiment")
	finaldf$cl_num <- as.numeric(as.character(finaldf$cl_num))
	finaldf$sentiment <- as.numeric(as.character(finaldf$sentiment))
	finaldf<-finaldf[order(finaldf$cl_num),]

	#Ordering the tweets within a cluster based on their manhattan distance from centroid
	orderCluster <- function(km2, wv, df,k){
	  	cluster <- km2$cluster
	  	centers <- km2$centers
	  	dist <- vector()
	  	vecs <- vector()
	  	for(i in 1:k) {
	  	  	vecs <-  wv[cluster==i,]
	  	  	for(j in 1:nrow(vecs)){
	  	    	dist[length(dist) + 1] <- ManhattanDist(as.vector(vecs[j,]),centers[i,])
	  	    }
	  	}
	  	df$cl_num <- as.numeric(as.character(df$cl_num))
	  	df$dist <- as.numeric(dist)
	  	
	  	return(df)
	}


	print("6")
	dfOrdered <- orderCluster(km2,words.vecR2,finaldf,k)
	dfOrdered <- dfOrdered[order(dfOrdered$cl_num,dfOrdered$dist),]
	dfUnique <- count(dfOrdered, vars = c("cl_num","tweet","sentiment","dist"))
	dfUnique <- dfUnique[order(dfUnique$cl_num,dfUnique$dist),]
	
	#computing the silhouette widths of all clusters
	sum_sil <- summary(sil)
	ind_sil_score <- sum_sil$clus.avg.widths
	poor_cluster_indices <- which(ind_sil_score < 0.2)

	#Removing those clusters which have a silhouette width less than 0.2
	if(length(poor_cluster_indices) != 0){
	  	ind_sil_score <- ind_sil_score[-(poor_cluster_indices)]
	  	k_final <- k - length(poor_cluster_indices)
	  	dfUnique <- dfUnique[ ! dfUnique$cl_num %in% poor_cluster_indices, ]
	  	new_cluster_nos <- seq(1,k)
	  	new_cluster_nos <- new_cluster_nos[-(poor_cluster_indices)]
	  	for(i in 1:k_final) {
	  	  if (i != new_cluster_nos[i]) {
	  	    dfUnique$cl_num[dfUnique$cl_num == new_cluster_nos[i]] <- i
	  	  }
	  	}
	}
	else{  
	  k_final <- k
	}

	#Average silhouette width
	avg_sil_score <- mean(ind_sil_score)
	sil_scores <- ind_sil_score
	
	#Calculating (sil_width * (# unique_tweets/# toatl_tweets)) for each cluster
	unique_tweets <- vector()
	total_tweets <- vector()
	for(i in 1:k_final){
		unique_tweets[i] <- nrow(dfUnique[dfUnique$cl_num == i,])
		total_tweets[i] <- sum(dfUnique[dfUnique$cl_num == i,]$freq)	
		}
	names(sil_scores) <- 1:k_final
	cluster_ranking <- sil_scores * ( unique_tweets/total_tweets)
	cluster_ranking <- rev(sort(cluster_ranking))
	cluster_order <- as.numeric(names(cluster_ranking))

	#arranging the clusters in the descending order of above calculated measure
	for(i in 1:k_final){
		dfUnique[dfUnique$cl_num == cluster_order[i],]$cl_num = 0 - i
	
	}
	dfUnique$cl_num <- abs(dfUnique$cl_num)
	dfUnique <- dfUnique[order(dfUnique$cl_num,dfUnique$dist),]
	
	#Writing silhouette widths of each cluster in a file 
	fileConn <- file(paste0(file_count,".txt"))
	writeLines(as.character(c(avg_sil_score,sil_scores)),fileConn)
	close(fileConn)
	write.csv(dfUnique,filename)
	
	return(k_final)
}



#This function calculates abstraction and sentiment of each of the narratives obtained from the above function
final_clusters <- function(search_term,file_count,n){
  
  	cluster_count <- Part1(search_term,n,paste0(file_count,"_","Clusters",".csv"),file_count)
  	print("7")
  	file_name <- paste0(file_count,"_","Clusters",".csv")
  	
  	#cleaning#
  	#----------------#	
  	all_Tweets <- read.csv(file_name) 
  	#removing non-ascii characters and emoticons
  	all_Tweets$cleantweet <- gsub("[^\x01-\x7F]", "", all_Tweets$tweet)
  	all_Tweets$cleantweet <- gsub("<\\w+ *>", "", all_Tweets$cleantweet)	
  	#removing URLS
  	all_Tweets$cleantweet <- gsub("http\\S+\\s*", "", all_Tweets$cleantweet)
  	#removing amp symbol
  	all_Tweets$cleantweet <- gsub("&amp", "", all_Tweets$cleantweet)
  	#removing rt header from tweet 
  	all_Tweets$cleantweet <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", all_Tweets$cleantweet)
  	#removing punctuation
  	all_Tweets$cleantweet <- gsub("[[:punct:]]+", "", all_Tweets$cleantweet)
  	#conversion to lower case
  	all_Tweets$cleantweet <- tolower(all_Tweets$cleantweet)
  	#removing search term
  	all_Tweets$cleantweet <- gsub(search_term, "", all_Tweets$cleantweet)
  	#removing spaces
  	all_Tweets$cleantweet <- gsub("^\\s+|\\s+$", "", all_Tweets$cleantweet)
   
  	df <- data.frame("cl_num"= all_Tweets$cl_num,"original_tweet"=all_Tweets$tweet,"cleaned_tweet"=all_Tweets$cleantweet,"sentiment"=all_Tweets$sentiment,"dist"=all_Tweets$dist,"freq"=all_Tweets$freq)
  
  	#Assigning polarity based on the sentiment for each tweet 2=negative, 1=positive, 0=neutral
  	df$polarity <- NA
  	df$polarity[which(df$sentiment < 0)] <- "2"
  	df$polarity[which(df$sentiment > 0)] <- "1"
  	df$polarity[which(df$sentiment == 0)] <- "0"
  	
  	#Breaking the tweet into words and assigning the polarity of that tweet to each of the words 
  	#this is a tweet with polarity 0 => this0 is0 a0 tweet0 with0 polarity0 00
  	df$polar_for_words <- NA
  	for(i in 1:nrow(df)){
    	p=strsplit(as.character(df$cleaned_tweet[i]), " ")[[1]]
    	p=p[p != ""]
    	k=paste0(p, df$polarity[i])
    	df$polar_for_words[i] <- paste(k, collapse = " ")
  	}
  
  
  	all_words <- vector()
  	senti_for_all_words <- vector()
  	dist_for_all_words <- vector()
  	freq_for_all_words <- vector()
  	helper_list <- list()
  	document <- vector()
 
  	#Iterating over all clusters
  	for(i in 1:cluster_count){
		
    	cluster <- df[df$cl_num == i,]
    	document <- append(document,toString(cluster$polar_for_words))
    	#Iterating over all tweets in that cluster
    	for(j in 1:nrow(cluster)){
    			#Collecting words
			  	current_words <- strsplit(cluster$polar_for_words[j]," ")[[1]]
			  	all_words <- append(all_words,current_words)
			  	#Collecting sentiment value for the words
			  	current_sent <- rep(cluster$sentiment[j],length(current_words))
			  	senti_for_all_words <- append(senti_for_all_words,current_sent)
			  	#Collecting frequency for the words which is same as the frequency of the tweet in which those words have occured
 			  	current_freq <- rep(cluster$freq[j],length(current_words))
 			  	freq_for_all_words <- append(freq_for_all_words, current_freq)
 			  	#Collecting distance of the words which is same as the distance of the tweet, in which those words have occured, from centroid of the cluster
			  	current_dist <- rep(cluster$dist[j],length(current_words))
			  	dist_for_all_words <- append(dist_for_all_words, current_dist)
    	}
    	#weighted sentiment is calculated by multiplying frequency of word with sentiment
    	weighted_senti <- freq_for_all_words * senti_for_all_words
    	
    	df_weighted_senti <- data.frame("term"=all_words,"freq"= freq_for_all_words, "senti"= weighted_senti)
    	df_weighted_senti_final <- ddply(df_weighted_senti, "term", numcolwise(sum))	
    	
    	df_weighted_senti_final$senti <- df_weighted_senti_final$senti/df_weighted_senti_final$freq  
    	df_dist <- data.frame("term"=all_words,"dist"=dist_for_all_words)
    	df_dist_final <- ddply(df_dist, "term", numcolwise(max))
    
    	df_final <- merge(df_weighted_senti_final,df_dist_final,by="term")

    	#for each narrative, all the words in that narrative, the weighted average of sentiment of each of the words,
    	#frequency for each of the words and the distance of the word from centroid for each of the words are stored here  
    	helper_list[[i]] <- df_final
  	}
  
	#If a cluster has a range of sentiment values, we are scaling the values 
	for(i in 1:cluster_count){
    
   		helper_list[[i]]$senti <- as.numeric(helper_list[[i]]$senti)
   		pos_senti <- helper_list[[i]]$senti[helper_list[[i]]$senti > 0]
   		if(length(pos_senti) > 0){
    		max <- max(pos_senti)
    		min <- min(pos_senti)
    		if(max > min)
      			helper_list[[i]]$senti[helper_list[[i]]$senti > 0] <- 1 + range01(helper_list[[i]]$senti[helper_list[[i]]$senti > 0])
    		else
      			helper_list[[i]]$senti[helper_list[[i]]$senti > 0] <- min
   		}
   
   		helper_list[[i]]$senti[helper_list[[i]]$senti == 0] <- 1.5
    
   		neg_senti <- abs(helper_list[[i]]$senti[helper_list[[i]]$senti < 0])
   		if(length(neg_senti) > 0){
    		max <- max(neg_senti)
    		min <- min(neg_senti)
    		if(max > min)
      			helper_list[[i]]$senti[helper_list[[i]]$senti < 0] <- 1 + range01(helper_list[[i]]$senti[helper_list[[i]]$senti < 0])
    		else
      			helper_list[[i]]$senti[helper_list[[i]]$senti < 0] <- abs(min)
    	}
   
  	}

  	#Ranking#
  	#-----------#
  	wb_helper = createWorkbook()
  	count_by_dist <- list()
  	
  	#print(helper_list[[1]])
  	#print(book_words[book_words$document==1,c("term","count")])
  	for(i in 1:cluster_count){

    	helper_list[[i]]$count = helper_list[[i]]$freq
    	drops <- "freq"
    	#dropping of that colum
    	helper_list[[i]] <- helper_list[[i]][ ,!(names(helper_list[[i]]) %in% drops)]
    	#Words within a cluster are arranged in the descending order of (termfrequency/manhattan distance)
    	count_by_dist[[i]] <- (helper_list[[i]]$count)/(helper_list[[i]]$dist)
    	helper_list[[i]]$count_by_dist <- count_by_dist[[i]]
    	helper_list[[i]] <- helper_list[[i]][order(helper_list[[i]]$count_by_dist, decreasing = TRUE),]
    	sheet = createSheet(wb_helper, paste0("Narrative ",i))
    	if(i==1)
      		addDataFrame(helper_list[[i]], sheet=sheet, startColumn = 1, row.names=FALSE)
    	else
      		addDataFrame(helper_list[[i]], sheet=sheet, startColumn = 1, row.names=FALSE)
  	}
  	print("ending here")
  	
  	#removing stop words from abstraction#
  	#--------------------#
  	saveWorkbook(wb_helper, paste0(file_count,"_count_by_dist", ".xlsx"))
  	stopwords_original <- stopwords('en')
  	stopwords_original <- c(stopwords_original,"rt")
  	stopwords_modified <- c(paste0(stopwords_original,2),paste0(stopwords_original,1), paste0(stopwords_original,0))
  	stopwords_regex <- paste(stopwords_modified, collapse = '\\b|\\b')
  	stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  
  	wb = createWorkbook()
  	for(j in 1:cluster_count){
    	tweets <- df[df$cl_num==j,"original_tweet"]
    	tweet_count <- df[df$cl_num==j , "freq"]
    	#Displaying the top twenty tweets of the cluster
    	tweets <- tweets[1:20]
    	tweet_count <- tweet_count[1:20]
    
    	#abstraction
    	abstraction <- helper_list[[j]]$term
    	sentiment <- helper_list[[j]]$senti

    	#dataframe containg the abstraction and the corresponding sentiment value of a narrative
    	df1 <- data.frame("abstraction" = abstraction, "sentiment" = sentiment)
    	#removing the stopwords from abstraction
    	df1$abstraction <- stringr::str_replace_all(df1$abstraction, stopwords_regex, "")
    	df1 <- df1[df1$abstraction != "",]
    
    	#Taking top 5 words from abstraction 
    	senti_value <- df1$sentiment[1:5]
    	abstraction <- df1$abstraction[1:5]
    	expression <- substring(abstraction, nchar(abstraction))
   		
   		#Converting sentiment polarity number to a string
    	expression[which(expression == "1")] <- "Positive"
    	expression[which(expression == "2")] <- "Negative"
    	expression[which(expression == "0")] <- "Neutral"
  
    	expression[(length(expression) + 1):length(tweets)] <- ""
    	abstraction[(length(abstraction) + 1):length(tweets)] <- ""
    	abstraction <- substr(abstraction,1,nchar(abstraction)-1)

    	#Saving
    	df_helper <- data.frame(tweets,tweet_count,abstraction,expression)
    	sheet = createSheet(wb, paste0("Narrative ",j))
    	if(j==1)
      		addDataFrame(df_helper, sheet=sheet, startColumn = 1, row.names=FALSE)
    	else
      		addDataFrame(df_helper, sheet=sheet, startColumn = 1, row.names=FALSE)
  	}
  	saveWorkbook(wb, paste0(file_count,"_final", ".xlsx"))
}

#Function call
final_clusters("womensmarch","womenmarch",300)
#final_clusters("jallikattu","17",10000)
#final_clusters("jallikattu","22",10000)
#final_clusters("jallikattu","23",10000)
#final_clusters("inauguration","inauguration",10000)
#final_clusters("demonetization","demonetization-tweets",10000)
