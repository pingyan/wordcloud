
###### ping@opendns.com September 07,2012 ############
###### grab tweets and create a wordcloud #############

require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)

twitter.search <- "#opendns"
QUERY <- URLencode(twitter.search)
numPages = 50  # number of pages of press news links

source('CaptureTwitterSearch.r')
tweets<-getTweets(numPages)

wcloud_given_links<-function(tweets){

	x <- tweets
	x <- gsub("\t","",x)  # cleaning
	
	x <- gsub("opendns","",x,fixed=TRUE,ignore.case=TRUE) # remove the largest weight word
	
	x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
	x <- sub("http://(.*)", "", x, perl=TRUE) # remove the links on the tweets, or should I go grab them?
	x <- x[!(x %in% c("", "|"))]
	ap.corpus <- Corpus(DataframeSource(data.frame(as.character(x))))
	ap.corpus <- tm_map(ap.corpus, removePunctuation)
	ap.corpus <- tm_map(ap.corpus, tolower)
	ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, c(stopwords("english"),"opendns")))
	ap.tdm <- TermDocumentMatrix(ap.corpus)
	ap.m <- as.matrix(ap.tdm)
	ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
	ap.d <- data.frame(word = names(ap.v),freq=ap.v)
	table(ap.d$freq)
	pal2 <- brewer.pal(8,"Dark2")
	png("opendns_tweets.png", width=1580,height=1080)
	wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=2,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
	dev.off()
}

wcloud_given_links(tweets)

