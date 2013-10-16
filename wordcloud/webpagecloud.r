
###### ping September 06,2012 ############
###### grab the press news from our press room and create a wordcloud #############

require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)

numPages = 16  # number of pages
source('getLinks.r')
links<-getLinks(numPages)
links_2012<-links[grep("2012",links$pressdates),"links"]
links_2011<-links[grep("2011",links$pressdates),"links"]
links_2010<-links[grep("2010",links$pressdates),"links"]

source('getText.r')

wcloud_given_links<-function(links_by_year,yr){
	texts=NULL
	texts<-sapply(links_by_year,function(x) getText(x,texts))
	x <- texts
	x <- gsub("\t","",x)  # cleaning
	x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
	x <- x[!(x %in% c("", "|"))]
	ap.corpus <- Corpus(DataframeSource(data.frame(as.character(x))))
	ap.corpus <- tm_map(ap.corpus, removePunctuation)
	ap.corpus <- tm_map(ap.corpus, tolower)
	ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
	ap.tdm <- TermDocumentMatrix(ap.corpus)
	ap.m <- as.matrix(ap.tdm)
	ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
	ap.d <- data.frame(word = names(ap.v),freq=ap.v)
	table(ap.d$freq)
	pal2 <- brewer.pal(8,"Dark2")
	png(paste(yr,".png",sep=""), width=1280,height=800)
	wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=2,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
	dev.off()
}

#wcloud_given_links(links_2012,"2012")
wcloud_given_links(links_2011,"2011")
wcloud_given_links(links_2010,"2010")




getLinks <- function(numPages) 
{
	pressroom = "http://xxxxxx"
	linkDates = NULL
	for (i in 1:numPages) {
		url = paste(pressroom,i,"/", sep = "")
	
 		download.file(url, "./texts/index.html")
 		txt<-readLines('./texts/index.html')
		txt <- htmlTreeParse(txt, error=function(...){}, useInternalNodes = TRUE)
 	
		nArticles <- getNodeSet ( txt, "//*/div[@class='grid_12 push_1']/h3/a")
 		links<-sapply(nArticles, function(x) xmlGetAttr(x,'href'))
	
	       	dtime<-getNodeSet(txt, "//*/p[@class='press-date']")
		pressdates<-sapply(dtime, function(x) xmlValue(x))
		linkDates<-rbind(linkDates, cbind(data.frame(links),data.frame(pressdates)))
		
	}
	return(linkDates)
}




getText<-function(x, texts){
	url =as.character(x)
	print(url)
	try_out = try (download.file(url,'./texts/index.html'))
	if (!inherits(try_out,"try-error")){
	txt<-readLines('./texts/index.html') 
	txt <- htmlTreeParse(txt, error=function(...){}, useInternalNodes = TRUE, encoding = "UTF-8",trim=TRUE)
	x <- xpathSApply(txt,"//*/p", xmlValue)
        texts <-rbind(texts,data.frame(x,stringsAsFactors=FALSE))}
	return(texts)
	


}
