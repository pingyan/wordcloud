getTweets<-function(numPages)
# Capture Twitter search for wordcloud
{
read.more=TRUE
tweets <- NULL
tweet.count <- 0
page <- 1

while (read.more)
{
  # construct Twitter search URL
  URL <- paste('http://search.twitter.com/search.atom?q=',QUERY,'&rpp=100&page=', page, sep='')
  # fetch remote URL and parse
  XML <- htmlTreeParse(URL, useInternal=TRUE)

  # Extract list of "entry" nodes
  entry     <- getNodeSet(XML, "//entry")

  read.more <- (length(entry) > 0)
  if (read.more)
  {
    for (i in 1:length(entry))
    {
      subdoc     <- xmlDoc(entry[[i]])   # put entry in separate object to manipulate

      published  <- unlist(xpathApply(subdoc, "//published",    xmlValue))

      # see Converting time zones in R: tips, tricks and pitfalls
      # http://blog.revolutionanalytics.com/2009/06/converting-time-zones.html

      # Example "published" string:  "2011-05-19T00:57:41Z"
      # Let's remove "T" and "Z" from string
      published  <- gsub("Z"," ", gsub("T"," ",published) )

      # Convert from GMT to central time
      time.gmt   <- as.POSIXct(published,"GMT")
      local.time <- format(time.gmt, tz="America/Los_Angeles")

      title<- unlist(xpathApply(subdoc, "//title",xmlValue))

      #contents<- unlist(xpathApply(subdoc, "//content",xmlValue))

      author<- unlist(xpathApply(subdoc, "//author/name",  xmlValue))

      #tweet <- paste(local.time, " @", author, ":  ", title, sep="")
      tweet <- paste( title)
      entry.frame <- data.frame(tweet, author, local.time, stringsAsFactors=FALSE)
      tweet.count <- tweet.count + 1
      rownames(entry.frame) <- tweet.count
      tweets <- rbind(tweets, entry.frame)
    }
    page <- page + 1
    read.more <- (page <= numPages)   # Seems to be 15 page limit
  }
}
  return(tweets$tweet)


}
