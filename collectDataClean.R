setup_twitter_oauth("Y5HGqEALWJPkpnu1k4hzylaOq", "bO1M8ZzRXJK4dtf14lpYaWIv5MCZGTUyozflQEO9Cp3Io6v14V", "1188451962-NEOSrDyqUdEjqghlA5I4hHVSt4o63hA6RMB8pls", "zKLTZ9hAbVDN5yU0zx4YSG210PglU4oaBiph0cbQCxtN9")
temp_tweet=searchTwitter("iphone8", n=10000, lang="en")
temp_tweet_Text = sapply(temp_tweet, function(x) x$getText())
# remove retweet entities
temp_tweet_Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", temp_tweet_Text)

temp_tweet_Text = gsub("<[^>]+>","", temp_tweet_Text)
# remove at people
temp_tweet_Text = gsub("@\\w+", "", temp_tweet_Text)
# remove punctuation
temp_tweet_Text = gsub("[[:punct:]]", "", temp_tweet_Text)
# remove numbers
temp_tweet_Text = gsub("[[:digit:]]", "", temp_tweet_Text)
# remove html links
temp_tweet_Text = gsub("http\\w+", "", temp_tweet_Text)
# remove unnecessary spaces
temp_tweet_Text = gsub("[ \t]{2,}", "", temp_tweet_Text)
temp_tweet_Text = gsub("^\\s+|\\s+$", "", temp_tweet_Text)

# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
temp_tweet_Text = sapply(temp_tweet_Text, try.error)
# remove NAs in some_txt
temp_tweet_Text = temp_tweet_Text[!is.na(temp_tweet_Text)]
write.csv(temp_tweet_Text, file = "/Users/liarthur/Desktop/trainingData.csv", row.names = F)