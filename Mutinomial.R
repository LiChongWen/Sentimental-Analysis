posTweet1 <- read.csv(file="/Users/liarthur/Desktop/posdata.csv", header=TRUE)
posTweet <- posTweet1$data
posCount <- as.numeric(length(as.matrix(posTweet)))
negTweet1 <- read.csv(file="/Users/liarthur/Desktop/negdata.csv", header=TRUE)
negTweet <- negTweet1$data
negCount <- as.numeric(length(as.matrix(negTweet)))
neuTweet1 <- read.csv(file="/Users/liarthur/Desktop/nurdata.csv", header=TRUE)
neuTweet <- neuTweet1$data
neuCount <- as.numeric(length(as.matrix(neuTweet)))
all_Tweet1 <- rbind(posTweet1, negTweet1, neuTweet1)
all_Tweet <-all_Tweet1$data
pos_Tweet <- posCount/(posCount+neuCount+negCount)
neu_Tweet <- neuCount/(posCount+neuCount+negCount)
neg_Tweet <- negCount/(posCount+neuCount+negCount)
token <- function(sentence){
  #remove unnecessary characters and split up by word 
  sentence <- gsub('[[:punct:]]', '', sentence)
  sentence <- gsub('[[:cntrl:]]', '', sentence)
  sentence <- gsub('\\d+', '', sentence)
  sentence <- tolower(sentence)
  wordList <- str_split(sentence, '\\s+')
  words <- unlist(wordList)
  return(words)
}
post_count1<- as.data.frame(token(posTweet))
post_count <- table(post_count1)
neut_count1<- as.data.frame(token(neuTweet))
neut_count <- table(neut_count1)
negt_count1<- as.data.frame(token(negTweet))
negt_count <- table(negt_count1)
allt_count1<- as.data.frame(token(all_Tweet))
allt_count <- table(allt_count1)
temp_tweet = read.csv(file = "/Users/liarthur/Desktop/labeledIphone.csv", header=TRUE)
temp_tweet1 = temp_tweet$data
tweet_df <- as.matrix(temp_tweet)
tweet_df1 <- as.matrix(temp_tweet1)
sim <- as.numeric(length(tweet_df1))
tweet_df <- as.matrix(cbind(temp_tweet, 'predict'))
for(i in 1:sim){
  process <- token(tweet_df[i])
  #  process_table <- table(process)
  iter <- as.numeric(length(process))
  pos_like <- 1
  neg_like <- 1
  neu_like <- 1
  vocal <- c("iphone","x","the","a","and","of","to","is","s","it","in","that","as","with","its","for","this","an","nt","on","about","at","by","i","his","lrb","rrb","up","can","director","would","thier","us","way","which","was","her","from","be","are","were","all","he","she","me","does","we","makes","our","my","story","if","your","some","what","have","has","had","just","also","both","make")
  for(j in 1:iter){
    if(is.na(match(process[j],vocal))){
      count_pos <- as.numeric(length(grep(process[j],posTweet)))
      count_neg <- as.numeric(length(grep(process[j],negTweet)))
      count_neu <- as.numeric(length(grep(process[j],neuTweet)))
      pos_like <-as.numeric(pos_like*(count_pos+1)/(length(all_Tweet)+posCount))
      neg_like <-as.numeric(neg_like*(count_neg+1)/(length(all_Tweet)+negCount))
      neu_like <-as.numeric(neu_like*(count_neu+1)/(length(all_Tweet)+neuCount))
    }
    pos_result <- pos_like * pos_Tweet
    neg_result <- neg_like * neg_Tweet
    neu_result <- neu_like * neu_Tweet
    max_pro <- max(pos_result, neg_result, neu_result)
    if(max_pro == pos_result){
      tweet_df[i,3]=1
    }else if(max_pro == neg_result){
      tweet_df[i,3]=-1
    }else{
      tweet_df[i,3]=0
    }
  }
}
contablet <- table(tweet_df[,3],dnn=list('predicted'))
contablet
write.csv(tweet_df, file = "/Users/liarthur/Desktop/iphoneTweetPredicted.csv", row.names = F)
tweetCompare <- read.csv(file="/Users/liarthur/Desktop/iphoneTweetPredicted.csv", header=TRUE)
posNum <- as.numeric(contablet[3:3])
negNum <- as.numeric(contablet[1:1])
neuNum <- as.numeric(contablet[2:2])
result <- posNum - negNum
if(result > 0){ 
  sentimentalResult <- "postive"
}else{
  sentimentalResult <- "negative"
}
sentimentalResult
#contablet <- table(tweet_df[,2],tweet_df[,3],dnn=list('actual','predicted'))
#contablet
#Accuracy = sum(diag(contablet)) / sum(contablet)
#Accuracy
confsvm.mat <- confusionMatrix(tweetCompare[1:438,]$result, tweetCompare[1:438,]$X.predict.)
confsvm.mat
confsvm.mat$overall['Accuracy']