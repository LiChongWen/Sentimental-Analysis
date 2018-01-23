data<- read.csv(file = "/Users/liarthur/Desktop/iphoneTweet.csv", header=FALSE, stringsAsFactors=FALSE)
data = data$V1
result = calculate_score(data)
for (i in 1 : length(result)) {
  if(result[i]>=1){
    result[i]= 1
  }else if(result[i]< -1){
    result[i]= -1
  }else if(result[i]==-1){
    result[i]= 0
  }else if(result[i]==0){
    result[i]= 0
  }
}
matrix = as.matrix(cbind(data, result))
write.csv(matrix, file = "/Users/liarthur/Desktop/labeledIphone.csv", row.names = F)

#matrixNeg = subset(matrix, result== -1)
#matrixPos = subset(matrix, result == 1)
#matrixNeu = subset(matrix, result== 0)
#write.csv(matrixNeg, file = "/Users/liarthur/Desktop/negData.csv", row.names = F)
#write.csv(matrixNeu, file = "/Users/liarthur/Desktop/nurData.csv", row.names = F)
#write.csv(matrixPos, file = "/Users/liarthur/Desktop/posData.csv", row.names = F)