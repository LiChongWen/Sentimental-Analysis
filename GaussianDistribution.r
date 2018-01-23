library(stringr)

#get lexicon
totalData <- read.csv("11111.csv", header = FALSE)
totalData <- unique(totalData)
words <- totalData$V1

words = str_split(words,"\\s+")
words = unlist(words)
lexicon <- unique(words)


#get posData

posData <- read.csv("posdata.csv", header = FALSE)
posData <- unique(posData)
posData <- posData$V1
posDataNo <- str_split(posData, "\\s+")
posDataNo <- length(unlist(posDataNo))
posMatrix = matrix(0,nrow=length(posData),ncol=length(lexicon))
for(i in 1:length(posData)){
	temp = str_split(posData[i],"\\s+")
	temp = unlist(temp)
	for(j in 1:length(lexicon)){
		posMatrix[i,j] = length(grep(lexicon[j],temp))
	}
}

#get negData

negData <- read.csv("negdata.csv", header = FALSE)
negData <- unique(negData)
negData <- negData$V1
negDataNo <- str_split(negData, "\\s+")
negDataNo <- length(unlist(negData))
negMatrix = matrix(0,nrow=length(negData),ncol=length(lexicon))
for(i in 1:length(negData)){
	temp = str_split(negData[i],"\\s+")
	temp = unlist(temp)
	for(j in 1:length(lexicon)){
		negMatrix[i,j] = length(grep(lexicon[j],temp))
	}
}

#get neuData

neuData <- read.csv("neudata.csv", header = FALSE)
neuData <- unique(neuData)
neuData <- neuData$V1
neuDataNo <- str_split(neuData, "\\s+")
neuDataNo <- length(unlist(neuData))
neuMatrix = matrix(0,nrow=length(neuData),ncol=length(lexicon))
for(i in 1:length(neuData)){
	temp = str_split(neuData[i],"\\s+")
	temp = unlist(temp)
	for(j in 1:length(lexicon)){
		neuMatrix[i,j] = length(grep(lexicon[j],temp))
	}
}

#prior probability
posPriorProb = length(posData)/length(totalData)
negPriorProb = length(negData)/length(totalData)
neuPriorProb = length(neuData)/length(totalData)


testData <- read.csv("labeledIphone.csv", header = FALSE)
#testData <- unique(testData)
testMatrix <- as.matrix(cbind(testData,"predict"))
testData <- testData$V1


for(i in 1:length(testData)){
	posProb <- 1
	negProb <- 1
	neuProb <- 1
	temp <- str_split(testData[i],"\\s+")
	temp = unlist(temp)
	temp_unique = unique(temp)
	
#	对于词典里的 每个词  看出现次数，没出现过为0  出现过 在计算出现次数
#	但是这样之后  最后的书 会编程 INF 和NAN， 即使使用 log高斯  也是这样
# 
# 	rs = match(lexicon, temp_uniqued)
#	for(j in 1:length(rs)){
#		if(is.na(rs[j])){
#			attrb_value = 0
#		}
#		else{
#			attrb_value <- length(grep(temp_unique[rs[j]],temp))
#		}
		
#		pos_mean <- mean(posMatrix[,j])
#		pos_sd <- sd(posMatrix[,j])
#		
#		neg_mean <- mean(negMatrix[,j])
#		neg_sd <- sd(negMatrix[,j])
		
#		neu_mean <- mean(neuMatrix[,j])
#		neu_sd <- sd(neuMatrix[,j])
		
#		posProb = posProb + dnorm(attrb_value,mean=pos_mean,sd=pos_sd,log=TRUE)
#		negProb = negProb + dnorm(attrb_value,mean=neg_mean,sd=neg_sd,log=TRUE)
#		neuProb = neuProb + dnorm(attrb_value,mean=neu_mean,sd=neu_sd,log=TRUE)
#	}
	
	
	
	
	for(j in 1:length(temp_unique)){
		attrb_value <- length(grep(temp_unique[j],temp))
		attrb_location <- match(temp_unique[j],lexicon)
		if(is.na(attrb_location)) next
		
		pos_mean <- mean(posMatrix[,attrb_location])
		pos_sd <- sd(posMatrix[,attrb_location])
		
		neg_mean <- mean(negMatrix[,attrb_location])
		neg_sd <- sd(negMatrix[,attrb_location])
		
		neu_mean <- mean(neuMatrix[,attrb_location])
		neu_sd <- sd(neuMatrix[,attrb_location])
		
		posProb = posProb * dnorm(attrb_value,mean=pos_mean,sd=pos_sd,log=FALSE)
		negProb = negProb * dnorm(attrb_value,mean=neg_mean,sd=neg_sd,log=FALSE)
		neuProb = neuProb * dnorm(attrb_value,mean=neu_mean,sd=neu_sd,log=FALSE)
	}
	
	pos_result <- posProb * posPriorProb
    neg_result <- negProb * negPriorProb
    neu_result <- neuProb * neuPriorProb
	
	max_pro <- max(pos_result, neg_result, neu_result)
    if(max_pro == pos_result){
      testMatrix[i,3]="positive"
    }else if(max_pro == neg_result){
      testMatrix[i,3]="negative"
    }else{
      testMatrix[i,3]="neutral"
    }
}
contablet <- table(testMatrix[,2],testMatrix[,3],dnn=list('actual','predicted'))
contablet
Accuracy = sum(diag(contablet)) / sum(contablet)
Accuracy
