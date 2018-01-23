#! /path/to/Rscript

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
posMatrix = matrix(0,nrow=length(posData),ncol=length(lexicon))
for(i in 1:length(posData)){
	temp = str_split(posData[i],"\\s+")
	temp = unlist(temp)
#	temp = unique(temp)
	rs <- match(temp,lexicon)
	for(j in 1:length(rs)){
		posMatrix[i,rs[j]] = 1
	}
}

#get negData

negData <- read.csv("negdata.csv", header = FALSE)
negData <- unique(negData)
negData <- negData$V1
negMatrix = matrix(0,nrow=length(negData),ncol=length(lexicon))
for(i in 1:length(negData)){
	temp = str_split(negData[i],"\\s+")
	temp = unlist(temp)
#	temp = unique(temp)
	rs <- match(temp,lexicon)
	for(j in 1:length(rs)){
		negMatrix[i,rs[j]] = 1
	}
}


#get neuData

neuData <- read.csv("neudata.csv", header = FALSE)
neuData <- unique(neuData)
neuData <- neuData$V1
neuMatrix = matrix(0,nrow=length(neuData),ncol=length(lexicon))
for(i in 1:length(neuData)){
	temp = str_split(neuData[i],"\\s+")
	temp = unlist(temp)
#	temp = unique(temp)
	rs <- match(temp,lexicon)
	for(j in 1:length(rs)){
		neuMatrix[i,rs[j]] = 1
	}
}

# testData
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
	temp = unique(temp)
	rs = match(temp,lexicon)
	
	
#	对于词典中的每个词 都判断 出没出现，这样做之后  negative 出现次数为0，准确度 下降只17%
#	rs = match(lexicon, unique(temp))
#	for(j in 1:length(rs)){
#		if(is.na(rs[j])){
#			posProb = posProb * (length(posData)-sum(posMatrix[,j])+1)/(length(posData)+2)
#			negProb = negProb * (length(negData)-sum(negMatrix[,j])+1)/(length(negData)+2)
#			neuProb = neuProb * (length(neuData)-sum(neuMatrix[,j])+1)/(length(neuData)+2)
#		}
#		else{
#			posProb = posProb * (sum(posMatrix[,j])+1)/(length(posData)+2)
#			negProb = negProb * (sum(negMatrix[,j])+1)/(length(negData)+2)
#			neuProb = neuProb * (sum(neuMatrix[,j])+1)/(length(neuData)+2)
#		}
#	}
	
	for(j in 1:length(rs)){
		if(!is.na(rs[j])){
			posProb = posProb * (sum(posMatrix[,rs[j]])+1)/(length(posData)+2)
			negProb = negProb * (sum(negMatrix[,rs[j]])+1)/(length(negData)+2)
			neuProb = neuProb * (sum(neuMatrix[,rs[j]])+1)/(length(neuData)+2)
		}
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

