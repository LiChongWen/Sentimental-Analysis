#! /path/to/Rscript

library(stringr)

#get lexicon
totalData <- read.csv("totaldata.csv", header = TRUE)
totalData <- unique(totalData)
totalData <- totalData$data
totalData = gsub("<[^>]+>","",totalData)

words = str_split(totalData,"\\s+")
words = unlist(words)
lexicon <- unique(words)



#get posData

posData <- read.csv("posdata.csv", header = TRUE)
posData <- unique(posData)
posData <- posData$data
posData = gsub("<[^>]+>","",posData)
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

negData <- read.csv("negdata.csv", header = TRUE)
negData <- unique(negData)
negData <- negData$data
negData = gsub("<[^>]+>","",negData)
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

neuData <- read.csv("nurdata.csv", header = TRUE)
#neuData <- unique(neuData)
neuData <- neuData$data
neuData = gsub("<[^>]+>","",neuData)
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

testData <- read.csv("labeledIphone.csv", header = TRUE)
#testData <- unique(testData)
testMatrix <- as.matrix(cbind(testData,"predict"))
testData <- testData$data
testData = gsub("<[^>]+>","",testData)
for(i in 1:length(testData)){
	posProb <- 1
	negProb <- 1
	neuProb <- 1
	temp <- str_split(testData[i],"\\s+")
	temp = unlist(temp)
	temp = unique(temp)
	rs = match(lexicon, temp)
	for(j in 1:length(rs)){
		if(is.na(rs[j])){
			posProb = posProb * (length(posData)-sum(posMatrix[,j])+1)/(length(posData)+2)
			negProb = negProb * (length(negData)-sum(negMatrix[,j])+1)/(length(negData)+2)
			neuProb = neuProb * (length(neuData)-sum(neuMatrix[,j])+1)/(length(neuData)+2)
		}	else{
			posProb = posProb * (sum(posMatrix[,j])+1)/(length(posData)+2)
			negProb = negProb * (sum(negMatrix[,j])+1)/(length(negData)+2)
			neuProb = neuProb * (sum(neuMatrix[,j])+1)/(length(neuData)+2)
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
print(Accuracy)

