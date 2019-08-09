source("R/Model_training_environment.R")

load(file = "rdata/DTall.rdata")
load(file = "rdata/labelsTrain.rdata")
featureSelection <- read.delim(file = "rdata/feature_name.txt", stringsAsFactors = F)
featureSelection[,2:3] <- sapply(featureSelection[,2:3], as.logical)


for (i in 1:length(DTall))
{
  DTall[is.na(DTall[,i]),i] <- 0
  DTall[DTall[,i] == -Inf,i] <- 0
  DTall[DTall[,i] == Inf,i] <- 0
}

sum(sapply(DTall, function(x) sum(is.na(x))))



labelNames <- names(labelsTrain)[4:31]


highCorr <- findCorrelation(cor(DTall), cutoff = 0.75)
DTlowCor <- DTall[,c(-1, -highCorr)]
row.names(DTlowCor) <- DTall$index



# Feature selection -------------------------------------------------------


featureDiffDist <- list()

for (i in labelNames)
{
  featureDiffDist[[i]] <- sapply(DTlowCor, diffDist, obs = labelsTrain[,i])
  
  print(i)
}

selectedFeatures <- list()

for (i in labelNames)
{
  selectedFeatures[[i]] <- featureDiffDist[[i]] > quantile(x = featureDiffDist[[i]], probs = 306/356)
}



# Sample selection --------------------------------------------------------

trainSamples <- list()
trainLabels <- list()

for (i in labelNames)
{
  numberOfPos <- sum(labelsTrain[,i])
  
  numberOfNeg <- max(numberOfPos, 1000)
  
  posSamples <- DTlowCor[labelsTrain[,i], selectedFeatures[[i]]]
  
  negSamples <- DTlowCor[!labelsTrain[,i],selectedFeatures[[i]]]
  
  trainSamples[[i]] <- rbind(posSamples, negSamples[sample(1:nrow(negSamples), numberOfNeg),])
  
  trainLabels[[i]] <- c(rep("POS", numberOfPos), rep("NEG", numberOfNeg))
  
  rm(list = c("numberOfPos", "numberOfNeg", "posSamples", "negSamples"))
}


# Save trainings ----------------------------------------------------------

save(list = c("DTall", "DTlowCor", "labelsTrain", "selectedFeatures", "trainLabels", "trainSamples", "highCorr", "labelNames"),
     file = "rdata/modeling_environment.rdata")




# Test Set ----------------------------------------------------------------


load(file = "rdata/DTallTest.rdata")
load(file = "rdata/fileListSumTest.rdata")




for (i in 1:length(DTallTest))
{
  DTallTest[is.na(DTallTest[,i]),i] <- 0
  DTallTest[DTallTest[,i] == -Inf,i] <- 0
  DTallTest[DTallTest[,i] == Inf,i] <- 0
}

sum(sapply(DTallTest, function(x) sum(is.na(x))))

DTlowCorTest <- DTallTest[,c(-1, -highCorr)]
row.names(DTlowCorTest) <- DTallTest$index


# Sample selection --------------------------------------------------------

testSamples <- list()

for (i in labelNames)
{
  testSamples[[i]] <- DTlowCorTest[,selectedFeatures[[i]]]
}

save(list = c("DTallTest", "DTlowCorTest", "selectedFeatures", "testSamples", "highCorr", "labelNames"),
     file = "rdata/predicting_environment.rdata")


