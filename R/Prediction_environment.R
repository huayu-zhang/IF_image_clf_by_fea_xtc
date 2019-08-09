
# Librarys ----------------------------------------------------------------
required_libs <- c("ggplot2", "EBImage", "caret", "doParallel", "naivebayes", "reshape2", "impute", "randomForest", "pls", "gbm", "kernlab")

new_libs <- required_libs[!(required_libs %in% installed.packages()[,"Package"])]

if (length(new_libs) > 0) install.packages(new_libs, dependencies = T, quiet = T)

new_libs <- required_libs[!(required_libs %in% installed.packages()[,"Package"])]

if (length(new_libs) > 0)
{
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", quiet = T)
  
  BiocManager::install(new_libs)
}


for (i in 1:length(required_libs))
  library(required_libs[i], character.only = T)

rm(list = c("i", "new_libs", "required_libs"))

sessionInfo()




# Data import -------------------------------------------------------------

load("rdata/predicting_environment.rdata")
load("rdata/fileListSumTest.rdata")
load("rdata/yetTobeTest.rdata")

load("rdata/labelsTrain.rdata")

load(file = "rdata/GbmOutput.rdata")
load(file = "rdata/GbmAllOutput.rdata")
load(file = "rdata/PLSOutput.rdata")

yetTobeTestPred <- read.delim(file = "rdata/yetTobeTestMat.txt", stringsAsFactors = F)

# Model import ------------------------------------------------------------

load("rdata/modelsGbmNew.rdata")
load("rdata/ModelsPls50Fea.rdata")




paths_Output <- c(
  trainOutput = "E:/trainOutput/",
  testOutput = "E:/testOutput/"
)

fileListOutput <- lapply(X = paths_Output, 
                         FUN = function(x) {
                           
                           
                           fileList <- list.files(path = x, full.names = T)
                           
                           if (length(fileList) == 0) return(NULL)
                           
                           fileListAttr <- data.frame(do.call(rbind, strsplit(x = fileList, split = "\\/|\\_|\\.")), stringsAsFactors = F)
                           
                           fileListAttr <- fileListAttr[, 3:4]
                           names(fileListAttr) <-  c("index", "sample")
                           
                           fileListAttr$index <- as.numeric(fileListAttr$index)
                           
                           fileListAttr$link <- fileList
                           
                           fileListAttr <- fileListAttr[order(fileListAttr$index), ]
                           
                           return(fileListAttr)
                         })


yetTobeTestMat <- fileListOutput$testOutput[yetTobeTest,1:2]



# Prediction wrap up ------------------------------------------------------

predict.Models <- function(model, newdata)
{
  predListProbs <- list()
  
  for (i in names(model))
  {
    predListProbs[[i]] <- predict(object = model[[i]], newdata = newdata[[i]], type = "prob")
  }
  
  
  
  predMatProbs <- data.frame(sapply(predListProbs, function(x) x[,2]))
  
  return(predMatProbs)
}


predict.Finalise <- function(predMatProbs, fileList, appendix)
{
  
  predMatProbs$index <- fileList$index
  predMatProbs$sample <- fileList$sample
  
  
  predFinal <- rbind(predMatProbs, appendix)
  
  predFinal <- predFinal[order(predFinal$index),]
  
  return(predFinal)
}


csv.submission <- function(predFinal, column,  filename)
{
  sampleSub <- read.csv(file = "rdata/sample_submission.csv", stringsAsFactors = F)
  
  if (sum(sampleSub$Id == predFinal$sample) == 11702)
  {
  finalSub <- cbind(Id = sampleSub[,"Id"], Predicted = predFinal[column])
  names(finalSub) <- c("Id", "Predicted")
  write.csv(x = finalSub, file = filename, row.names = F)
  return(finalSub)
  } else {
    stop()
  }
}


collapseLabels <- function(binLabel)
{
  paste(which(binLabel > 0.5) - 1, collapse = " ")
}

flattenLabels <- function(labelStr)
{
  0:27 %in% as.numeric(strsplit(labelStr, split = " ")[[1]])
}

indvidualLabels <- function(labelStr)
{
  as.numeric(strsplit(labelStr, split = " ")[[1]])
}




# Target Annealling -------------------------------------------------------


labelCombi <- names(table(labelsTrain$Target))
labelCombiProb <- table(labelsTrain$Target)

labelCombi <- sapply(X = labelCombi, FUN = flattenLabels)
labelCombi <- sapply(X = data.frame(labelCombi), FUN = collapseLabels)
labelCombi.Indi <- lapply(X = labelCombi, FUN = indvidualLabels)



targetAnnealing <- function(ProbandPredicted)
{
  Predicted <- as.character(ProbandPredicted[29])
  Probs <- as.numeric(ProbandPredicted[1:28])
  
  if (Predicted %in% labelCombi) return(Predicted)  
  
  Predicted.Indi <- indvidualLabels(labelStr = Predicted)
  
  matchScore <- sapply(labelCombi.Indi, 
                       function(x) {
                         ((length(Predicted.Indi)/sum(Predicted.Indi %in% x) + length(x)/sum(x %in% Predicted.Indi))/2)^-1
                       }) 
  
  Predicted.Anl <- labelCombi[matchScore == max(matchScore)]
  
  if (length(Predicted.Anl) == 1) return(Predicted.Anl)
  
  anlScore <- sapply(Predicted.Anl, function(x) prod(Probs[indvidualLabels(x) + 1]))
  
  return(Predicted.Anl[which.max(anlScore)])
}