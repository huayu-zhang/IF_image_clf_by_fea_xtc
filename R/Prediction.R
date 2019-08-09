source("R/Prediction_environment.R")

GbmOutput <- list()

GbmOutput$Probs <- predict.Models(model = ModelsGbmNew, newdata = testSamples)
GbmOutput$Probs <- predict.Finalise(predMatProbs = GbmOutput$Probs, fileList = fileListSumTest, appendix = yetTobeTestPred)

GbmOutput$Probs$Predicted <- apply(X = GbmOutput$Probs[,1:28], MARGIN = 1, 
                                   FUN = function(x) 
                                     {
                                     predicted <- paste(which(x > 0.5) - 1, collapse = " ")
                                     if (predicted != "") return(predicted)
                                     predicted <- paste(which.max(x) - 1)
                                     return(predicted)
                                     })

GbmOutput$Submission <- csv.submission(predFinal = GbmOutput$Probs, column = "Predicted", filename = "submission/test.csv")

ratioLabelTraining <- sapply(labelsTrain[4:31], FUN = function(x) sum(x)/length(x))

GbmOutput$cutoff.ratioAdj <- c()

for (i in labelNames)
  GbmOutput$cutoff.ratioAdj[i] <- quantile(GbmOutput$Probs[,i], probs = 1 - ratioLabelTraining[i]) 


GbmOutput$Probs$Predicted.Adj <- apply(X = GbmOutput$Probs[,1:28], MARGIN = 1, 
                                   FUN = function(x) 
                                   {
                                     predicted <- paste(which(x > GbmOutput$cutoff.ratioAdj) - 1, collapse = " ")
                                     if (predicted != "") return(predicted)
                                     predicted <- paste(which.max(x) - 1)
                                     return(predicted)
                                   })

GbmOutput$Submission.ratioAdj <- csv.submission(predFinal = GbmOutput$Probs, column = "Predicted.Adj", filename = "submission/testAdj.csv")



save(GbmOutput, file = "rdata/GbmOutput.rdata")


# Gbm All prediction ------------------------------------------------------


GbmAllOutput <- list()

GbmAllOutput$Probs <- predict.Models(model = ModelsGbmAll, newdata = testSamples)
GbmAllOutput$Probs <- predict.Finalise(predMatProbs = GbmAllOutput$Probs, fileList = fileListSumTest, appendix = yetTobeTestPred)

GbmAllOutput$Probs$Predicted <- apply(X = GbmAllOutput$Probs[,1:28], MARGIN = 1, 
                                   FUN = function(x) 
                                   {
                                     predicted <- paste(which(x > 0.5) - 1, collapse = " ")
                                     if (predicted != "") return(predicted)
                                     predicted <- paste(which.max(x) - 1)
                                     return(predicted)
                                   })

GbmAllOutput$Submission <- csv.submission(predFinal = GbmAllOutput$Probs, column = "Predicted", filename = "submission/GbmAll.csv")

ratioLabelTraining <- sapply(labelsTrain[4:31], FUN = function(x) sum(x)/length(x))

GbmAllOutput$cutoff.ratioAdj <- c()

for (i in labelNames)
  GbmAllOutput$cutoff.ratioAdj[i] <- quantile(GbmAllOutput$Probs[,i], probs = 1 - ratioLabelTraining[i]) 


GbmAllOutput$Probs$Predicted.Adj <- apply(X = GbmAllOutput$Probs[,1:28], MARGIN = 1, 
                                       FUN = function(x) 
                                       {
                                         predicted <- paste(which(x > GbmAllOutput$cutoff.ratioAdj) - 1, collapse = " ")
                                         if (predicted != "") return(predicted)
                                         predicted <- paste(which.max(x) - 1)
                                         return(predicted)
                                       })

GbmAllOutput$Submission.ratioAdj <- csv.submission(predFinal = GbmAllOutput$Probs, column = "Predicted.Adj", filename = "submission/GbmAllAdj.csv")
GbmAllOutput$Probs$Predicted.Anl <- apply(X = GbmAllOutput$Probs[,c(1:28, 31)], MARGIN = 1, FUN = targetAnnealing)
GbmAllOutput$Probs$Predicted.Adj.Anl <- apply(X = GbmAllOutput$Probs[,c(1:28, 32)], MARGIN = 1, FUN = targetAnnealing)

GbmAllOutput$Submission.Anl <- csv.submission(predFinal = GbmAllOutput$Probs, column = "Predicted.Anl", filename = "submission/GbmAllAnl.csv")
GbmAllOutput$Submission.Adj.Anl <- csv.submission(predFinal = GbmAllOutput$Probs, column = "Predicted.Adj.Anl", filename = "submission/GbmAllAdjAnl.csv")


save(GbmAllOutput, file = "rdata/GbmAllOutput.rdata")





# PLS prediction ----------------------------------------------------------

load(file = "rdata/ModelsPls100Fea.rdata")
load(file = "rdata/ModelsPls50Fea.rdata")


PLSOutput <- list()

PLSOutput$Probs <- predict.Models(model = ModelsPls50Fea, newdata = testSamples)
PLSOutput$Probs <- predict.Finalise(predMatProbs = PLSOutput$Probs, fileList = fileListSumTest, appendix = yetTobeTestPred)

PLSOutput$Probs$Predicted <- apply(X = PLSOutput$Probs[,1:28], MARGIN = 1, 
                                   FUN = function(x) 
                                   {
                                     predicted <- paste(which(x > 0.5) - 1, collapse = " ")
                                     if (predicted != "") return(predicted)
                                     predicted <- paste(which.max(x) - 1)
                                     return(predicted)
                                   })

PLSOutput$Submission <- csv.submission(predFinal = PLSOutput$Probs, column = "Predicted", filename = "submission/PLS.csv")

ratioLabelTraining <- sapply(labelsTrain[4:31], FUN = function(x) sum(x)/length(x))

PLSOutput$cutoff.ratioAdj <- c()

for (i in labelNames)
  PLSOutput$cutoff.ratioAdj[i] <- quantile(PLSOutput$Probs[,i], probs = 1 - ratioLabelTraining[i]) 


PLSOutput$Probs$Predicted.Adj <- apply(X = PLSOutput$Probs[,1:28], MARGIN = 1, 
                                       FUN = function(x) 
                                       {
                                         predicted <- paste(which(x > PLSOutput$cutoff.ratioAdj) - 1, collapse = " ")
                                         if (predicted != "") return(predicted)
                                         predicted <- paste(which.max(x) - 1)
                                         return(predicted)
                                       })

PLSOutput$Submission.ratioAdj <- csv.submission(predFinal = PLSOutput$Probs, column = "Predicted.Adj", filename = "submission/PLSadj.csv")

save(PLSOutput, file = "rdata/PLSOutput.rdata")























