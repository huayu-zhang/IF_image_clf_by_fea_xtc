source("R/Model_training_environment.R")

ctrl <- trainControl(method = "repeatedcv", number = 10, allowParallel = T, verboseIter = T, classProbs = T)






modelWrap <- function(modelData, modelName, selectedFeature = NA, tuneGrid)
{
  
  if (is.na(selectedFeature)) selectedFeature <- names(modelData$x)
  
  registerDoParallel(detectCores())
  
  ptm <- proc.time()
  
  modelData[[modelName]] <- list()
  
  modelData[[modelName]]$tuneGrid <- tuneGrid 
  
  modelData[[modelName]]$model <- train(x = modelData$x, y = modelData$y, 
                               method = "gbm", 
                               preProcess = c("scale", "center"), 
                               metric = "Kappa", 
                               trControl = ctrl, 
                               tuneGrid = tuneGrid)
  
  modelData[[modelName]]$comp_time <- proc.time() - ptm
  print(modelData[[modelName]]$comp_time)
  stopImplicitCluster()
  
  modelData[[modelName]]$test.mat <- data.frame(
    obs = modelData$test.y,
    pred = predict(object = modelData[[modelName]]$model, newdata = modelData$test.x)
  )
  
  modelData[[modelName]]$test.Accuracy <- sum(apply(modelData[[modelName]]$test.mat, MARGIN = 1, FUN = function(x) x[1] == x[2]))/nrow(modelData[[modelName]]$test.mat)
  
  modelData[[modelName]]$varImp <- varImp(object = modelData[[modelName]]$model)$importance
  
  return(modelData)
}

trainTargetsTable <- table(labelsTrain$Target)
sum(trainTargetsTable > 50)/length(trainTargetsTable)

for (i in c(20, 30, 40, 50))
print(sum(trainTargetsTable[trainTargetsTable > i])/nrow(labelsTrain))

modelData <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 500, sampleSize = 200)


# Tuning one mc500 ns50 ---------------------------------------------------


modelData_500_50 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 500, sampleSize = 50)

tuneGrid <- expand.grid(interaction.depth = c(5,9,13),
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = c(5, 10))

for (i in 1:6)
{
  modelData_500_50 <- modelWrap(modelData = modelData_500_50, 
                                modelName = paste(names(tuneGrid), tuneGrid[i,], sep = "_", collapse = ".."), 
                                selectedFeature = NA, 
                                tuneGrid = tuneGrid[i,]
                                )
  print(names(modelData_500_50))
}



# tuning mC 100 50 20 ---------------------------------------------------------
modelData_mC100_nS50 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 100, sampleSize = 50)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC100_nS50 <- modelWrap(modelData = modelData_mC100_nS50, 
                                 modelName = "tG_5_500_0.01_10",
                                 selectedFeature = NA, 
                                 tuneGrid = tuneGrid
)




modelData_mC50_nS50 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 50, sampleSize = 50)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC50_nS50 <- modelWrap(modelData = modelData_mC50_nS50, 
                                  modelName = "tG_5_500_0.01_10",
                                  selectedFeature = NA, 
                                  tuneGrid = tuneGrid
)



modelData_mC20_nS50 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 20, sampleSize = 50)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC20_nS50 <- modelWrap(modelData = modelData_mC20_nS50, 
                                  modelName = "tG_5_500_0.01_10",
                                  selectedFeature = NA, 
                                  tuneGrid = tuneGrid
)





# mc 50 ns 500 ------------------------------------------------------------


modelData_mC50_nS500 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 50, sampleSize = 500)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC50_nS500 <- modelWrap(modelData = modelData_mC50_nS500, 
                                  modelName = "tG_5_500_0.01_10",
                                  selectedFeature = NA, 
                                  tuneGrid = tuneGrid
                                  )


# mc50 ns1000 -------------------------------------------------------------


modelData_mC50_nS1000 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 50, sampleSize = 1000)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC50_nS1000 <- modelWrap(modelData = modelData_mC50_nS1000, 
                                  modelName = "tG_5_500_0.01_10",
                                  selectedFeature = NA, 
                                  tuneGrid = tuneGrid
)


# mc50 ns2341 -------------------------------------------------------------



modelData_mC50_nS2341 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 50, sampleSize = 2341)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC50_nS2341 <- modelWrap(modelData = modelData_mC50_nS2341, 
                                   modelName = "tG_5_500_0.01_10",
                                   selectedFeature = NA, 
                                   tuneGrid = tuneGrid
)


# mc20 ns2341 -------------------------------------------------------------



modelData_mC20_nS2341 <- samplingTargets(labelsTrain = labelsTrain, DTlowCor = DTlowCor, minCount = 20, sampleSize = 2341)


tuneGrid <- expand.grid(interaction.depth = 5,
                        n.trees = 500,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

modelData_mC20_nS2341 <- modelWrap(modelData = modelData_mC20_nS2341, 
                                   modelName = "tG_5_500_0.01_10",
                                   selectedFeature = NA, 
                                   tuneGrid = tuneGrid
)

# test Predict ------------------------------------------------------------
load("rdata/predicting_environment.rdata")
sampleSubmission <- read.csv(file = "rdata/sample_submission.csv", stringsAsFactors = F)
yetTobeTestMat <- read.delim(file = "rdata/yetTobeTestMat.txt", stringsAsFactors = F)

yetTobeTestMat$Predicted <- apply(yetTobeTestMat[1:28], 1, function(x) make.names(paste(which(x == 1) - 1, collapse = " ")))
test.Pred <- data.frame(index = as.numeric(row.names(DTlowCorTest)), Predicted = as.character(predict(object = modelData$Gbm$model, newdata = DTlowCorTest)), stringsAsFactors = F)
test.Pred <- rbind(test.Pred, yetTobeTestMat[, c("index", "Predicted")])

test.Pred <- test.Pred[order(test.Pred$index),]

test.Pred$Id <- sampleSubmission$Id

test.Pred <- test.Pred[3:1]

test.Pred$Predicted <- sub(pattern = "X", replacement = "", x = test.Pred$Predicted)

test.Pred$Predicted <- gsub(pattern = "\\.", replacement = " ", x = test.Pred$Predicted)

test.Pred <- cbind(test.Pred, t(sapply(X = test.Pred$Predicted, FUN = flattenLabels)))

colnames(test.Pred) <- c("Id", "Predicted", "index", labelNames)

predictionCounts <- sapply(X = test.Pred[4:31], sum)

for (i in 1:28)
{
  if (predictionCounts[i] < 10)
  test.Pred[,i + 3] <- test.Pred[,i+3] | GbmAllOutput$Probs[,i] > 0.5
}



predictionCounts <- sapply(X = test.Pred[4:31], sum)

test.Pred$Rods...rings <- GbmAllOutput$Probs$Rods...rings > GbmAllOutput$cutoff.ratioAdj[28]

predictionCounts <- sapply(X = test.Pred[4:31], sum)

test.Pred$Predicted.Combi <- apply(test.Pred[,4:31], MARGIN = 1, FUN = function(x) paste(which(x) -1, collapse = " ") )
test.Pred$Predicted.Combi.Anl <- apply(X = cbind.data.frame(GbmAllOutput$Probs[1:28], test.Pred$Predicted.Combi), MARGIN = 1, FUN = targetAnnealing)

test.Pred[4:31] <-  t(sapply(X = test.Pred$Predicted.Combi.Anl, FUN = flattenLabels))

predictionCounts <- sapply(X = test.Pred[4:31], sum)


write.csv(x = data.frame(Id = test.Pred$Id, Predicted = test.Pred$Predicted.Combi.Anl), file = "submission/mC50_nS2341_model_combi_anl.csv", row.names = F)
