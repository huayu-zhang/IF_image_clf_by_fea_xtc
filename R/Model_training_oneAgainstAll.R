source("R/Model_training_environment.R")

ctrl <- trainControl(method = "repeatedcv", repeats = 10, classProbs = TRUE)



# Modelling Pls 50 Fea------------------------------------------------------------


ModelsPls50Fea <- list()

for (i in labelNames[])
{
  registerDoParallel(detectCores())
  
  ModelsPls50Fea[[i]]  <- train(
    x = trainSamples[[i]], 
    y = trainLabels[[i]], 
    method = "pls", 
    preProcess = c("center","scale"), 
    trControl = ctrl,
    metric = "Kappa",
    tuneLength = 20
  )
  
  stopImplicitCluster()
  
  print(i)    
}



# Modelling Pls 100 Fea------------------------------------------------------------


ModelsPls100Fea <- list()

for (i in labelNames)
{
  registerDoParallel(detectCores())
  
  ModelsPls100Fea[[i]]  <- train(
    x = trainSamples[[i]], 
    y = trainLabels[[i]], 
    method = "pls", 
    preProcess = c("center","scale"), 
    trControl = ctrl,
    metric = "Kappa",
    tuneLength = 20
  )
  
  stopImplicitCluster()
  
  print(i)    
}

save(ModelsPls50Fea, file = "rdata/ModelsPls50Fea.rdata")
save(ModelsPls100Fea, file = "rdata/ModelsPls100Fea.rdata")


# Models gbm --------------------------------------------------------------



ModelsGbmBestTune <- list()

tuneGrid <- expand.grid(interaction.depth = seq(1, 21, by = 4),
                        n.trees = seq(100, 1100, 200),
                        shrinkage = c(0.01, 0.1),
                        n.minobsinnode = c(5, 10, 15, 20))


for (i in labelNames[27:28])
{
  ptm <- proc.time()
  ModelsGbmBestTune[[i]] <- tuningModel(x = trainSamples[[i]], y = trainLabels[[i]], method = "gbm", tuneGrid = tuneGrid, ctrl = ctrl)
  print(i)
  print(proc.time() - ptm)
}


ModelsGbmNew <- list()


for (i in labelNames)
{
  ptm <- proc.time()
  
  registerDoParallel(detectCores())
  
  ModelsGbmNew[[i]] <- train(
    x = trainSamples[[i]], 
    y = trainLabels[[i]], 
    method = "gbm", 
    preProcess = c("center","scale"), 
    trControl = ctrl,
    metric = "Kappa",
    tuneGrid = ModelsGbmBestTune[[i]]
  )
  
  stopImplicitCluster()
  
  print(i)
  print(proc.time() - ptm)
}

save(ModelsGbm, file = "rdata/modelsGbm.rdata")
save(ModelsGbmBestTune, file = "rdata//modelsGbmBestTune.rdata")
save(ModelsGbmNew, file = "rdata/modelsGbmNew.rdata")



#ModelsGbmAll <- list()

load("rdata/GbmAll.rdata")


ModelsGbmAll[[1]]$results
