
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

load("rdata/modeling_environment.rdata")
load("rdata/modelsGbmBestTune.rdata")

# Distribution comparison -------------------------------------------------


diffDist <- function(value, obs)
{
  minValue <- min(value)
  maxValue <- max(value)
  
  negDist <- hist(x = value[!obs], breaks = seq(minValue, maxValue, by = (maxValue - minValue)/100), plot = F)$density
  posDist <- hist(x = value[obs], breaks = seq(minValue, maxValue, by = (maxValue - minValue)/100), plot = F)$density
  
  aucDist <- sum(abs(negDist - posDist))/101
  
  return(aucDist)
}



# Sampling wrap -----------------------------------------------------------

samplingModel <- function(x, y, method, tuneGrid, ctrl)
{
  y.table <- table(y)
  y.labels <- names(y.table)
  
  index.NEG <- row.names(x[y == y.labels[1],])
  index.POS <- row.names(x[y == y.labels[2],])
  
  samplingNumber <- seq(200, 1000, 100)
  
  samplingIndex <- list()
  samplingLabels <- list()
  
  for (i in samplingNumber)
  {
    if (i/2 <= y.table[2])
    {
      samplingIndex[[paste("sampling", i, sep = "_")]] <- c(sample(index.POS, i/2),
                                                            sample(index.NEG, i/2))
      samplingLabels[[paste("sampling", i, sep = "_")]] <- c(rep("POS", i/2), rep("NEG", i/2))
    } else {
      samplingIndex[[paste("sampling", i, sep = "_")]] <- c(index.POS, 
                                                            sample(index.NEG, i - length(index.POS)))
      samplingLabels[[paste("sampling", i, sep = "_")]] <- c(rep("POS", length(index.POS)), rep("NEG", i - length(index.POS)))
    }
  }
  
  #Test Sample
  
  registerDoParallel(detectCores())
  
  ModelTest <- train(
    x = x[samplingIndex[[5]],], 
    y = samplingLabels[[5]], 
    method = method, 
    preProcess = c("center","scale"), 
    trControl = ctrl,
    metric = "Kappa",
    tuneGrid = tuneGrid
  )
  
  stopImplicitCluster()

  bestGrid <- ModelTest$bestTune
  
  ptm <- list()
  Model <- list()
  
  for (i in 1:length(samplingNumber))
  {
    ptm_now <- proc.time()
    registerDoParallel(detectCores())
    
    Model[[i]] <- train(
      x = x[samplingIndex[[i]],], 
      y = samplingLabels[[i]], 
      method = method, 
      preProcess = c("center","scale"), 
      trControl = ctrl,
      metric = "Kappa",
      tuneGrid = bestGrid
    )
    
    stopImplicitCluster()
    ptm[[i]] <- proc.time() - ptm_now
  }
  
  ptm <- do.call(rbind, ptm)
  
  return(list(ModelTest, Model, bestGrid, ptm))
}


#tuningWrap


tuningModel <- function(x, y, method, tuneGrid, ctrl)
{
  y.table <- table(y)
  y.labels <- names(y.table)
  
  index.NEG <- row.names(x[y == y.labels[1],])
  index.POS <- row.names(x[y == y.labels[2],])
  
  samplingNumber <- 600
  
  samplingIndex <- list()
  samplingLabels <- list()
  
  for (i in samplingNumber)
  {
    if (i/2 <= y.table[2])
    {
      samplingIndex[[paste("sampling", i, sep = "_")]] <- c(sample(index.POS, i/2),
                                                            sample(index.NEG, i/2))
      samplingLabels[[paste("sampling", i, sep = "_")]] <- c(rep("POS", i/2), rep("NEG", i/2))
    } else {
      samplingIndex[[paste("sampling", i, sep = "_")]] <- c(index.POS, 
                                                            sample(index.NEG, i - length(index.POS)))
      samplingLabels[[paste("sampling", i, sep = "_")]] <- c(rep("POS", length(index.POS)), rep("NEG", i - length(index.POS)))
    }
  }
  
  #Test Sample
  
  registerDoParallel(detectCores())
  
  ModelTest <- train(
    x = x[samplingIndex[[1]],], 
    y = samplingLabels[[1]], 
    method = method, 
    preProcess = c("center","scale"), 
    trControl = ctrl,
    metric = "Kappa",
    tuneGrid = tuneGrid
  )
  
  stopImplicitCluster()
  
  bestGrid <- ModelTest$bestTune
  
  return(bestGrid)
  
}



# Performance visualization -----------------------------------------------



crossTable <- function(model)
{
  crossTable <- cbind.data.frame(pred = predict(object = model[[labelName]], newdata = DTnuc[,selectedFea]),
                                 obs = factor(labelsTrain[,labelName], levels = c(FALSE, TRUE), labels = c("NEG", "POS"))
  )
  
  table(crossTable)
}



probPlot <- function(model)
{
  probTable <- cbind.data.frame(pred = predict(object = model[[labelName]], newdata = DTnuc[,selectedFea], type = "prob"),
                                 obs = factor(labelsTrain[,labelName], levels = c(FALSE, TRUE), labels = c("NEG", "POS"))
  )
  
  p <- ggplot(probTable, aes(x = obs, y = pred.POS)) +
    geom_violin()
  
  p
}



samplingTargets <- function(labelsTrain, DTlowCor, minCount, sampleSize)
{
  targetIncluded <- names(table(labelsTrain$Target))[table(labelsTrain$Target) > minCount]
  
  sampleIncluded <- c()
  targetNumber <- c()
  
  for (i in targetIncluded)
  { 
    targetNumber[i] <- min(sum(labelsTrain$Target == i), sampleSize)
    sampleIncluded <- c(sampleIncluded, 
                        sample(x = which(labelsTrain$Target == i), size =  targetNumber[i]))
  }
  
  return(list(
    x = DTlowCor[sampleIncluded,],
    y = make.names(rep(targetIncluded, targetNumber)),
    test.x = DTlowCor[-sampleIncluded,],
    test.y = make.names(labelsTrain$Target[-sampleIncluded])))
  
}