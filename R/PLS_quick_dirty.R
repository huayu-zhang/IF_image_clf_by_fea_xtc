source("R/Model_training_environment.R")




ctrl <- trainControl(method = "repeatedcv", repeats = 10, classProbs = TRUE)


# feature plot all models -------------------------------------------------

DTnucPlots <- list()

for (i in labelNames[1:6])
{

graphDir <- paste("graph/", i, sep = "")
dir.create(path = graphDir)


DTnucPlots[[i]] <- ggplot(data = DTnucGG, mapping = aes(x = DTnucGG[,i], y = value)) +
  geom_violin() +
  facet_wrap(facets = .~variable, ncol = 6, scales = "free_y")

ggsave(filename = paste(i, "_violin.png", sep = ""), plot = DTnucPlots[[i]], device = "png", path = graphDir, width = 15, height = 60, limitsize = F)

}

# Structure to store Pls modeling data ------------------------------------

ModelsPls <- list()


labelNames <- names(labelsTrain)[4:31]


for (i in labelNames)
  ModelsPls[[i]] <- list(Model = NA,
                         selectedFea = NA)



for (i in labelNames[2])
{
  print(i)
  
  ModelsPls[[i]]$selectedFea <- sapply(X = DTnuc , FUN = diffDist,  obs = labelsTrain[,i])
  
  ModelsPls[[i]]$selectedFea <- ModelsPls[[i]]$selectedFea > quantile(x = ModelsPls[[i]]$selectedFea, probs = 0.723)
  
  tuneGrid <- expand.grid(ncomp = 1:20)
  
  registerDoParallel(detectCores())
  
  ModelsPls[[i]]$Model <- train(
    x = DTnuc[,ModelsPls[[i]]$selectedFea], 
    y = factor(labelsTrain[,i], levels = c(FALSE, TRUE), labels = c("NEG", "POS")), 
    method = "kernelpls", 
    preProcess = c("center","scale"), 
    metric = "Kappa",
    trControl = ctrl, 
    tuneGrid = tuneGrid
  )
  
  stopImplicitCluster()
  
  
}



