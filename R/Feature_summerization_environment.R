
# Librarys ----------------------------------------------------------------
required_libs <- c("tiff", "ggplot2", "png", "EBImage", "raster", "SearchTrees")

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




# File list ---------------------------------------------------------------

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



# Find duplicating cells --------------------------------------------------


find.duplicate <- function(nucObj)
{
  nucInt <- nucObj[, "x.a.b.mean"]
  
  nucIntStats <- boxplot.stats(nucInt)
  
  highInt <- names(nucIntStats$out > nucIntStats$stats[5])
  
  
  if (length(highInt) < 2) return(NA)
  
  dupNucs <- nucObj[highInt,]
  proxiDist <- 5 * median(nucObj[, "x.0.s.radius.mean"])
  
  
  
  #Make dist map of dupNucs
  
  d <- as.matrix(dist(dupNucs[,1:2], upper = T))
  diag(d) <- NA
  smallD <- d < proxiDist
  
  if (sum(smallD, na.rm = T) == 0) return(NA)
  
  dupPairs <- list()
  
  for (i in 1:(nrow(smallD) - 1))
    for (j in (i + 1):ncol(smallD) )
      if (smallD[i,j]) dupPairs[[paste(i,j, sep = "_")]] <- c(row.names(smallD)[i],colnames(smallD)[j])
  
  dupPairsObjs <- lapply(dupPairs, function(x) nucObj[x,])
  
  return(dupPairsObjs)
}


find.largeGreen <- function(greenObj)
{
  particleArea <- greenObj[,"x.0.s.area"]
  particleStats <- boxplot.stats(particleArea)
  
  if (sum(particleStats$out > particleStats$stats[5]) == 0) return(NA)
  
  largeArea <- names(particleStats$out > particleStats$stats[5])
  
  
  largeAreaObjs <- greenObj[largeArea,]
  
  if (length(largeArea) == 1) return(t(as.matrix(largeAreaObjs)))
  
  return(largeAreaObjs)
}


G.angle <- function(triPoints)
{
  d <- as.matrix(dist(triPoints, upper = T))
  
  if (d[3,1] > d[2,1] & d[3,2] > d[2,1]) return(0)
  
  angle <- acos((d[3,1]^2 + d[3,2]^2 - d[2,1]^2)/(2*d[3,1]*d[3,2]))
  
  return(angle)
}


G.between.Dup <- function(nuc, greenHuayu)
{
  dupPairsObjs <- find.duplicate(nucObj = nuc)
  
  if (is.na(dupPairsObjs[1])) return(c(max = 0, over120 = 0))
  
  largeGreenObjs <- find.largeGreen(greenObj = greenHuayu)
  
  if (is.na(largeGreenObjs[1])) return(c(max = 0, over120 = 0))
  
  GAngles <- c()
  
  for (i in 1:length(dupPairsObjs))
    for (j in 1:nrow(largeGreenObjs))
    {
      GAngles <- c(GAngles, G.angle(triPoints = rbind(dupPairsObjs[[i]][,1:2], largeGreenObjs[j,1:2]) ))
    }
  
  if (sum(GAngles > 0) == 0) return(c(max = max(GAngles), over120 = 0))
  
  return(c(max = max(GAngles), over120 = sum(GAngles > 2.094)/sum(GAngles > 0)))
}



dist.fromNuc <- function(nucRef, greenParticle)
{
  tree <- createTree(data = nucRef[,1:2])
  
  G.dist <- c()
  
  for (i in 1:nrow(greenParticle))
  {
    ind <- knnLookup(tree = tree, newx = greenParticle[i,1], newy = greenParticle[i,2], k = 1)
    G.dist <- c(G.dist, sum((nucRef[ind, 1:2] - greenParticle[i, 1:2])^2))
  }
  
  G.dist <- sqrt(G.dist/median(nucRef[,"x.0.s.area"]))
  
  return(G.dist)
}


ratio.ProxiTheta <- function(green, lowCut, highCut)
{
  
  distTheta <- c(dist(x = green[,"x.0.m.theta"]))
  distGreen <- c(dist(x = green[,1:2]))
  
  meanQ10 <- mean(distTheta[distGreen <= quantile(x = distGreen, prob = lowCut)])
  meanQ90 <- mean(distTheta[distGreen >= quantile(x = distGreen, prob = highCut)])
  
  return(meanQ10/meanQ90)
  
}


# Pipeline ----------------------------------------------------------------


featureSumPipe <- function(index)
{
  load(file = fileListOutput$trainOutput$link[fileListOutput$trainOutput$index == index])
  sample <- strsplit(x = fileListOutput$trainOutput$link[fileListOutput$trainOutput$index == index], split = "\\/|\\.|\\_")[[1]][4]
  
  fileName <- paste("E:/trainSum/",index, "_", sample, "_sum.rdata", sep = "")
  
  if (file.exists(fileName)) return()
  
  if (!("featureList" %in% ls())) return(NA)
  
  for (i in 2:12) 
    if (is.null(featureList[[i]])) 
    {
      featureList[[i]] <- matrix(rep(0, 89), nrow = 1)
      colnames(featureList[[i]]) <- colnames(featureList$M.Blue_Its.Blue)
    }
  
  
  
  
  if (nrow(featureList$M.G.Nuc.Huayu_Its.Green) >50)
  {
    featureList$M.G.Nuc.Huayu_Its.Green <- featureList$M.G.Nuc.Huayu_Its.Green[order(featureList$M.G.Nuc.Huayu_Its.Green[,"x.0.s.area"]),]
    
    featureList$M.G.Nuc.Huayu_Its.Green <- featureList$M.G.Nuc.Huayu_Its.Green[1:50,]
  }
  
  
  if (nrow(featureList$M.G.Cyt.Huayu_Its.Green) >50)
  {
    areaOrder <- order(featureList$M.G.Cyt.Huayu_Its.Green[,"x.0.s.area"])
    
    featureList$M.G.Cyt.Huayu_Its.Green <- featureList$M.G.Cyt.Huayu_Its.Green[areaOrder,]
    featureList$M.G.Cyt.Huayu_Its.Green <- featureList$M.G.Cyt.Huayu_Its.Green[1:49,]
    
    featureList$M.G.Cyt.Huayu_Its.Red <- featureList$M.G.Cyt.Huayu_Its.Red[areaOrder,]
    featureList$M.G.Cyt.Huayu_Its.Red <- featureList$M.G.Cyt.Huayu_Its.Red[1:49,]
    
    
    featureList$M.G.Cyt.Huayu_Its.Yellow <- featureList$M.G.Cyt.Huayu_Its.Yellow[areaOrder,]
    featureList$M.G.Cyt.Huayu_Its.Yellow <- featureList$M.G.Cyt.Huayu_Its.Yellow[1:49,]
  }
  
  
  
  
  featureVector <- c(
    manualFeature = featureList$manualFeatures,
    btwDupGreen = G.between.Dup(nuc = featureList$M.Blue_Its.Blue, greenHuayu = featureList$M.G.Cyt.Huayu_Its.Green),
    G.Otsu.ProxiTheta = ratio.ProxiTheta(green = featureList$M.G.Cyt.Otsu_Its.Green, lowCut = 0.05, highCut = 0.95),
    G.Huayu.ProxiTheta = ratio.ProxiTheta(green = featureList$M.G.Cyt.Huayu_Its.Green, lowCut = 0.2, highCut = 0.8),
    mean = do.call(c, lapply(X = featureList[2:12], FUN = function(x) apply(x, 2, mean))), 
    sd = do.call(c, lapply(X = featureList[2:12], FUN = function(x) apply(x, 2, sd))),
    max = do.call(c, lapply(X = featureList[2:12], FUN = function(x) apply(x, 2, max)))
  )
  
  varName <- paste("var", index, sample, sep = "_")
  
  assign(x = varName, value = featureVector)
  
  save(list = varName, file = fileName)
}






featureSumPipe.Test <- function(index)
{
  load(file = fileListOutput$testOutput$link[fileListOutput$testOutput$index == index])
  sample <- strsplit(x = fileListOutput$testOutput$link[fileListOutput$testOutput$index == index], split = "\\/|\\.|\\_")[[1]][4]
  
  fileName <- paste("E:/testSum/",index, "_", sample, "_sum.rdata", sep = "")
  
  if (file.exists(fileName)) return()
  
  if (!("featureList" %in% ls())) return(NA)
  
  for (i in 2:12) 
    if (is.null(featureList[[i]])) 
    {
      featureList[[i]] <- matrix(rep(0, 89), nrow = 1)
      colnames(featureList[[i]]) <- colnames(featureList$M.Blue_Its.Blue)
    }
  
  
  
  
  if (nrow(featureList$M.G.Nuc.Huayu_Its.Green) >50)
  {
    featureList$M.G.Nuc.Huayu_Its.Green <- featureList$M.G.Nuc.Huayu_Its.Green[order(featureList$M.G.Nuc.Huayu_Its.Green[,"x.0.s.area"]),]
    
    featureList$M.G.Nuc.Huayu_Its.Green <- featureList$M.G.Nuc.Huayu_Its.Green[1:50,]
  }
  
  
  if (nrow(featureList$M.G.Cyt.Huayu_Its.Green) >50)
  {
    areaOrder <- order(featureList$M.G.Cyt.Huayu_Its.Green[,"x.0.s.area"])
    
    featureList$M.G.Cyt.Huayu_Its.Green <- featureList$M.G.Cyt.Huayu_Its.Green[areaOrder,]
    featureList$M.G.Cyt.Huayu_Its.Green <- featureList$M.G.Cyt.Huayu_Its.Green[1:49,]
    
    featureList$M.G.Cyt.Huayu_Its.Red <- featureList$M.G.Cyt.Huayu_Its.Red[areaOrder,]
    featureList$M.G.Cyt.Huayu_Its.Red <- featureList$M.G.Cyt.Huayu_Its.Red[1:49,]
    
    
    featureList$M.G.Cyt.Huayu_Its.Yellow <- featureList$M.G.Cyt.Huayu_Its.Yellow[areaOrder,]
    featureList$M.G.Cyt.Huayu_Its.Yellow <- featureList$M.G.Cyt.Huayu_Its.Yellow[1:49,]
  }
  
  
  
  
  featureVector <- c(
    manualFeature = featureList$manualFeatures,
    btwDupGreen = G.between.Dup(nuc = featureList$M.Blue_Its.Blue, greenHuayu = featureList$M.G.Cyt.Huayu_Its.Green),
    G.Otsu.ProxiTheta = ratio.ProxiTheta(green = featureList$M.G.Cyt.Otsu_Its.Green, lowCut = 0.05, highCut = 0.95),
    G.Huayu.ProxiTheta = ratio.ProxiTheta(green = featureList$M.G.Cyt.Huayu_Its.Green, lowCut = 0.2, highCut = 0.8),
    mean = do.call(c, lapply(X = featureList[2:12], FUN = function(x) apply(x, 2, mean))), 
    sd = do.call(c, lapply(X = featureList[2:12], FUN = function(x) apply(x, 2, sd))),
    max = do.call(c, lapply(X = featureList[2:12], FUN = function(x) apply(x, 2, max)))
  )
  
  varName <- paste("var", index, sample, sep = "_")
  
  assign(x = varName, value = featureVector)
  
  save(list = varName, file = fileName)
}
