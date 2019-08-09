
# Librarys ----------------------------------------------------------------
required_libs <- c("tiff", "ggplot2", "png", "EBImage")

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














# Filelist creation -------------------------------------------------------

create.filelist <- function(folder){
  files <- data.frame(list.files(folder), stringsAsFactors = FALSE)
  
  files <- cbind(files, data.frame(do.call(rbind, strsplit(x = files[,1], split = "_|\\.")), stringsAsFactors = FALSE)[1:2])
  
  files$link <- list.files(folder, full.names = T)
  
  colnames(files) <- c("file", "sample", "channel", "link")
  
  fileList <- list()
  nChannels <- nlevels(factor(files$channel))
  
  for (i in 1:nrow(files) - 1)
    fileList[[files$sample[nChannels*i+1]]] <- files[nChannels*i + 1:nChannels,]
  
  save(fileList, file = paste("rdata/", 
                              strsplit(folder, split = "/")[[1]][length(strsplit(folder, split = "/")[[1]])], 
                              "_fileList.rdata", sep = ""))
  
  return(fileList)
}

# Channel merging ---------------------------------------------------------

##Input dataFrames and output folder
merge.channels <- function(imgDF, folder)
{
  blue <- readTIFF(source = imgDF$link[1])
  
  TIFF <- array(dim = c(dim(x = blue), 4))
  
  TIFF[,,1] <- readTIFF(source = imgDF$link[3])
  TIFF[,,2] <- readTIFF(source = imgDF$link[2])
  TIFF[,,3] <- blue
  TIFF[,,4] <- readTIFF(source = imgDF$link[4])
  
  c1 <- writeTIFF(what = TIFF[,,1:3], where = paste(folder, "/RGB/",imgDF$sample[1], "_RGB.tif", sep = ""))
  c2 <- writeTIFF(what = TIFF[,,c(4,2,3)], where = paste(folder, "/YGB/",imgDF$sample[1], "_YGB.tif", sep = ""))
  return(c1&c2)
}

##Input dataFrames and output folder
merge.remove <- function(imgDF, folder)
{
  blue <- readTIFF(source = imgDF$link[1])
  
  TIFF <- array(dim = c(dim(x = blue), 4))
  
  TIFF[,,1] <- readTIFF(source = imgDF$link[3])
  TIFF[,,2] <- readTIFF(source = imgDF$link[2])
  TIFF[,,3] <- blue
  TIFF[,,4] <- readTIFF(source = imgDF$link[4])
  
  c1 <- writeTIFF(what = TIFF[,,1:3], where = paste(folder, "/RGB/",imgDF$sample[1], "_RGB.tif", sep = ""))
  c2 <- writeTIFF(what = TIFF[,,c(4,2,3)], where = paste(folder, "/YGB/",imgDF$sample[1], "_YGB.tif", sep = ""))
  
  if (c1&c2)
    file.remove(imgDF$link)
}


# Channel IO --------------------------------------------------------------

img.Import <- function(imgDF)
{
  BLUE <- readPNG(source = imgDF$link[1])
  GREEN <- readPNG(source = imgDF$link[2])
  RED <- readPNG(source = imgDF$link[3])
  YELLOW <- readPNG(source = imgDF$link[4])
  
  listChannel <- list(RED, GREEN, BLUE, YELLOW)
  names(listChannel) <- c("RED", "GREEN", "BLUE", "YELLOW")
  
  return(listChannel)
}


# Image Operations --------------------------------------------------------

##cutoff + fillHull, return binarized channels


##Fill the coords of a image to a certain value
fill.coords <- function(coords, img, fill = 0)
{
  if (is.list(coords)) coords <- data.frame(do.call(rbind, coords))
  
  coords$pos <- coords$x + (coords$y - 1) * nrow(img)
  
  img_lin <- c(img)
  
  img_lin[coords$pos] <- fill
  
  img <- matrix(img_lin, nrow = nrow(img), ncol = ncol(img))
  
  return(img)
}


# Control functions -----------------------------------------------------

##This function segregates every image into 3 parts: nuclues, cytoplasm and residual
##input image df and set = "train" or "test"
##Optimization still needed to input accuracy of detection
segregation.PNG <- function(imgDF, set, saveFiles = F)
{
  #input
  BLUE <- readPNG(source = imgDF$link[1])
  GREEN <- readPNG(source = imgDF$link[2])
  RED <- readPNG(source = imgDF$link[3])
  YELLOW <- readPNG(source = imgDF$link[4])
  
  
  #Binary matrix
  BLUE[BLUE >= 0.012] <- 1 
  BLUE[BLUE != 1] <- 0
  BLUE <- fillHull(BLUE)
  
  YELLOW[YELLOW >= 0.02] <- 1
  YELLOW[YELLOW != 1] <- 0
  
  RED[RED >= 0.02] <- 1
  RED[RED != 1] <- 0
  
  
  #Define area NOT BLUE = cytosol, BLUE = nucleus, NOT RED = residual
  RED[YELLOW == 1] <- 1 
  RED[BLUE == 1] <- 1
  RED <- fillHull(fillHull(RED))
  
  GREEN_nucleus <- GREEN
  GREEN_nucleus[BLUE == 0] <- 0
  
  GREEN_cytosol <- GREEN
  GREEN_cytosol[BLUE == 1] <- 0
  
  GREEN_residual <- GREEN
  GREEN_residual[RED == 1] <- 0 
  
  if (saveFiles)
  {
  writeTIFF(what = GREEN_nucleus, where = paste(set,"_nucleus/", imgDF$sample[1],"_nucleus.tif", sep = ""))
  writeTIFF(what = GREEN_cytosol, where = paste(set,"_cytosol/", imgDF$sample[1],"_cytosol.tif", sep = ""))
  writeTIFF(what = GREEN_residual, where = paste(set, "_residual/", imgDF$sample[1],"_residual.tif", sep = ""))
  }
  
  return(list(GREEN_nucleus, GREEN_cytosol, GREEN_residual))
}


segregation.TIFF <- function(imgDF, set)
{
  #input
  BLUE <- readTIFF(source = imgDF$link[1])
  GREEN <- readTIFF(source = imgDF$link[2])
  RED <- readTIFF(source = imgDF$link[3])
  YELLOW <- readTIFF(source = imgDF$link[4])
  
  
  #Binary matrix
  BLUE[BLUE >= 0.012] <- 1 
  BLUE[BLUE != 1] <- 0
  BLUE <- fillHull(BLUE)
  
  YELLOW[YELLOW >= 0.02] <- 1
  YELLOW[YELLOW != 1] <- 0
  
  RED[RED >= 0.02] <- 1
  RED[RED != 1] <- 0
  
  
  #Define area NOT BLUE = cytosol, BLUE = nucleus, NOT RED = residual
  RED[YELLOW == 1] <- 1 
  RED[BLUE == 1] <- 1
  RED <- fillHull(fillHull(RED))
  
  GREEN_nucleus <- GREEN
  GREEN_nucleus[BLUE == 0] <- 0
  
  GREEN_cytosol <- GREEN
  GREEN_cytosol[BLUE == 1] <- 0
  
  GREEN_residual <- GREEN
  GREEN_residual[RED == 1] <- 0 
  
  if (saveFiles)
  {
  writeTIFF(what = GREEN_nucleus, where = paste(set,"_nucleus/", imgDF$sample[1],"_nucleus.tif", sep = ""))
  writeTIFF(what = GREEN_cytosol, where = paste(set,"_cytosol/", imgDF$sample[1],"_cytosol.tif", sep = ""))
  writeTIFF(what = GREEN_residual, where = paste(set, "_residual/", imgDF$sample[1],"_residual.tif", sep = ""))
  }
  
  return(list(GREEN_nucleus, GREEN_cytosol, GREEN_residual))
}


combiArea <- function(x = NULL, newArea)
{
  if (is.null(x))
  {
    return(newArea)
  }
  
  area <- rbind(x, newArea)
  area <- unique(area)
  return(area)
}


Nb8 <- expand.grid(x = -1:1, y = -1:1)

nbEight <- function(x, minEdge = 0, maxEdge = 513) {
  n <- nrow(x)
  series <- rep(1:n, each = 9)
  nbs <- x[series,]
  nbs$x <- nbs$x + Nb8$x
  nbs$y <- nbs$y + Nb8$y
  
  nbs <- rbind(nbs, x)
  
  nbs <- nbs[!duplicated(nbs, fromLast = T),]
  
  nbs <- nbs[1:(nrow(nbs) - nrow(x)),]
  
  nbs
}

framingBin <- function(Bin) {
  colFrame <- matrix(data = 0, nrow = nrow(Bin), ncol = 2)
  rowFrame <- matrix(data = 0, nrow = 2, ncol = ncol(Bin) + 4)
  
  Bin <- cbind(colFrame, Bin, colFrame)
  Bin <- rbind(rowFrame, Bin, rowFrame)
  
  Bin
}

quatroCrossBin <- function(startCoord, map)
{
  r1 <- startCoord[1]
  c1 <- startCoord[2]
  
  c2 <- c1
  r2 <- r1
  
  while (map[r2 + 1,c2] == 1)
    r2 <- r2 + 1
  
  cross <- data.frame(matrix(data = r1:r2, ncol = 1, dimnames = list(NULL, "x")))
  cross$y <- c2
  
  if ((r2 - r1) < 6)  return(cross)
  
  c3 <- c2
  r3 <- ceiling((r2 + r1)/2)
  
  r4 <- r3
  c4 <- c3
  
  while (map[r4, c4 - 1] == 1)
    c4 <- c4 - 1
  
  r5 <- r3
  c5 <- c3
  
  while (map[r5, c5 + 1] == 1)
    c5 <- c5 + 1
  
  cross <- rbind(cross, data.frame(matrix(data = c(rep(r3, length(c4:c5)),c4:c5), ncol = 2, dimnames = list(NULL, c("x", "y")))))
  
  cross <- unique(cross)
  
  return(cross)
}


particleAnalysis <- function(Bin)
{
  objList <- list()
  
  Bin <- framingBin(Bin)
  
  Nrow <- nrow(Bin) - 2
  Ncol <- ncol(Bin) - 2
  
  
  for (i in 3:Nrow)
    for (j in 3:Ncol)
      if (Bin[i,j] == 1)
      {
        
        seedArea <- quatroCrossBin(startCoord = c(i,j), map = Bin)
        objArea <- NULL
        incomplete <- TRUE
        
        while (incomplete) { 
          
          objArea <- combiArea(x = objArea, newArea = seedArea)
          
          nbsArea <- nbEight(x = seedArea)
          
          for (k in 1:nrow(seedArea))
            Bin[seedArea$x[k], seedArea$y[k]] <- 0
          
          posNbs <- apply(X = nbsArea, MARGIN = 1, function(x) Bin[x[1], x[2]] > 0)
          
          seedArea <- nbsArea[posNbs,]
          
          incomplete <- nrow(seedArea) > 0
        }
        
        include <- !(min(objArea$x) == 3 | max(objArea$x) == Nrow - 2 | min(objArea$y) == 3 | max(objArea$x) == Ncol - 2 )
        
        if (include) 
          {
          objArea$x <- objArea$x - 2
          objArea$y <- objArea$y - 2
          objList <- c(objList, list(objArea))
          
        }
        
      }

  
  return(objList)
}

particleAnalysis.test <- function(Bin)
{
  objList <- list()
  
  Bin <- framingBin(Bin)
  
  Nrow <- nrow(Bin) - 2
  Ncol <- ncol(Bin) - 2
  
  k <- 1
  
  for (i in 3:Nrow)
    for (j in 3:Ncol)
      if (Bin[i,j] == 1)
      {
        
        seedArea <- quatroCrossBin(startCoord = c(i,j), map = Bin)
        objArea <- NULL
        incomplete <- TRUE
        
        
        while (incomplete) { 
          
          objArea <- combiArea(x = objArea, newArea = seedArea)
          
          nbsArea <- nbEight(x = seedArea)
          
          for (k in 1:nrow(seedArea))
            Bin[seedArea$x[k], seedArea$y[k]] <- 0
          
          posNbs <- apply(X = nbsArea, MARGIN = 1, function(x) Bin[x[1], x[2]] > 0)
          
          seedArea <- nbsArea[posNbs,]
          
          incomplete <- nrow(seedArea) > 0
          
          writeTIFF(Bin, where = paste(i,j,k, ".tif", sep = "_"))
          k <- k+1
        }
        
        include <- !(min(objArea$x) == 3 | max(objArea$x) == Nrow - 2 | min(objArea$y) == 3 | max(objArea$x) == Ncol - 2 )
        
        if (include) 
        {
          objArea$x <- objArea$x - 2
          objArea$y <- objArea$y - 2
          objList <- c(objList, list(objArea))
          
        }
        
      }
  
  
  return(objList)
}



# Image Analysis  -------------------------------------------------
##Correlation of channels
corr.channels <- function(imgDF)
{
  blue <- c(readTIFF(imgDF$link[1]))
  green <- c(readTIFF(imgDF$link[2]))
  red <- c(readTIFF(imgDF$link[3]))
  yellow <- c(readTIFF(imgDF$link[4]))
  
  return(c(GlosB = cor(green, blue), GlosR = cor(green, red), GlosY = cor(green, yellow)))
}




# Object Analysis (defined by coordinates) ---------------------------------------------------


