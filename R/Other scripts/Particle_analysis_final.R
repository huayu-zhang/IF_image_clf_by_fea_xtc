library(tiff)


testBin <- readTIFF(source = "testBLUE.tif")


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



particleAnalysis <- function(Bin, minSize = 0, maxSize = 512)
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
        
        objList <- c(objList, list(objArea))
      }
  
  objList <- objList[sapply(X = objList, FUN = function (x) nrow(x) > minSize)]
  
  return(objList)
}


ptm <- proc.time()

testObj <- particleAnalysis(Bin = testBin)

proc.time() - ptm

