labelsTrain <- read.csv(file = "raw/train.csv", stringsAsFactors = FALSE)



labelsList <- strsplit(x = labelsTrain$Target, split = " ")
labelsList <- lapply(labelsList, as.integer)
names(labelsList) <- labelsTrain$Id




for (i in 0:27)
  labelsTrain[i + 3] <- sapply(X = labelsList, FUN = function(x) {i %in% x})


labelStructure <- read.delim(file = "raw/Label_Structure.txt", stringsAsFactors = FALSE)

colnames(labelsTrain)[3:30] <- labelStructure$Structure
row.names(labelsTrain) <- labelsTrain$Id

write.table(x = labelsTrain, file = "rdata/flattened_labels.txt", row.names = FALSE, sep = "\t")


labelsMat <- t(as.matrix(labelsTrain[,3:30]))

coexistMat <- matrix(nrow = 28, ncol = 28)

for (i in 1:28)
{
  for (j in 1:28)
    coexistMat[i,j] <- sum(labelsMat[i,] & labelsMat[j,])/sum(labelsMat[i,])
}

row.names(coexistMat) <- paste("When", labelStructure$Structure)
colnames(coexistMat) <- paste("Also", labelStructure$Structure)

write.table(x = coexistMat, file = "rdata/coexistMap.txt", sep = "\t")

