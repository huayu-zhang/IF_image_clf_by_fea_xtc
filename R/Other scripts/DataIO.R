source("BasicFunctions.R")
Library("png")
Library("tiff")

#File information

fileTrain <- data.frame(list.files("F:/train_full_size"), stringsAsFactors = FALSE)

fileTrain <- cbind(fileTrain, data.frame(do.call(rbind, strsplit(x = fileTrain[,1], split = "_|\\.")), stringsAsFactors = FALSE)[1:2])

fileTrain$link <- list.files("F:/train_full_size", full.names = T)

colnames(fileTrain) <- c("file", "sample", "channel", "link")

write.table(x = fileTrain, file = "rdata/fileTrain.txt", sep = "\t", row.names = FALSE)



fileTest <- data.frame(list.files("F:/test_full_size"), stringsAsFactors = FALSE)

fileTest <- cbind(fileTest, data.frame(do.call(rbind, strsplit(x = fileTest[,1], split = "_|\\.")), stringsAsFactors = FALSE)[1:2])

fileTest$link <- list.files("F:/test_full_size", full.names = T)

colnames(fileTest) <- c("file", "sample", "channel", "link")

write.table(x = fileTest, file = "rdata/fileTest.txt", sep = "\t", row.names = FALSE)


#Make a list for file names, every element contains 4 channels

fileTestList <- list()

for (i in 0:11701)
  fileTestList[[fileTest$sample[4*i+1]]] <- fileTest[4*i + 1:4,]

fileTrainList <- list()

for (i in 0:31071)
  fileTrainList[[fileTrain$sample[4*i+1]]] <- fileTrain[4*i + 1:4, ]

save(list = c("fileTestList", "fileTrainList"), file = "rdata/fileLists.rdata")


#Input image, output merged TIFF. Parallel processing
#File compression and deletion

#Library("png")
Library("tiff")
Library("parallel")

cl <- makeCluster(8)

clusterEvalQ(cl, library(tiff))

parLapply(cl = cl, X = fileTrainList, fun = mergeANDremove, folder = "F:/trainMerge")
parLapply(cl = cl, X = fileTestList, fun = mergeANDremove, folder = "F:/testMerge")

stopCluster(cl)


#Confirmed correct names and links in fileTrain
#j <- 0
#for (i in 1:nrow(fileTest))
#  if (!grepl(pattern = fileTest$sample[i], x = fileTest$file[i]))
#  {
#    j <- j+1
#    print(i)
#  }
#rm(list = c("i", "j"))

