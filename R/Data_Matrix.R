

fileListSum <- data.frame(do.call(rbind, 
                       strsplit( list.files(path = "E:/trainSum/"), split = "\\_|\\.")
                  )[,1:2], stringsAsFactors = FALSE)
colnames(fileListSum)[1:2] <- c("index", "sample")


fileListSum$links <- list.files(path = "E:/trainSum/", full.names = T)

fileListSum$index <- as.numeric(fileListSum$index)

fileListSum <- fileListSum[order(fileListSum$index), ]



# Naive method to import------------------------------------------------------------

DTList <- list()

for (i in 0:30)
{

index = i * 1000 + 1

load(fileListSum$links[index])



DTList[[i+1]] <- data.frame(t(as.matrix(c( index = as.numeric(strsplit(ls(pattern = "var"), "\\_")[[1]][2]), 
                                   get(ls(pattern = "var"))))), stringsAsFactors = F)

rm(list = ls(pattern = "var"))

lowRange <- i*1000 + 2
highRange <- min((i+1)*1000, 30262)

for (index in lowRange:highRange )
{
  load(fileListSum$links[index])
  
  DTList[[i+1]] <- rbind(DTList[[i+1]], c( index = as.numeric(strsplit(ls(pattern = "var"), "\\_")[[1]][2]), 
                           get(ls(pattern = "var"))))
  
  rm(list = ls(pattern = "var"))
  
}

print(i)
}



DTall <- do.call(rbind, DTList)


save(DTall, file = "rdata/DTall.rdata")

write.table(x = DTall, file = "rdata/DTall.txt", row.names = F, sep = "\t")


labelsTrain <- read.delim(file = "rdata/flattened_labels.txt", stringsAsFactors = F)

labelsTrain <- merge(x = fileListSum[1:2], y = labelsTrain, by.x = "sample", by.y = "Id", all.x = TRUE)

save(labelsTrain, file = "rdata/labelsTrain.rdata")

write.table(x = labelsTrain, file = "labelsTrain.txt", row.names = F, sep = "\t")




# Test data matrix --------------------------------------------------------


fileListSumTest <- data.frame(do.call(rbind, 
                                  strsplit( list.files(path = "E:/testSum/"), split = "\\_|\\.")
)[,1:2], stringsAsFactors = FALSE)
colnames(fileListSumTest)[1:2] <- c("index", "sample")


fileListSumTest$links <- list.files(path = "E:/testSum/", full.names = T)

fileListSumTest$index <- as.numeric(fileListSumTest$index)

fileListSumTest <- fileListSumTest[order(fileListSumTest$index), ]





DTListTest <- list()

for (i in 0:11)
{
  
  index = i * 1000 + 1
  
  load(fileListSumTest$links[index])
  
  
  
  DTListTest[[i+1]] <- data.frame(t(as.matrix(c( index = as.numeric(strsplit(ls(pattern = "var"), "\\_")[[1]][2]), 
                                             get(ls(pattern = "var"))))), stringsAsFactors = F)
  
  rm(list = ls(pattern = "var"))
  
  lowRange <- i*1000 + 2
  highRange <- min((i+1)*1000, 11667)
  
  for (index in lowRange:highRange )
  {
    load(fileListSumTest$links[index])
    
    DTListTest[[i+1]] <- rbind(DTListTest[[i+1]], c( index = as.numeric(strsplit(ls(pattern = "var"), "\\_")[[1]][2]), 
                                             get(ls(pattern = "var"))))
    
    rm(list = ls(pattern = "var"))
    
  }
  
  print(i)
}

DTallTest <- do.call(rbind, DTListTest)


save(DTallTest, file = "rdata/DTallTest.rdata")

write.table(x = DTallTest, file = "rdata/DTallTest.txt", row.names = F, sep = "\t")



save(fileListSumTest, file = "rdata/fileListSumTest.rdata")

write.table(x = fileListSumTest, file = "rdata/fileListSumTest.txt", row.names = F, sep = "\t")
