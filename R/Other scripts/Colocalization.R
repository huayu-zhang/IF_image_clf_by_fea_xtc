Library("tiff")
Library("parallel")

cl <- makeCluster(8)

clusterEvalQ(cl, library(tiff))

colocList <- parLapply(cl = cl, X = fileTrainList, fun = colocChannels)

stopCluster(cl)

save.image()
save(colocList, file = "rdata/colocList.rdata")
write.table(x = colocList, file = "rdata/colocList.txt", sep = "\t")

#system(command = "shutdown -s -t 60 -y")
