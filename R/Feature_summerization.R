source("R/Feature_summerization_environment.R")



library(parallel)





cl <- makeCluster(detectCores())


clusterEvalQ(cl = cl, library(EBImage))
clusterEvalQ(cl = cl, library(raster))
clusterEvalQ(cl = cl, library(SearchTrees))


clusterExport(cl = cl, "fileListOutput")
clusterExport(cl = cl, "dist.fromNuc")
clusterExport(cl = cl, "featureSumPipe")
clusterExport(cl = cl, "featureSumPipe.Test")
clusterExport(cl = cl, "find.duplicate")
clusterExport(cl = cl, "find.largeGreen")
clusterExport(cl = cl, "G.angle")
clusterExport(cl = cl, "G.between.Dup")
clusterExport(cl = cl, "ratio.ProxiTheta")


parLapply(cl = cl, X = 1:11702, fun = featureSumPipe.Test)


stopCluster(cl = cl)


doneTest <- as.numeric(do.call(rbind, strsplit(list.files(path = "E:/testSum/"), split = "\\_"))[,1])

yetTobeTest <- 1:11702

yetTobeTest <- yetTobeTest[!(yetTobeTest %in% doneTest)]

save(yetTobeTest, file = "rdata/yetTobeTest.rdata")

lapply(X = yetTobe, FUN = featureSumPipe)



