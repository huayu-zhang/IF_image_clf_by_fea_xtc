source("R/Feature_extraction_environment.R")


library(parallel)


cl <- makeCluster(detectCores())


clusterEvalQ(cl = cl, library(tiff))
clusterEvalQ(cl = cl, library(png))
clusterEvalQ(cl = cl, library(EBImage))
clusterEvalQ(cl = cl, library(raster))
clusterExport(cl = cl, "binChannel")
clusterExport(cl = cl, "coloc")
clusterExport(cl = cl, "create.filelist")
clusterExport(cl = cl, "featurePipe.Test")
clusterExport(cl = cl, "huayu.cutoff")
clusterExport(cl = cl, "img.Import")
clusterExport(cl = cl, "img.Import_512")
clusterExport(cl = cl, "nbEight")
clusterExport(cl = cl, "rimOfNuclei")
clusterExport(cl = cl, "files")
clusterExport(cl = cl, "Nb8")


parLapply(cl = cl, X = 1:11702, fun = featurePipe.Test)


stopCluster(cl = cl)
