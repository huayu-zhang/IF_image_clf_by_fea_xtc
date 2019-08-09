source("R/Feature_extraction_environment.R")


featurePipe <- function(index)
{
  
  
  # Input -------------------------------------------------------------------
  
  img <- img.Import(index = index, fileList = files$trainFiles)
  img_512 <- img.Import_512(index = index, fileList = files$trainFiles_512)
  
  sampleName <- strsplit(x = files$trainFiles$RGB[index], split = "\\.|\\_|\\/")[[1]][4]
  wh <- dim(img$R)[1]
  
  
  
  
  # Object mask creation ------------------------------------------------------------
  
  
  
  ##Cutoff for each channel: to normalize the higher or lower signal (emperical way to improve efficiency)
  ##For each channel, get the value med <- medium(non-zero intensity)
  ##Blue channel: med/5
  ##Red channel: med/5
  ##Yellow channel: med/5
  ##Green channel is more complicated and will be discussed later. med/5 used for now
  
  img_cutoffs <- sapply(img_512, binChannel)
  
  
  
  
  ##BLUE channel binarization
  img_bin_512 <- list(
    "R" = img_512$R > img_cutoffs["R"],
    "G" = img_512$G > img_cutoffs["G"],
    "B" = img_512$B > img_cutoffs["B"],
    "Y" = img_512$Y > img_cutoffs["Y"]
  )
  
  img_masks_512 <- list(
    "B" = fillHull(img_bin_512$B),
    "RY" = img_bin_512$R | img_bin_512$Y
  )
  
  
  
  
  ##Watersheding for Blue and noise cleaning
  img_masks_512$B <- watershed(x = distmap(img_masks_512$B), tolerance = 1, ext = 3)
  img_table_512 <- list(
    "B" = table(img_masks_512$B)
  )
  img_masks_512$B <- rmObjects(x = img_masks_512$B, index = names(img_table_512$B)[img_table_512$B < 100])
  img_table_512$B <- table(img_masks_512$B)
  img_table_512$Bminus <- table(img_masks_512$B[2:511, 2:511])
  
  img_masks_512$Bminus <- rmObjects(x = img_masks_512$B, index = names(img_table_512$B)[img_table_512$B > img_table_512$Bminus])
  
  #display(colorLabels(x = img_masks_512$B))
  #display(colorLabels(x = img_masks_512$Bminus))
  
  
  
  ##Segregation and Noise cleaning for RY
  img_masks_512$RYinv <- bwlabel(1 - img_masks_512$RY)
  img_table_512$RYinv <- table(img_masks_512$RYinv)
  img_masks_512$RYinv <- rmObjects(x = img_masks_512$RYinv, index = names(img_table_512$RYinv)[img_table_512$RYinv > 20])
  img_masks_512$RY[img_masks_512$RYinv > 0] <- 1
  img_masks_512$RYinv <- NULL
  
  img_masks_512$RY <- bwlabel(img_masks_512$RY)
  img_table_512$RY <- table(img_masks_512$RY)
  img_masks_512$RY <- rmObjects(x = img_masks_512$RY, index = names(img_table_512$RY)[img_table_512$RY < 100])
  img_masks_512$RY[img_masks_512$B > 0] <- 0 
  
  #img_table_512$RY <- table(img_masks_512$RY)
  #display(img_masks_512$RY)
  #display(x = img_masks_512$RY > 0)
  #display(colorLabels(img_masks_512$RYinv))
  
  ##Creat a mask for nuclear rim
  img_masks_512$rimB <- rimOfNuclei(nucMask = img_masks_512$Bminus)
  #display(x = img_masks_512$rimB)
  
  
  ##Upscale the masks to fUll resolution
  img_masks <- lapply(img_masks_512, function(x, fact) as.matrix(disaggregate(x = raster(x), fact = fact)), fact = wh/512)
  
  ##Use the masks to dissect subcellular regions of Green signal
  img_Green <- list(
    "Nucleus" = img$G,
    "Cytosol" = img$G,
    "Rest" = img$G
  )
  
  
  ##Disect the green signal according to the masks
  img_Green$Nucleus[img_masks$B == 0] <- 0
  img_Green$Cytosol[img_masks$RY == 0] <- 0
  img_Green$Rest[img_masks$B > 0| img_masks$RY > 0 ] <- 0
  
  #display(img_Green$Nucleus)
  #display(img_Green$Cytosol)
  #display(img_Green$Rest)
  
  
  # Green HDR library -------------------------------------------------------
  
  
  ##Make HDR object masks for Green signals
  ##Emprical way to deside HDR cutoffs for green signal
  ##Method 1: Otsu method which is more loose
  ##Right most fluctuation of 2nd derivative/Right most non-zero 2nd derivative, which is more stringent
  ##For Nuclues and Cytosol use both methods
  ##For Rest use Otsu only
  ##Also create the library for who image with Otsu and Huayu Cutoffs
  ##Libraries with too many labels is a huge computation burden, sampling will be done
  
  img_cutoffs <- c(img_cutoffs, 
                   G.Otsu = sapply(img_Green, otsu),
                   G.Huayu = sapply(img_Green[1:2], huayu.cutoff),
                   G.Otsu = otsu(img_512$G), 
                   G.Huayu = huayu.cutoff(img_512$G),
                   "R.Otsu" = otsu(img_512$R),
                   "B.Otsu" = otsu(img_512$B),
                   "Y.Otsu" = otsu(img_512$Y)
  )
  
  
  img_masks_G.Otsu <- list(
    "Nucleus" = bwlabel(x = img_Green$Nucleus > img_cutoffs["G.Otsu.Nucleus"]),
    "Cytosol" = bwlabel(x = img_Green$Cytosol > img_cutoffs["G.Otsu.Cytosol"]),
    "Rest" = bwlabel(x = img_Green$Rest > img_cutoffs["G.Otsu.Rest"])
    
  )
  
  img_table_G.Otsu <- lapply(X = img_masks_G.Otsu, FUN = table)
  
  img_masks_G.Otsu$Nucleus <- rmObjects(x = img_masks_G.Otsu$Nucleus, index = names(img_table_G.Otsu$Nucleus)[img_table_G.Otsu$Nucleus < 10])
  if (sum(img_table_G.Otsu$Nucleus >= 10) > 200)
    img_masks_G.Otsu$Nucleus <- rmObjects(x = img_masks_G.Otsu$Nucleus, index = names(img_table_G.Otsu$Nucleus)[sample(1:sum(img_table_G.Otsu$Nucleus >= 10),sum(img_table_G.Otsu$Nucleus >= 10) - 200)])
  
  
  img_masks_G.Otsu$Cytosol <- rmObjects(x = img_masks_G.Otsu$Cytosol, index = names(img_table_G.Otsu$Cytosol)[img_table_G.Otsu$Cytosol < 10])
  if (sum(img_table_G.Otsu$Cytosol >=10) > 200)
    img_masks_G.Otsu$Cytosol <- rmObjects(x = img_masks_G.Otsu$Cytosol, index = names(img_table_G.Otsu$Cytosol)[sample(1:sum(img_table_G.Otsu$Cytosol >= 10),sum(img_table_G.Otsu$Cytosol >= 10) - 200)])
  
  img_masks_G.Otsu$Rest <- rmObjects(x = img_masks_G.Otsu$Rest, index = names(img_table_G.Otsu$Rest)[img_table_G.Otsu$Rest < 10])
  if (sum(img_table_G.Otsu$Rest >=10) > 200)
    img_masks_G.Otsu$Rest <- rmObjects(x = img_masks_G.Otsu$Rest, index = names(img_table_G.Otsu$Rest)[sample(1:sum(img_table_G.Otsu$Rest >= 10),sum(img_table_G.Otsu$Rest >= 10) - 200)])
  
  #Updating table is not necessary but very time consuming
  #img_table_G.Otsu <- lapply(X = img_masks_G.Otsu, FUN = table)
  
  #display(colorLabels(img_masks_G.Otsu$Cytosol))
  
  
  
  img_masks_G.Huayu <- list(
    "Nucleus" = bwlabel(x = img_Green$Nucleus > img_cutoffs["G.Huayu.Nucleus"]),
    "Cytosol" = bwlabel(x = img_Green$Cytosol > img_cutoffs["G.Huayu.Cytosol"])
  )
  
  img_table_G.Huayu <- lapply(X = img_masks_G.Huayu, FUN = table)
  
  
  if (sum(img_table_G.Huayu$Nucleus >= 10) > 100)
    img_masks_G.Huayu$Nucleus <- rmObjects(x = img_masks_G.Huayu$Nucleus, index = names(img_table_G.Huayu$Nucleus)[order(img_table_G.Huayu$Nucleus, decreasing = T)[51:length(img_table_G.Huayu$Nucleus)]])
  
  
  if (sum(img_table_G.Huayu$Cytosol >=10) > 100)
    img_masks_G.Huayu$Cytosol <- rmObjects(x = img_masks_G.Huayu$Cytosol, index = names(img_table_G.Huayu$Cytosol)[order(img_table_G.Huayu$Cytosol, decreasing = T)[51:length(img_table_G.Huayu$Cytosol)]])
  
  
  #display(colorLabels(img_masks_G.Huayu$Nucleus))
  #display(colorLabels(img_masks_G.Huayu$Cytosol))
  
  
  #Updating table is not necessary but very time consuming
  #img_table_G.Huayu <- lapply(X = img_masks_G.Huayu, FUN = table)
  
  
  #Whole image HDR (Deleted part)
  
  #img_masks$G.Otsu <- bwlabel(img$G > img_cutoffs["G.Otsu"])
  #img_table_512$G.Otsu <- table(img_masks$G.Otsu)
  #img_masks$G.Otsu <- rmObjects(img_masks$G.Otsu, index = names(img_table_512$G.Otsu)[img_table_512$G.Otsu < 10])
  #img_table_512$G.Otsu <- table(img_masks$G.Otsu)
  
  #img_masks$G.Huayu <- bwlabel(img$G > img_cutoffs["G.Huayu"])
  #img_table_512$G.Huayu <- table(img_masks$G.Huayu)
  #img_masks$G.Huayu <- rmObjects(img_masks$G.Huayu, index = names(img_table_512$G.Huayu)[img_table_512$G.Huayu <  10])
  #img_table_512$G.Huayu <- table(img_masks$G.Huayu)
  
  #display(img_masks$G.Otsu > 0)
  #display(img_masks$G.Huayu > 0)
  
  # Computing features ------------------------------------------------------
  
  ##List of meaningful features
  ##Manual features:
  ##A list of colocalizations
  
  manualFeatures <- c(
    "GcolocB" = coloc(bin1 = img$G > img_cutoffs["G.Otsu"], bin2 = img$B > img_cutoffs["B.Otsu"]),
    "GcolocR" = coloc(bin1 = img$G > img_cutoffs["G.Otsu"], bin2 = img$R > img_cutoffs["R.Otsu"]),
    "GcolocY" = coloc(bin1 = img$G > img_cutoffs["G.Otsu"], bin2 = img$Y > img_cutoffs["Y.Otsu"]),
    "GcolocRimB" = coloc(bin1 = img$G > img_cutoffs["G.Otsu"], bin2 = img_masks$rimB),
    img_cutoffs
  )
  
  
  ##Automatic features:
  
  featureList <- list(
    "manualFeatures" = manualFeatures,
    "M.Blue_Its.Blue" = computeFeatures(x = img_masks$B, ref = img$B), ##Mask: Blue, Ref: Blue, Information of nucleus properties
    "M.Blue_Its.Green" = computeFeatures(x = img_masks$B, ref = img$G), ##Mask: Blue, Ref: Green, Gain: Average signal properties per nucleus
    "M.G.Nuc.Otsu_Its.Green" = computeFeatures(x = img_masks_G.Otsu$Nucleus, ref = img$G), ##Mask: HDR Green in Nucleus, Ref: Green, Gain: Green signal properties in Nucleus
    "M.G.Nuc.Huayu_Its.Green" = computeFeatures(x = img_masks_G.Huayu$Nucleus, ref = img$G), ##See above
    "M.G.Cyt.Otsu_Its.Green" = computeFeatures(x = img_masks_G.Otsu$Cytosol, ref = img$G),##Mask: HDR Green in Cytosol, Ref: Green, Gain: Green signal properties in Cytosol
    "M.G.Cyt.Huayu_Its.Green" = computeFeatures(x = img_masks_G.Huayu$Cytosol, ref = img$G), ##See above
    "M.G.Rest.Otsu_Its.Green" = computeFeatures(x = img_masks_G.Otsu$Rest, ref = img$G),##Mask: Green in Rest, Ref: Green, Gain: Green signal properties in Rest
    "M.G.Cyt.Otsu_Its.Red" = computeFeatures(x = img_masks_G.Otsu$Cytosol, ref = img$R),##Mask: HDR Green in Cytosol, Ref: Red, Gain: Relation of green signal and red signal
    "M.G.Cyt.Huayu_Its.Red" = computeFeatures(x = img_masks_G.Huayu$Cytosol, ref = img$R),##See above
    "M.G.Cyt.Otsu_Its.Yellow" = computeFeatures(x = img_masks_G.Otsu$Cytosol, ref = img$Y),##Mask: HDR Green in Cytosol, Ref: Yellow, Gain: Relation of green signal and yellow signal
    "M.G.Cyt.Huayu_Its.Yellow" = computeFeatures(x = img_masks_G.Huayu$Cytosol, ref = img$Y)##See above
  )  
  
  save(list = c("img_bin_512", "img_masks_512", "img_masks_G.Huayu", "img_masks_G.Otsu", "featureList"), file = paste("E:/trainOutput/", sampleName, ".rdata", sep = ""))
  print(index)
}
#Example
#featurePipe(index = 1)

