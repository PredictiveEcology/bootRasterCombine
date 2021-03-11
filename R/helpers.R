## these are Isolde's original functions, to be refactored to work more efficiently

createNoBootDF <- function(birdSp, BCR, downloadedRasters) {
  ## check there is a file to save created rasters and bootstrap numbers info to
  fileCombinedRasters <- checkPath("meanVarRasters", create = TRUE)
  ## get number of bootstrap replicates
  noBootsBCR <- length(downloadedRasters)

  csvFile <- file.path(fileCombinedRasters, "noBootsDF.csv")
  if (!file.exists(csvFile)) {
    noBootsDF <- tibble(Bird = birdSp, noBCR = BCR, noBoots = noBootsBCR)
    write.csv(noBootsDF, csvFile)
  } else {
    noBootsDF <- read.csv(csvFile, header = TRUE)
    noBootsDF <- noBootsDF %>%
      dplyr::select(Bird, noBCR, noBoots) %>%
      add_row(Bird = birdSp, noBCR = as.integer(BCR), noBoots = noBootsBCR)

    write.csv(noBootsDF, csvFile)
  }

  ## confirm that a row has been added to the DF
  print(paste0("BCR", BCR, " row added to noBootsDF"))
}

downloadBootRasters <- function(folderUrl, birdSp, BCR, rastersPath) {
  # check that there is a folder at the given rastersPath. if not, create it.
  rastersPath <- checkPath(rastersPath, create = TRUE)

  # drive_ls function is used to list all the files it finds using the folder url with the given pattern
  filesToDownload <- googledrive::drive_ls(path = as_id(folderUrl), pattern = paste0("BCR_", BCR, "-boot-")) #note: has to be changed if filenaming system changes
  ## TODO: why are you listing everything? There are >12k files this takes FOREVER. Create e.g., a zip of everything or batches and download data ONCE, or at least save/cache the file list.

  # grepl function searches for all items in the filesToDownload that are on the birdSp & stores their names in rastersforBirdList
  rastersList <- filesToDownload$name[grepl(pattern = paste(birdSp, collapse = "|"), x = filesToDownload$name)]

  ## TODO: use e.g., reproducible::prepInputs to get the files and check to see if already downloaded, deal with checksuming, etc.

  # for each item in turn from rastersList the following function is applied:
  downloadedRasters <- lapply(X = rastersList, FUN = function(rasterFile) {
    # if the item in rastersList is not already present at rastersPath, googledrive package downloads it
    if (!file.exists(file.path(rastersPath, rasterFile))) {
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% rasterFile, ]$id), #rasterFile,
                                  path = file.path(rastersPath, rasterFile),
                                  overwrite = TRUE)
    }
    ## otherwise, if it is already present and downloaded, just get the name of the item
    return(raster(file.path(rastersPath, rasterFile), verbose = TRUE))
  })

  ## get the species codes and BCR as names for the downloadedRasters object, rather than using the whole filepath
  X <- lapply(rastersList, substr, 9, 19) #works for strings of the form "pred250-XXXX_BCR_XX"
  # X <- lapply(X,
  #if substr(X, 19, nchar(teststring)-1)

  ## name the rasters
  names(downloadedRasters) <- X

  createNoBootDF(birdSp = birdSp, BCR = BCR, downloadedRasters = downloadedRasters)

  return(downloadedRasters)
}

createMeanVarRasters <- function(birdSp, BCRList, folderUrl, folderRawBoot, uploadFolder) {
  ## check there is a file to save created rasters and bootstrap numbers info to
  fileCombinedRasters <- checkPath("meanVarRasters", create = TRUE)

  BCRRasters <- lapply(BCRList, FUN = function(BCR) {
    downloadedRasters <- downloadBootRasters(folderUrl = folderUrl,
                                             birdSp = birdSp,
                                             BCR = BCR,
                                             rastersPath = folderRawBoot)
    downloadedRasterStack <- raster::stack(downloadedRasters)
    # noBootRasters <- length(downloadedRasterStack)

    ## calculate mean raster
    meanRaster <- raster::calc(downloadedRasterStack, mean)

    ## calculate variance raster
    varRaster <- raster::calc(downloadedRasterStack, stats::var)

    ## combine mean and var rasters in raster stack
    meanVarRasterStack <- raster::stack(meanRaster, varRaster)

    print(paste0("BCR", BCR, " mean and variance rasters calculated"))

    #### BEWARE!!!! ####
    #file.remove(file.path(folderRawBoot))

    return(meanVarRasterStack)
  })

  ## mosaic the rasters of different BCRs together
  # x <- as.list(meanVarRasters)
  # names(x)[1:2] <- c('x', 'y')
  BCRRasters$fun <- mean
  BCRRasters$na.rm <- TRUE
  mosaicedRasters <- do.call(mosaic, BCRRasters)

  ## name rasters
  names(mosaicedRasters)[1:2] <- c("mean", "var")

  ## save mean raster
  meanRaster <- file.path(fileCombinedRasters, paste0("mean_", birdSp, ".tif"))
  raster::writeRaster(mosaicedRasters$mean, filename = meanRaster, format = "GTiff", overwrite = TRUE)

  ## upload mean raster to googledrive folder
  drive_put(media = meanRaster,
            path = uploadFolder,
            name = basename(meanRaster),
            verbose = TRUE)

  ## save variance raster
  varRaster <- file.path(fileCombinedRasters, paste0("var_", birdSp, ".tif"))
  raster::writeRaster(mosaicedRasters$var, filename = varRaster, format = "GTiff", overwrite = TRUE)

  ## upload var raster to googledrive folder
  drive_put(media = varRaster,
            path = uploadFolder,
            name = basename(varRaster),
            verbose = TRUE)

  ## upload noBootsDF to googledrive folder
  drive_put(media = file.path("meanVarRasters", "noBootsDF.csv"),
            path = uploadFolder,
            name = "noBootsDF.csv",
            verbose = TRUE)

  print(paste0(birdSp, " mosaic of mean and variance rasters completed, saved and uploaded"))

  #return end product of function
  return(mosaicedRasters)
}
