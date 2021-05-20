defineModule(sim, list(
  name = "bootRasterCombine",
  description = "",
  keywords = "",
  authors = c(
    person("Alex M", "Chubaty", role = c("aut", "cre"), email = "achubaty@for-cast.ca"),
    person("Isolde", "Lane-Shaw", role = "aut", email = "") ## TODO: add Isolde's email
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.6.9018", bootRasterCombine = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "bootRasterCombine.Rmd")),
  reqdPkgs = list(
    "future.apply", "googledrive", "raster", "reproducible", "qs", "PredictiveEcology/LandR"
  ),
  parameters = rbind(
    defineParameter("cl", "cluster", NULL, NA, NA, "cluster object created using 'parallel:makeCluster()'."),
    defineParameter("csvUrl", "character", "https://drive.google.com/file/d/1ldESo9gb6icRD8ZsuPgaEDSIwFjJEe4W/",
                    NA, NA, "Link to Google Drive file specifying the nubmer of bootstrap reps per species/bcr."),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here."),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter("scratchDir", "character", NULL, NA, NA,
                    paste("Single path to a directory to use as scratch location for raster operations.",
                          "If 'NULL', the temporary R session directory (`tempdir()`) will be used.")),
    defineParameter("uploadURL", "character",
                    "https://drive.google.com/drive/folders/1fCTr2P-3Bh-7Qh4W0SMJ_mT9rpsKvGEA",
                    NA, NA, "Google Drive URL corresponding to a folder to which outputs will be uploaded."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter(".useFuture", "logical", TRUE, NA, NA,
                    paste("Should future be used for raster processing tasks?",
                          "If TRUE, uses future plan 'cluster' using the cluster `P(sim)$cl`.")),
    defineParameter(".verbose", "logical", TRUE, NA, NA, "Should additonal info messages be printed?")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "raster to match. default LCC2005.", sourceURL = NA),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area polygon", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "bootRasters", objectClass = "character",
                  desc = "vector of relative paths to the downloaded bootstrap raster files"),
    createsOutput(objectName = "bootstrapReplicates", objectClass = "data.table",
                  desc = "summary of the number of bootstrap replicates per BCR and bird species")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.bootRasterCombine = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "bootRasterCombine", "download", .highest())
      sim <- scheduleEvent(sim, start(sim), "bootRasterCombine", "meanvar", .normal())
      sim <- scheduleEvent(sim, start(sim), "bootRasterCombine", "mosaic", .normal())
      sim <- scheduleEvent(sim, end(sim), "bootRasterCombine", "upload", .lowest())
    },
    download = {
      sim <- doDownload(sim)
    },
    meanvar = {
      sim <- doMeanVar(sim)
    },
    mosaic = {
      sim <- doMosaic(sim)
    },
    upload = {
      sim <- doUpload(sim)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  ## there are >12k files, so stash a local copy of the drive_ls result, and use it when available
  bootPattern <- "BCR_.*-boot-.*[.]tif$"
  f <- file.path(mod$dPath, "gdrive_ls_cache.qs")
  mod$filesToDownload <- if (file.exists(f)) {
    filesToDownload <- qs::qload(f, env = environment())
    if (is(filesToDownload, "environment")) {
      filesToDownload <- as_dribble(filesToDownload$drive_resource)
    }
    filesToDownload
  } else {
    folderUrl <- "https://drive.google.com/drive/folders/1f9NvoSSdHav8FqnswwPYuV0TFLr19ny5"
    filesToDownload <- drive_ls(path = as_id(folderUrl), pattern = bootPattern)
    qs::qsave(filesToDownload, f)
    filesToDownload
  }

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

doDownload <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  rPath <- asPath(checkPath(file.path(mod$dPath, "rawBootRasters"), create = TRUE), 1)

  ## check available disk space (GB)
  availDiskSpace <- disk.usage(rPath)[[2]] %>% revText(.) %>% substring(., 2) %>% revText(.) %>% as.numeric()
  if (availDiskSpace < 335) {
    stop("Downloading all raw bootstrap raster files requires 335 GB of disk space, ",
         "but only ", availDiskSpace, " GB is available at location:\n", rPath)
  }

  ## 1. download boostrapped rasters
  filesToDownload <- mod$filesToDownload
  res <- apply(filesToDownload, 1, function(f) {
    fname <- file.path(rPath, f[["name"]])
    if (file.exists(fname) && file.size(fname) > 0) {
      TRUE
    } else {
      unlink(fname)
      tryCatch({
        retry(quote(drive_download(file = as_id(f[["id"]]), path = fname, overwrite = TRUE)),
                    retries = 5, exponentialDecayBase = 2)
        tryCatch({
          r <- raster::raster(fname)
          TRUE
        }, error = function(e) {
          unlink(fname, force = TRUE)
          FALSE
        })
      }, error = function(e) FALSE)
    }
  })
  names(res) <- filesToDownload[["name"]]

  if (any(isFALSE(res))) {
    scheduleEvent(sim, time(sim), "bootRasterCombine", "download", .highest())
  }

  bootRasters <- sort(file.path(rPath, filesToDownload[["name"]]))
  BCRs <- basename(bootRasters) %>%
    strsplit(., "_") %>%
    vapply(., `[[`, character(1), 2) %>%
    strsplit(., "-") %>%
    vapply(., `[[`, character(1), 1) %>%
    as.integer(.)
  birdSpp <- basename(bootRasters) %>%
    substr(., 9, 12)
  bootreps <- basename(bootRasters) %>%
    strsplit(., "-boot-") %>%
    vapply(., `[[`, character(1), 2) %>%
    strsplit(., "[.]") %>%
    vapply(., `[[`, character(1), 1) %>%
    as.integer(.)

  ## omit species that do not have >1 bootstrap replicate for all BCRs
  dl_dt <- data.table(bcr = BCRs, bird = birdSpp, rep = bootreps, file = bootRasters)
  mod$availableRasters <- copy(dl_dt)

  reps_dt <- copy(dl_dt)
  set(reps_dt, NULL, c("rep", "file"), NULL)
  reps_dt[, N := .N, by = c("bcr", "bird")]
  reps_dt <- unique(reps_dt)
  reps_dt <- reps_dt[N > 0, ]
  reps_dt[, allbcrs := lapply(.SD, function(x) all(unique(BCRs) %in% bcr)), by = "bird"]
  reps_dt <- reps_dt[allbcrs == TRUE, ]

  birdSppPruned <- unique(reps_dt[["bird"]])
  dl_dt <- dl_dt[bird %in% birdSppPruned, ]

  sim$bootRasters <- dl_dt[["file"]] ## pruned list

  mod$downloadedRaster <- copy(dl_dt)
  mod$BCRs <- sort(unique(BCRs))
  mod$birdSpp <- birdSppPruned
  mod$mPath <- checkPath(file.path(outputPath(sim), "meanRasters"), create = TRUE)
  mod$vPath <- checkPath(file.path(outputPath(sim), "varRasters"), create = TRUE)
  mod$scratchDir <- checkPath(ifelse(is.null(P(sim)$scratchDir), tempdir(), P(sim)$scratchDir), create = TRUE)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

doMeanVar <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  z <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = Inf) ## TODO: workaround bug in future_apply
  on.exit(options(future.globals.maxSize = z, add = TRUE))

  origPlan <- if (isFALSE(is.null(P(sim)$cl)) && isTRUE(P(sim)$.useFuture)) {
    plan("cluster", workers = P(sim)$cl)
  } else (
    plan("sequential")
  )
  on.exit(plan(origPlan), add = TRUE)
#browser()
  BCRs <- mod$BCRs
  birdSpp <- mod$birdSpp
  bootRasters <- sim$bootRasters
  mPath <- mod$mPath
  vPath <- mod$vPath
  scratchDir <- mod$scratchDir
  nBirds <- future_lapply(birdSpp, function(bird) {
    nBCRs <- lapply(BCRs, function(bcr) {
      #if (bird == "EUST" && bcr == 12) browser()
      bootPattern <- paste0(bird, "-BCR_", bcr, ".*[.]tif$")
      f <- grep(bootPattern, bootRasters, value = TRUE)
      message(paste(paste(bird, bcr, "..."), collapse = "\n"))

      if (length(f)) {
        raster::rasterOptions(default = TRUE)
        options(rasterTmpDir = scratchDir)

        ## 2. create mean and variance rasters for each BCR x birdSpp
        f_mPath <- file.path(mPath, paste0("mean_", bird, "_BCR_", bcr, ".tif"))
        f_vPath <- file.path(vPath, paste0("var_", bird, "_BCR_", bcr, ".tif"))

        if (isFALSE(all(file.exists(f_mPath, f_vPath)))) {
          stk <- raster::stack(f)

          meanRaster <- tryCatch(raster::calc(stk, mean), error = function(e) NA)

          if (is(meanRaster, "RasterLayer")) {
            writeRaster(meanRaster, f_mPath, overwrite = TRUE)

            varRaster <- tryCatch(raster::calc(stk, stats::var), error = function(e) NA)
            if (is(varRaster, "RasterLayer")) {
              writeRaster(varRaster, f_vPath, overwrite = TRUE)
            }
          }

          if (!is(meanRaster, "RasterLayer") || !is(varRaster, "RasterLayer")) {
            f <- character(0)
          }
        }
      }

      ## 2a. determine number of bootstrap samples per bird x BCR
      length(f)
    })
    names(nBCRs) <- nBCRs

    data.table(BCR = BCRs, BIRD = bird, N = nBCRs)
  }, future.globals = c("BCRs", "birdSpp", "bootRasters", "mPath", "vPath", "scratchDir"),
  future.packages = "raster", future.seed = TRUE)
  names(nBirds) <- as.character(mod$birdSpp)

  ## 2b. create data.table with (updated) number of bootstrap replicates
  ##     i.e., only those whose files could actually be loaded
  sim$bootstrapReplicates <- rbindlist(nBirds)
  fwrite(sim$bootstrapReplicates, file.path(outputPath(sim), "bootstrap_replicates.csv"))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

doMosaic <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  z <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = Inf) ## TODO: workaround bug in future_apply
  on.exit(options(future.globals.maxSize = z, add = TRUE))

  origPlan <- if (isFALSE(is.null(P(sim)$cl)) && isTRUE(P(sim)$.useFuture)) {
    plan("cluster", workers = P(sim)$cl)
  } else {
    plan("sequential")
  }
  on.exit(plan(origPlan), add = TRUE)

  birdSpp <- mod$birdSpp
  mPath <- mod$mPath
  vPath <- mod$vPath
  oPath <- outputPath(sim)
  res <- future_lapply(birdSpp, function(bird) {
    ## TODO: skip re-doing that already exist
    mRasters <- lapply(list.files(mPath, paste0("mean_", bird, "_BCR_.*[.]tif$"), full.names = TRUE), raster)
    mRasters$fun <- mean
    mRasters$na.rm <- TRUE
    mRasters$filename <- file.path(oPath, paste0("mosaic_mean_", bird, ".tif"))
    mMosaic <- do.call(raster::mosaic, mRasters)

    vRasters <- lapply(list.files(vPath, paste0("var_", bird, "_BCR_.*[.]tif$"), full.names = TRUE), raster)
    vRasters$fun <- mean ## TODO: does mean variance makes sense here?
    vRasters$na.rm <- TRUE
    vRasters$filename <- file.path(oPath, paste0("mosaic_var_", bird, ".tif"))
    vMosaic <- do.call(raster::mosaic, vRasters)

    NULL
  }, future.globals = c("birdSpp", "mPath", "oPath", "vPath"), future.packages = "raster", future.seed = TRUE)
  names(res) <- birdSpp

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

doUpload <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  uploadURL <- P(sim)$uploadURL
  verbose <- isTRUE(P(sim)$.verbose)

  ## mean and var rasters
  filesToUpload <- list.files(outputPath(sim), pattern = "mosaic_", full.names = TRUE)
  res <- lapply(filesToUpload, function(f) {
    ## TODO: only upload new files?? use drive_update() to update existing ones
    retry(quote(drive_upload(media = f, path = uploadURL, name = basename(f), verbose = verbose, overwrite = TRUE)),
          retries = 5, exponentialDecayBase = 2)
  })
  names(res) <- basename(filesToUpload)

  if (!all(vapply(res, function(x) is(x, "dribble"), logical(1)))) {
    # TODO: don't reupload them all; figure out which ones failed and only do those (see dl code above)
    scheduleEvent(sim, start(sim), "bootRasterCombine", "upload")
  }

  ## csv of bootstrap replicates
  csvFile <- file.path(outputPath(sim), "bootstrap_replicates.csv")
  drive_update(media = csvFile, file = P(sim)$csvURL, name = "noBootsDF.csv", verbose = verbose)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  mod$dPath <- asPath(checkPath(file.path(getOption("reproducible.destinationPath", dataPath(sim))),
                                create = TRUE), 1)
  message(currentModule(sim), ": using dataPath '", mod$dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  if (!suppliedElsewhere("studyArea")) {
    bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"
    bcrshp <- Cache(prepInputs,
                    url = bcrzip,
                    destinationPath = mod$dPath,
                    targetCRS = mod$targetCRS,
                    fun = "sf::st_read")

    sim$studyArea <- bcrshp[bcrshp$BCR == 11, ] ## 10 and 11 are relatively small
  }

  if (!suppliedElsewhere("rasterToMatch")) {
    sim$rasterToMatch <- LandR::prepInputsLCC(year = 2005, destinationPath = mod$dPath,
                                              studyArea = sim$studyArea, filename2 = NULL)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
