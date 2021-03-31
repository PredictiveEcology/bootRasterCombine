---
title: "bootRasterCombine"
author: "Alex M. Chubaty and Isolde Lane-Shaw"
date: "15 March 2021"
output: 
  html_document: 
    keep_md: yes
editor_options:
  chunk_output_type: console
---




```r
library("Require")
Require("PredictiveEcology/reproducible@development")
```

```
## Loading required package: reproducible
```

```r
Require("PredictiveEcology/SpaDES.core@development")
```

```
## Loading required package: SpaDES.core
```

```
## Loading required package: quickPlot
```

```
## 
## Attaching package: 'SpaDES.core'
```

```
## The following objects are masked from 'package:stats':
## 
##     end, start
```

```
## The following object is masked from 'package:utils':
## 
##     citation
```

```r
Require("PredictiveEcology/LandR@development", require = FALSE)
```

```
## PredictiveEcology/reproducible@development 
##                                       TRUE 
## PredictiveEcology/SpaDES.core@development 
##                                      TRUE
```

# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

# Usage


```r
scratchDir <- checkPath("~/scratch/bootRasterCombine", create = TRUE)

raster::rasterOptions(default = TRUE)
options(
  rasterMaxMemory = 5e+12,
  rasterTmpDir = scratchDir,
  reproducible.cacheSaveFormat = "qs"
)


setPaths(
  cachePath = checkPath("cache"),
  modulePath = file.path(".."),
  inputPath = "data",
  outputPath = "outputs"
)

times <- list(start = 0, end = 1)

cl <- parallel::makeCluster(parallel::detectCores() / 2)
parameters <- list(
  bootRasterCombine = list(
    cl = cl,
    scratchDir = scratchDir,
    .useCache = FALSE, ## TODO: use cache later
    .useFuture = TRUE,
    .verbose = TRUE
  )
)
modules <- list("bootRasterCombine")
objects <- list()
inputs <- list()
outputs <- list()

mySimOut <- simInitAndSpades(times = times, params = parameters, modules = modules, objects = objects)

parallel::stopCluster(cl)
```

# Parameters

Provide a summary of user-visible parameters.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|paramName        |paramClass |default      |min |max |paramDesc                                                                                                                                        |
|:----------------|:----------|:------------|:---|:---|:------------------------------------------------------------------------------------------------------------------------------------------------|
|cl               |cluster    |             |NA  |NA  |cluster object created using 'parallel:makeCluster()'.                                                                                           |
|.plots           |character  |screen       |NA  |NA  |Used by Plots function, which can be optionally used here.                                                                                       |
|.plotInitialTime |numeric    |start(sim)   |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                        |
|.plotInterval    |numeric    |NA           |NA  |NA  |Describes the simulation time interval between plot events.                                                                                      |
|.saveInitialTime |numeric    |NA           |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                        |
|.saveInterval    |numeric    |NA           |NA  |NA  |This describes the simulation time interval between save events.                                                                                 |
|scratchDir       |character  |/tmp/Rtm.... |NA  |NA  |Single path to a directory to use as scratch location for raster operations.                                                                     |
|.useCache        |logical    |FALSE        |NA  |NA  |Should caching of events or module be activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |
|.useFuture       |logical    |TRUE         |NA  |NA  |Should future be used for download/upload and GIS tasks? If TRUE, uses future plan 'multicore'.                                                  |
|.verbose         |logical    |TRUE         |NA  |NA  |Should additonal info messages be printed?                                                                                                       |

# Events

Describe what happens for each event type.

## Init

Fetches the complete list of raw bootstrap raster files from Google Drive, saving a copy to disk.
Subsequent runs on the same machine will use this local stashed copy, so no additional downloads are needed.
If the Google Drive folder contents are updated, simply delete the `gdrive_ls_cache.qs` file in the module's data directory, and rerun the module to fetch the updated list.

## Download

Downloads all the raw bootstrap raster files from the Google Drive location.
*WARNING: total uncompressed size of all these files is ~330 GB.*

## GIS

All rasters cropped, reprojected, masked using `rasterToMatch` and `studyArea`.
*WARNING: this also requires substantial disk space, based on size of study area.*

## Calculate mean and variance

Calculated per species across all BCRs.

## Mosaic

Mosaic all mean/var rasters across BCRs per bird species into a single `rasterLayer`.

## Upload

Upload the resulting mosaicked rasters to Google Drive.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("bootRasterCombine", "..")` may be sufficient.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName    |objectClass              |desc                              |sourceURL |
|:-------------|:------------------------|:---------------------------------|:---------|
|rasterToMatch |RasterLayer              |raster to match. default LCC2005. |NA        |
|studyArea     |SpatialPolygonsDataFrame |study area polygon                |NA        |

## Output data

Description of the module outputs.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName          |objectClass |desc                                                                   |
|:-------------------|:-----------|:----------------------------------------------------------------------|
|bootRasters         |character   |vector of relative paths to the downloaded bootstrap raster files      |
|bootstrapReplicates |data.table  |summary of the number of bootstrap replicates per BCR and bird species |

# Links to other modules

This module prepares data layers for use with BAM posthoc binning module (<https://github.com/ilaneshaw/postHocBinning>).
