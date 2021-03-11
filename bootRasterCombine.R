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
  reqdPkgs = list("googledrive", "magrittr", "raster", "reproducible",
                  "LandR", ## TODO: where is this used?
                  "tibble", ## TODO: remove this package
                  "dplyr" ## TODO: remove this package
  ),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
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
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "bootRasterCombine", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "bootRasterCombine", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "bootRasterCombine", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "bootRasterCombine", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "bootRasterCombine", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "bootRasterCombine", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
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

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  ## TODO:
  BCRList <- c("61", "60", "4", "10") #, "11", "12", "4", "83", "82", "81", "80", "71", "70", "14")
  #rasterToMatch <- LandR::prepInputsLCC()
  birdSp <- "BAWW" ## specify the bird species
  #BCR <- "4"
  folderRawBoot <- "folderRawBoot" ## say where to download to ## TODO: rename directory
  folderUrl <- "https://drive.google.com/drive/folders/1f9NvoSSdHav8FqnswwPYuV0TFLr19ny5" # give file location
  uploadFolder <- "https://drive.google.com/drive/folders/1fCTr2P-3Bh-7Qh4W0SMJ_mT9rpsKvGEA"

  ## 1. download boostrapped rasters
  # TODO: currently being done as part of creatMEanVarRasters() below
  #       this should be done beforehand and cached for future use

  ## 2. create mean and variance rasters
  meanVarRasters <- createMeanVarRasters(birdSp = birdSp, BCRList = BCRList, folderUrl = folderUrl,
                                         folderRawBoot = folderRawBoot, uploadFolder = uploadFolder)

  ## 3. upload outputs
  drive_put(media = file.path(paste0(getwd(),"/meanVarRasters/var", birdSp,".tif")),
            path = "https://drive.google.com/drive/folders/1fCTr2P-3Bh-7Qh4W0SMJ_mT9rpsKvGEA?usp=sharing",
            name = paste0("var", birdSp, ".tif"),
            verbose = TRUE)



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
