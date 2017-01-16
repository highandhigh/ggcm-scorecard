# environment variables
bivio.ev <- "BIVIO_FILE" # bivio account export file
scorecard.dir.ev <- "SCORECARD_DIR" # search for latest workspace
scorecard.workspace.ev <- "SCORECARD_WORKSPACE" # override, always use this workspace


# options for many functions
options("stringsAsFactors" = FALSE)
options("getSymbols.auto.assign" = FALSE)
options("getSymbols.warning4.0" = FALSE)
options("verbose"=TRUE)

invisible(suppressPackageStartupMessages(
  lapply(
    c(
      "XML",
      "yaml",
      "zoo",
      "timeSeries",
      "quantmod",
      "grid",
      "gridExtra",
      "gtable",
      "tidyr",
      "dplyr",
      "magrittr",
      "blotter",
      "lubridate",
      "formattable",
      "stringr",
      "PerformanceAnalytics",
      "ggplot2",
      "directlabels",
      "scales",
      "tools"
    ),
    library,
    warn.conflicts = FALSE,
    character.only = TRUE,
    verbose = getOption("verbose")
  )
))


# load everything in the lib directory
# should include the model out-of-sample update functions
for ( libname in list.files("lib",pattern="[.][Rr]$")) {
  source(file.path("lib",libname))
}

