error_in_function <- function(f) {
  tryCatch({
    f()
    TRUE
  }, error = function(e) {
    FALSE
  })
}

error_in_write_model <- function(spec) {
  f <- function() {
    write_model(spec, noLogLike = FALSE, writeDir = tempdir())
    write_model(spec, noLogLike = TRUE , writeDir = tempdir())
  }

  error_in_function(f)
}

if (require("RUnit", quietly = TRUE)) {
  packageName <- utils::packageDescription("BayesHMM")$Package
  require(packageName, quietly = TRUE, character.only = TRUE) ||
    stop("Package '", packageName, "' not found.")

  # Setup
  filePattern     <- "^test.*\\.R$"
  functionPattern <- "^test_+"
  fileDir <- system.file(
    "tests",
    package = "BayesHMM"
  )

  # Load helpers
  source(file.path(fileDir, "diagnostics.R"))
  source(file.path(fileDir, "simulation.R"))

  # Create test suite
  testSuite <- defineTestSuite(
    name           = paste(packageName, "RUnit Tests"),
    dirs           = fileDir,
    testFileRegexp = filePattern,
    testFuncRegexp = functionPattern,
    rngKind        = "default",
    rngNormalKind  = "default"
  )

  # Run tests, display and save results
  testResult <- runTestSuite(testSuite, verbose = 0)

  if (testResult[[1]]$nErr > 0 | testResult[[1]]$nFail > 0 ) {
    tmpOut <- tempfile(fileext = ".html")
    printHTMLProtocol(testResult, fileName = tmpOut)
    utils::browseURL(tmpOut)

    stop(
      sprintf(
        "%s\n More information: %s.",
        printTextProtocol(testResult, showDetails = TRUE),
        tmpOut
      )
    )
  }
}
