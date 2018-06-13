if (require("RUnit", quietly = TRUE)) {
  packageName <- utils::packageDescription("BayesHMM")$Package
  require(packageName, quietly = TRUE, character.only = TRUE) ||
    stop("Package '", packageName, "' not found.")

  # Helper
  error_on_write_model <- function(spec) {
    tryCatch({
      write_model(spec, noLogLike = FALSE, writeDir = tempdir())
      write_model(spec, noLogLike = TRUE , writeDir = tempdir())
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  # Setup
  filePattern     <- "^test.*\\.R$"
  functionPattern <- "^test_+"
  fileDir <- system.file(
    "tests",
    package = "BayesHMM"
  )

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
  testResult <- runTestSuite(testSuite)

  if (testResult[[1]]$nErr > 0 | testResult[[1]]$nFail > 0 ) {
    tmpOut <- tempfile(fileext = ".html")
    printHTMLProtocol(testResult, fileName = tmpOut)
    utils::browseURL(tmpOut)

    stop(printTextProtocol(testResult, showDetails = FALSE))
  }
}
