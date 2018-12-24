#' Checks if there is no error in a model specification
#' @param string A character vector with a specification.
#' @return TRUE is the string is a valid specification, FALSE if the string cannot be parsed or cannot be translated into a valid Stan model.
no_error_in_spec <- function(string) {
  spec <-
    tryCatch({
      eval(parse(text = string))
    }, error = function(error) {
      # Early stop if the string cannot be parsed.
      return(RUnit::checkTrue(FALSE, sprintf(
        "The model string cannot be parsed: %s.", error$message
      )))
    })

  tryCatch({
    explain(spec)
  }, error = function(error) {
    return(RUnit::checkTrue(FALSE, sprintf(
      "Could not explain specification: %s.", error$message
    )))
  })

  tryCatch({
    BayesHMM:::write_model.Specification(spec, noLogLike = FALSE, writeDir = tempdir())
    BayesHMM:::write_model.Specification(spec, noLogLike = TRUE , writeDir = tempdir())
  }, error = function(error) {
    return(RUnit::checkTrue(FALSE, sprintf(
      "The model cannot be translated to Stan: %s.", error$message
    )))
  })

  invisible()
}

no_error_in_expr <- function(expr) {
  tryCatch({
    eval(expr)
  }, error = function(error) {
    return(RUnit::checkTrue(FALSE, sprintf(
      "The expr could not be evaluated: %s.", error$message
    )))
  })
}

load_safe <- function(filename) {
  filename <- file.path(
    system.file("tests", package = "BayesHMM"),
    filename
  )

  myObj    <- if (file.exists(filename)) {
    readRDS(filename)
  } else {
    NULL
  }

  if (is.null(myObj)) {
    RUnit::checkTrue(FALSE, "Could not load sim.RDS")
  } else {
    return(myObj)
  }
}

# Test suite --------------------------------------------------------------
if (require("RUnit", quietly = TRUE)) {
  packageName <- utils::packageDescription("BayesHMM")$Package
  require(packageName, quietly = TRUE, character.only = TRUE) ||
    stop("Package '", packageName, "' not found.")

  # Set up test suit
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
