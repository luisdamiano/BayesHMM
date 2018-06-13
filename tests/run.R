if (require("RUnit", quietly = TRUE)) {
  packageName <- packageDescription("BayesHMM")$Package
  require(packageName, quietly = TRUE, character.only = TRUE) ||
    stop("Package '", packageName, "' not found.")

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
  printTextProtocol(testResult)
  printHTMLProtocol(testResult, fileName = "RUnit.html")
}
