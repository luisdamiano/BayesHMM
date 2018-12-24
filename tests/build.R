#' Write RUnit test files based on specifications stored in text files. Please, DO NOT USE double quotation marks (i.e. avoid the name field).
#'
#' @param filename A character vector with the filename where the specifications are stored.
#' @param pathIn   A character vector with the path where the specification files are stored.
#' @param pathOut  A character vector with the path where the tests should be written.
#' @return Nothing. It will write the tests to a file named `tests/test_%s.R`, where `%s` is the filename passed as argument without extension.
#' @examples
#' build_tests("density.txt", "tests", "inst/tests")
build_tests <- function(filename, pathIn, pathOut) {
  makeFilename <- function(string) {
    str <- tools::file_path_sans_ext(string)
    paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str))))
  }

  pathIn   <- sprintf("%s/%s", pathIn, filename)
  pathOut  <- sprintf("%s/test%s.R", pathOut, makeFilename(filename))
  strList  <- parse(pathIn)
  outList  <- lapply(seq_along(strList), function(i) {
    sprintf(
      "test_%s_%d <- function() { no_error_in_spec(\"%s\") } \n",
      filename, i, paste0(deparse(strList[[i]]), collapse = "\n")
    )
  })

  write(x = paste0(outList), file = pathOut)
}

#' Write RDS objects with precomputed computations used by unit tests.
#'
#' @param pathOut  A character vector with the path where the objects should be written.
#' @return Nothing. It will write RDS files in the `pathOut` folder.
#' @examples
#' build_objects("inst/tests")
build_objects <- function(pathOut) {
  seed    <- 9000
  tLength <- 100
  nIter   <- 100 # keep it low to get a small file

  # Specs
  mySimSpec <- hmm(
    K = 2, R = 1,
    observation = Gaussian(-5, 1) + Gaussian(5, 2),
    initial     = Dirichlet(alpha = c(1, 1)),
    transition  = Dirichlet(alpha = c(1, 1)),
    name = "Univariate Gaussian Hidden Markov Model for unit tests"
  )

  myFitSpec <- hmm(
    K = 2, R = 1,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
    ),
    initial     = Dirichlet(alpha = c(1, 1)),
    transition  = Dirichlet(alpha = c(1, 1)),
    name = "Univariate Gaussian Hidden Markov Model for unit tests"
  )

  # 1. sim
  mySims <- sim(mySimSpec, T = tLength, nSimulations = 1, seed = seed)
  saveRDS(mySims, file.path(pathOut, "sim.RDS"))

  # 2. validate_calibration
  myCal  <- validate_calibration(
    myFitSpec, N = 1, T = tLength, seed = seed, nCores = 12, iter = nIter
  )
  saveRDS(mySims, file.path(pathOut, "sim.RDS"))

  # 3. compile
  myComp <- compile(myFitSpec)
  saveRDS(myComp, file.path(pathOut, "compile.RDS"))

  # 4. drawSamples
  ySim   <- extract_ysim(mySims, chain = 1)
  zSim   <- extract_zsim(mySims, chain = 1)
  myFit  <- drawSamples(myFitSpec, myComp, y = ySim, chains = 2, iter = nIter)
  saveRDS(myFit, file.path(pathOut, "drawSamples.RDS"))

  # 5. optimizating keep all
  myOpt  <- optimizing(
    myFitSpec, myComp, y = ySim, nRuns = 10, nCores = 10, keep = "all"
  )
  saveRDS(myOpt, file.path(pathOut, "optimizingAll.RDS"))

  # 5. optimizating keep best
  myOpt  <- optimizing(
    myFitSpec, myComp, y = ySim, nRuns = 10, nCores = 10, keep = "best"
  )
  saveRDS(myOpt, file.path(pathOut, "optimizingBest.RDS"))
}

# Build tests
# build_tests("density.txt", "tests", "inst/tests")
# build_tests("specification.txt", "tests", "inst/tests")
