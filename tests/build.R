#' Write RUnit test files based on specifications stored in text files. Please, DO NOT USE double quotation marks (i.e. avoid the name field).
#'
#' @param filename A character vector with the filename where the specifications are stored
#' @param pathIn   A character vector with the path where the specification files are stored
#' @param pathOut  A character vector with the path where the tests should be written
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

# Build tests
build_tests("density.txt", "tests", "inst/tests")
build_tests("specification.txt", "tests", "inst/tests")
