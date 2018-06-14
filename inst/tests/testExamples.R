test_examples <- function() {
  exampleFiles <- dir(
    system.file(
      "examples",
      package = "BayesHMM"
    ),
    full.names = TRUE
  )

  for (f in exampleFiles) {
    langs <- parse(f, keep.source = TRUE)
    ind   <- which(grepl("hmm", as.character(langs)) == TRUE)
    for (i in ind) {
      tmpEnv <- new.env()
      eval(langs[[i]], tmpEnv)
      res    <- checkTrue(
        error_in_write_model(tmpEnv[[ls(tmpEnv)]]),
        sprintf("Can run example model in %s.", tmpEnv)
      )
      rm(tmpEnv)
      res
    }
  }
}
