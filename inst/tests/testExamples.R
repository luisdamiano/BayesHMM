test_examples <- function() {
  DEACTIVATED("Testing examples is temporarily deactivated.")

  exampleFiles <- dir(
    system.file(
      "examples",
      package = "BayesHMM"
    ),
    full.names = TRUE
  )

  for (f in exampleFiles) {
    langs     <- parse(f, keep.source = TRUE)
    if (length(langs)) {
      keyWords  <- c("hmm\\(", "mixture\\(", "spec\\(")
      codeBlock <- apply(
        sapply(keyWords, function(key) {
          grepl(key, as.character(langs))
        }),
        1,
        any
      )
      codeIndex <- which(codeBlock)
      for (i in codeIndex) {
        tmpEnv <- new.env()
        eval(langs[[i]], tmpEnv)
        sprintf(
          "Testing file %s:\n%s",
          f,
          as.character(langs[[i]])
        )
        res    <- checkTrue(
          error_in_write_model(tmpEnv[[ls(tmpEnv)]]),
          sprintf("Can run example model in %s.", tmpEnv)
        )
        rm(tmpEnv)
        res
      }
    }
  }
}
