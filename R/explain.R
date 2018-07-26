make_text <- function(text) {

}

make_text_line <- function() {
  theme      <- getOption("BayesHMM.print")
  char       <- theme$char
  textWidth  <- theme$textWidth
  paste(rep(char, textWidth), collapse = "")
}

make_text_header <- function(text) {
  textLine   <- make_text_line()

  sprintf(
    "%-80s\n%s\n",
    toupper(text), textLine
  )
}

make_text_subheader <- function(text) {
  textLine   <- make_text_line()

  sprintf(
    "%s\n%s\n",
    textLine, text
  )
}

explain_observation.Specification <- function(spec) {
  R <- spec$observation$R

  block1 <-
    sprintf(
      "%s observations (R = %d): %s.\n",
      if (R > 1) { "Multivariate" } else { "Univariate" },
      R, "Variable names"
    )

  l <- densityApply(spec$observation$density, explain)

  block2 <-
    if (all(sapply(l, identical, l[[1]]))) {
      sprintf(
        "Observation model for all states\n%s\n", l[[1]]
      )
    } else {
      k <- sub("k[[:digit:]].k([[:digit:]])r[[:digit:]]", "\\1", names(l))
      r <- sub("k[[:digit:]].k[[:digit:]]r([[:digit:]])", "\\1", names(l))
      sprintf(
        "\nObservation model for State %s and Variable %s\n%s\n",
        k, r, l
      )
    }

  collapse(c(block1, block2))
}

explain_initial.Specification <- function(spec) {
  l <- densityApply(spec$initial$density, explain)

  block1 <-
    if (all(sapply(l, identical, l[[1]]))) {
      sprintf(
        "Initial distribution model\n%s\n", l[[1]]
      )
    } else {
      k <- sub("i[[:digit:]].i([[:digit:]])j[[:digit:]]", "\\1", names(l))
      sprintf(
        "\nInitial probability for State %s\n%s\n",
        k, l
      )
    }

  collapse(block1)
}

explain_transition.Specification <- function(spec) {
  l <- densityApply(spec$transition$density, explain)

  block1 <-
    if (all(sapply(l, identical, l[[1]]))) {
      sprintf(
        "Transition model\n%s\n", l[[1]]
      )
    } else {
      i <- sub("i[[:digit:]].i([[:digit:]])j[[:digit:]]", "\\1", names(l))
      j <- sub("i[[:digit:]].i[[:digit:]]j([[:digit:]])", "\\1", names(l))
      sprintf(
        "\nTransition probability from State %s to State %s\n%s\n",
        i, j, l
      )
    }

  collapse(block1)
}
