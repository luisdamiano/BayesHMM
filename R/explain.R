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






