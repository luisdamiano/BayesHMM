setGeneric("extract_n_chains", function(stanfit) {standardGeneric("extract_n_chains")} )

setMethod("extract_n_chains", "stanfit", function(stanfit) {
  stanfit@sim$chains
})

is.stanfit <- function(x) {
  class(x) == "stanfit"
}
