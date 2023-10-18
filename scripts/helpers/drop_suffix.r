drop_suffix <- function(d) {
  # Drop everything after underscore from variable name
  out <- sapply(colnames(d), FUN=function(x) unlist(strsplit(x, '_'))[1])
  return(out)
}