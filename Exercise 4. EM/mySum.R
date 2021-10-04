mySum <- function(x) {
#this function returns sum of the elements of x, whereas infinitly large elements are ignored
  sum(x[is.finite(x)])
}