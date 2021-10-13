
#' @export
dim.grain <- function(x) {
  x[, c("ny", "nx")]
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nrow.grain <- function(x) {
  dim(x)[2]
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ncol.grain <- function(x) {
  dim(x)[1]
}

#' @export
n_cell <- function(x) {
  prod(dim(x))
}
#' @param x
#'
#' @export
x_res <- function(x) {
  (x_max(x) - x_min(x))/ncol(x)
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
y_res <- function(x) {
  (y_max(x) - y_min(x))/nrow(x)
}
