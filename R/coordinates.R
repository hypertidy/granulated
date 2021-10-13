
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
x_centre <- function(x) {
  xl <- xlim(x)
  resx <- x_res(x)
  seq(xl[1L] + resx/2, xl[2L] - resx/2, length.out = dim(x)[2L])
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
y_centre <- function(x) {
  yl <- ylim(x)
  resy <- y_res(x)
  seq(yl[1L] + resy/2, yl[2L] - resy/2, length.out = dim(x)[1L])
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
x_corner <- function(x) {
  xl <- xlim(x)
  resx <- x_res(x)
  seq(xl[1L], xl[2L], length.out = dim(x)[2L] + 1L)
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
y_corner <- function(x) {
  yl <- ylim(x)
  resy <- y_res(x)
  seq(yl[1L], yl[2L], length.out = dim(x)[1L] + 1L)
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
coords <- function(x) {
  cell <- seq_len(n_cell(x))
  cbind(x = x_from_cell(x, cell),
        y = y_from_cell(x, cell))
}

#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
x_from_col <- function(x, y) {
  x_centre(x)[y]
}
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
y_from_row <- function(x, y) {
  rev(y_centre(x))[y]
}

#' @export
col_from_x <- function(object, x) {
  colnr <- trunc((x - x_min(object)) / x_res(object)) + 1
  colnr[ x == x_max(object) ] <- ncol(object)
  colnr[ x < x_min(object) | x > x_max(object) ] <- NA
  return(as.vector(colnr))
}
#' @export
row_from_y <- function(object, y) {
  rownr <- 1 + (trunc((y_max(object) - y) / y_res(object)))
  rownr[y == y_min(object) ] <- nrow(object)
  rownr[y > y_max(object) | y < y_min(object)] <- NA
  return(as.vector(rownr))
}
