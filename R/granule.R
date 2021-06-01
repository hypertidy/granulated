#' @param xmin
#'
#' @param xmax
#' @param ymin
#' @param ymax
#' @param ncol
#' @param nrow
#' @param crs
#' @export
#' @importFrom vctrs vec_assert new_rcrd field vec_cast vec_recycle_common
new_granule <- function(xmin = double(), xmax = double(), ymin = double(), ymax = double(), ncol = integer(), nrow = integer(), crs = list()) {
  vctrs::vec_assert(xmin, ptype = double())
  vctrs::vec_assert(xmax, ptype = double())
  vctrs::vec_assert(ymin, ptype = double())
  vctrs::vec_assert(ymax, ptype = double())
  vctrs::vec_assert(nrow, ptype = integer())
  vctrs::vec_assert(ncol, ptype = integer())

  if (ncol < 1) stop("ncol must be 1 or greater")
  if (nrow < 1) stop("nrow must be 1 or greater")

  vctrs::new_rcrd(list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, ncol = ncol, nrow = nrow), crs = crs, class = "vctrs_granule")
}
#' @name format
#' @export
format.vctrs_granule <- function(x, ...) {
  xmin <- vctrs::field(x, "xmin")
  xmax <- vctrs::field(x, "xmax")
  ymin <- vctrs::field(x, "ymin")
  ymax <- vctrs::field(x, "ymax")
  nrow <- vctrs::field(x, "nrow")
  ncol <- vctrs::field(x, "ncol")

  out <- sprintf("xmin: %f, xmax: %f, ymin: %f, ymax: %f\ndim: %ix%i", xmin, xmax, ymin, ymax, ncol, nrow)
  #out[is.na(n) | is.na(d)] <- NA

  out
}
#' Title
#'
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @param ncol
#' @param nrow
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
granule <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  UseMethod("granule")
}
#' @name granule
#' @export
#' @importFrom zeallot %<-%
granule.default <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  xmin <- vctrs::vec_cast(xmin, double())
  xmax <- vec_cast(xmax, double())
  ymin <- vec_cast(ymin, double())
  ymax <- vec_cast(ymax, double())
  ncol <- vec_cast(ncol, integer())
  nrow <- vec_cast(nrow, integer())
  c(xmin, xmax, ymin, ymax, ncol, nrow) %<-% vctrs::vec_recycle_common(xmin, xmax, ymin, ymax, ncol, nrow)

  ## do we allow ranges of 0 (and dimension of 0)
  ## what about negative ranges (could be fun)
  new_granule(xmin, xmax, ymin, ymax, ncol, nrow, crs)
}
#' @name granule
#' @export
granule.Extent <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  x <- xmin
  ## note that we can override ncol, nrow from 1x1 here (but not xmax,ymin,ymax)
  granule(x@xmin, x@xmax, x@ymin, x@ymax, ncol, nrow, crs)
}
#' @name granule
#' @export
granule.bbox <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  x <- xmin
  att <- attr(x, "crs")
  ## could be old proj4string, but sf smashed that so unlikely these days input can be anything
  ## it's not necessarily a record of what what was there, it's assumed it's user-typed
  ## $proj4string is a function call so you can't use it without roundtripping $wkt through  GDAL
  if (!is.null(att)) crs <- list(input = att$input, proj4 = NULL, wkt = att$wkt)
  granule(x[1L], x[3L], x[2L], x[4L], 1L, 1L, crs)
}
#' @name granule
#' @export
granule.BasicRaster <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  x <- xmin
  granule(x@extent, ncol = x@ncols, nrow = x@nrows, crs = crs)
}
#' @name granule
#' @export
granule.stars <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  x <- xmin
  att <- attr(x, "dimensions")
  ncol <- att[[1]]$to - att[[1]]$from + 1
  nrow <- att[[2]]$to - att[[2]]$from + 1

  xmin <- att[[1]]$offset + (att[[1]]$from - 1)* att[[1]]$delta
  xmax <- att[[1]]$offset + att[[1]]$to* att[[1]]$delta
  ymin <- att[[2]]$offset + (att[[2]]$to) * att[[2]]$delta
  ymax <- att[[2]]$offset + (att[[2]]$from - 1)* att[[2]]$delta
  granule(xmin, xmax, ymin, ymax, ncol, nrow, crs)
}
#' @name granule
#' @export
granule.SpatRaster <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1, crs = list()) {
  x <- xmin
  ## we can't do this without terra so probably remove, but explore for now
  ext <- x@ptr$extent$vector
  granule(ext[1L], ext[2L], ext[3L], ext[4L], ncol = x@ptr$ncol, nrow = x@ptr$nrow, crs = crs)
}
