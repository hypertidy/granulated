#' Extent is a workhorse, from this is derived
#' xlim,ylim,xmin,xmax,ymin,ymax and from those
#' x_res,y_res,x,y_centre,corner, etc.
#' @export
extent <- function(x) {
  UseMethod("extent")
}
#' @export
extent.default <- function(x) {
  ## catch nativeRaster
  extent.array(x)
}
#' @export
extent.grain <- function(x) {
  unname(x[1L, c("xmin", "xmax", "ymin", "ymax")])
}
#' @export
extent.array <- function(x) {
  dm <- dim(x)
  ## do we look for weird attributes? rayshader ...
  c(0, dm[1L], 0, dm[2L])
}
#' @export
extent.grd <- function(x) {
  ## we don't *use* {grd} code, because we can probably rely on
  ## a simple format-contract (for now at least)
  ## - .grain() here is a bit silly but there's nothing in R that
  ## can do this just apart from xyz <- list(x = vec, y = vec, z = img) with
  ## image() and raster::raster(xyz)
  unlist(x$bbox[c("xmin", "xmax", "ymin", "ymax")], use.names = FALSE)
}
#' @export
xlim <- function(x) {
  UseMethod("xlim")
}
#' @export
xlim.default <- function(x) {
  extent(x)[1:2]
}
#' @export
ylim <- function(x) {
  UseMethod("ylim")
}
#' @export
ylim.default <- function(x) {
  extent(x)[3:4]
}
#' @export
x_min <- function(x) {
  UseMethod("x_min")
}
#' @export
x_min.default <- function(x) {
  xlim(x)[1L]
}
#' @export
x_max <- function(x) {
  UseMethod("x_max")
}
#' @export
x_max.default <- function(x) {
  xlim(x)[2L]
}

#' @export
y_min <- function(x) {
  UseMethod("y_min")
}
#' @export
y_min.default <- function(x) {
  ylim(x)[1L]
}

#' @export
y_max <- function(x) {
  UseMethod("y_max")
}
#' @export
y_max.default <- function(x) {
  ylim(x)[2L]
}
