## grid logic functions

# xlim, ylim, x_min,x_max,y_min,y_max,
# extent, n_cell, x_res, y_res,
# dim,nrow,ncol,
# col_from_cell, x_from_cell,
# row_from_cell, y_from_cell,
# x_centre, y_centre,
# x_corner, y_corner,
# x_from_col, y_from_row
# cell_from_col, cell_from_row, cell_from_rowcol
# cell_from_xy
# align_extent, origin
# cell_from_rowcol_combine
# row_from_y, col_from_x
#  cell_from_extent, extent_from_cell
# xy_from_cell, rowcol_from_cell
# crop_grain (negative or neutral) set_extent (positive or negative)
## TODO
##
## decide on nrow/ncol or ncol/nrow in this .grain thing
# cropij (positive and negative but by *number of cells width,height*)
#
# refactor these functions to have core versions, something like:
## fun(query, dimension, extent, projection)
## perhaps analogs that map directly to C++ versions, just input vectors or single values (xmin, nrow, ...)
#' @rawNamespace exportPattern("^[^\\.]")
##https://github.com/hypertidy/cells/blob/7e2e4d82466db679720036702877f17ec86c5e74/R/grain.R
.grain <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                   nx = 1, ny = 1, ...) {
  nx <- as.integer(nx)
  ny <- as.integer(ny)
  structure(cbind(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  nx = nx, ny = ny), extra = list(...), class =
              c("grain", "matrix", "array"))
}









#' Title
#'
#' @param x
#' @param show
#'
#' @return
#' @export
#'
#' @examples
plot.grain <- function(x, show = c("centre", "corner")) {
  show <- match.arg(show)
  dm <- dim(x)
  if (dm[2L] < 80) {
    if (show == "centre") {
      xs <- x_centre(x)
    } else {
      xs <- x_corner(x)
    }
  } else {
    xs <- xlim(x)
    warning("too big to show every x coordinate")
  }

  if (dm[1L] < 80) {
    if (show == "centre") {
      ys <- y_centre(x)
    } else {
      ys <- y_corner(x)
    }

  } else {
    ys <- ylim(x)
    warning("too big to show every x coordinate")
  }
  plot(range(xs), range(ys), type = "n", asp = y_res(x)/x_res(x))
  abline(v = xs, col = "firebrick")
  abline(h = ys, col  = "dodgerblue")
}
