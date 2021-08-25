## grid logic functions

# xlim, ylim, x_min,x_max,y_min,y_max,
# ext_ent, n_cell, x_res, y_res,
# dim,nrow,ncol,
# col_from_cell, x_from_cell,
# row_from_cell, y_from_cell,
# x_centre, y_centre,
# x_corner, y_corner,
# x_from_col, y_from_row
# ## TODO
# cell_from_col, cell_from_row, cell_from_rowcol
# cell_from_xy, cell_from_extent,
# xy_from_cell, rowcol_from_cell


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

#' @rawNamespace exportPattern("^[^\\.]")
##https://github.com/hypertidy/cells/blob/7e2e4d82466db679720036702877f17ec86c5e74/R/grain.R
.grain <- function(xmin, xmax, ymin, ymax,
                   nx, ny, ...) {
  nx <- as.integer(nx)
  ny <- as.integer(ny)
  resx <- (xmax - xmin)/nx
  resy <- (ymax - ymin)/ny
  structure(cbind(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  nx = nx, ny = ny, xres = resx, yres = resy), extra = list(...), class =
              c("grain", "matrix", "array"))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
xlim <- function(x) {
  UseMethod("xlim")
}
#' @export
xlim.grain <- function(x) {
  x[1L, c("xmin", "xmax")]
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ylim <- function(x) {
  UseMethod("ylim")
}
#' @export
ylim.grain <- function(x) {
  x[1L, c("ymin", "ymax")]
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
x_min <- function(x) {
  UseMethod("x_min")
}
#' @export
x_min.grain <- function(x) {
  x[1L, c("xmin")]
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
x_max <- function(x) {
  UseMethod("x_max")
}
#' @export
x_max.grain <- function(x) {
  x[1L, c("xmax")]
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
y_min <- function(x) {
  UseMethod("y_min")
}
#' @export
y_min.grain <- function(x) {
  x[1L, c("ymin")]
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
y_max <- function(x) {
  UseMethod("y_max")
}
#' @export
y_max.grain <- function(x) {
  x[1L, c("ymax")]
}
#' @export
dim.grain <- function(x) {
  x[, c("ny", "nx")]
}

#' @export
ext_ent <- function(x) {
  x[, c("xmin", "xmax", "ymin", "ymax")]
}
#' @export
n_cell <- function(x) {
  prod(dim(x))
}
#' @param x
#'
#' @export
x_res <- function(x) {
  x[, c("xres")]
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
  x[, c("yres")]
}

#' Title
#'
#' @param x
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
col_from_cell <- function(x, cell) {
  cell <- round(cell)
  cell[cell < 1L | cell > n_cell(x)] <- NA
  rownr <- trunc((cell - 1)/ncol(x)) + 1L
  as.integer(cell - ((rownr - 1) * ncol(x)))
}
#' Title
#'
#' @param x
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
x_from_cell <- function(x, cell) {
  ## improve with x_from_col
  x_centre(x)[col_from_cell(x, cell)]
}
#' Title
#'
#' @param x
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
row_from_cell <- function(x, cell) {
  cell <- round(cell)
  cell[cell < 1 | cell > n_cell(x)] <- NA
  trunc((cell - 1)/ncol(x)) + 1
}
#' Title
#'
#' @param x
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
y_from_cell <- function(x, cell) {
  ## improve with y_from_row
  rev(y_centre(x))[row_from_cell(x, cell)]
}

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
