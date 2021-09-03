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
## TODO
#  cell_from_extent, extent_from_cell
# xy_from_cell, rowcol_from_cell
# crop (positve and negative)
# cropij (positive and negative but by *number of cells width,height*)
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



cell_from_xy <- function(x, xy) {
  xx <- xy[,1L, drop = TRUE]
  yy <- xy[,2L, drop = TRUE]


  len <- length(xx)
  ncols <- ncol(x)
  nrows <- nrow(x)
  xmin <- x_min(x)
  xmax <- x_max(x)
  ymin <- y_min(x)
  ymax <- y_max(x)
  yres_inv = nrows / (ymax - ymin)
  xres_inv = ncols / (xmax - xmin)
    ## cannot use trunc here because trunc(-0.1) == 0
    row = floor((ymax - yy) * yres_inv);
    ## points in between rows go to the row below
    ## except for the last row, when they must go up
    row <- ifelse(yy == ymin, nrows - 1, row)

    col = floor((xx - xmin) * xres_inv)
    ## as for rows above. Go right, except for last column
    col <- ifelse (xx == xmax, ncols-1, col)

    ifelse (row < 0 || row >= nrows || col < 0 || col >= ncols, NA_real_, row * ncols + col + 1)
}
cell_from_row_col <- function(x, row, col) {
   colrow <- cbind(col, row)  ## for recycling
  colnr <- colrow[,1L]
   rownr <- colrow[,2L]

   nr <- nrow(x)
   nc <- ncol(x)
   i <- seq_along(rownr)-1
   nn <- length(rownr)

  r <- rownr[ifelse(i < nn, i, i %% nn) + 1]
  c <- colnr[ifelse(i < nn, i, i %% nn) + 1]
  ifelse(r < 1 | r > nr | c < 1 | c > nc, NA,  (r-1) * nc + c)
}

cell_from_row <- function(x, row) {
  row <- round(row)
  cols <- rep(1:ncol(x), times=length(row))
  rows <- rep(row, each=ncol(x))
  cell_from_row_col(x, rows, cols)
}

cell_from_col <- function(x, col) {
  col <- round(col)
  rows <- rep(1:nrow(x), times = length(col))
  cols <- rep(col, each = nrow(x))
  cell_from_row_col(x, rows, cols)
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
extent <- function(x) {
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
  (x_max(x) - x_min(x))/n_col(x)
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
  (y_max(x) - y_min(x))/n_row(x)
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
