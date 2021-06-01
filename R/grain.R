## grid logic functions

# xlim, ylim, x_min,x_max,y_min,y_max,
# ext_ent, n_cell, x_grain, y_grain,
# dim,nrow,ncol,
# col_from_cell, x_from_cell,
# row_from_cell, y_from_cell,
# x_centre, y_centre,
# x_corner, y_corner,

## TODO
# x_from_col, y_from_row
#


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

xlim <- function(x) {
  UseMethod("xlim")
}
xlim.grain <- function(x) {
  x[1L, c("xmin", "xmax")]
}
ylim <- function(x) {
  UseMethod("ylim")
}
ylim.grain <- function(x) {
  x[1L, c("ymin", "ymax")]
}
x_min <- function(x) {
  UseMethod("x_min")
}
x_min.grain <- function(x) {
  x[1L, c("xmin")]
}
x_max <- function(x) {
  UseMethod("x_max")
}
x_max.grain <- function(x) {
  x[1L, c("xmax")]
}


y_min <- function(x) {
  UseMethod("y_min")
}
y_min.grain <- function(x) {
  x[1L, c("ymin")]
}
y_max <- function(x) {
  UseMethod("y_max")
}
y_max.grain <- function(x) {
  x[1L, c("ymax")]
}

dim.grain <- function(x) {
  x[, c("ny", "nx")]
}
ext_ent <- function(x) {
  x[, c("xmin", "xmax", "ymin", "ymax")]
}
n_cell <- function(x) {
  prod(dim(x))
}
x_grain <- function(x) {
  x[, c("xres")]
}
y_grain <- function(x) {
  x[, c("yres")]
}

col_from_cell <- function(x, cell) {
  cell <- round(cell)
  cell[cell < 1L | cell > n_cell(x)] <- NA
  rownr <- trunc((cell - 1)/ncol(x)) + 1L
  as.integer(cell - ((rownr - 1) * ncol(x)))
}
x_from_cell <- function(x, cell) {
  ## improve with x_from_col
  x_centre(x)[col_from_cell(x, cell)]
}
row_from_cell <- function(x, cell) {
  cell <- round(cell)
  cell[cell < 1 | cell > n_cell(x)] <- NA
  trunc((cell - 1)/ncol(x)) + 1
}
y_from_cell <- function(x, cell) {
  ## improve with y_from_row
  y_centre(x)[row_from_cell(x, cell)]
}

x_centre <- function(x) {
  xl <- xlim(x)
  resx <- x_grain(x)
  seq(xl[1L] + resx/2, xl[2L] - resx/2, length.out = dim(x)[2L])
}
y_centre <- function(x) {
  yl <- ylim(x)
  resy <- y_grain(x)
  seq(yl[1L] + resy/2, yl[2L] - resy/2, length.out = dim(x)[1L])
}

x_corner <- function(x) {
  xl <- xlim(x)
  resx <- x_grain(x)
  seq(xl[1L], xl[2L], length.out = dim(x)[2L] + 1L)
}
y_corner <- function(x) {
  yl <- ylim(x)
  resy <- y_grain(x)
  seq(yl[1L], yl[2L], length.out = dim(x)[1L] + 1L)
}
coords <- function(x) {
  cell <- seq_len(n_cell(x))
  cbind(x = x_from_cell(x, cell),
        y = y_from_cell(x, cell))
}
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
  plot(range(xs), range(ys), type = "n", asp = y_grain(x)/x_grain(x))
  abline(v = xs, col = "firebrick")
  abline(h = ys, col  = "dodgerblue")
}
