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
## TODO
##
## decide on nrow/ncol or ncol/nrow in this .grain thing
# crop (positve and negative)
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

rowcol_from_cell <- function(object, cell) {

  cell <- round(cell)
  ncols <- dim(object)[2]
  cell[cell < 1 | cell > prod(dim(object)[1:2])] <- NA
  row <- as.integer(trunc((cell-1)/ncols) + 1)
  col <- as.integer(cell - ((row-1) * ncols))
  return(cbind(row, col))
}
xy_from_cell <- function(object, cell) {
  xmin <- x_min(object)
  xmax <- x_max(object)
  ymin <- y_min(object)
  ymax <- y_max(object)
  len <- length(cell)
  nrows <- nrow(object)
  ncols <- ncol(object)
  yres = (ymax - ymin) / nrows
  xres = (xmax - xmin) / ncols

    c = cell - 1
    row = floor(c / ncols)
    col = c - row * ncols
    cbind((col + 0.5) * xres + xmin,
          ymax - (row + 0.5) * yres)

}
col_from_x <- function(object, x) {
  colnr <- trunc((x - x_min(object)) / x_res(object)) + 1
  colnr[ x == x_max(object) ] <- ncol(object)
  colnr[ x < x_min(object) | x > x_max(object) ] <- NA
  return(as.vector(colnr))
}
row_from_y <- function(object, y) {
  rownr <- 1 + (trunc((y_max(object) - y) / y_res(object)))
  rownr[y == y_min(object) ] <- nrow(object)
  rownr[y > y_max(object) | y < y_min(object)] <- NA
  return(as.vector(rownr))
}
intersect_extent <- function(x, y) {
  y <- .grain(y[1], y[2], y[3], y[4])
  xmin <- max(x_min(x), x_min(y))
  xmax <- min(x_max(x), x_max(y))
  ymin <- max(y_min(x), y_min(y))
  ymax <- min(y_max(x), y_max(y))

  if ((xmax <= xmin) | (ymax <= ymin) ) {
    ## objects do not overlap
    return(NULL)
  }
  c(xmin, xmax, ymin, ymax)
}

cell_from_extent <- function(object, extent) {
  extent <- align_extent(extent, object)
  inner_ext <- intersect_extent(object, extent)
  if (is.null(inner_ext)) {
    return(NULL)
  }

  srow <- row_from_y(object, inner_ext[4L] - 0.5 * y_res(object))
  erow <- row_from_y(object,   inner_ext[3L] + 0.5 * y_res(object))
  scol <- col_from_x(object,   inner_ext[1L] + 0.5 * x_res(object))
  ecol <- col_from_x(object,   inner_ext[2L] - 0.5 * x_res(object))

  # if (expand) {
  #   srow <- srow - round((extent@ymax - innerBox@ymax) / yres(object))
  #   erow <- erow + round((innerBox@ymin - extent@ymin) / yres(object))
  #   scol <- scol - round((innerBox@xmin - extent@xmin) / xres(object))
  #   ecol <- ecol + round((extent@xmax - innerBox@xmax) / xres(object))
  # }
  #
  return(cell_from_rowcol_combine(object, srow:erow, scol:ecol))
}
extent_from_cell <- function(object, cells) {
  cells <- stats::na.omit(unique(round(cells)))
  cells <- cells[cells > 0 & cells <= prod(dim(object)[1:2])]
  if (length(cells) < 1) {
    stop('no valid cells')
  }
  r <- c(x_res(object), y_res(object))
  dx <- r[1] * c(-0.5, 0.5)
  dy <- r[2] * c(-0.5, 0.5)
  c(range(x_from_cell(object, cells)) + dx, range(y_from_cell(object, cells)) + dy)
}

cell_from_rowcol_combine <-
  function(object, row, col) {
    nr <- nrow(object)
    nc <- ncol(object)
  row[row < 1 | row > nr] <- NA
  col[col < 1 | col > nc] <- NA
  cols <- rep(col, times=length(row))
  dim(cols) <- c(length(col), length(row))
  cols <- t(cols)
  row <- (row-1) * nc
  cols <- cols + row
  as.vector(t(cols))
  }

origin <-   function(x) {
  e <- extent(x)
  r <- c(x_res(x), y_res(x))
  x <- e[1L] - r[1]*(round(e[1L] / r[1]))
  y <- e[4L] - r[2]*(round(e[4L] / r[2]))

  if (isTRUE(all.equal((r[1] + x), abs(x)))) {
    x <- abs(x)
  }
  if (isTRUE(all.equal((r[2] + y), abs(y)))) {
    y <- abs(y)
  }
  return(c(x, y))
}

align_extent <- function(extent, object, snap = c("out", "near", "in")) {
  snap <- match.arg(snap)
  res <- c(x_res(object), y_res(object))
  orig <- origin(object)
  xmin <- extent[1L]
  xmax <- extent[2L]
  ymin <- extent[3L]
  ymax <- extent[4L]
  # snap points to pixel boundaries
  if (snap == 'near') {
    xmn <- round((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- round((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- round((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- round((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  } else if (snap == 'out') {
    xmn <- floor((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- ceiling((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- floor((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- ceiling((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  } else if (snap == 'in') {
    xmn <- ceiling((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- floor((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- ceiling((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- floor((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  }

  if (xmn == xmx) {
    if (xmn <= xmin) {
      xmx <- xmx + res[1]
    } else {
      xmn <- xmn - res[1]
    }
  }
  if (ymn == ymx) {
    if (ymn <= ymin) {
      ymx <- ymx + res[2]
    } else {
      ymn <- ymn - res[2]
    }
  }
  e <- c(xmn, xmx, ymn, ymx)
  #intersect(e, extent(object))
  return(e)
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
