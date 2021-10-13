
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



#' @export
rowcol_from_cell <- function(object, cell) {

  cell <- round(cell)
  ncols <- dim(object)[2]
  cell[cell < 1 | cell > prod(dim(object)[1:2])] <- NA
  row <- as.integer(trunc((cell-1)/ncols) + 1)
  col <- as.integer(cell - ((row-1) * ncols))
  return(cbind(row, col))
}
#' @export
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
#' @export
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
#' @export
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
#' @export
cell_from_rowcol_combine <-function(object, row, col) {
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
