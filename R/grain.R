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




crop_grain <- function(x, extent, snap = "out") {
  #raster::crop does
  e <- intersect_extent(x, extent)
  if (is.null(e)) {
    stop("extents do not overlap")
  }
  e <- align_extent(e, x, snap = snap)
 e
}
# we are implicitly keepres=TRUE (so maybe this is crop_grid ...)
## FIXME: error here somewhere ylim not working
set_extent <- function(x, e) {
  #out <- setExtent(x, e, keepres = TRUE)
  newobj <- x #clearValues(x)
  xrs <- x_res(newobj)
  yrs <- y_res(newobj)
  extent <- align_extent(e, x)
  #newobj@extent <- bb
  nc <- as.integer(round( (extent[2L] - extent[1L]) / xrs ))
  if (nc < 1) {
    stop( "xmin and xmax are less than one cell apart" )
  }
  nr <- as.integer(round( (extent[4L] - extent[3L]) / yrs ) )
  if (nr < 1) {
    stop( "ymin and ymax are less than one cell apart" )
  }

  extent[2L] <- extent[1L] + nc * xrs
  extent[4L] <- extent[3L] + nr * yrs
  .grain(extent[1], extent[2], extent[3], extent[4], nc, nr)
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
