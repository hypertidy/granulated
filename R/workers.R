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

