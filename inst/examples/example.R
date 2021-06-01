#
# other projects:

# hypertidy/cells raster grid logic # fold this one in
#
# spex
# grout Abstract raster grid tiling
# gridcol Column vectors belong raster
# qwad General raster grids
# discrete Discrete axes abstration
#
# discretize scanline adventures
#



library(vctrs)
x <- new_granule(0, 1, 0, 1, 1L, 1L)

## hypertidy/cells has a rudimentary "grain" matrix, so every row can have one of these windows (but vctrs rcrd a better framework)
as.matrix.granule <- function(x) {
  granulated:::.grain(field(x, "xmin"), field(x, "xmax"), field(x, "ymin"), field(x, "ymax"), field(x, "ncol"), field(x, "nrow"))
}

g <- as.matrix.granule(granule(-180, 180, -90, 90, 36, 18))
## grain class has helpers
dim.grain(g)
x_from_cell(g, 1:36)
y_from_cell(g, seq(36, 36 * 18, by = 36))
plot.grain(g, show = "corner")
granule(1, 1:2, 3, 4)
library(raster)
rr <- raster::raster(ncols = 60, nrows = 30)
#g <- as.matrix.granule(granule(rr))

plot(extent(rr), add = T)

granule(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax= 2)))
granule(raster::extent(0, 1, 0, 2))

granule(raster::raster())

r <- raster::raster()
raster::projection(r) <- NA
granule(sf::st_crop(stars::st_as_stars(r), sf::st_bbox(c(xmin = 100, xmax = 120, ymin = -50, ymax = 0))))

tt <- terra::rast(r)
