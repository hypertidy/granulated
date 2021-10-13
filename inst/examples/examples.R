library(granulated)
## test cases, matrix, .grain, and grd
m <- volcano

## grain and grd should agree
g <- granulated:::.grain(0, ncol(volcano), 0, nrow(volcano), ncol(volcano), nrow(volcano))
x <- grd::grd_rct(volcano)

## EXTENT

extent(m)
extent(g)
extent(x)

xlim(m)
xlim(g)
xlim(x)

ylim(m)
ylim(g)
ylim(x)

x_min(m)
x_min(g)
x_min(x)

x_max(m)
x_max(g)
x_max(x)

y_min(m)
y_min(g)
y_min(x)

y_max(m)
y_max(g)
y_max(x)

## COORDINATES

x_centre(x)
y_centre(x)
x_corner(x)
y_corner(x)
coords(x)

## supply y or not
x_from_col(x)
y_from_row(x)
x_from_col(x, 2)
y_from_row(x, 2)

col_from_x(x, 10)
row_from_y(x, 10)

## CELL

cell_from_xy(x, cbind(1, 2))
cell_from_row_col(x, 1, 1)
cell_from_row(x, 1)
cell_from_col(x, 1)
col_from_cell(x)
x_from_cell(x, 1)
row_from_cell(x, 1)
y_from_cell(x, 1)
rowcol_from_cell(x, 1)
xy_from_cell(x, 1)
cell_from_extent(x, c(0, 10, 5, 6))
extent_from_cell(x, 4942)
cell_from_rowcol_combine(x, row = 1, 1)


## main

dim(x)
nrow(x)
ncol(x)
n_cell(x)
x_res(x)
y_res(x)

## workers

## this is crop, you don't get the input extent you get its discretized version
crop_grain(g, extent = c(0, 10, 5.1, 8))
set_extent(g, e = c(0, 10, 5.1, 8))  ## forget what this is atm
intersect_extent(g, c(-10, 10, 5.1, 8))
