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

x_from_col(x, 1)
y_from_row(x, 1)



