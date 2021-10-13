
<!-- README.md is generated from README.Rmd. Please edit that file -->

# granulated

<!-- badges: start -->
<!-- badges: end -->

The goal of granulated is to provide the basic
*cell,raster,grid-abstraction* tools I’ve become accustomed to with the
raster package. I.e. there’s nothing interesting here, it’s very basic
core functionality and doesn’t work yet.

There are no examples or tests … this project has floated around for a
while but is under active development, stay tuned.

There’s a dummy class “grain” which acts as a placeholder to get all the
functionality working, since R has no idea about a raster *grid with
extent* apart from implicit functionality in `graphics::image()` and
`graphics::rasterImage()`.

The very new and experimental grd package provides a more sensible basis
for proving this on, and work is currently aimed at that. The raster
package has an uncertain future, and no other project we’re aware of
aims to capture this functionality that sits somewhere between generic
array data and bitmaps in R.

## Motivations

-   raster (or bitmap) grids in R don’t know about *spatial extent*
    although helpers exist in image(),rasterImage()
-   native rasters (as.raster(), and the internal ‘nativeRaster’) also
    don’t have plot helpers for value-scaling (bytes or raw rgb encoding
    are assumed)
-   I have become accustomed to treating raster data in the abstract,
    there is properties *dimension*, *extent*, *projection* and most
    work revolves around that - in raster package, data is stored (when
    it’s materialized) in flat column form - grd and granulated aim to
    capture that abstracted space and the missing pieces in R and we’ll
    work out the spaces in between
-   the raster package provides excellent support for almost everything
    here (!) but, it is subject to the turbulence and uncertain future
    of sp et al. 

Despite some heroic efforts (raster, stars, ggspatial, lazyraster, grd,
rayvista, …) there’s no easy *canvas* support in R, ‘here’s my domain
fill it with pixels’ and we can do that with GDAL’s warper
(e.g. `vapour::vapour_warp_raster()`) but we need these basic-er tools
to enrich the space (because otherwise we are constantly juggling
objects, formats, and plumbing when all we really need is *dimension*,
*extent*, *projection* as a simple framework). Please see
hypertidy/gdalio for some exploration of that idea.

------------------------------------------------------------------------

## Code of Conduct

Please note that the granulated project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
