
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trailcover

<!-- badges: start -->
<!-- badges: end -->

The goal of trailcover is to allow you to easily compare your tracks
(gpx created by your physical walking, running, hiking, etc.) with
official trails.

## Installation

You can install the development version of trailcover from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("monkeywithacupcake/trailcover")
```

## Current State

transferring over functions from outside_oly - if you already mostly
know how to use sf and r, feel free to try those functions.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(trailcover)

# read in the example track
# showing a hike from Lake Angeles in the Olympic National Park
this_track <- read_geo(trailcover_example("Lake_Angeles.gpx"))
#> building line from gpx layer 'tracks'
```

``` r

# read in the example Olympic National Park trails
ex_onp_trails <- read_geo(trailcover_example("onp.geojson"))
#> Reading layer `ONP' from data source 
#>   `/private/var/folders/2h/k8pm08x94n73r28zjl2xplxh0000gn/T/Rtmp5RZe8u/temp_libpathac26244a4eb1/trailcover/extdata/onp.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 202 features and 2 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -124.733 ymin: 47.50167 xmax: -123.0398 ymax: 48.27514
#> Geodetic CRS:  WGS 84
```

``` r
# just focus on one trail for which we have a track
this_trail <- ex_onp_trails[ex_onp_trails$TRAIL_NAME == 'LAKE ANGELES TRAIL',]

# see the basic overlap map
map_track_v_trail(this_trail, this_track)
#> Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not
#> give correct results for longitude/latitude data
#> Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not
#> give correct results for longitude/latitude data
```

<img src="man/figures/README-example-1.png" width="100%" />
