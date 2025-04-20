# move geojson to rda
library(geojsonio)
onf_trails <- geojson_read("data-raw/ONF.geojson", what = "sp")
#save(onf_trails, file = "data/onf_trails.rda")
usethis::use_data(onf_trails, overwrite = TRUE)

onp_trails <- geojson_read("data-raw/ONP.geojson", what = "sp")
#save(onp_data, file = "data/onp_trails.rda")
usethis::use_data(onp_trails, overwrite = TRUE)
