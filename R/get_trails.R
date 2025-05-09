
#' Get Trails for a Track
#'
#' Pulls trails from Open Street Map for
#' the bounding box of the passed tracks.
#'
#' @details
#' subjet to all memory limits of Overpass
#' queries through osmdata
#'
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#'
#' @param track_sf
#'
#' @return sf
#' @export
#'
#' @examples \dontrun{
#' this_track <- read_geo(trailcover_example("Lake_Angeles.gpx"))
#' this_trails <- get_trails(this_track)
#' }
get_trails <- function(track_sf){
  this_bb <-  get_bbox(track_sf)

  this_go <- this_bb %>%
    osmdata::opq() %>%
    osmdata::add_osm_feature(key = "highway") %>%
    osmdata::osmdata_sf()

  this_trails <- this_go$osm_lines[grepl("Trail",
                                         this_go$osm_lines$name),]
  this_trails <- this_trails[c('name','geometry')]
  return(this_trails)
}
