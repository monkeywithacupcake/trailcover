# map your map

#' Map Coverage
#' @description
#' Creates a visual representation of the portion
#' of trails covered overlaying the trail network
#'
#' @importFrom ggplot2 ggplot geom_sf aes theme theme_void scale_color_manual scale_color_viridis_c
#' @importFrom scales percent_format
#'
#' @param trail_network sf the trail network data
#' same data as used as big_sf in the other functions.
#' @param covered_sf sf output of get_coverage()
#' @param underlying_map sf that might represent the
#' area around your trail network.
#' @param title string title you want for your map.
#' defaults to "Your Completed Trail Network"
#' @param caption string caption you want for your map.
#' defaults to "created with R/trailcover"
#' @export
#'
#' @examples
#' ex_onp_trails <- read_geo(trailcover_example("onp.geojson"))
#' this_track <- read_geo(trailcover_example("Lake_Angeles.gpx"))
#' onp_tracked <- get_coverage(big_sf = ex_onp_trails,
#'                             little_sf = this_track)
#' map_coverage(trail_network = ex_onp_trails,
#'              covered_sf = onp_tracked)
map_coverage <- function(trail_network,
                         covered_sf,
                         underlying_map = NA,
                         title = "Your Completed Trail Network",
                         caption = "created with R/trailcover"){

  if(!"portion" %in% names(covered_sf)){
    covered_sf <- get_coverage(big_sf = trail_network,
                               little_sf = covered_sf)
  }
  trails_w_color <- covered_sf # now we have portion to color

  tot_p <- get_covered_percent(big_sf = trail_network,
                               covered_sf = covered_sf)

  p <- handle_underlying_map(underlying_map = underlying_map)
  p <- p +
    ggplot2::geom_sf(data = trail_network, color = "grey80") +
    ggplot2::geom_sf(data = trails_w_color,
                     ggplot2::aes(color = portion)) +
    ggplot2::scale_color_viridis_c(option = "turbo",
                          na.value = "grey80", direction = -1,
                          breaks = 0.2*0:5,
                          labels = scales::percent(0.2*0:5) ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = title,
                  subtitle = paste(
                    scales::percent_format(0.01)(tot_p),
                    "covered"),
                  color = "Portion of Trail Covered",
                  caption = caption
    )
  return(p)
}
