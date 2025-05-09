#' Map Track versus Trails
#'
#' Creates a spatial plot overlying a track and a trail.
#' Allows for an underlying map that can be used to add context.
#'
#' @param trail_sf sf object representing the base layer
#' @param track_sf sf object representing the top layer
#' @param underlying_map sf object that will make the baseplot
#' @param trail_sf_name_col string the name of the column identifying the label
#' @param track_sf_name_col string the name of the column identifying the label
#' @param trail_color color defaults to 'grey70'
#' @param track_color color defaults to 'deeppink'
#'
#' @importFrom dplyr select mutate rename arrange bind_rows
#' @importFrom tidyselect any_of
#' @importFrom sf st_point_on_surface st_zm
#' @importFrom ggplot2 ggplot geom_sf aes theme theme_void scale_color_manual
#' @importFrom ggrepel geom_label_repel
#'
#' @return ggplot object
#' @export
#'
#' @examples \dontrun{
#'
#' # you want to pass at a minimum two sfs
#' ex_onp_trails <- read_geo(trailcover_example("onp.geojson"))
#' this_trail <- ex_onp_trails[ex_onp_trails$TRAIL_NAME == 'LAKE ANGELES TRAIL',]
#' this_track <- read_geo(trailcover_example("Lake_Angeles.gpx"))
#' map_track_v_trail(this_trail, this_track)
#'
#' # because the way the labels are setup, you will see warnings
#' # from ggplot for placing them on the plot
#' # you can suppress these, but only if you wrap
#' # the call with a print, something like so
#' this_map <- map_track_v_trail(this_trail, this_track)
#' suppressWarnings(print(this_map))
#'
#' }
map_track_v_trail <- function(trail_sf, track_sf,
                              underlying_map = NA,
                              trail_sf_name_col = "TRAIL_NAME",
                              track_sf_name_col = "name",
                              trail_color = 'grey70',
                              track_color = 'deeppink') {

  # make the legend
  keys <- c('trail','track')
  keys.col <- c(trail_color,track_color)
  keys.lw <- c(2,1)
  names(keys.col) <- keys
  names(keys.lw) <- keys

  # clean up the names and add keys
  my_trails <- trail_sf %>%
    dplyr::rename(lbl = tidyselect::any_of(trail_sf_name_col)) %>%
    dplyr::mutate(key = 'trail')
  my_tracks <- track_sf %>%
    dplyr::rename(lbl = tidyselect::any_of(track_sf_name_col)) %>%
    dplyr::mutate(key = 'track')
  my_tt <- dplyr::bind_rows(
    dplyr::select(my_tracks, lbl, key, geometry),
    dplyr::select(my_trails, lbl, key, geometry)
    ) %>%
    dplyr::arrange(desc(key))


  # create the map
  p <- handle_underlying_map(underlying_map = underlying_map)

  p <- p +
    ggplot2::geom_sf(data = my_tt, ggplot2::aes(color = key, linewidth = key)) +
    ggrepel::geom_label_repel(data = my_tt,
                              ggplot2::aes(label = lbl, geometry = geometry, color = key),
                              stat = "sf_coordinates",
                              min.segment.length = 10) +
    ggplot2::scale_linewidth_manual('',values = keys.lw) +
    ggplot2::scale_color_manual('',values = keys.col) +
    ggplot2::coord_sf(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  return(p)

}

