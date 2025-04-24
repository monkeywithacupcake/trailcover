#' Collapse Geo
#'
#' Many geometries come to us with multiple rows representing
#' a single unique geometry. This function is designed specifically
#' with trails in mind, so it collapses to MULTILINESTRING
#'
#' @param sfdf object of class sf
#' @param id_var string naming the column to collapse on
#'
#' @importFrom sf st_cast st_length
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise ungroup mutate
#' @export
#'
#' @examples \dontrun{
#' ex_onf_trails <- read_kmz(trailcover_example("onf.kmz"))
#' ex_onf_simple <- collapse_geo(ex_onf_trails, "Name")
#' }
collapse_geo <- function(sfdf, id_var){
  sfdf <- sfdf %>% # collapse it down
    dplyr::group_by(.data[[id_var]]) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("MULTILINESTRING") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(len = as.numeric(sf::st_length(geometry)))
}
