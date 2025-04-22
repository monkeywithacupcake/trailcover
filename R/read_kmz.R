#' Read a Single KMZ into a Simple Features
#'
#'
#' @param fpath string path to gpx file
#'
#' @importFrom sf st_read
#' @importFrom magrittr "%>%"
#' @importFrom zip unzip
#'
#' @export
#'
#' @examples \dontrun{
#' ex_onf_trails <- read_kmz(trailcover_example("onf.kmz"))
#' my_kmz_trails <- read_single_gpx("path_to_my_file.kmz")
#' }
read_kmz <- function(fpath){
  tmp <- tempfile()
  zip::unzip(fpath, exdir = tmp)
  the_files <- dir(tmp, recursive = TRUE)
  kmz_sf <- sf::st_read(file.path(tmp,the_files[grep(".kml", the_files)]))
  return(kmz_sf)
}

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

#' Export as KMZ
#'
#' @param my_sf object of class sf
#' @param fname string file name ending with '.kmz'
#' @param id_var string varname you want in the kml for each row
#'
#' @export
#'
#' @examples \dontrun{
#' export_as_kmz(my_tracks, "mytracks.kmz")
#' }
export_as_kmz <- function(my_sf, fname, id_var){
  # Create a temporary KML file (replace with your desired file name)
  kml_file <- "temp.kml"
  my_sf <- my_sf[,c(id_var, "geometry")]
  names(my_sf) <- c("Name", "geometry") # required for KML
  sf::st_write(my_sf, kml_file, driver = "KML", append = FALSE)
  zip::zip(
    zipfile = fname,
    files = kml_file
  )

  # garbage handling
  file.remove(kml_file)
}
