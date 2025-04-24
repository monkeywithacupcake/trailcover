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
