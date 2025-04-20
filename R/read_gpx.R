# read all the .gpx in a folder

#' Read a Single GPX into a Simple Features
#' Uses track_points and/or tracks layers
#' Attempts to capture total elevation gain (meters)
#'
#' @param fpath string path to gpx file
#'
#' @importFrom sf read_sf st_cast st_layers
#' @importFrom magrittr "%>%"
#' @importFrom dplyr summarise ungroup
#'
#' @return object of class sf
#' @export
#'
#' @examples \dontrun{
#' my_track <- read_single_gpx("path_to_my_track.gpx")
#' }
read_single_gpx <- function(fpath){
  # use track_points to get elev gain & tracks as line
  name <- gsub("\\s+"," ",trimws(gsub(".gpx","",basename(fpath))))
  gain <- NA_real_
  layers_avail <- sf::st_layers(fpath)
  if('track_points' %in% layers_avail$name){
    gpx <- sf::read_sf(fpath, layer = 'track_points')
    # get the total elevation gained
    if("ele" %in% colnames(gpx)){ gain <- get_elev_gain(gpx$ele)
    }
    if(!'tracks' %in% layers_avail$name) { # build it from this
      message("building line from gpx layer 'track_points'")
      gpx <- gpx %>%
        dplyr::summarise(do_union=FALSE) %>%
        sf::st_cast("MULTILINESTRING") %>%
        dplyr::ungroup()
    }
  }
  if('tracks' %in% layers_avail$name){
    message("building line from gpx layer 'tracks'")
    gpx <- sf::read_sf(fpath, layer = 'tracks')
    gpx <- gpx[,c("geometry")] # remove extra columns
  }

  gpx$name <- name
  gpx$gain <- gain
  return(gpx)
}


#' Read GPX in Folder
#'
#' Reads all of the GPX in a folder into a simple features dataframe
#'
#' @param folder_name string path to a folder containing .gpx files
#' @param crs one of (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value (numeric), or (iii) an object of class crs
#'
#' @importFrom sf st_crs st_transform
#' @importFrom purrr map
#'
#' @return object of class sf
#' @export
#'
#' @examples \dontrun{
#' my_tracks <- read_gpx_in_folder("path_to_my_tracks")
#' }
read_gpx_in_folder <- function(folder_name, crs = 0){

  myfiles = list.files(path=folder_name, pattern=".+\\.gpx", full.names=TRUE)
  if(length(myfiles) == 0){
    warning("no .gpx files found")
  }

  if(crs == 0){ # then get crs from first file
    crs <- sf::st_crs(sf::read_sf(myfiles[[1]]))
    message(paste("crs set to crs of first file,", crs))
  }

  sfdf <- myfiles |>
    purrr::map(read_single_gpx) |>  # each of these will have their own crs
    bind_rows()
  sfdf <- sf::st_transform(sfdf, crs) # make sure they are all the same
  return(sfdf)
}

#' internal function
get_elev_gain <- function(elev_vector){
  ev <- as.numeric(elev_vector)
  j <- diff(ev)
  total_increase <- sum(j[j > 0])
  return(total_increase)
}
