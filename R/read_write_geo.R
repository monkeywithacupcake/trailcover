#' Read Geo
#'
#' Attempts to identify if your file needs special handling
#' like it is a kmz or a gpx, otherwise attempts
#' to read directly with sf::st_read()
#'
#' If you want to read in a whole folder of .gpx tracks,
#' you should use read_gpx_in_folder() instead
#'
#' @param fpath
#'
#' @importFrom sf st_read
#' @export
#'
#' @examples \dontrun{
#'
#' ex_onf_trails <- read_geo(trailcover_example("onf.kmz"))
#' ex_onp_trails <- read_geo(trailcover_example("onp.geojson"))
#' }
read_geo <- function(fpath){
  if(grepl(".gpx$", fpath)){ # assume this is a single .gpx

    sfdf <- read_single_gpx(fpath = fpath)
    return(sfdf) # exit here

  }

  if(grepl(".kmz$", fpath)){ # assume this is a .kmz

    sfdf <- read_kmz(fpath = fpath)
    return(sfdf) # exit here

  }

  # we are assuming that we are dealing with a conventional geometry here
  sfdf <- sf::st_read(fpath)
  return(sfdf)
}



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
#' Converts your simple features object into
#' a kml valid crs and then exports and zips
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
  my_sf <- sf::st_transform(my_sf, sf::st_crs("+proj=longlat +datum=WGS84"))
  names(my_sf) <- c("Name", "geometry") # required for KML
  sf::st_write(my_sf, kml_file, driver = "KML", append = FALSE)
  zip::zip(
    zipfile = fname,
    files = kml_file
  )

  # garbage handling
  file.remove(kml_file)
}


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
#' ex_track <- read_single_gpx(trailcover_example("Lake_Angeles.gpx"))
#' my_track <- read_single_gpx("path_to_my_track.gpx")
#' }
read_single_gpx <- function(fpath){
  # use track_points to get elev gain & tracks as line
  fpath <- check_fpath(fpath)
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
