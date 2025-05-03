# get_coverage

#' Get Coverage
#'
#' @description
#' creates a sfdf representing what has been completed
#' of the trail network (big_sf) based on your tracks.
#'
#' For example, if you have done a hike and your .gpx
#' contains some portions on forest road and some trails
#' you would compare the trail network (or group of trails)
#' to your hike gpx (little_sf) and get returned to you
#' the trails with the portion your hike covered on them.
#'
#'
#' @param big_sf sf trail network map
#' @param little_sf sf trail gpx you completed
#' @param completion_tolerance numeric value you believe indicates
#' trail completion. defaults to 1, meaning 100% of official
#' trail length covered. practical completions may be lower.
#'
#' @importFrom sf st_buffer st_cast st_union st_intersection st_length
#'
#' @export
#'
#' @examples \dontrun{
#' ex_onp_trails <- read_geo(trailcover_example("onp.geojson"))
#' this_track <- read_geo(trailcover_example("Lake_Angeles.gpx"))
#' onp_tracked <- get_coverage(big_sf = ex_onp_trails,
#'                             little_sf = this_track)
#' onp_tracked$TRAIL_NAME
#' [1] "HEATHER PARK TRAIL" "LAKE ANGELES TRAIL"
#' }
#'
get_coverage <- function(big_sf, little_sf,
                         var = "TRAIL_NAME",
                         completion_tolerance = 1){
  # id trails touched more than 50 m (so not just the cross)
  # assume big_sf and little_sf geometry is linestrings
  # turn the track into a polygon 50 m wide
  my_poly <- little_sf %>%
    sf::st_buffer(50) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_union() %>% sf::st_make_valid()

  # set up bigsf to have full_len and an id
  big_sf$full_len = as.numeric(sf::st_length(big_sf$geometry))

  o <- sf::st_intersection(big_sf, my_poly)
  o$len = as.numeric(sf::st_length(o$geometry))
  o <- subset(o, len > 50)
  # and now get the portion
  o$portion <- as.numeric(o$len/o$full_len)
  o$portion = ifelse(o$portion > tolerance, 1, o$portion)
  return(o[!grepl("^full_len$", names(o))])
}

#' Get Covered Portion
#' creates a dataframe of all trails in the trail
#' network and identifies how much is completed
#'
#' @param big_sf sf trail network map
#' @param trail_coverage sf output of get_coverage()
#' @param var string variable indicating trail name
#' @param tolerance numeric value you believe indicates
#' trail completion. defaults to 1, meaning 100% of official
#' trail length covered. practical completions may be lower.
#' @returns df one row for each row of big_sf with the
#' portion covered in trail_coverage
get_covered_portion <- function(big_sf,
                                trail_coverage,
                                var = "TRAIL_NAME",
                                tolerance = 1){

  if(!"len" %in% names(big_sf)){
    big_sf$len = as.numeric(sf::st_length(big_sf$geometry))
  }
  p <- sf::st_set_geometry(big_sf, NULL)[c(var, 'len')]
  names(p)[names(p) == "len"] <- "full_len"
  p <- merge(p,
             sf::st_set_geometry(trail_coverage, NULL),
             all.x = TRUE)
  p$portion <- as.numeric(p$len/p$full_len)
  p$portion = ifelse(p$portion > tolerance, 1, p$portion)
  return(p[c(var,'portion')])
}

#' Get Covered Percent of Trail Network
#'
#' @description
#' For a given covered portion of trail, returns
#' the portion of the whole network covered.
#' If you already have the lengths, this is only saving
#' you the math.
#'
#' @details
#' Does not match up the trails, so you could just put in
#' a number or unrelated trail and get a result.
#' Usage is for things like comparing lifetime, yearly, or
#' a single multi trail adventure with total trails in
#' a trail system.
#'
#'
#' @param big_sf sf trail network map
#' @param covered_len numeric sum of length of track
#' @param covered_portion output of get_covered_portion()
#'
#' @examples \dontrun{
#' ex_onp_trails <- read_geo(trailcover_example("onp.geojson"))
#' this_track <- read_geo(trailcover_example("Lake_Angeles.gpx"))
#' onp_tracked <- get_coverage(big_sf = ex_onp_trails,
#'                             little_sf = this_track)
#'
#' get_covered_percent(big_sf = ex_onp_trails,
#'                     covered_sf = onp_tracked)
#' # same result
#' get_covered_percent(big_sf = ex_onp_trails,
#'                     covered_len = 5530.338)
#' }
get_covered_percent <- function(big_sf,
                                covered_len = 0,
                                covered_sf = NULL){

  if(covered_len == 0){
    # try to get covered_len from covered_sf
    if(!"len" %in% names(covered_sf)){
      covered_sf$len = as.numeric(sf::st_length(covered_sf$geometry))
    }
    covered_len = sum(covered_sf$len, na.rm = TRUE)
  }
  if(!"len" %in% names(big_sf)){
    big_sf$len = as.numeric(sf::st_length(big_sf$geometry))
  }
  big_len <- sum(big_sf$len, na.rm = TRUE)
  tot_p <- as.numeric(covered_len/big_len)
  print(paste("Total coverage of",
              scales::percent_format(0.01)(tot_p),
              "not including any double coverage")
  )
  return(tot_p)
}
