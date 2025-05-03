check_fpath <- function(path) {
  if (!is_string(path)) {
    stop("`path` must be a string", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("`path` does not exist: ", sQuote(path), call. = FALSE)
  }
  path
}


is_string <- function(x) {
  length(x) == 1 && is.character(x)
}

handle_underlying_map <- function(underlying_map = NA){
  if(!inherits(underlying_map, "logical")){ # something was passed
    if(!inherits(underlying_map,"sf")){
      warning("underlying_map is not an sf object and is dropped")
      underlying_map <- NA # reset it to nothing
    }
  }
  p <- ggplot2::ggplot()

  if(!inherits(underlying_map, "logical")){
    p <- p + ggplot2::geom_sf(data = underlying_map)
  }
  return(p)
}
