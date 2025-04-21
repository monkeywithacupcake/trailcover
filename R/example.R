#' Get path to example
#'
#' trailcover comes bundled an example gpx in `inst/extdata`
#' directory. This function allows access
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' trailcover_example()
#' trailcover_example("Lake_Angeles.gpx")
trailcover_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "trailcover"))
  } else {
    system.file("extdata", path, package = "trailcover", mustWork = TRUE)
  }
}
