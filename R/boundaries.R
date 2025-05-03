#' Get bounding Box
#'
#' Identifies the largest bounding box that covers
#' all geometries in list
#'
#' @details
#' You do not need this unless you want an underlying map.
#' For example, you are plotting a trail and a track and
#' want to situate it over roads or other features but still
#' have the bounding box focused on the trail.
#'
#'
#' @importFrom sf st_crs st_bbox
#' @return bounding box
#' @param geolist is a single geometry or a list of geometries
#'
#' @export
get_bbox <- function(geolist){
  if(class(geolist)[[1]] != "list"){ geolist <- list(geolist)}
  bx <- lapply(geolist, st_bbox)
  bbox <- sf::st_bbox(c(xmin = min(unlist(lapply(bx, function(x) x[1])) ),
                        xmax = max(unlist(lapply(bx, function(x) x[3])) ),
                        ymax = max(unlist(lapply(bx, function(x) x[4])) ),
                        ymin = min(unlist(lapply(bx, function(x) x[2])) )
  ),
  crs = sf::st_crs(geolist[[1]])
  )
  return(bbox)
}

#' Bounding Box Shrink by Factor
#'
#' Shrinks a bounding box by a factor.
#'
#' @param bb bounding box
#' @param e numeric, defaults to 1.
#' Negative value will grow the bounding box.
#'
#' @importFrom sf st_crs st_bbox
#' @return bounding box
#' @export
#'
#' @examples \dontrun{
#' # uses example data in the sf package
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc_bbox <- sf::st_bbox(nc)
#' cropped_bbox <- bb_shrink(bb = nc_bbox, e = 0.7)
#' }
bb_shrink <- function(bb, e = 1) {
  stopifnot(inherits(bb, "bbox"))
  dx = diff(bb[c("xmin", "xmax")])
  dy = diff(bb[c("ymin", "ymax")])
  x1 = bb["xmin"] + e * dx
  x2 = bb["xmax"] - e * dx
  y1 = bb["ymin"] + e * dy
  y2 = bb["ymax"] - e * dy
  sf::st_bbox(setNames(c(min(x1, x2),
                     min(y1, y2),
                     max(x1, x2),
                     max(y1, y2)),
                     c("xmin", "ymin", "xmax", "ymax")),
          crs = sf::st_crs(bb))
}

#' Crop to Bounding Box
#'
#' Crops a simple feature into a bounding box.
#'
#' @details
#' If the bb bounding box is larger than sfdf
#' sfdf is returned unchanged
#'
#' @param sfdf object of class sf
#' @param bb bounding box
#'
#' @importFrom sf st_crs st_crop st_transform
#' @return cropped sfdf
#' @export
#'
#' @examples \dontrun{
#' # uses example data in the sf package
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc_bbox <- sf::st_bbox(nc)
#' cropped_bbox <- bb_shrink(bb = nc_bbox, e = 0.7)
#' cnc <- get_cropped_to_bb(sfdf = nc, bb = cropped_bbox)
#' ggplot() + geom_sf(data = nc, color = "blue") + geom_sf(data = cnc, fill = "pink")
#' }
get_cropped_to_bb <- function(sfdf, bb){
  o <- sf::st_crop(sf::st_transform(sfdf, crs = sf::st_crs(bb)), bb)
  return(o)
}
