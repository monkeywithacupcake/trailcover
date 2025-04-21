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
