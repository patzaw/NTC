#' RGB to HSL
#'
#' @param rgb a numeric vector of length 3 specifying red, green and blue in
#' that order
#'
#' @return a numeric vector of length 3 named "h", "s", "l"
#'
rgb2hsl <- function(rgb) {
  r <- rgb[1]
  g <- rgb[2]
  b <- rgb[3]
  val_max <- max(rgb)
  val_min <- min(rgb)
  h <- s <- l <- (val_max + val_min) / 2
  if (val_max == val_min){
    h <- s <- 0
  } else {
    d <- val_max - val_min
    s <- ifelse(l > 0.5, d / (2 - val_max - val_min), d / (val_max + val_min))
    if (val_max == r) { h <- (g - b) / d + (ifelse(g < b, 6, 0)) }
    if (val_max == g) { h <- (b - r) / d/ + 2 }
    if (val_max == b) { h <- (r - g) / d + 4 }
    h <- (h / 6) * 360
  }
  return(c(h=h, s=s, l=l))
}
