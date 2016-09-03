#' Name that color
#'
#' @param the color to be named
#'
#' @return a list with the code, the name, the RGB and HSL values of the closest
#' matching color.
#'
#' @export
ntc <- function(colors){
  toRet <- lapply(
    colors,
    function(color){
      rgbCol <- col2rgb(color)
      hslCol <- rgb2hsl(rgbCol)
      color <- rgb(t(rgbCol), maxColorValue = 255)
      ##
      if(color %in% names(ntcColList)){
        return(
          ntcColList[[color]]
        )
      }
      ##
      distToCol <- unlist(lapply(
        ntcColList,
        function(col){
          sum((rgbCol - col$rgb)^2) + sum((hslCol - col$hsl)^2)*2
        }
      ))
      return(ntcColList[[names(which.min(distToCol))]])
    }
  )
  names(toRet) <- colors
  return(toRet)
}
