rm(list=ls())
source("../R/rgb2hsl.R")

colTab <- as.data.frame(
  jsonlite::fromJSON(readLines("namedColors.json")),
  stringsAsFactors=FALSE
)
colnames(colTab) <- c("code", "name")
colTab$code <- paste0("#", colTab$code)
ntcColList <- by(
  colTab, colTab$code,
  function(d){
    code <- d$code
    name <- d$name
    rgb <- col2rgb(code)
    hsl <- rgb2hsl(rgb)
    return(list("code"=code, "name"=name, "rgb"=rgb, "hsl"=hsl))
  }
)
class(ntcColList) <- "list"
save(ntcColList, file="../data/ntcColList.rda")
