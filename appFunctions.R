
logInfo <- data.frame(action=character(), time=character(), uri=character(), stringsAsFactors = F)

Logit <- function(action, url){
  
  
  newRow <- data.frame(action=action, time=now(), uri=url, stringsAsFactors = F)
  if(nrow(logInfo) >= maxLogRecords){
    ids <- seq(from=maxLogRecords, to=nrow(logInfo), by=1)
    logInfo <<- logInfo[-ids,]
  }
  logInfo <<- rbind(newRow, logInfo, stringsAsFactors = F)
  return(logInfo)
}

includeHTML2 <- function (path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # lines2 <-paste(lines, collapse = "\\r\\n")
  # l3 <- str_split(lines2, '<body>')
  # l4 <- str_split(l3[[1]][2], '</body>')
  # l5 <- l4[[1]][1]
  # l6 <- str_replace_all(l5, "[\\\\r\\\n]" , "")
  return(HTML(lines))
}

templateR <- raster(nrows=Ausnumrows, ncols=Ausnumcols, xmn=Ausminx, xmx=Ausmaxx, ymn=Ausminy, ymx=Ausmaxy, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))

getcellsForALatLon <- function(lon, lat){
  cell <- cellFromXY(templateR, cbind(c(lon), c(lat)))
  colNum <- colFromCell(templateR, cell)
  rowNum <- rowFromCell(templateR, cell)
  return(data.frame(colNum=colNum, rowNum=rowNum))
}


getThreddsDay <- function(theDate){
  d <- as.numeric(as.Date(paste(theDate), "%d-%m-%Y") - as.Date(paste( '20-11-2015'), "%d-%m-%Y"))
  return(d)
}



retrieveData <- function(url){
  
  req <- GET(url)
  stop_for_status(req)
  t1 <- content(req, 'text', encoding = 'UTF-8')
  return(t1)
}


library(httr)

getRasterFromAPI <- function(url){
  # r <- GET(url)
  # stop_for_status(r)
  # tf <- tempfile(fileext = '.tif')
  # bin <- content(r, "raw")
  #writeBin(bin, tf)
  # mr <- raster(bin)
  
  #return(mr)
  # con <- readBin(bin ,'raw')
  return(raster(url))
}

addCustomLegend <- function(
  map, html, position = c("topright", "bottomright", "bottomleft", "topleft"),
  className = "info legend", layerId = NULL, group = NULL, data = getMapData(map)
) {
    position <- match.arg(position)

    deps <- htmltools::resolveDependencies(htmltools::findDependencies(html))
    html <- as.character(html)

    map$dependencies <- c(map$dependencies, deps)
    legend <- list(html = html,  position = position, layerId = layerId, className = className, group = group)

    invokeMethod(map, data, "addCustomLegend", legend)
}