
library(jsonlite)

convertJSONtoDF <- function(resp){
  
  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t, stringsAsFactors = F)
  cnames<- c('DateTime', rep('x', nrow(xin)))
  
  for (i in 1:nrow(xin)) {
   
    outDF <- cbind(outDF, xin$DataStream[[i]]$v)
    cnames[i+1] <-  paste0(xin$DataType[[i]], "_", xin$UpperDepth[[i]])
    
  }
  colnames(outDF) <- cnames
  return(outDF)
}







convertJSONtoTS <- function(resp, format){
  
  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))
  
  for (i in 1:nrow(xin)) {
    outDF <- cbind(outDF, xin$DataStream[[i]]$v)
    cnames[i+1] <-  paste0(xin$DataType[[i]], "_", xin$UpperDepth[[i]])
  }
  
  colnames(outDF) <- cnames
  
  ds <- na.omit(outDF)
  d <- as.POSIXct(str_trim(ds$DateTime) , format = format)
  ts <- xts(x=ds[,-1], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))
  return(ts)
}


getResponseInfo <- function(resp){
  
  df <- fromJSON(resp)
  d <- list()
  d$SiteID <- df$SiteID[1]
  d$SiteName <- df$SiteName[1]
  d$Backend <- df$Backend[1]
  d$Units <- df$Units[1]
  d$DataType <- df$DataType[1]
  return(d)
}







