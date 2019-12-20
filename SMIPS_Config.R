
#http://esoil.io/thredds/catalog/SMIPSall/catalog.html?dataset=SMIPS/SMIPSv0.5.nc


debugMode <<- F
helpColour <- 'green'


useWMS=T
#http://esoil.io/thredds/wms/SMIPSall/SMIPSv0.5.nc?&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&layers=Openloop_Volumetric_SM&CRS=CRS:84&WIDTH=800&HEIGHT=600&FORMAT=image/png&bbox=112.9,-43.7,154.0,-9.0&TIME=2019-07-21T00:00:00.000Z&styles=boxfill/sm_blues&TRANSPARENT=true


# This loads required function, settings and some data tables (frames)



source('appUtils.R')
#source('SMIPSFunctions.R')
source("helpers.R")

#defaultUser <- 'Admin'
#defaultPwd <- 'ross'
defaultUser <- 'Public'
defaultPwd <- 'Public'
#defaultMap <- 'None'
defaultMap <- 'CSIRO - SMIPS-Raw'

#adminName <- 'Ross Searle'
#adminEmail <- 'ross.searle@csiro.au'

defaultDisplayDate <- 4 # days before now
maxMapDownloads <- 31

wmsnumrows <- 348
wmsnumcols <- 412


mrad <- 7

maxLogRecords <- 15

#dpnRoot = "http://esoil.io"
#mapext <- c(130,  150, -50, -15)
defaultSensor <- 'Soil Moisture'
maxPoints <- 100000

#moistureMapServer <- 'http://ternsoils.nexus.csiro.au:8080'
#SMIPS_WCS_Server <- 'http://ternsoils.nexus.csiro.au/cgi-bin/mapserv.exe?map=e:/MapServer/SMIPS/moisture.map'
sensorFederation_Server <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'

SMIPSAPIServer <- 'http://esoil.io/SMIPS_API/SMIPS'


######  CSIRO Thredds serve info  ########################
CSIRO_OpenDAP_Server <- 'http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc'
originDay = 42326
originDate <- '20-11-2015'
Ausminx <- 112.905
Ausminy <-  -43.735
Ausmaxx <- 154.005
Ausmaxy <- -9.005
AusRes <- 0.01
Ausnumrows <- 3474
Ausnumcols <- 4110


productsDF <- read.csv(paste0(deployDir, '/ConfigFiles/RasterServices_config.csv'), stringsAsFactors=F)
productsDF <- productsDF[order(productsDF$ProductOrder),]

unitsDF <- read.csv('ConfigFiles/units.csv')



StreamTypes <- vector("list", nrow(unitsDF)) 
sensorLabels <- c( 'SensorGroup','Backend','Access')
sensorTypes <- c('Soil-Moisture', 'Rainfall')