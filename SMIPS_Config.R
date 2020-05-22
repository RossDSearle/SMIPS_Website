
#http://esoil.io/thredds/catalog/SMIPSall/catalog.html?dataset=SMIPS/SMIPSv0.5.nc

ALWAYS_DRILL <- TRUE
ENABLE_SITES <- FALSE
SHOW_WELCOME_MESSAGE <- FALSE
SHOW_GPS_BUTTON <- FALSE


THREDDSServer <- 'http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc'
SMIPSAPIServer <- 'http://esoil.io/SMIPS_API/SMIPS'
configURL <- 'http://esoil.io/SMIPS_API/SMIPS/Config?pwd=hdei-44%24%26lDDDF%401%21KLB55hds%23%2B-3hde'

TsNumDays <- 60

adminName <- 'Ross Searle'
adminEmail <- 'ross.searle@csiro.au'

defaultDisplayDate <- 2 # days before today


useWMS = T

######  CSIRO Thredds serve info  ########################
# not used but leaving here for now
originDate <- '20-11-2015'
Ausminx <- 112.905
Ausminy <-  -43.735
Ausmaxx <- 154.005
Ausmaxy <- -9.005
AusRes <- 0.01
Ausnumrows <- 3474
Ausnumcols <- 4110



unitsDF <- read.csv('ConfigFiles/units.csv')


