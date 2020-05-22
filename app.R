library(raster)
library(stringr)
library(shiny)
library(leaflet)
#library(leaflet.extras)
#library(htmltidy)
library(XML)
library(xml2)
library(dygraphs)
library(xts)
library(mapview)
library(leaflet.extras)
library(htmltools)
library(RCurl)
library(jsonlite)
library(shinyBS)
library(htmltools)
library(rhandsontable)
library(RColorBrewer)
library(zip)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(lubridate)
library(raster)
library(httr)

#product <- 'Openloop_Volumetric_SM'

machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discovery') {
  deployDir <<-'/srv/shiny-server/SMIPS'
} else if (machineName=="lw-94-cdc") {
  deployDir <<-'/home/som05d/CODE/SMIPS_Website'
} else {
  deployDir <<-'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/SMIPS_Website'
}

setwd(deployDir)
source('./SMIPS_Config.R')
source('./appFunctions.R')
source('./appUtils.R')
source("./helpers.R")

#productsDF <- read.csv(paste0(deployDir, '/ConfigFiles/RasterServices_config.csv'), stringsAsFactors=F)
productsDF <- fromJSON(configURL)
productsDF <- productsDF[!startsWith(productsDF$Name, "!"),]
productsDF <- productsDF[order(productsDF$ProductOrder),]
defaultMap= productsDF$Name[2]

server <- function(input, output,session) {
  

  RV <- reactiveValues()
  # RV$currentTS <- NULL
  # RV$currentSite <- NULL
  RV$currentDownloadTS <- NULL

  # RV$Usr <- defaultUser
  # RV$Pwd <- defaultPwd
  # RV$AvailableSensors <- NULL
  # RV$sensorLocs <- NULL

 # RV$AvailableMapProducts <- c( 'None', productsDF$Name)
  RV$AvailableMapProducts <- c(  productsDF$Name)
  RV$RecreateSlider <- TRUE
  RV$Log <- logInfo

  SMIPSDrillbtn <- if (isTRUE(ALWAYS_DRILL)) reactiveVal('SmipsDrillOn') else reactiveVal('SmipsDrillOff')



  
  output$wWCS <- renderUI({
    
    #req( input$moistureMap)
    
    pres <- 0.0008333333333467680612
    #rows <- (input$moistureMap_bounds$north - input$wMainMap_bounds$south) / pres
    #cols <- (input$moistureMap_bounds$east - input$wMainMap_bounds$west) / pres
    #pixels <- rows*cols
    
    shiny::tags$html()
    url <-  'http://esoil.io/thredds/wcs/SMIPSall/SMIPSv0.5.nc?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&FORMAT=GeoTIFF_Float&COVERAGE=Blended_Precipitation&CRS=OGC:CRS84&TIME=2020-01-01T00:00:00Z'
    
    
      #res <- paste0('&RESX=', pres, '&RESY=', pres)
      bbox <- paste0(input$moistureMap_bounds$west, ',', input$moistureMap_bounds$south, ',',input$moistureMap_bounds$east, ',',input$moistureMap_bounds$north )
      print(input$moistureMap_bounds)
     # tags$a(paste0("Download ", " Current Extent"), href=paste0(url, '&BBOX=', bbox), html(' download="something.txt"'))
      
     #HTML( paste0('<a href=', url,  ' download="w3logohhhh.tif">Test</a>') )
     HTML( paste0('<a href="https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png" download>
       <img src="https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png" alt="W3Schools" width="104" height="142">
       </a>'))
    # }else{
    #   tags$html("")
    # }
  })
  
  # observe({
  #   req(RV$Usr,RV$Pwd)
  # 
  #   url <- paste0(sensorFederation_Server,"/getSensorLocations?sensortype=Soil-Moisture&usr=",RV$Usr, '&pwd=',RV$Pwd )
  #   d <- getURL(url)
  #   sensorLocs <- fromJSON(d)
  #   RV$sensorLocs <- sensorLocs
  #   
  #   RV$Log <-Logit('Get a list of the available sensors', url)
  #   
  #  
  # })
  

# 
#   Login <- function(failed = FALSE) {
#     modalDialog(title = "SMIPS Login", size = 's',
#                 textInput("usrID", "User Name", placeholder = 'Public'),
#                 passwordInput("usrPwd", "Password", placeholder = 'Public'),
#                 HTML(paste0('For Information about obtaining a login contact <a href=mailto:', adminEmail, '?Subject=SMIPS&nbsp;Access>', adminName, '</a>')), 
#                 tags$a(href=paste0("mailto:", adminEmail, "?Subject=SMIPS%20access%20Request%20", adminName)),
#                 if (failed)
#                   div(tags$b("Login failed", style = "color: red;")),
#                 
#                 footer = tagList(
#                   modalButton("Cancel"),
#                   actionButton("ok", "Login")
#                 )
#     )
#   }
#   
#   # Show modal when button is clicked.
#   observeEvent(input$showLogin, {
#     showModal(Login())
#   })
#   
#   observeEvent(input$ok, {
# 
#     if (!is.null(input$usrID) && nzchar(input$usrID) && !is.null(input$usrPwd) && nzchar(input$usrPwd) ) {
#      
#       req(input$usrID,input$usrPwd)
# 
#       RV$Usr <- input$usrID
#       RV$Pwd <- input$usrPwd
#       loginResult <-T # smipsLogin(usr = input$usrID, pwd = input$usrPwd )
#       if(loginResult){
#             # if(RV$Usr == 'Public'){
#             #   
#             #   RV$AvailableMapProducts <- c( 'None', productsDF[productsDF$Organisation=='CSIRO',2])
#             # }else{
#             #   RV$AvailableMapProducts <- c( 'None', productsDF[productsDF$Name,2])
#             # }
#         removeModal()
#         
#       }else{
#         showModal(Login(failed = TRUE))
#       }
#      
#      } else {
#        showModal(Login(failed = TRUE))
#     }
#   })
#   
#   # Display information about selected data
#   output$loginStatus <- renderText({
#     if (!is.null(RV$Usr)){
#       paste0(RV$Usr)
#     }
#   })
  
  
  
  
  
  ################### Render the Map  #################


  observe({
    req(input$SMIPSDrillbtn)
    if (!is.null(input$SMIPSDrillbtn)) {
      SMIPSDrillbtn(input$SMIPSDrillbtn)
    } })
  
  output$moistureMap <- renderLeaflet({

      #req(RV$sensorLocs)
      #legendurl <- paste0(wmsServer, '?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetLegendGraphic&LAYER=', product, '&FORMAT=image%2Fpng')

      lf <- leaflet() %>% clearMarkers() %>%
      addTiles(group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      setView(lng = 135, lat = -28, zoom = 4)
        # addMouseCoordinates(style = "basic") %>%
          # addEasyButton(
          #   easyButton(
          #     icon = 'fa-globe',
          #     title="Zoom to Full Extent",
          #     id='b2',
          #     onClick=JS("function(btn, map){map.setView([-28, 135], 4); }"))
          # )%>%
          #
         if (isTRUE(SHOW_GPS_BUTTON)) {
          lf <- addControlGPS(lf, options = gpsOptions(setView=T, autoCenter=T, maxZoom=10))
         }
         sbo <- scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)
          lf <- addLayersControl(lf,
            baseGroups = c("Map", "Satelite Image"),
            overlayGroups = ifelse(isTRUE(ENABLE_SITES), c("Moisture Maps", "All Sensors"), c("Moisture Maps")),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addScaleBar(position = "bottomleft", options = sbo)


          lf <- if (isTRUE(ALWAYS_DRILL)) lf else addEasyButtonBar(lf, position = "topright" ,
            easyButton( position = "topright" ,
              states = list(
                easyButtonState(
                  stateName="sOff",
                  #icon="fa-check",
                  #icon=htmltools::span(class = "star", htmltools::HTML('<img src="Buttons/drill.png"></font>')),
                  icon=htmltools::span(class = "star", htmltools::HTML('<font color="gray">Get&nbsp;Timeseries&nbsp;<b>Off</b></font>')),
                  title="Click to turn on drilling of a pixel on the map to return a timeseries of soil moisture values",
                  onClick = JS("
              function(btn, map) {
                btn.state('sOn');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');
               $('.leaflet-container').css('cursor','crosshair');
              }")
                ),
                easyButtonState(
                  stateName="sOn",
                  icon=htmltools::span(class = "star", htmltools::HTML('<font color="green">Get&nbsp;Timeseries&nbsp;<b>On</b></font>')),
                  title="Click to turn off drilling of a pixel on the map to return a timeseries of soil moisture values",
                  onClick = JS("
              function(btn, map) {
                btn.state('sOff');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff');
                $('.leaflet-container').css('cursor','');
              }")
                )
              )
            ))
          lf <- if (isTRUE(ALWAYS_DRILL))
          htmlwidgets::onRender(lf, "
          function(el, x) {
            var moistureMap = this;
            window.moistureMap = moistureMap;
            $('.leaflet-container').css('cursor','crosshair');
          }")
          else
            htmlwidgets::onRender(lf, "
          function(el, x) {
            var moistureMap = this;
            window.moistureMap = moistureMap;
          }")


    })
  # if (isTRUE(ENABLE_SITES)) {
  # observe({
  #  
  #   req(RV$sensorLocs)
  # 
  #   sdf <- RV$sensorLocs
  # 
  #   labs <- lapply(seq(nrow(sdf)), function(i) {
  #     paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
  #             '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
  #             '<li>Backend : ', sdf[i, "Backend"], '</li>',
  #             '<li>Access : ', sdf[i, "Access"], '</li>',
  #             '<li>Site ID : ', sdf[i, "SiteID"], '</li>'
  #             #,'<li>Available Sensors : ',  paste(tsens, collapse = ', '), '</li>'
  #     )
  #   })
  #   
  #   #colCnt <- length(unique(sdf[,input$SensorLabel]))
  #   #colCats <- unique(sdf[,input$SensorLabel])
  #   colCnt <- length(unique(sdf[,'SensorGroup']))
  #   colCats <- unique(sdf[,'SensorGroup'])
  #   colField <- sdf[,'SensorGroup']
  #   factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
  #  print( head(sdf))
  # 
  #   proxy <- leafletProxy("moistureMap", data = RV$sensorLocs)
  #   proxy %>% clearGroup("All Sensors")
  #   #proxy %>% clearMarkers()
  #   #proxy %>% clearControls()
  # 
  #   proxy %>% addCircleMarkers(   lng = ~Longitude, lat = ~Latitude,
  #                                    label = lapply(labs, HTML),
  #                                    stroke = FALSE, 
  #                                    fillOpacity = 1,
  #                                    color = factpal(sdf[,'SensorGroup']), 
  #                                    radius = 4, 
  #                                    layerId = paste0(sdf$SiteID),
  #                                    group = "All Sensors" )  %>%
  #   leaflet::addLegend("bottomleft", pal = factpal, values = colCats,title = 'SensorGroup', group = "All Sensors")
  # })
  # }
      
  # Use a separate observer to recreate the legend as needed.
  # observe({
  # 
  #   sdf <- RV$sensorLocs
  # 
  #   colCnt <- length(unique(sdf[,input$SensorLabel]))
  #   colCats <- unique(sdf[,input$SensorLabel])
  #   colField <- sdf[,input$SensorLabel]
  #   factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
  # 
  #   proxy <- leafletProxy("moistureMap", data = RV$sensorLocs)
  #   proxy %>% clearControls()
  #   proxy %>% setView(lng = center()[1],lat = center()[2],zoom = zoom())
  #   proxy %>% leaflet::addLegend("bottomleft", pal = factpal, values = colCats,title = input$SensorLabel)
  #  })     
  #
  
  
  
  # Use a separate observer to recreate wms maps as needed
  # observe({
  #   
  #   mDate <- input$moistureMapDate
  #   wmsyear <- paste0( substr(mDate, 1, 4))
  #   rec <- productsDF[productsDF$Name==input$ProductType,][1,]
  #   org <- rec$Organisation
  #   prod <- rec$ProductCode
  #   cproduct <- paste0(org, '_', prod)
  #   trans <- input$moistureMapTrans
  #   
  #   if(input$ProductType == 'None'){
  #     proxy <- leafletProxy("moistureMap", data = RV$sensorLocs)
  #     proxy %>% clearGroup("Moisture Maps")
  #     
  #   }else{
  # 
  #       wmsurl <-  paste0('http://ternsoils.nexus.csiro.au/cgi-bin/mapserv.exe?map=e:/MapServer/SMIPS/moisture.map&VERSION=1.1.1&mDate=', mDate, '&mYear=', wmsyear)
  #   
  #       proxy <- leafletProxy("moistureMap", data = RV$sensorLocs)
  #       proxy %>% clearControls()
  #       proxy %>% setView(lng = center()[1],lat = center()[2],zoom = zoom())
  #       proxy %>% addWMSTiles(wmsurl, group= "Moisture Maps",
  #                   layers = cproduct,
  #                   options = WMSTileOptions(format = "image/png", transparent = T ,opacity = trans),
  #                   attribution = "    Soil Moisture Estimates by CSIRO")
  #   
  #   }
  #   
  # })     
  
  
  
  # Use a separate observer to recreate wms maps as needed
  observe({

    mDate <- input$moistureMapDate
    wmsyear <- paste0( substr(mDate, 1, 4))
    rec <- productsDF[productsDF$Name==input$ProductType,][1,]
    org <- rec$Organisation
    prod <- rec$ProductCode
    wmsServer <- rec$Server
    #wmsStyle <- rec$wmsStyle
    wmsVersion <- rec$wmsVersion
    attrib <- rec$Attribution
    
    trans <- input$moistureMapTrans
    if (is.null(trans)) { trans <- 1.0 } # Trans defaults to NULL if the slider is not created yet
    proxy <- leafletProxy("moistureMap", data = RV$sensorLocs) %>%
    clearGroup("Moisture Maps")
    if(input$ProductType == 'None'){
      if (RV$RecreateSlider == FALSE) {
        proxy <- removeControl(proxy,"overlay_transparency_slider")
        RV$RecreateSlider <- TRUE
      }
    }else{
      
      dbits <- str_split(mDate, '-')
      dt <- paste0(dbits[[1]][3], '-', dbits[[1]][2], '-', dbits[[1]][1])

      #proxy %>% clearControls()
      
      if(useWMS) {
        
        if(org == 'CSIRO'){
          #wmsurl <- paste0(wmsServer, '.nc?TIME=', mDate, 'T00:00:00.000Z&numcolorbands=9&colorscalerange=0,1&belowmincolor=transparent&abovemaxcolor=extend')
          wmsurl <- paste0(wmsServer, '.nc?TIME=', mDate, 'T00:00:00.000Z&belowmincolor=transparent')
          
        }else if(org == 'BoM'){
          wmsurl <- paste0(wmsServer, '/', prod, '_', wmsyear, '.nc?time=', mDate)
          
        }else if(org == 'ANU'){
          #http://dapds00.nci.org.au/thredds/wms/ub8/au/OzWALD/daily/OzWALD.daily.Ssoil.2018.nc?&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&layers=Ssoil&CRS=CRS:84&bbox=112.0,-44,154.0,-10.0&format=image/png&time=2018-06-13T00:00:00.000Z&styles=&TRANSPARENT=true&width=400&height=300&style=boxfill/anu_wald_darkblues&transparent=true
          wmsServer <- sub("<prod>", prod, wmsServer, fixed=TRUE)
          wmsServer <- sub("<year>", wmsyear, wmsServer, fixed=TRUE)
          wmsurl <- paste0(wmsServer, '.nc?time=', mDate, 'T00:00:00.000Z&colorscalerange=0,1800&belowmincolor=transparent&abovemaxcolor=extend')
        }
        print(wmsurl)
        proxy <- addWMSTiles(proxy, wmsurl, group= "Moisture Maps",
                  layers = prod,
                  #options = WMSTileOptions(format = "image/png", transparent = T, opacity = trans,  styles = wmsStyle, version = wmsVersion),
                  options = WMSTileOptions(format = "image/png", transparent = T, opacity = trans, version = wmsVersion),
                  attribution = attrib)

      }else{
        bds <- input$moistureMap_bounds
        url <- paste0(SMIPSAPIServer, '/RasterWindow?bbox=',max(bds$west, Ausminx),'%3B', min(bds$east, Ausmaxx),'%3B', max(bds$south, Ausminy),'%3B', min(bds$north, Ausmaxy) ,'&date=', dt, '&rows=',wmsnumrows,'&cols=', wmsnumcols)
        mr <- getRasterFromAPI(url)
      pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), seq(0, 1, .1),na.color = "transparent")
      pal_rev <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), seq(0, 1, .1),na.color = "transparent", reverse = TRUE)

      proxy <- addRasterImage(proxy, mr, colors = pal, opacity = 0.8, group = "Moisture Maps") %>%
      leaflet::addLegend(pal = pal_rev, values = seq(0, 1, .1), title = "Moisture Map", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      }
      if (RV$RecreateSlider == TRUE) {
        sliderCtrl <- div(id = "slider_move_catcher", sliderInput("moistureMapTrans", width= 220, label = HTML('<div style="width: 200px; position: absolute; margin: 0px auto -70px 70px;">Overlay Opacity</div>'), ticks = FALSE, min = 0.0,  max = 1.0, value = trans, animate = FALSE),
                          onmouseover = "window.moistureMap.dragging.disable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff'); $('.leaflet-container').css('cursor','');", onmouseout = "window.moistureMap.dragging.enable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');$('.leaflet-container').css('cursor','crosshair');")
        proxy <- addControl(proxy, position = "bottomright", layerId = "overlay_transparency_slider", className = "info", sliderCtrl)
        RV$RecreateSlider <- FALSE
      }
      #proxy %>% leaflet::addLegend(pal = pal, values = c("Wet", 'Dry'), title = "Moisture Map")
      

      
      # proxy %>% addWMSTiles(wmsurl, group= "Moisture Maps",
      #                       layers = cproduct,
      #                       options = WMSTileOptions(format = "image/png", transparent = T ,opacity = trans),
      #                       attribution = "    Soil Moisture Estimates by CSIRO")
      
    }
    
  })     
      
      
  # center <- reactive({
  #   if(is.null(input$moistureMap_center)){
  #     return(c(135, -28))
  #   }else{
  #     return(input$moistureMap_center)
  #   }
  # })   
  #     
  # zoom <- reactive({
  #   ifelse(is.null(input$moistureMap_zoom),4,input$moistureMap_zoom)
  # })
  
  output$wmsLegend <- renderUI({
   if(input$ProductType != 'None'){
    rec <- productsDF[productsDF$Name==input$ProductType,][1,]
    org <- rec$Organisation
    prod <- rec$ProductCode
    wmsServer <- rec$Server
    wmsStyle <- rec$wmsStyle
    wmsVersion <- rec$wmsVersion
     if (org == "CSIRO") {
       wmsServer <- paste0(wmsServer, ".nc")
       wmsStyle <- "sm_blues"
     } else {
       parts <- strsplit(wmsStyle, '/', fixed=TRUE)[[1]]
       print(parts)
       wmsStyle <- parts[length(parts)]
     }
     print(wmsStyle)
     imageLoc <- paste0(wmsServer, "?VERSION=", wmsVersion, "&SERVICE=WMS&REQUEST=GetLegendGraphic&COLORBARONLY=true&WIDTH=10&HEIGHT=650&LAYER=",prod,"&PALETTE=",wmsStyle,"&FORMAT=image%2Fpng")
     print(imageLoc)

    tags$img(src = imageLoc)
   } else {
    tags$img(src = '')
   }
  })
  # output$wmsLegend <- renderImage({
  #   if(input$ProductType != 'None'){
  #
  #     # rec <- productsDF[productsDF$Name==input$ProductType,][1,]
  #     #  org <- rec$Organisation
  #     #  prod <- rec$ProductCode
  #     #  cproduct <- paste0(org, '_', prod)
  #     # legendurl <- paste0(deployDir, '/www/Legends/','CSIRO_Volumetric-Moisture', '_Legend.png')
  #
  #     legendurl <- 'http://esoil.io/thredds/wms/SMIPSall/SMIPSv0.5.nc?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetLegendGraphic&LAYER=Openloop_Wetness_Index&FORMAT=image%2Fpng'
  #
  #     #tf <- tempfile(fileext = '.png')
  #     download.file(legendurl, destfile = '/tmp/a.png', mode = 'wb')
  #     # legendurl <- 'http://dapds00.nci.org.au/thredds/wms/ub8/au/OzWALD/daily/OzWALD.daily.Ssoil.2017.nc?REQUEST=GetLegendGraphic&LAYER=Ssoil&PALETTE=nrm_temperature_reverse'
  #
  #     return(list(
  #       # src = "/srv/shiny-server/SMIPS/www/Legends/CSIRO_Wetness-Index_Legend.png",
  #       src = 'http://esoil.io/thredds/wms/SMIPSall/SMIPSv0.5.nc?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetLegendGraphic&COLORBARONLY=True&LAYER=Openloop_Wetness_Index&FORMAT=image%2Fpng',
  #       filetype = "image/png",
  #       alt = "Legend for the currently displayed map" ))
  #   }
  #   else{
  #     return(list(
  #       #src = "/srv/shiny-server/SMIPS/www/Legends/CSIRO_Wetness-Index_Legend.png",
  #       src = "",
  #       filetype = "image/png",
  #       alt = "" ))
  #    }
  #
  # }, deleteFile = FALSE)

  

  
  ################## Render the Chart from a map drill  ##################
  output$dummyDygraph <- renderDygraph({})
  output$moisitureChart1 <- renderDygraph({


    if(!is.null(RV$currentTS)){
      
      isolate({

        maxVal <- max(RV$currentTS)
     
        dygraph(RV$currentTS ,  main = paste0(RV$currentSiteInfo$DataType, '  (',RV$currentSiteInfo$Units, ') - ', RV$currentSiteInfo$SiteName, ' - ', RV$currentSiteInfo$Owner ), ylab =  RV$currentSiteInfo$DataType) %>%
        #dySeries('Values', label = RV$currentSiteInfo$DataType) %>%
        dyAxis("y", label = RV$currentSiteInfo$DataType, valueRange = c(0, maxVal)) %>%
        dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
        dyRangeSelector()
      
      })
    }
  })
  output$showDyGraph <- reactive({ifelse(is.null(RV$currentTS), "false", "true")})
    ################## Render the Info Table from a map drill  ##################
  output$SensorDrillInfoTable = renderRHandsontable({
    
    req(RV$currentSiteInfo)
    d <- RV$currentSiteInfo
    
    flds <- names(d)
    vls <- do.call( rbind, d)[,1]
    newdf <- data.frame(flds, vls, row.names=NULL, stringsAsFactors = F)
    
    

    if(nrow(newdf) > 0){
      colnames(newdf) <- c('.', 'Sensor Information') 
      rhandsontable(newdf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F, width = 850) %>%
       hot_cols(colWidths= c(100, 750))
    }else{
      return(NULL)
  }
  })

  

  ################  Get data from Clicking on a sensor  #################
#  observe({
#    click<-input$moistureMap_marker_click
#    if(is.null(click))
#      return()
#    
#    RV$currentTS <- NULL
#    
#    sid <- click$id
#    
#      isolate({
#        DataType <- input$SelectedStreamType
#      })
#      
#      print('sensordrill')
#      
#      shinyBS::createAlert(session, "progressAlert", "chartAlert", title = "", content = paste0("<div id='zs1' ><img src=spinner.gif> Retrieving ", DataType ," data for ", sid, " .....</div>"), append = FALSE)
#      
#      minDateShiny <- input$moistureDateRange[[1]][1]
#      maxDateShiny  <- input$moistureDateRange[[2]][1]
#      sd <- str_split(minDateShiny, '-')
#      isoSDate = paste0(sd[[1]][1], '-', sd[[1]][2], '-', sd[[1]][3], 'T00:00:00')
#      ed <- str_split(maxDateShiny, '-')
#      isoEDate = paste0(ed[[1]][1], '-', ed[[1]][2], '-', ed[[1]][3], 'T23:59:59')
#      
#      
#      url <- paste0(sensorFederation_Server, "/getSensorDataStreams?siteid=", sid,"&sensortype=", DataType,"&startdate=", isoSDate, '&enddate=', isoEDate, "&aggperiod=days")
#      
#      RV$Log <-Logit('Get timeseries data from clicking on a sensor', url)
#      stnsRaw <- getURL(paste0(url))
#      
#      if(!grepl('error', stnsRaw, ignore.case = F)){
#        
#        ts <- convertJSONtoTS(stnsRaw, "%Y-%m-%d %H:%M:%S")
# 
#        if(nrow(ts) > 0){
#          RV$currentTS <- ts
#          
#          url <- paste0(sensorFederation_Server, '/getSensorLocations?sensortype=', DataType, '&siteid=', sid, '&usr=',RV$Usr, '&pwd=',RV$Pwd)
#          d <- getURL(url)
#          SI <- fromJSON(d)
#          
#          #curl -X GET "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?siteid=15117&sensortype=Soil-Moisture" 
#          url2 <- paste0(sensorFederation_Server, '/getSensorInfo?sensortype=', DataType, '&siteid=', sid, '&usr=',RV$Usr, '&pwd=',RV$Pwd)
#          RV$Log <-Logit('Get metadata for a specific sensor', url)
#          d2 <- getURL(url2)
#          SI2 <- fromJSON(d2)
#          
#          sli <- list()
#          
#          sli$SiteID <- SI$SiteID[1]
#          sli$SiteName <- SI$SiteName[1]
#          sli$Provider <- SI$Provider [1]
#          sli$Backend <- SI$Backend[1]
#          sli$Access  <- SI$Access [1]
#          sli$Longitude <- SI$Longitude[1]
#          sli$Latitude  <- SI$Latitude [1]
#          sli$Active  <- SI$Active [1]
#          sli$Owner <- SI$Owner[1]
#          sli$Contact <- SI$Contact[1]
#          sli$ProviderURL <- SI$ProviderURL[1]
#          sli$StartDate <- SI$StartDate[1]
#          sli$EndDate <- SI$EndDate[1]
#          #sli$Description <- SI$Description[1]
#          #sli$Units <- SI2[1,]$Units
#          sli$Description <- SI$Description[1]
#          
#          sli$Units <- SI2[1,]$Units
#          sli$DataType <- SI2[1,]$DataType
#          sli$SensorNames <- paste(SI2$SensorName, collapse = '; ')
#          
#          RV$currentSiteInfo <- sli
# 
#          
#        }else{
#          RV$currentTS <- NULL
#          RV$currentSiteInfo <- NULL
#        }
#      }
#      else{
#        RV$currentTS <- NULL
#        RV$currentSiteInfo <- NULL
#      }
#      shinyBS::closeAlert(session, "chartAlert")
# })
  
  
  
  ################  Get data from drilling a map pixel ##########
  observeEvent(input$moistureMap_click, {
    
    click <- input$moistureMap_click

    if(is.null(click))
      return()

    req(SMIPSDrillbtn)
    if(SMIPSDrillbtn() == 'SmipsDrillOn')
    
    if(input$ProductType != 'None'){
      
      cproduct <- productsDF[productsDF$Name==input$ProductType,]$ProductCode

      minDateShiny <- input$moistureDateRange[[1]][1]
      maxDateShiny  <- input$moistureDateRange[[2]][1]
      sd <- str_split(minDateShiny, '-')
      minDate <- paste0(sd[[1]][3], '-', sd[[1]][2], '-', sd[[1]][1])
      ed <- str_split(maxDateShiny, '-')
      maxDate <- paste0(ed[[1]][3], '-', ed[[1]][2], '-', ed[[1]][1])

     shinyBS::createAlert(session, "progressAlert", "drillingAlertInstance", title = "", content = paste0("<img src=wait.gif>Retrieving timeseries data from SMIPS at ",click$lng, " ", click$lat ), append = FALSE)
     urlSMIPS <- paste0('http://esoil.io/SMIPS_API/SMIPS/TimeSeries?product=', input$ProductType ,'&longitude=',click$lng ,'&latitude=', click$lat ,'&sdate=', minDate ,'&edate=', maxDate)
     
     print(urlSMIPS)
     resp <- getURL(URLencode(urlSMIPS))
     print(resp)
      is_error <- grepl('"error":', substr(resp, 1, 9), fixed=TRUE)
      if (isTRUE(is_error))
        return()
     print('smipsdrill')
      RV$Log <- Logit('Get timeseries data from drilling a map pixel',  urlSMIPS)

      ts <- convertJSONtoTS(resp, "%Y-%m-%dT%H:%M:%S")

      SI <- fromJSON(resp)

      if(!is.null(SI) > 0){
        
        RV$currentTS <- ts
        
        sli <- list()
        sli$SiteID <- SI$SiteID[1]
        sli$SiteName <- SI$SiteName[1]
        sli$Provider <- SI$Provider [1]
        sli$Backend <- SI$Backend[1]
        sli$Access  <- SI$Access [1]
        sli$Longitude <- SI$Longitude[1]
        sli$Latitude  <- SI$Latitude[1]
        sli$Active  <- "True"
        sli$Owner <- SI$Provider [1]
        sli$Contact <- SI$Contact[1]
        sli$ProviderURL <- urlSMIPS
        sli$StartDate <- originDate
        sli$EndDate <- format(Sys.Date(), format = "%d-%m-%Y")
        sli$Units <- SI$Units
        sli$Description <- SI$Description[1]
        sli$DataType <- 'Modelled Soil-Moisture'

        RV$currentSiteInfo <- sli
        
       }else{
        RV$currentSiteInfo <- NULL
      }

      shinyBS::closeAlert(session, "drillingAlertInstance")
    }
      
    
  })
  
  
  
  ################$  Update List Choices   ###############################
  # update the MapProduct List choices depending on login  
  observe({
    updateSelectInput(session, "ProductType", choices =  RV$AvailableMapProducts, selected = defaultMap )
    updateSelectInput(session, "MapProductsListForDownload", choices =  RV$AvailableMapProducts[-1] )
  })
  
  # update the site SelectedSiteDataDownload  list  
  observe({
    req(RV$sensorLocs)
       updateSelectInput(session, "SelectedSiteDataDownload",choices = RV$sensorLocs$SiteName )
  })
  

  
  # update the MapYearsForDownload selection  list 
  observe({
    #updateSelectInput(session, "MapYearsForDownload", choices =  seq(2016, format(Sys.Date(), "%Y")) )
  })

  # update the MapMonthsForDownloadlist 
  observe({
    
    # if(as.numeric(input$MapYearsForDownload < as.numeric(format(Sys.Date(), "%Y")))) {
    #   updateSelectInput(session, "MapMonthsForDownload", choices =  str_pad(seq(1, 12), 2, pad = "0") )
    # }else{
    #   updateSelectInput(session, "MapMonthsForDownload", choices =  str_pad(seq(1, format(Sys.Date(), "%m")), 2, pad = "0" ))
    # }
    
  })
  
  
  
  
  
 
  
  # observeEvent(input$StreamTypes, {
  #   
  #  
  #   sdf <- StreamTypes[[input$StreamTypes]]
  # 
  #   pal <- colorFactor(topo.colors(length(unique(sdf$organisation))), domain =  sdf$organisation)
  # 
  #   coordinates(sdf) <-  ~lon+lat
  #   crs(sdf) <- CRS("+proj=longlat +datum=WGS84")
  # 
  #   n <- nrow(sdf@data)
  #   b1 <- sdf@data$org[1:n]
  #   b2 <-sdf@data$variable[1:n]
  # 
  # })
  # 
  
  
  observeEvent(input$init, {
    if (input$ProductType == 'None') {
      
      #shinyalert("Oops!", "You don't have any maps displayed. You need to select a map to display before you can download it.", type = "error")
      showModal(
        modalDialog(
          title = 'Oops!',
          p("You don't have any maps displayed. You need to select a map to display before you can download it.")
        )
      )
    } else {
      shinyjs::runjs("document.getElementById('downloadRasterData').click();")
    }
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(data, file)
  #   }
  # )
  
  output$downloadRasterData <- downloadHandler(
    
   # check <- function(){shinyalert("Oops!", "Please provide a numeric value for the Transformation.", type = "error")},
    
    filename = function() {
      
      # shinyalert("Oops!", "Please provide a numeric value for the Transformation.", type = "error")
      # 
       product <- input$ProductType
       mDate <- input$moistureMapDate
      # yr <- paste0( substr(mDate, 1, 4))
      # prod <- productsDF[productsDF$Name==product,]
      #  rname <- paste0(prod$Organisation, '_', prod$ProductCode, '_', mDate , '.tif')
      #  paste0(rname)
       
       paste('test.tif')
       
    },
    content = function(file) {
      
      if(input$ProductType != 'None'){
        # product <- input$ProductType
        # mDate <- input$moistureMapDate
        # yr <- paste0( substr(mDate, 1, 4))
        # prod <- productsDF[productsDF$Name==product,]
        # 
        url <-  'http://esoil.io/thredds/wcs/SMIPSall/SMIPSv0.5.nc?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&FORMAT=GeoTIFF_Float&COVERAGE=Blended_Precipitation&CRS=OGC:CRS84&TIME=2020-01-01T00:00:00Z'
       # url <- 'http://esoil.io/SMIPS_API/SMIPS/Raster?date=01-01-2019&product=SMIPS-RawIndex&resFactor=50'
        
       # rname <- paste0(prod$Organisation, '_', prod$ProductCode, '_', mDate , '.tif')
        #fullname <- paste(SMIPSDataRoot , prod$Organisation, prod$ProductCode, 'Final', yr, rname,   sep='/')
        url <-  'http://esoil.io/thredds/wcs/SMIPSall/SMIPSv0.5.nc?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&FORMAT=GeoTIFF_Float&COVERAGE=Blended_Precipitation&CRS=OGC:CRS84&TIME=2020-01-01T00:00:00Z'
        
        
        #res <- paste0('&RESX=', pres, '&RESY=', pres)
        bbox <- paste0(input$moistureMap_bounds$west, ',', input$moistureMap_bounds$south, ',',input$moistureMap_bounds$east, ',',input$moistureMap_bounds$north )
        
        print('starting download....')

        #writeRaster(r, file, overwrite=T)
         #writeRaster(r, file)
        print(url)
        outFile <- paste0(tempfile(), '.tif')
        download.file(URLencode(paste0(url, '&BBOX=', bbox)), outFile, mode = 'wb', quiet = T)
        file.copy(outFile, file)
      }else{
        
        shinyalert("Oops!", "Please provide a numeric value for the Transformation.", type = "error")
        NULL
      }
      
    }
  )
  
  
  output$urlText <- renderText({
    paste("URL ", RV$currentURL)
  })
 
  
 
  
 
  
  
  
# generate the Map Downloads Table  
    output$MapDownloadsTable  = renderRHandsontable({
      
          rec <- productsDF[productsDF$Name==input$MapProductsListForDownload,][1,]
          org <- rec$Organisation
          prod <- rec$ProductCode
          yr <- input$MapYearsForDownload
      
          fRoot <- paste0(SMIPSDataRoot, '/', org, '/' , prod, '/Final/', input$MapYearsForDownload)
          fs <- list.files(fRoot, full.names = T)
          fs2 <- fs[which(str_detect(fs, paste0('-', input$MapMonthsForDownload, '-')))]
          
          DF <- data.frame(Download=F, FileNames=basename(fs2), stringsAsFactors = FALSE)
          RV$AvailableMapFile <- DF
          
          f <- function(s) strsplit(s, "_")[[1]][3]
          dates <- sapply(DF$FileNames,  f)
          dates <- str_replace(dates, '.tif', '')
          f2 <- function(s) strsplit(s, "-")[[1]][1]
          yrs <- sapply(dates,  f2)
          
          wcsurls <- paste0( SMIPS_WCS_Server, '&SERVICE=WCS&VERSION=2.0.0&REQUEST=GetCoverage&COVERAGEID=', org, '_', prod, '&FORMAT=image/tiff&mYear=', yrs, '&mDate=', dates, '&mFName=', org, '_',prod)
          
          DFShow <- data.frame(Download=F, FileNames=paste0('<a href=', wcsurls, '>', basename(fs2), '</a>'))
          rhandsontable(DFShow, rowHeaders = NULL, allowedTags = "<em><b><strong><a><big>" ) %>%
          hot_col(col = "FileNames", readOnly = T, renderer = 'html') %>%
          hot_col(col = "FileNames",renderer = htmlwidgets::JS("safeHtmlRenderer"))
            
  })
  
  
  # observeEvent(input$domapdown, {
  #   
  #  
  #   
  #   if (!is.null(input$MapDownloadsTable)) {
  # 
  #     selectedFiles = hot_to_r(input$MapDownloadsTable)
  #     recs <-  RV$AvailableMapFile[which(selectedFiles$Download), ]
  #     
  # 
  #     zipfile <- tempfile(fileext = ".zip")
  #     zipfile <- 'c:/temp/test.zip'
  #     origDir <- getwd()
  #     setwd(dirname(as.character(recs$File)[1]))
  #     zip::zip(zipfile, basename(as.character(recs$File)))
  #     zip::zip_list(zipfile)
  #     setwd(origDir)
  #   }
  # 
  #   session$sendCustomMessage(type = 'testmessage',message = 'Thank you for clicking')
  # })
  # 
  
  
  observe({
   
    if (!is.null(input$MapDownloadsTable)) {
      #values[["previous"]] <- isolate(values[["DF"]])
      mfDF = hot_to_r(input$MapDownloadsTable)
    } else {
        mfDF <- NULL
    }
    RV$MapFileList <- mfDF
  })
 
  output$downloadZippedMapData <- downloadHandler(
    filename = function() {
      rec <- productsDF[productsDF$Name==input$MapProductsListForDownload,][1,]
      org <- rec$Organisation
      prod <- rec$ProductCode
      yr <- input$MapYearsForDownload
      paste0(org, '_', prod, '_', yr, '.zip')
    },
    content = function(file) {
      
      if (!is.null(input$MapDownloadsTable)) {
        
        rec <- productsDF[productsDF$Name==input$MapProductsListForDownload,][1,]
        org <- rec$Organisation
        prod <- rec$ProductCode
        
        
        fRoot <- paste0(SMIPSDataRoot, '/', org, '/' , prod, '/Final/', input$MapYearsForDownload)
        
        selectedFiles = hot_to_r(input$MapDownloadsTable)
        recs <-  RV$AvailableMapFile[which(selectedFiles$Download), ]
        origDir <- getwd()
        setwd(fRoot)
        zip::zip(file, paste0( recs$FileNames))
        setwd(origDir)
      }
    }
  )
  
 
  
  
  
  output$downloadSiteTSData <- downloadHandler(
    filename = function() {
      paste(input$SelectedSiteDataDownload, ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv( RV$currentDownloadTS, file, row.names = FALSE)
    }
  )
  
  




  
  output$welcomeMessage <- renderText({"
<p>Click 'Get Timeseries' then anywhere on the map to display a modelled soil moisture timeseries chart for the selected modelled product,
    or select a sensor location marker to retrieve a measured timeseries of either soil moisture or rainfall for a location. 
    The timeseries will be displayed below the map.</p>
"})

    output$helpMessage <- renderText({"
<p>Click anywhere on the map to display a modelled soil moisture timeseries chart for the selected modelled product. The timeseries will be displayed below the map.</p>
"})
  
  
  
 


  
  # 
  # observeEvent(input$retrieveTSData, {
  # 
  #   withBusyIndicatorServer("retrieveTSData", {
  #   
  #   RV$RefreshSensorDataDownload = F
  #   sitename <- input$SelectedSiteDataDownload
  #   
  #   sid <- RV$sensorLocs[RV$sensorLocs$SiteName==sitename,1][1]
  #   DataType <- input$SelectedStreamType
  #   
  #   minDateShiny <- input$moistureDateRange[[1]][1]
  #   maxDateShiny  <- input$moistureDateRange[[2]][1]
  #   sd <- str_split(minDateShiny, '-')
  #   isoSDate <- paste0(sd[[1]][1], '-', sd[[1]][2], '-', sd[[1]][3], 'T00:00:00')
  #   ed <- str_split(maxDateShiny, '-')
  #   isoEDate <- paste0(ed[[1]][1], '-', ed[[1]][2], '-', ed[[1]][3], 'T23:59:59')
  #   
  #   url <- paste0(sensorFederation_Server, "/getSensorDataStreams?siteid=", sid,"&sensortype=", DataType,"&startdate=", isoSDate, '&enddate=', isoEDate, "&aggperiod=days")
  #   stnsRaw <- getURL(paste0(url))
  #   RV$Log <-Logit(' Get timeseries data from a specific sensor', url)
  #   
  #   if(!grepl('error', stnsRaw, ignore.case = F)){
  #   ts <- convertJSONtoDF(stnsRaw)
  #   
  #   RV$currentDownloadTS <- ts
  # 
  #   RV$RefreshSensorDataDownload <- NULL
  #   
  #   }else{
  #     msg <- fromJSON(stnsRaw)
  # 
  #     stop(paste0("There was a problem retrieving the requested sensor data - ", msg$error))
  #   }
  #   
  #   })
  #   
  # })
  
  
  output$siteTimeSeriesTable  = renderRHandsontable({
    req(RV$currentDownloadTS)
      rhandsontable(RV$currentDownloadTS,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })
  
  output$LogTable  = renderRHandsontable({
    req(RV$Log)
    rhandsontable(RV$Log,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })
  
  
  if (isTRUE(ENABLE_SITES)) {
  observe ({
    if((input$toggleSensorSettingsInfo  %% 2) == 1){
      updateActionButton(session, "toggleSensorSettingsInfo", label = "Hide Sensor Settings")
    }else{
      updateActionButton(session, "toggleSensorSettingsInfo", label = "Show Sensor Settings")
    }
  })
  }
}


ui <- navbarPage(windowTitle = 'National Soil Moisture Info', theme = 'bootstrap.css', fluid = FALSE, inverse = TRUE,
                 title = HTML('<a href="#" class="navbar-left">&nbsp;<img src="Logos/CSIRO-logo-inverted.svg" width="60" height="60"></a>&nbsp;&nbsp;<a href="#" class="navbar-left"><img src="Logos/tern1-inverted.png" height="65"></img></a><span>&nbsp;&nbsp;National Soil Moisture Info</span>'),
                 header = tagList(
  tags$head( # Start <head>
    tags$link(rel = "stylesheet", type = "text/css", href = "Styles/csiro.css"),
   # tags$style(HTML("label {  font-size: 12px; }")),
  #tags$head(tags$script(src = "message-handler.js")),
  useShinyjs(),
  tags$style(appCSS),
  tags$style('
#crosshair-button {
position: relative;
top: 10px;
left: 70px;
}')
  ), # End </head>

  # Start <body>
  # Header above panes (but below the nav)

     # div(class = 'container-fluid', fluidRow( column(12,
     #  #HTML("<p><img src='csiroLogo.png'> &nbsp;&nbsp;&nbsp;&nbsp;National Soil Moisture Information Products - PROTOTYPE ONLY</p>")
     #   h1(
     #  tags$img(src = 'Logos/TERN.png', width = '100px', height = '100px'),
     #   tags$img(src = 'Logos/CSIRO.jpg', width = '100px', height = '60px'),
     #   HTML("National Soil Moisture Information Products - PROTOTYPE") )
     # )))
                 ), # End header taglist
      # Start tab panel list:
                     tabPanel("Map View",
                          sidebarLayout(fluid = FALSE,
                            sidebarPanel(width = 3,
                              div(class = 'container-fluid',
                                  conditionalPanel('false', fluidRow(

                                column(6, actionLink("showLogin", "Login")), HTML(''),
                                column(6, htmlOutput("loginStatus")), HTML('<br><br>')

                                )),
                                fluidRow(htmlOutput("helpMessage")),
                                fluidRow(selectInput("ProductType", "Map Product Type ", c('None'), selected = defaultMap, width = "100%")),
                              #  fluidRow( helpText("Select map date below"),
                                fluidRow(dateInput('moistureMapDate', label = 'Map Date', format = 'dd-mm-yyyy', value = Sys.Date()-defaultDisplayDate, width = 130))),


                            #  selectInput("SensorLabel", "Sensor Label ", sensorLabels, selected = 'SensorGroup', width= 120),

                            fluidRow( column(12, actionButton("init", "Download Map Data", icon = icon("download")),
                            downloadButton("downloadRasterData", "Download", style = "visibility: hidden;")
                           
                            
                            )),
                             #fluidRow( downloadButton('downloadRasterData', 'Download Map Data')),
                             fluidRow( HTML('&nbsp;')),
                            fluidRow(div(style = "text-align:left;", uiOutput("wWCS"))),
                              if (isTRUE(ENABLE_SITES)) {
                             fluidRow(column(12, actionLink("toggleSensorSettingsInfo", "Sensor Settings")),

                             wellPanel(
                             conditionalPanel(

                             condition = "input.toggleSensorSettingsInfo % 2 == 1",

                             fluidRow(column(12,selectInput("SelectedStreamType", "Sensor Type", sensorTypes, selected = defaultSensor, width= 150))),
                             fluidRow(column(12, dateRangeInput('moistureDateRange',label = 'Date range : yyyy-mm-dd',start =  as.Date('2017-05-27'), end = as.Date('2017-06-29')),)))
                             )
                             )} else {
                                fluidRow(column(12, span("Drill Settings"),
                             wellPanel(fluidRow(column(12, dateRangeInput('moistureDateRange',label = 'Date range : yyyy-mm-dd',start =  as.Date('2017-05-27'), end = as.Date('2017-06-29')),)))))
                              },

                          ),
                        mainPanel( width = 9,


                      fluidRow( column(12,

                        tags$style(' .leaflet-bar button,
.leaflet-bar button:hover {
background-color: #fff;
border: none;
border-bottom: 1px solid #ccc;
width: 120px;
height: 30px;
line-height: 30px;
display: block;
text-align: left;
text-decoration: none;
color: black;
}

.leaflet-bar button {
background-position: 50% 50%;
background-repeat: no-repeat;
overflow: hidden;
display: block;
}

.leaflet-bar button:hover {
background-color: #f4f4f4;
}

.leaflet-bar button:first-of-type {
border-top-left-radius: 10px;
border-top-right-radius: 10px;
}

.leaflet-bar button:last-of-type {
border-bottom-left-radius: 10px;
border-bottom-right-radius: 10px;
border-bottom: none;
}

.leaflet-bar.disabled,
.leaflet-bar button.disabled {
cursor: default;
pointer-events: none;
opacity: .4;
}

.easy-button-button .button-state{
display: block;
width: 100%;
height: 100%;
position: relative;
}


.leaflet-touch .leaflet-bar button {
width: 120px;
height: 30px;
line-height: 30px;

.leaflet-container.crosshair-cursor-enabled {
cursor:crosshair;
}

}'
                        ),

                        if (isTRUE(SHOW_WELCOME_MESSAGE)) div( class = "panel panel-default", div(class = "panel-heading", HTML("Welcome to SMIPS")), div( class = "panel-body", htmlOutput("welcomeMessage"), style=paste0('color:', 'green', '; width: 850px;'))) else div()
                      )),

                      fluidRow(
                        column(12, withSpinner( leafletOutput("moistureMap", height = 650))),
                        absolutePanel(column(1, htmlOutput("wmsLegend")), right=0, height=650)) ,
                      fluidRow(column(12, bsAlert("progressAlert"))),
                      dygraphOutput("dummyDygraph", height = "0px"), #Dummy dygraph to ensure loading of dygraph libraries at boot time, even when none are shown
                      div(style="visibility:hidden;",verbatimTextOutput("showDyGraph")), # Dummy invisible output of showDygraph to ensure its included in the conditional render
                      conditionalPanel('output.showDyGraph == "true"',
                        fluidRow( column(12, withSpinner( dygraphOutput("moisitureChart1", height = "350px")))),
                        fluidRow( HTML("<BR><BR>")),
                        fluidRow( column(12, rHandsontableOutput("SensorDrillInfoTable")))
                      )


           ))),
                  #  tabPanel("Sensor Site Data View",
                  #  
                  #           fluidRow(
                  #             absolutePanel( 
                  #             absolutePanel(  selectInput("SelectedSiteDataDownload", "Select Site ", width = 300, choices = c('')), left=20),
                  #             absolutePanel(  withBusyIndicatorUI( actionButton("retrieveTSData", "Retrieve Data")), left=360, top = 25, width=200),
                  #             absolutePanel(  downloadButton("downloadSiteTSData", "Download Data"), left=560, top = 25),
                  #             absolutePanel(  withSpinner( rHandsontableOutput("siteTimeSeriesTable")), left=20, top = 80)
                  #             ))
                  #      ),
         
                  # tabPanel("Map Downloads",  div(class = 'container-fluid', fluidRow(
                  #   fluidRow(column(2,     selectInput("MapProductsListForDownload", "Map Product to download", choices = c(''))),
                  #   column(1, selectInput("MapYearsForDownload", "Year", choices = c(''))),
                  #   column(1, selectInput("MapMonthsForDownload", "Month", choices = c(''))))
                  # )),
                           
                          # actionButton("domapdown", "Get Available Rasters"),
                  #         downloadButton("downloadZippedMapData", "Download Data"),
                  #         rHandsontableOutput("MapDownloadsTable")),
                  # tabPanel("Batch Extract",
                  #          HTML("<p><p><h1>Under Development</h1>") 
                  # ),
                  # tabPanel("Under the Hood",
                  #          HTML("<br><p>The functionality of this web site is underpinned by a range of openly available web services. </p>
                  #               <p>Shortly this page will be updated to show live demos of these web services in action so that you can
                  #               see how to directly access these web services for you own specific use cases. </p>") ,
                  #          rHandsontableOutput("LogTable")
                  # 
                  # ),

                  tabPanel("About", "",
                            includeHTML2(paste0( "www/StaticPages/About.html"))
                           # HTML("<h1>The SOil Moisture Information Processing System</h1>
                           #      <p></p>") 
                  ),
                 footer = div(class = 'footer',
                              hr(),
                   fluidRow( column(12,
                   HTML('<div style="margin: 0px auto 0px auto; width:450px;"><img src="Logos/TERN-NCRIS-digital-Primary.jpg" width="100%"></img></div>')
                 )),
                   #Trick to escape the fixed-size container for the full-width lower-footer
                   HTML('</div><!--close footer--></div><!--close row--></div><!--close container-->
                   <div class="container-fluid"><div class="row lower-footer"><div class="col-sm-12"><p>&nbsp;<br/>&nbsp;<br/></p>')
                 )
            ) 

 


     
shinyApp(ui = ui, server = server)


