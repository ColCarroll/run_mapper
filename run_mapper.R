library(plotKML)
library(ggmap)

get.city.state <- function(loc.string){
  city.rex <- "[[:digit:]]{2}/[[:digit:]]{2}/20[[:digit:]]{2} (?<loc>[[:alpha:][:blank:],]+)"
  matches <- regexpr(city.rex, loc.string, perl=T)
  starts <- as.data.frame(attr(matches, "capture.start"))
  stops <- starts + as.data.frame(attr(matches, "capture.length")) - 1
  loc <- substring(loc.string, starts$loc, stops$loc)
  
  loc = gsub(",","",loc)
  loc = gsub(" ","_",loc)
  return(loc)
}

make.plots <- function(){
  # GPX files downloaded from wherever
  files <- paste("data/", dir("data", pattern = "\\.gpx"), sep="")
  #files <- files[c((length(files) - 4):length(files))]
  print(length(files))

  city.tracks <- list()
  # Consolidate routes in one data frame
  index <- c()
  latitude <- c()
  longitude <- c()
  for (i in 1:length(files)) { #for producing
  # for (i in 1:50){ #for testing
    
    route <- readGPX(files[i])
    route.info <- names(route$tracks[[1]])[1]
    if(is.null(route.info)){
      next
    }
    loc <- get.city.state(route.info)

    if(loc == "_"){
      print(route.info)
      print(loc)
      next
    }
    if (!(loc %in% names(city.tracks)) && (loc!="_")){
      print(sprintf("Found a route from %s.  %.2f%% done.", loc, 100 * (i + 1)/length(files)))
      city.tracks[loc] = list("index"=c(), "latitude"=c(), "logitude"=c())
      }

    location <- route$tracks[[1]][[1]]
    city.tracks[[loc]][["index"]] <- c(city.tracks[[loc]][["index"]], rep(i, dim(location)[1]))
    city.tracks[[loc]][["latitude"]] <- c(city.tracks[[loc]][["latitude"]], location$lat)
    city.tracks[[loc]][["longitude"]] <- c(city.tracks[[loc]][["longitude"]], location$lon)

    }

  for (city in names(city.tracks)){
    index <- city.tracks[[city]][["index"]]
    latitude <- city.tracks[[city]][["latitude"]]
    longitude <- city.tracks[[city]][["longitude"]]
    routes <- data.frame(cbind(index, latitude, longitude))
    routes <- as.data.frame(lapply(routes[2:nrow(routes),],unlist))
    ids <- unique(routes$index)
    print(paste("Plotting",length(ids),"runs from",city,"..."))

    loc <- scale_bounding_box(c(
      min(routes$longitude),
      min(routes$latitude),
      max(routes$longitude),
      max(routes$latitude)), 0.1)

    map <- get_map(
      location=loc,
      source="stamen",
      maptype="terrain",
      zoom=12,
      crop=F)

    # Map the routes
    p <- ggmap(map) + geom_path(
          data=routes,
          size = I(20),
          alpha = I(0.05),
          color="purple",
          aes(x = longitude, y = latitude, group=index)) + geom_path(
          data=routes,
          alpha = I(0.3),
          color="blue",
          aes(x = longitude, y = latitude, group=index))

    png(paste("maps/",city,".png",sep=""), width=4000, height=4000)
      print(p)
    dev.off()
  }
}

scale_bounding_box <- function(bbox, rate){
  xmean <- (bbox[1] + bbox[3])/2;
  ymean <- (bbox[2] + bbox[4])/2;
  means <- c(xmean, ymean, xmean, ymean)
  return((1+rate) * (bbox - means) + means)
}
