
pkgs = c(
  "raster",         # to create raster datasets
  "suncalc",        # to calculate sun angles
  "maptools",       # world map data
  "ggplot2",        # visualisation
  "lubridate",       # dates package
  "sp",
  "sf",
  "assertthat"
)
to_install = !pkgs %in% installed.packages()
if(any(to_install)) {
  install.packages(pkgs[to_install])
}

require(raster)
require(suncalc)
require(ggplot2)
require(maptools)
require(lubridate)
data(wrld_simpl)

#function to convert a raster to a data frame that can be used by ggplot
# it will output a dataframe that looks like
# x y z
# 10 50 24

rasToDf <- function(inRas) {
  dfOut <- as.data.frame(coordinates(inRas))
  dfOut$z=getValues(inRas)
  
  # remove na from data frame to make smaller
  dfOut <- na.omit(dfOut)
  
  return(dfOut)
}

# create a blank background
backSet <- theme_bw() +  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(plot.title = element_text(size = 20,hjust = 0.5))

# create your own time
timeToShow <- ymd_hms("2020-09-30 12:00:00")
# or use the current time
timeToShow=Sys.time()

# create a date label
dateLabel <- format(timeToShow, format="%B %d %Y  %H:%M")

# create a global raster at 0.25 deg resolution
baseRaster <- raster(extent(-180, 180, -90, 90), res=0.25)
projBase <- "+init=epsg:4326"
crs(baseRaster) <- CRS(projBase) 

dfGlobal <- as.data.frame(coordinates(baseRaster))
dfGlobal$date=timeToShow
colnames(dfGlobal) = c("lon","lat","date")

# create data frame that contains the sun position this function from the suncalc package
# requires input of a data frame with lat lon and date column
sunPos <- getSunlightPosition(data = dfGlobal,keep = c("altitude", "azimuth"))
# convert radians to degrees
sunPos$sunAngle <- (sunPos$altitude* 180) /pi

# set any values below -18 to -18 as this is complete night
sunPos$sunAngle[sunPos$sunAngle < -18] <- -18
# set vlaues above 88 to 90 to make a little sun in the image
sunPos$sunAngle[sunPos$sunAngle > 88] <- 90

dfGlobal$z=sunPos$sunAngle

# set the values of the base raster to the sun angle
# we can then use this later to project to a different projection
values(baseRaster) <- sunPos$sunAngle


#create the night day palette
nightPal <- colorRampPalette(c("#1e001e","#22042b","#270539","#2d0a49",
                               "#33115a","#342e6d","#2b4e82","#6683a7","#c1c9d6","#dddddd"))(36)
dayPal <- colorRampPalette(c("#dddddd","#f5e9bf","#f8e58f","#f7de5f",
                             "#f3d81c","#f8cb05","#febd01","#ffab00","#ff9300"))(175)
cols <- c(nightPal,dayPal,rep("#ff5522",4))

p <- ggplot() +  xlim(c(-200,185)) +
  geom_tile(data=dfGlobal, aes(x=lon, y=lat, fill=z), alpha=0.8) +
  ggtitle(paste("Angle of the Sun on",dateLabel)) +
  scale_fill_gradientn(colours = cols) +
  geom_polygon(data=wrld_simpl, aes(x=long, y=lat, group=group), 
               fill=NA,color="#666666", size=0.3) +
  backSet

# add latitude lines  
latLines <- c(66.5,23.5,0,-23.5,-66.5)
latDef <- c("Arctic\ncircle","Tropic of\nCancer","Equator","Tropic of\nCapricorn","Antarctic\nCircle")
dfAnnotation <- data.frame(y=latLines,yend=latLines)
dfAnnotation$x=-180
dfAnnotation$xend=180
dfAnnotation$label=latDef

p <- p + geom_text(data=dfAnnotation,aes(x=x-1,y=y,label=label),hjust=1)
p <- p + geom_segment(data=dfAnnotation,aes(x=x,xend=xend,y=y,yend=yend),color="#66666666",linetype=2)
p

# set a limit over Europe to display the map
p <- p + coord_cartesian(xlim = c(-30,60), ylim = c(30,70)) 
p

# project the raster into a projection of your choice
projStr = "+proj=moll"
#projStr = "+proj=eqearth"

# project the raster into a different projection
mollRaster <- projectRaster(baseRaster,crs=projStr,method="ngb")
# project the polygons into same projection
wrld_moll <- spTransform(wrld_simpl,CRS(projStr))

# convert to data frame that can be used by ggplot
dfProj <- rasToDf(mollRaster)

# sort out annotations appearing in the correct place
# feels like there must be a better way of doing this!
df1 <- data.frame(lon=dfAnnotation$x, lat=dfAnnotation$y)
df2 <- data.frame(lon=dfAnnotation$xend, lat=dfAnnotation$yend)

coordinates(df1) <- c("lon", "lat")
proj4string(df1) <- CRS("+init=epsg:4326") # WGS 84
coordinates(df2) <- c("lon", "lat")
proj4string(df2) <- CRS("+init=epsg:4326") # WGS 84

df1Out <- as.data.frame(spTransform(df1, CRS(projStr)))
df2Out <- as.data.frame(spTransform(df2, CRS(projStr)))

dfAnnotationProj <- dfAnnotation
dfAnnotationProj$x=df1Out$lon
dfAnnotationProj$xend=df2Out$lon
dfAnnotationProj$y=df1Out$lat
dfAnnotationProj$yend=df2Out$lat


p <- ggplot() +
  geom_tile(data=dfProj, aes(x=x, y=y, fill=z), alpha=0.8) +
  ggtitle(paste("Angle of the Sun on",dateLabel)) +
  scale_fill_gradientn(colours = cols) +
  geom_polygon(data=wrld_moll, aes(x=long, y=lat, group=group), 
               fill=NA,color="#666666", size=0.3) +
  backSet

p <- p + geom_text(data=dfAnnotationProj,aes(x=x-1,y=y,label=label),hjust=1)
p <- p + geom_segment(data=dfAnnotationProj,aes(x=x,xend=xend,y=y,yend=yend),color="#66666666",linetype=2)
p

