#-Where is user tweeting from?-

user$location
loc <- subset(dfT, select = c('latitude','longitude'))
print(paste0("User's location: ", user$location))

dfT$longitude
dfT$latitude


#coords2continent = function(points)
#{  
#  countriesSP <- getMap(resolution='low')
#  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
#
# # converting points to a SpatialPoints object
#  # setting CRS directly to that from rworldmap
#  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
#
#
#  # use 'over' to get indices of the Polygons object containing each point 
#  indices = over(pointsSP, countriesSP)
#
#  #indices$continent   # returns the continent (6 continent model)
#  indices$REGION   # returns the continent (7 continent model)
#  #indices$ADMIN  #returns country name
#  #indices$ISO3 # returns the ISO3 code 
#}
#
#points = data.frame(lon=c(0, 90, -45, -100, 130), lat=c(52, 40, -10, 45, -30 ))

coords2continent(points)
