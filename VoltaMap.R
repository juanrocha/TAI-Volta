# Map of the Volta basin
#  by Juan Carlos Rocha
# juan.rocha@su.se
# Stockholm, 160502


require(ggmap)

# get map from Google
map <- get_map(location=c(left = -6.0245, bottom =3.8105 , right =2.7205  , top=15.5712),
               zoom=6, color='color',  source='osm') # 
class(map)
plot(map)
ggmap(map) 
# save( map, file='mapVolta.RData', safe=T)