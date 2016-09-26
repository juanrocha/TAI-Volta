## Analysis Volta Project
## TAI project
## This script analyse crop time series over the Volta basin using MDS or 
## PCA. It also uses data collected to match 2-3 tier Ostrom variables 
## from the SES framework to see what determines the clustering of SES  
## typologies identified. This script loads the data collected by Katja  
## Malmborg and cleaned in ExtractionDataTAI.R by me. Preliminary analyses 
## can be found in DataPlay.R, CropYields.R, and MapsVolta

## by Juan Carlos Rocha
## juan.rocha@su.se
## Version March 30, 2016


# clean and load libraries

rm(list=ls())
library(tidyr)
library ('gdata')
library('dplyr')
library('reshape')
library ('ggplot2')
library(maptools)
library(rgeos)
library(RColorBrewer)
library(plot3D)
library('GGally')
ggmap::ggmap(volta.shp)

setwd("~/Documents/Projects/TAI/figures")

# load data

# load('~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha/Volta.RData')
load('~/Documents/Projects/TAI/scripts/TAI-Volta/160414_Volta.RData')

# area is harvested area per crop per year, value = Hectares
# prod is production in tons per crop per year, value = tons
# interact is the preliminary data for interactions, only cattle density
# resource has total.harvested.area, province.district.area, ratio.harvested.area, market.acces and tree.cover
# biophys has aridity, mean.temp, soil.water, wet.season. slope75
# users has sq_km total.pop wome, child, urban and rural totals, employees, 
# farmers, ration.children, ration.woman, pop.density, ratio.farmers, urbanization.

# load map for visualizations and data
volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp")


# Explore production data

# 
# prod$country <- ifelse(prod$TAI_ID > 3000, 'GH', 'BF')
# area$country <- ifelse(area$TAI_ID > 3000, 'GH', 'BF')
# # prod$TAI_ID1 <- as.factor(prod$TAI_ID1)
# # area$TAI_ID1 <- as.factor(area$TAI_ID1)
# 
# # use only common crops for both countries that occur every year.
# common <- intersect(levels(areaB$crop),  levels(areaG$crop))
# 
# # note to self: I'm not sure if having different time periods is valid. 
# # We have 7 years of data but different years. It can affect the aggregate 
# # calculations later. But even if it works (e.g. by changing the year stamp by yr1, yr2...yr7), 
# # in real life there were two different periods with probably different climate conditions, etc.
# 
# index <- vector()
# for (i in seq_along(common)){ 
# 	index <- c(index, which(prod$crop == common[i]))
# 	}
# 
# index2 <- vector()
# for (i in seq_along(common)){ 
# 	index2 <- c(index2, which(area$crop == common[i]))
# 	}
# 
# prod <- droplevels(prod[sort(index),])#subset(prod, crop == common, drop=T)
# area <- droplevels(area[sort(index2),]) #subset(area, crop == common, drop=T)
# prod$year_a <- ifelse(prod$country == 'GH', prod$Year - 2001, prod$Year - 2004)
# area$year_a <- ifelse(area$country == 'GH', area$Year - 2001, area$Year - 2004)
# 
# 
# table(prod$year_a)
# # correct for the missing year in GH
# which(prod$country == 'GH' & prod$year_a>5)
# 
# prod$year_a[which(prod$country == 'GH' & prod$year_a>5)] <- prod$year_a[which(prod$country == 'GH' & prod$year_a>5)] -1
# area$year_a[which(area$country == 'GH' & area$year_a>5)] <- area$year_a[which(area$country == 'GH' & area$year_a>5)] -1
# prod$TAI_ID2 <- as.factor(prod$TAI_ID1)
# area$TAI_ID2 <- as.factor(area$TAI_ID1)
# now year_a is the first year of data, 2, 3 ... 7 for both places.

# str(prod); str(area)

p <- ggplot(data=dat, mapping=aes(x=Year, y=sqrt(prod))) +
  geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + 
  facet_grid( crop ~ country) + geom_smooth(stat='smooth', method='loess') + 
  theme_bw(base_size=10, base_family='Helvetica') + 
  theme(axis.text.x = element_text(angle=0))  + 
  ggtitle(expression(paste('Crop per Province in'~ sqrt(Tons))))

p <- ggplot(data=dat, mapping=aes(x=year_a, y=area)) +
  geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + 
  facet_grid( country ~ crop) + geom_smooth(stat='smooth', method='loess') + 
  theme_bw(base_size=10, base_family='Helvetica') + 
  theme(axis.text.x = element_text(angle=0))  +
  ggtitle('Cultivated area per Province in Ha')

p

# quartz.save(file='160630_CropProd_boxplots.png', type='png')

p <- ggplot(data=dat, aes(crop, prod))+ geom_jitter(aes(colour=crop, alpha=0.2)) + 
  geom_boxplot(notch=T) + facet_grid(country~.)  +
  theme(axis.text.x = element_text(angle=90))+
  ggtitle('Production of crops (Tons) in the Volta basin')

p

## To do

# write a function that divides prod / area cultivated and produce the 
# long dataset for ploting & ordination methods.


dat$country <- ifelse(dat$TAI_ID1 > 3000, 'GH', 'BF')
dat$TAI_ID2 <- as.factor(dat$TAI_ID1)

# this is the way to get the common crops for the two countries
intersect(dat$crop[dat$country == 'GH'], dat$crop[dat$country =='BF'])

# then you can use filter to select them :) or a longer subsetting string
datKeyCrops <- (filter(dat, crop == 'Maize' | crop == 'Millet'| crop == 'Rice'| crop == 'Yam'| crop == 'Sorghum'| crop == 'Cowpea'| crop == 'Soy' ))
datKeyCrops <- drop.levels(datKeyCrops)
p <- ggplot(data=datKeyCrops, mapping=aes(x=Year, y=sqrt(prod))) +
  geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + 
  facet_grid( crop ~ country) + geom_smooth(stat='smooth', method='loess') + 
  theme_bw(base_size=10, base_family='Helvetica') + 
  theme(axis.text.x = element_text(angle=0))  + 
  ggtitle(expression(paste('Crop per Province in'~ sqrt(Tons))))

p


# calculate yieds
head(dat)

datKeyCrops <- mutate(datKeyCrops, yield = ifelse(prod == 0 & area == 0, 0, prod/area))


# This shows there is only data for both countries in years Year == 2002:2005 | Year == 2007:2009
datKeyCrops %>% 
	group_by( Year, country) %>%
	summarise(
		year_n= n()) %>%
	filter(Year > 2001)

datKeyCrops <- filter(datKeyCrops, Year == 2002:2005 | Year == 2007:2009)

# Note there is few horrible outliers making life difficult:
	filter(datKeyCrops, yield > 50) 

p <- ggplot(data= filter (datKeyCrops, yield < 100), mapping=aes(x=Year, y= yield)) + 
        geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + facet_grid(crop ~ country)  + 
        theme_bw(base_size=10, base_family='Helvetica') + 
        theme(axis.text.x = element_text(angle=0)) +
        ggtitle("Yield over time")
  #+ ggtitle(expression(paste('Crop per Province in'~ sqrt(Tons)))) + geom_smooth(stat='smooth', method='loess')
p
rm(p)
# quartz.save(file='Crop_province_KeyCrops.png', type='png')

## Normalizing by district area or per capita would make crop production comparable.
# Katja wisely says: 'If you do production through harvested area, you get yield, 
# and that is something different – it doesn’t say that much about the importance 
# of the crop in an area, but rather how productive it is.' In order to do that, 
# I need to join crop data with map data.

datKeyCrops <- left_join(datKeyCrops, select(users, TAI_ID1, Sq_km, Pop= Total))

# Normalize (do not confuse cropped area with area of the district 'Sq_km')
datKeyCrops <- mutate(datKeyCrops, prod_km2 = prod / Sq_km, prod_capita = prod / Pop, yield_capita = yield / Pop) 

### Note J160405: New problem: when importing data from excel it takes ,
# as separator for decimals. Somehow many values get converted to NA after 
# applying as.numeric to the columns of interest (see ExtracDataTAI.R file, lines 277 onwards)... 
## Update J160414: solved! it was a problem with the cell format (not number) on Excel that
# makes spaces in between numbers that R interpreted as commas [,] so as.number did not work properly

p <- ggplot(data= filter (datKeyCrops, yield < 100), mapping=aes(x=Year, y= prod_capita)) + 
  geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + facet_grid(country~crop)  + 
  theme_bw(base_size=10, base_family='Helvetica') + 
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Production per capita")
#+ ggtitle(expression(paste('Crop per Province in'~ sqrt(Tons)))) + geom_smooth(stat='smooth', method='loess')
p
rm(p)

# make a video :)
# Store the original data from shape file for backup, you will re use it
sh_data <- volta.shp@data
# minimize the dataset to what you really nead so joined tables are not super heavy
volta.shp@data <- select(volta.shp@data, c(1:3))

# then make a dataset with one year 
# a <- select(datKeyCrops, -TAI_ID1 ) %>%
#   filter(Year == 2002) %>%
#   drop.levels() %>%
#   cast(TAI_ID2 ~ crop, value = 'yield')

# make sure is a factor
volta.shp@data$TAI_ID1 <- as.factor(volta.shp@data$TAI_ID1 )

# volta.shp@data <- full_join(volta.shp@data, a, by= c('TAI_ID1' = 'TAI_ID2'))

# Following steps from tutorial Juan_TutorialCreatingMaps.R
#fortify
volta_f <- fortify(volta.shp)
# head(volta_f)
volta.shp@data$id <- rownames(volta.shp@data) # this id is to joing with fortified data
# head(volta.shp@data)

volta_f <- left_join(volta_f, volta.shp@data)
# It's a huge object
format(object.size(volta_f), units='auto') #16Mb
dim(volta_f)
head(volta_f)

# now add the data you want to plot from the original dataset using TAI_ID1
volta_f <- left_join (volta_f, filter(select(datKeyCrops, TAI_ID2, crop, yield, prod_km2, prod_capita, Year), Year == 2002, crop == 'Cowpea'), by=c('TAI_ID1' = 'TAI_ID2'))

filter(select(datKeyCrops, TAI_ID2, crop, yield, prod_km2, prod_capita, Year), Year == 2002)

g <- ggplot(volta_f, aes(long,lat, group = group, fill= yield)) +
  geom_polygon() + # facet_grid(.~ crop) +
  coord_equal() + theme_void() + 
  ggtitle(paste('Volta basin yields in', 2002, sep=' ') )

system.time()




# create the animation in html
# year <- c(2002:2005, 2007:2009)
map.year <- function(datos, layer, year, crop){
  lay_f <- fortify(layer)
  layer@data$id <- rownames(layer@data)
  lay_f <- left_join(lay_f, layer@data)
  print(format(object.size(lay_f), units='auto'))
  
  lay_f <- left_join (lay_f, 
                        filter(select(datos, TAI_ID2, crop, yield, prod_km2, prod_capita, Year), Year == year, crop == crop),
                        by=c('TAI_ID1' = 'TAI_ID2'))
 g <-  ggplot (data = lay_f, aes(long,lat, group = group, fill= yield)) +
    geom_polygon() + #facet_grid(.~ crop) +
    coord_equal() + theme_void() + 
    ggtitle( paste ('Volta basin yields of in', year, sep=' '))
  return(g)
}

# prueba
g <- map.year(datos = datKeyCrops, layer = volta.shp, year = 2002, crop = Cowpea) # this has a completely different behavior than do it manually... not working.


library(animation)
saveGIF({
  ani.options(nmax=7)
  for (i in 1:7){
    print()
  }
})
