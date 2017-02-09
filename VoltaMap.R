# Map of the Volta basin
#  by Juan Carlos Rocha
# juan.rocha@su.se
# Stockholm, 160502

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)

# load libraries for mapping
library(maptools)
library(rgeos)
library(RColorBrewer)

# get map from Google
map <- get_map(location=c(left = -6.0245, bottom =3.8105 , right =2.7205  , top=15.5712),
               zoom=6, color='color',  source='osm') # 
class(map)
plot(map)
ggmap(map) 
# save( map, file='mapVolta.RData', safe=T)


### J161212: Map the distribution of variables
## dat is created from the paper draft, run all chuncks before normalizing the data
dat <- dplyr::select(dat, 20, 1:19,21:34)
names(dat) # note I need to run this for every column of dat

setwd('~/Documents/Projects/TAI/figures/maps')

for (i in 22:34){ # there is problems with i  = 15, 21, 23
  # in each iteration I need to reload the map so it starts clean
  # load map for visualizations and data
  volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp")
  
  sh_data <- volta.shp@data
  # minimize the dataset to what you really nead so joined tables are not super heavy
  volta.shp@data <- dplyr::select(volta.shp@data, c(1:3))
  # make sure is a factor
  volta.shp@data$TAI_ID1 <- as.factor(volta.shp@data$TAI_ID1 )
  
  # Following steps from tutorial Juan_TutorialCreatingMaps.R
  #fortify
  volta_f <- fortify(volta.shp)
  # head(volta_f)
  volta.shp@data$id <- rownames(volta.shp@data) # this id is to joing with fortified data
  # head(volta.shp@data)
  dim(volta_f)
  
  volta_f <- left_join(volta_f, volta.shp@data)
  # It's a huge object
  format(object.size(volta_f), units='auto') #16Mb
  dim(volta_f)
  head(volta_f)
  
  
  # now add the data you want to plot from the original dataset using TAI_ID1
  dat$quantiles <-  cut(dat[,i],
                        breaks = quantile(dat[,i]), na.rm = T, include.lowest = TRUE)
  
  volta_f <- left_join (volta_f, dat, by=c('TAI_ID1' = 'TAI_ID2'))
  
  ### create visualization
  g <- ggplot(volta_f, aes(long,lat, group = group, fill= quantiles)) +
    geom_polygon() + ggtitle(names(dat)[i]) + # facet_grid(.~ crop) +
    coord_equal() + theme_void() + scale_fill_brewer("Quantiles", palette = "OrRd") + #theme(legend.position = 'bottom') + 
    inset(
      grob = ggplotGrob( ggplot(data = dat, aes(dat[i])) + geom_density() +
                          #geom_vline(xintercept = mean(dat$Pop_density), na.rm = T, aes(col = 'blue')) +
                          #geom_vline(xintercept = median(dat$Pop_density), na.rm = T, aes(col = 'red')) +
                          theme_inset()  ),
      xmin = 2e+05, xmax = 5e+05, ymin = 625000, ymax = 875000
    )

  format(object.size(g), units='auto')
  
  save(g, file = paste('g',i,'.Rdata', sep='') )

}

# g + theme_void(base_size = 5)

#calculate quantile breaks
# volta_f$qt <- cut(volta_f$Pop_density,
#                   breaks = quantile(volta_f$Pop_density), na.rm = T)



setwd("~/Documents/Projects/TAI/figures")
quartz.save(file = 'pop_dens_map_quantiles.png', type = 'png', dpi = 300 )

cut(dat$Pop_density,quantile(dat$Pop))

g <- ggmap (map)

g + geom_polygon(data=volta_f, aes(long,lat, group = group),
                 colour = 'white', fill = 'black', alpha = .4, size = .3)


################ Re start the maps from scratch and see if that saves memory

ghana <- readRDS(file = '~/Documents/Projects/TAI/GHA_adm2.rds' )
burkina <- readRDS(file = '~/Documents/Projects/TAI/BFA_adm2.rds' )
identicalCRS(ghana, burkina)

class(ghana)
class(burkina)
class(volta.shp)

volta.shp@data$ADM_2 %in% ghana@data$NAME_2 %>% sum
volta.shp@data$ADM_2 %in% burkina@data$NAME_2 %>% sum

cbind(sort(as.character(volta.shp@data$ADM_2[1:38])) , sort(burkina@data$NAME_2))
cbind(sort(as.character(volta.shp@data$ADM_2[39:99])) , sort(ghana@data$NAME_2))
