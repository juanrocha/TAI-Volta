## Figure for VR proposal with clean data from Katja

## Extract and clean data for Volta Basin
## TAI project
## Data was collected and cleaned by Katja Malmborg

## by Juan Carlos Rocha
## juan.rocha@su.se
## Version May 21 2016

# clean workspace
rm(ls=list())

# load libraries for data and ploting
library(tidyr)
library('dplyr')
library ('ggplot2')
library(readxl)

# load libraries for mapping
library(maptools)
library(rgeos)
library(RColorBrewer)

# load libraries for clustering
library (vegan)
# library(rgl)
library(cluster)
library(clValid)

# Working directory 
setwd("~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha")

## extract data for Areas
file <- '160521_BF_GH_crop_areas_TAI_ID.xlsx'
sn <- c(2001:2012)
area <- list()

for (i in seq_along(sn)){
  df <- read_excel(file, sheet=i)
  df$year <- rep(as.numeric(sn[i]), dim(df)[1])
  area[[i]] <- df
}

area <- bind_rows(area) #full_join(areaG, df)
area <- gather(area, 'crop', 'area', 3:12)

area$Province <- as.factor(area$Province)
area$TAI_ID2 <- as.factor(area$TAI_ID1)
area$year <- as.factor(area$year)

## extract data for Production
file <- '160521_BF_GH_crop_production_TAI_ID.xlsx'
sn <- c(2001:2012)
crop <- list()

for (i in seq_along(sn)){
  df <- read_excel(file, sheet=i)
  df$year <- rep(as.numeric(sn[i]), dim(df)[1])
  crop[[i]] <- df
}

crop <- bind_rows(crop)
crop <- crop %>%
  gather('crop', 'CropProd', 3:12)

crop$Province <- as.factor(crop$Province)
crop$TAI_ID2 <- as.factor(crop$TAI_ID1)
crop$year <- as.factor(crop$year)

## combine two datasets and make sure it doesn't create NAs 
dat <- full_join(crop %>% select(TAI_ID2, year, crop, CropProd, TAI_ID1),
                 area %>% select(TAI_ID2, year, crop, area, TAI_ID1), 
                 by = c("TAI_ID2", "year", "crop"))
# Now dat has all data of crop production in tons and area in Ha. 
str(dat) # 18940 obs;  $ TAI_ID1 : Factor w/ 155 levels

# reduce dataset for only Volta basin
# load map for visualizations and data
volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp")

dat <- dat [is.element(dat$TAI_ID2, volta.shp@data$TAI_ID1),]
dat <- droplevels(dat)
str(dat) # 11920 obs;  $ TAI_ID1 : Factor w/ 99 levels Only Volta basin!!

### Short analysis for VR proposal
names(dat)[4] <- 'prod'
dat$country <- ifelse((dat$TAI_ID1.x) > 3000, 'GH', 'BF')


p <- ggplot(data=dat, mapping=aes(x=year, y=dt_prod)) +
  geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + 
  facet_grid( crop ~ country) + geom_smooth(stat='smooth', method='loess') + 
  theme_bw(base_size=10, base_family='Helvetica') + 
  theme(axis.text.x = element_text(angle=0))  + 
  ggtitle(expression(paste('Crop per Province in'~ sqrt(Tons))))

p <- ggplot(data=dat, mapping=aes(x=year, y=area)) +
  geom_line(aes(colour=crop, alpha=0.2, group = TAI_ID2)) + 
  facet_grid( country ~ crop) + geom_smooth(stat='smooth', method='loess') + 
  theme_bw(base_size=10, base_family='Helvetica') + 
  theme(axis.text.x = element_text(angle=0))  +
  ggtitle('Cultivated area per Province in Ha')

p
# Calculate yields
# dat <- mutate(dat, yield = ifelse(prod == 0 & area == 0, 0, prod/area)) # Inf values when area = 0

# Skip this part for now, yield is rendering Inf values
# Normalize with geographical data
## Katja did a file with summary info for the bundling exercise. Use pop and other stats from such file. 
# file <- 'Volta_Bundling_prep.xlsx'
#   users <- read_excel(file, sheet=1) #, dec=','
#   users[4:16] <- apply(users[4:16], 2, function (x) {as.numeric(x)} )
#   users$TAI_ID1 <- as.factor(users$TAI_ID1)
# 
#   resource <- read_excel (file, sheet=2)
#   biophys <- read_excel(file, sheet=3)
#   interact <- read_excel(file,sheet=4)
# 
# dat <- left_join(dat, select(users, TAI_ID1, Sq_km, Pop= Total), by = c('TAI_ID2' = 'TAI_ID1'))
# dat <- mutate(dat, prod_km2 = prod / Sq_km, prod_capita = prod / Pop, yield_capita = yield / Pop) 


# Do the clustering for one year
dat <- mutate(dat, sq_prod = sqrt(prod))
dat <- mutate(dat, dt_prod = sq_prod - mean(sq_prod))

#response variable dataset, in this case production of year 2012


resp <- dat %>% 
  filter (year == 2004) %>%
  dplyr::select(TAI_ID2, crop, prod) %>% 
  spread (key = crop, value = prod) %>%
  dplyr::select(-TAI_ID2) 

mod1 <- metaMDS(resp, dist = 'manhattan', trymax = 400)
mod1

plot(mod1)

env.clim <- volta.shp@data[,c(11:18)]
env.social <- volta.shp@data[,c(4:10)]

plot(mds1, type='p', display='sites', cex=0.8)
ef1 <- envfit(mds1,env.clim, permu=999)
ef2 <- envfit(mds1, env.social, permu=999)
plot(ef1, p.max=0.05, col='blue', cex=0.8)
plot(ef2, p.max=0.05, col='purple', cex=.8)

# some basic clustering with hierarchical clustering
dis <- vegdist(resp, method='manhattan')
clus <- hclust(dis, 'average')
plot(clus, hang=-0.1)
rect.hclust(clus, 6)
grp <- cutree(clus, 6)

plot(mds1, type='p',display='site')
ordihull(mds1, grp, lty=2, col='red')


# now with PAM
mclus.out <- pam(mds1$points, metric='manhattan', k=6)
ordiplot(mds1, type='none' )
points(mds1$points, cex=1, lwd=1.5, col= brewer.pal(6, 'Set2')[mclus.out$clustering])
ordihull(mds1, mclus.out$clustering, label = TRUE, cex=0.7, col="purple", lty=2)
plot(ef1, p.max=0.05, col='blue', cex=0.8)
plot(ef2, p.max=0.05, col='purple', cex=.8)


# Other methods, all with 6 clusters
kClusters <- 6
fitClara <-clara(mds1$points, metric='manhattan', k=6)
fitKM <- kmeans(resp, centers= kClusters,iter.max=2000,nstart=50)
fitPAM <-pam(mds1$points, metric='manhattan', k=6)
fitSOM <- som(as.matrix(resp), grid= somgrid(3,2,'hexagonal'))

dis <- vegdist(resp, method='manhattan')
clus <- hclust(dis, method='average')
grp <- cutree(clus, 6)
fitHier <- grp

clusters <- data.frame( TAI_ID1 = volta.shp@data$TAI_ID1, # codes
                        clara = as.vector(fitClara$clustering ),
                        kmean = as.vector(fitKM$cluster),
                        PAM = as.vector(fitPAM$clustering),
                        SOM = as.vector(fitSOM$unit.classif),
                        hierarchical = as.vector(fitHier))

volta.shp@data <- left_join(volta.shp@data, clusters)
levelCols <- brewer.pal(6, "Set2")

## And map it for VR

quartz(width=8, height=5, family='Helvetica', pointsize=11)
layout.mat <- matrix(c(1,1,1,1,2,3,4,5), 2, 4, byrow=F)
layout(layout.mat)
#par(mfcol= layout.mat)

ordiplot(mds1, type='none' , main = 'Non-metric Multidimensional Scaling for crop production\n year 2012, clustering with PAM')
points(mds1$points, cex=1, lwd=1.5, col= brewer.pal(6, 'Set2')[mclus.out$clustering])
ordihull(mds1, mclus.out$clustering, label = TRUE, cex=0.7, col="purple", lty=2)
plot(ef1, p.max=0.05, col='blue', cex=0.8)
plot(ef2, p.max=0.05, col='purple', cex=.8)


for (i in c(21,23)){
  
  # Plot MDS with grouping according with each clustering algorithm
  ordiplot(mds1, type='none', main=colnames(volta.shp@data)[i])
  points(mds1$points, cex=1, lwd=1.5, 
         col= levelCols[volta.shp@data[,i]]) #volta.shp@data[,i]
  ordihull(mds1, groups= volta.shp@data[,i], label = TRUE, cex=0.7, 
           col="purple", lty=1)
  #plot(ef1, p.max=0.05, col='blue', cex=0.5)
  #plot(ef2, p.max=0.05, col='purple', cex=0.5)
  
  
  # Plot map with resulting clusters
  plot(volta.shp, col= levelCols[volta.shp@data[,i]],lwd=0.1)
  
}


quartz.save(file='FigureVR.png', type='png', dpi= 200)




## Validating number of clusters
## Internal validation
intern <- clValid(obj=as.data.frame(resp), nClust=2:9, clMethods=c('hierarchical', 'kmeans', 'diana', 'som', 'pam', 'sota', 'clara', 'model', 'fanny'), validation='internal') #, 'fanny'

## Stability validation
stab <- clValid(obj=as.data.frame(resp), nClust=2:9, clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny', 'som', 'pam', 'sota', 'clara', 'model'), validation='stability')

summary(intern) #optimal hierarchical with 2 clusters
summary(stab) # optimal hierarchical with 2 or 6, or som with 6



