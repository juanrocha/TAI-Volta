## Figure for VR proposal with clean data from Katja

## Extract and clean data for Volta Basin
## TAI project
## Data was collected and pre-processed in Excel by Katja Malmborg

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
library(MASS)
library(kohonen)
library(NbClust)

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


p <- ggplot(data=dat, mapping=aes(x=year, y=prop_cultivated_area)) +
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

p <- ggplot(data=dat, aes(country, log_prod)) + geom_jitter(aes(colour=country, alpha=0.2)) + 
  geom_boxplot(notch=T) + facet_grid(crop ~ year)  +
  theme(axis.text.x = element_text(angle=90))+
  ggtitle('Production of crops (Tons) in the Volta basin')

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

# J160623: Update the Ostrom variables with new file, follow the same procedure as the commented code above
file <- 'VARIABLES_1606.xlsx'
  users <- read_excel(file, sheet=1) #, dec=','
  users[3:11] <- apply(users[3:11], 2, function (x) {as.numeric(x)} )
  # users$TAI_ID1 <- as.factor(users$TAI_ID1)

  resource <- read_excel (file, sheet=2)
  # biophys <- read_excel(file, sheet=3)
  # interact <- read_excel(file,sheet=4)
  dams <- read_excel(path = 'Volta_Dam_density_KLC.xlsx', sheet = 1)
  kcals <- read_excel("crop_kcals.xlsx", sheet = 1)

# dat <- left_join(dat, select(users, TAI_ID1, Sq_km, Pop= Total), by = c('TAI_ID2' = 'TAI_ID1'))
# dat <- mutate(dat, prod_km2 = prod / Sq_km, prod_capita = prod / Pop, yield_capita = yield / Pop)

volta.shp@data <- left_join(volta.shp@data, users, by = 'TAI_ID1' )
volta.shp@data <- left_join(volta.shp@data, resource, by = 'TAI_ID1' )
volta.shp@data <- left_join(volta.shp@data, dplyr::select(dams, TAI_ID1, Area_sqkm, Dams, Dam_dens), 
                            by = 'TAI_ID1' )

# Do the clustering for one year
dat <- mutate(dat, sq_prod = sqrt(prod))
dat <- mutate(dat, dt_prod = sq_prod - mean(sq_prod))

#response variable dataset, in this case production of year 2012

mds <- list()
for (i in seq_along(2001:2012) ){  # seq_along(levels(dat$year))
  resp <- dat %>% 
    filter (year == c(2001:2012)[i]) %>%  #levels(dat$year)
    dplyr::select(TAI_ID2, crop, prod) %>% 
    spread (key = crop, value = prod) %>%
    dplyr::select(-TAI_ID2) 
  
  mds[[i]] <- metaMDS(resp, dist = 'manhattan', trymax = 1000)
  
}

# The example below was used for one year case
resp <- dat %>%
  filter (year == c(2001:2012)[8]) %>% #levels(dat$year)
  dplyr::select(TAI_ID2, crop, prod) %>%
  spread (key = crop, value = prod) %>%
  dplyr::select(-TAI_ID2)

mds <- metaMDS(resp, dist = 'manhattan', trymax = 1000)
mds1
plot(mds[[1]])

env.bio <- volta.shp@data[,c(12:16)] # c(11:18)
env.social <- volta.shp@data[,c(4:8,22:30)]
env.resys <- volta.shp@data[,c(9:11,17,32:35,37)]

## Plot them all
quartz(width=8, height=8, family='Helvetica', pointsize=11)
par(mfrow=c(3,4))
for (i in 1:length(mds)){
  plot(mds[[i]], type='p', display='sites', cex=0.8)
  ef1 <- envfit(mds[[i]],env.bio, permu=999)
  ef2 <- envfit(mds[[i]], env.social, permu=999)
  ef3 <- envfit(mds[[i]], env.resys, permu=999)
  plot(ef1, p.max=0.05, col='blue', cex=0.8)
  plot(ef2, p.max=0.05, col='purple', cex=.8)
  plot(ef3, p.max=0.05, col='grey', cex=.8)
  title(levels(dat$year)[i])
}

lapply(mds, function (x) {x$converged == TRUE}) # year 8 didn't coverge
# This re-loop the mds over the same data with the previous best solution found above.
# Run it until convergence, usually between 1500 - 2100 iterations
mds[[8]] <- metaMDS(resp, dist = 'manhattan', trymax = 1000, previous.best = mds[[8]])


# some basic clustering with hierarchical clustering
dis <- vegdist(resp, method='manhattan')
clus <- hclust(dis, 'average')
plot(clus, hang=-0.1)
rect.hclust(clus, 6)
grp <- cutree(clus, 6)

plot(mds1, type='p',display='site')
ordihull(mds1, grp, lty=2, col='red')


# now with PAM


mclus.out <- pam(mds1$points, metric='manhattan', k=9)
ordiplot(mds1, type='none' )
points(mds1$points, cex=1, lwd=1.5, col= brewer.pal(9, 'Set2')[mclus.out$clustering])
ordihull(mds1, mclus.out$clustering, label = TRUE, cex=0.7, col="purple", lty=2)
plot(ef1, p.max=0.05, col='blue', cex=0.8)
plot(ef2, p.max=0.05, col='purple', cex=.8)
plot(ef3, p.max=0.05, col='grey', cex=.8)

# Other methods, all with 6 clusters
kClusters <- 9
fitClara <-clara(mds1$points, metric='manhattan', k= kClusters)
fitKM <- kmeans(resp, centers= kClusters,iter.max=2000,nstart=50)
fitPAM <-pam(mds1$points, metric='manhattan', k= kClusters)
fitSOM <- som(as.matrix(resp), grid= somgrid(3,3,'hexagonal'))

dis <- vegdist(resp, method='manhattan')
clus <- hclust(dis, method='average')
grp <- cutree(clus, kClusters)
fitHier <- grp

clusters <- data.frame( TAI_ID1 = volta.shp@data$TAI_ID1, # codes
                        clara = as.vector(fitClara$clustering ),
                        kmean = as.vector(fitKM$cluster),
                        PAM = as.vector(fitPAM$clustering),
                        SOM = as.vector(fitSOM$unit.classif),
                        hierarchical = as.vector(fitHier))

volta.shp@data <- left_join(volta.shp@data, clusters)
levelCols <- brewer.pal(9, "Set3")
display.brewer.all()
## And map it for VR

quartz(width=8, height=5, family='Helvetica', pointsize=11)
layout.mat <- matrix(c(1,1,1,1,2,3,4,5), 2, 4, byrow=F)
layout(layout.mat)
#par(mfcol= layout.mat)

ordiplot(mds1, type='none' , main = 'Non-metric Multidimensional Scaling for crop production\n year 2001, clustering with PAM')
points(mds1$points, cex=1, lwd=1.5, col= brewer.pal(9, 'Set2')[mclus.out$clustering])
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
intern <- clValid(obj=as.data.frame(resp), nClust=2:9, 
                  clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny','som',
                              'pam', 'sota', 'clara', 'model'), 
                  validation='internal') 

## Stability validation
stab <- clValid(obj=as.data.frame(resp), nClust=2:9,
                clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny', 'som', 
                            'pam', 'sota', 'clara', 'model'), 
                validation='stability')

summary(intern) #optimal hierarchical with 2 clusters
summary(stab) # optimal hierarchical with 2 or 6, or som with 6

#summary statistics in plot
quartz(width=7, height=4, family='Helvetica', pointsize=8)
par(mfrow=c(2,4), mar=c(4,4,3,1))
plot(intern, legend=F)

plot(nClusters(intern), measures (intern, 'Dunn')[,,1], type='n', axes=F, xlab='', ylab='')
legend('center', clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))

plot(stab, measure=c('APN', 'AD', 'ADM'), legend=F)
plot(nClusters(stab), measures(stab, 'APN')[,,1], type='n', axes=F, xlab='', ylab='')
legend('center', clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))

## Validation of number of clusters with NbClust
# JB technique to test number of clusters
library(NbClust)
nbCluster <- NbClust( data = as.matrix (full[-c(1,30)]) ,
                     distance="manhattan", min.nc=2, max.nc=10, 
                     method = "ward.D2", index="all", 
                     alphaBeale=0.01)

# I get the following error
# Error in NbClust(data = as.matrix(full[-c(1, 30)]), distance = "manhattan",  : 
# The TSS matrix is indefinite. There must be too many missing values. The index cannot be calculated

par(mfrow=c(1,1))
hist(nbCluster$Best.nc[1,],breaks=max(na.omit(nbCluster$Best.nc[1,])),
     xlab="Number of clusters",col="lightgrey", main="Optimal number of clusters?")
detach(package:NbClust)

## Create an animation
library(animation)

dat$year <- as.factor(dat$year)


####
saveGIF({
  ani.options(nmax=12)
  par(mfrow = c(1,2))
  for (i in 1:12){
    # cluster with pam
    mclus.out <- pam(mds[[i]]$points, metric='manhattan', k=9)
    
    # NMDS
    ordiplot(mds[[i]], type='none' , main = levels(dat$year)[i] , xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25))
    points(mds[[i]]$points, cex=1, lwd=1.5, col= brewer.pal(9, 'Set3')[mclus.out$clustering])
    ordihull(mds[[i]], mclus.out$clustering, label = TRUE, cex=0.7, col="purple", lty=2)
    # environmental fitting
    ef1 <- envfit(mds[[i]],env.resys, permu=999)
    ef2 <- envfit(mds[[i]], env.social, permu=999)
    ef3 <- envfit(mds[[i]], env.bio, permu = 999) 
    plot(ef1, p.max=0.05, col='blue', cex=0.8)
    plot(ef2, p.max=0.05, col='purple', cex=.8)
    plot(ef3, p.max = 0.05, col = 'gray', cex = 0.8)
    
    #map
    plot (volta.shp, col = levelCols[as.vector (mclus.out$clustering)], lwd = 0.1)
    title (paste('Converged', mds[[i]]$converged, sep = ' '))
  }
}, movie.name = 'animation_cluster_map3.gif', ani.height = 500, ani.width = 1000, title = 'Clusters over time')

#######

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


## checking for correlations

library (corrgram)
corrgram(resp, order=T, main="Correlations", 
         lower.panel=panel.shade, 
         upper.panel=panel.pie, 
         diag.panel=panel.density)

str(volta.shp@data)

volta.shp@data[-c(1,2,21,31)] %>% 
  dplyr::select (Children, Women, Pop_dens, Farmers.x, Urban, Pop_density, Ratio_women, 
          Ratio_children, Urbanization, Farmers.y) %>%
  pairs(.)
  # eller hur
  corrgram( ., order=T, main="Correlations", 
         lower.panel=panel.shade, 
         upper.panel=panel.pie, 
         diag.panel=panel.density)
  
### Cluster with all data and identify the optimal number of clusters
# First summarize croping data
str(dat)
# dat$year <- as.numeric(dat$year); dat$year <- dat$year + 2000
agg.crop <- dat %>%
  dplyr::select(TAI_ID2, crop, sq_prod, year) %>%
  group_by(TAI_ID2, crop) %>%
  summarize ( . , avg = mean(sq_prod), medev = mad(sq_prod), med = median(sq_prod), 
              v = var(sq_prod), standev = sd(sq_prod)) %>% # I can use geometric mean or median 
  dplyr::select(TAI_ID2, crop, med) %>% 
  group_by(TAI_ID2, crop) %>%
  spread(key=crop, value = med)

corrgram (agg.crop, order=T, main="Correlations", 
          lower.panel=panel.pts, 
          upper.panel=panel.conf, 
          diag.panel=panel.density)
  
  filter (year == levels(dat$year)[12]) %>%
  
  spread (key = crop, value = prod) %>%
  dplyr::select(-TAI_ID2)
# Then compile a dataframe with all data to use.
# kcals
crops.nam <- tolower (names(agg.crop)) [-1]
crops.nam[2] <- 'taro' # cocoyam is aka. taro
crops.nam %in% kcals$CROPNAME

cals.fao <- filter(kcals, CROPNAME %in% crops.nam)[c(1,9,2:8,10),3]

#interaction dataset
dat.int <- dat %>% dplyr::select(TAI_ID2, crop, prod, year) %>% 
  group_by(TAI_ID2, crop) %>% 
  summarise (m= sd(prod)) %>% #median on sq_prod is also possible / add the yield.
  spread (key = crop, value = m) 

# Step 1: kcals first
dat.int[-1] <- t(apply(as.matrix(dat.int[-1]), 1, function(x){x * as.vector(as.matrix(cals.fao))/ 10^6})) ## millions of KCals
dat.int # now data is in mean Kcals *10^6

# Step 2: normalize (optional but important for MDS)
dat.int[-1] <- t( apply( as.matrix( dat.int[-1]), 1, function (x) {x / max(x)} )) ## millions of KCals
dat.int

dat.int <- mutate(dat.int, TAI_ID1 = as.numeric(as.character(TAI_ID2)))

### I could easily exchange mean by median given the pairs plot (itsn't normal)
## but for now work on 'the cube'
# side interactions

cube <- function(s1, s2, s3, s4){ # each side of the cube is s_
  mod1 <- metaMDS(s1, distance = 'manhattan', trymax = 1000)
  ef1 <- envfit(mod1, s2, permu=999)
  ef2 <- envfit(mod1, s3, permu=999)
  ef3 <- envfit(mod1, s4, permu=999)
  
  ## plot
  plot(mod1, type='p', display='sites', cex=0.8)
  plot(ef1, p.max=0.05, col='blue', cex=0.8)
  plot(ef2, p.max=0.05, col='purple', cex=.8)
  plot(ef3, p.max=0.05, col='grey', cex=.8)
  
  return(list(mod1, ef1, ef2, ef3))
}

int.side <- cube(s1 = dat.int[-1], s2 = env.resys, s3= env.bio, s4 = env.social)
bio.side <- cube(s1 = env.bio , s2 = env.resys, s3= dat.int[-1], s4 = env.social)
res.side <- cube(s1 = env.resys , s2 =  env.bio, s3= dat.int[-1], s4 = env.social)
soc.side <- cube(s1 = env.social , s2 = env.resys, s3= dat.int[-1], s4 =  env.bio)




mod1 <- metaMDS(dat.int[-1], dist = 'manhattan', trymax = 1000)
plot(mds[[i]], type='p', display='sites', cex=0.8)
ef1 <- envfit(mod1,env.bio, permu=999)
ef2 <- envfit(mod1, env.social, permu=999)
ef3 <- envfit(mod1, env.resys, permu=999)
plot(ef1, p.max=0.05, col='blue', cex=0.8)
plot(ef2, p.max=0.05, col='purple', cex=.8)
plot(ef3, p.max=0.05, col='grey', cex=.8)




### Now do the mds for all


full <- volta.shp@data[,c(3,12:16,4:8,22:30, 9:11,17,32:35,37)]
full <- left_join(full, dat.int, by = c('TAI_ID1' = 'TAI_ID1'))
full <- full [-c(11:14,16)]# get rid of highly correlated things that are probably just on different units
full <- full [-c(20,22)] # get rid of the variables on sqkm, keep only variables with values (0:2-3)


corrgram (full[-c(1,23)], order=T, main="Correlations", 
          lower.panel=panel.pts, 
          upper.panel=panel.conf, 
          diag.panel=panel.density)

summary (full[-c(1,23)])


mds.full <- metaMDS(full[-c(1,23)], dist = 'manhattan', trymax = 1000)
plot(mds.full, type = 'p', display = 'sites', cex = 0.8)

## Validating number of clusters
## Number of clusters with NdClust: uses 30 different index and compare them to decide the optimal partition
library (NbClust)
# euclidean and manhattan are not good for gradient separation (see help vegdist)
clust_num <- NbClust( data = full [-c(1,23)], dist = 'manhattan',  # diss = designdist(full [-c(2,23)], '1-J/sqrt(A*B)' )
                      min.nc = 2, max.nc = 12, method = 'ward.D2', alphaBeale = 0.1, 
                      index = 'all')

## 7 algorithms propose that the number of clusters is 3 and another 7 that it should be 9 clusters
quartz()
par(mfrow=c(1,1))
hist(clust_num$Best.nc[1,],breaks=max(na.omit(clust_num$Best.nc[1,])),
     xlab="Number of clusters",col="lightgrey", main="Optimal number of clusters?")




library (clValid)
## Internal validation
intern <- clValid(obj=as.data.frame(full[-c(1,23)]), nClust=c(3,9), 
                  clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny','som',
                              'pam', 'sota', 'clara', 'model'), 
                  validation='internal') 

## Stability validation
stab <- clValid(obj=as.data.frame(full[-c(1,23)]), nClust=c(3,9),
                clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny', 'som', 
                            'pam', 'sota', 'clara', 'model'), 
                validation='stability')

# Optimal Scores:
#   
#   Score  Method Clusters
# APN 0.0131 kmeans 3       
# AD  2.5233 pam    9       
# ADM 0.2478 kmeans 9       
# FOM 0.5509 model  9       


