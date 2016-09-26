## Quick analysis with new Katjas unnomralized file 
## 160902
## Juan Rocha
## juan.rocha@su.se

# Clean and load libraries
rm(list=ls())

library  ('gdata')
library ('dplyr')
library ('reshape')
library (corrgram)
library(vegan)

setwd("~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha")
# load map for visualizations and data
volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp")

# dat <- dat [is.element(dat$TAI_ID2, volta.shp@data$TAI_ID1),]
# dat <- droplevels(dat)
# str(dat) 

volta.only <- function(x){
  x <- x [ is.element (x$TAI_ID1, volta.shp@data$TAI_ID1 ), ]
  x <- droplevels(x)
}

### Normalizing by scaling everything to 0:1
rescale_hw <- function (x){ # This is Hadley Wickman function from his book R for DataScience
  rng <- range(x, na.rm= TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

## Read data
#### New file with raw values from Katja: 160901

file <- 'Volta_Vars_raw_160901.xlsx'
sn <- sheetNames(xls=file)

correct.num <- function (x){
  l <- length(x)
  x[3:l] <- apply (x[3:l], 2, function (x){as.numeric(x)} )
  return(x)
}


users <- read.xls(file, sheet=2, dec=',')
users <- correct.num(users)

resource <- read.xls (file, sheet=3, dec=',')
biophys <- resource [c(1,2,11:15)]
resource <- resource [-c(11:15)]
biophys <- correct.num(biophys)
resource <- correct.num (resource)
# biophys <- read.xls(file, sheet=3, dec=',')

# Threre is 3 datasets for interactions depending on the sheet
# 4 = kcals per distric area in sq.km
# 5 = kcals_harv.area
# 6 = ratio_harv.area
# 7 = other
interact <- read.xls(file,sheet=5, dec=',')
interact <- correct.num(interact)
interact <- volta.only(interact)

str(interact); summary (interact)

biophys %>% select(-ADM_2, -TAI_ID1) %>%
  # decostand (., method = 'log', MARGIN = 2, na.rm = TRUE)%>%
  # wisconsin(.) 
  apply (., 2, rescale_hw) %>% summary()
  corrgram (., order=T, main="Correlations", 
            lower.panel=panel.ellipse, 
            upper.panel=panel.pts, 
            diag.panel=panel.density)
  

### Combine all datasets for clustering all
dat <- left_join (users, resource, by = c("TAI_ID1", "ADM_2"))
dat <- left_join(dat, biophys, by = c("TAI_ID1", "ADM_2"))
dat <- left_join(dat, interact, by = "TAI_ID1")

# Note this is non-normalized data, original values. After the following line all is "rescaled" to 0-1

dat %>% select (-ADM_2, -TAI_ID1, -Province) %>%
  apply (., 2, rescale_hw) %>% summary()

dat <- dat[-26] # delete Province
dim(dat)
dat[3:35] <- apply(dat[3:35], 2, rescale_hw)
summary (dat)

corrgram (dat [-c(1,2)], order=T, main="Correlations", 
          lower.panel=panel.shade, 
          upper.panel=panel.pts, 
          diag.panel=panel.density)

## You can delete "Pop_dens_log" since it's rescaled and you already have pop_density.
dat <- dat[-4] 

## Transformations of data
## If it were necessary, here I will transform things with Logs, sqrt or wisconsin




########################
### Clustering all
########################
mds <- metaMDS(dat[-c(1,2)], distance = 'manhattan', trymax = 1000)
## plot
plot(mds, type='p', display='sites', cex=0.8)

## Validating number of clusters
## Number of clusters with NdClust: uses 30 different index and compare them to decide the optimal partition
library (NbClust)
# euclidean and manhattan are not good for gradient separation (see help vegdist)
clust_num <- NbClust( data = dat[-c(1,2)], dist = 'manhattan',  # diss = designdist(full [-c(2,23)], '1-J/sqrt(A*B)' )
                      min.nc = 3, max.nc = 12, method = 'ward.D2', alphaBeale = 0.1, 
                      index = 'all')

## 6 algorithms propose that the number of clusters is 7 and another 5 that it should be 3 clusters
# ******************************************************************* 
#   * Among all indices:                                                
#   * 5 proposed 3 as the best number of clusters 
# * 4 proposed 4 as the best number of clusters 
# * 4 proposed 5 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 6 proposed 7 as the best number of clusters 
# * 1 proposed 11 as the best number of clusters 
# * 1 proposed 12 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  7 
# ******************************************************************* 


quartz()
par(mfrow=c(1,1))
hist(clust_num$Best.nc[1,],breaks=max(na.omit(clust_num$Best.nc[1,])),
     xlab="Number of clusters",col="lightgrey", main="Optimal number of clusters?")

library (clValid)
## Internal validation
intern <- clValid(obj=as.data.frame(dat[-c(1,2)]), nClust=c(3:9), 
                  clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny','som',
                              'pam', 'sota', 'clara', 'model'), 
                  validation='internal') 

## Stability validation
stab <- clValid(obj=as.data.frame(dat[-c(1,2)]), nClust=c(3:9),
                clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny', 'som', 
                            'pam', 'sota', 'clara', 'model'), 
                validation='stability')

summary (intern)
# Optimal Scores:
#   
#   Score   Method       Clusters
# Connectivity 10.2159 hierarchical 3       
# Dunn          0.3697 hierarchical 5       
# Silhouette    0.3444 hierarchical 3     


summary (stab)
# Optimal Scores:
#   
#   Score  Method       Clusters
# APN 0.0006 hierarchical 5       
# AD  0.9623 kmeans       9       
# ADM 0.0026 hierarchical 5       
# FOM 0.1385 som          9 

### So I need hierarchical, k-means and som
# so I'll give it a try with 5 clusters (kClusters <- 5) and various fits
k <- 9
# fitClara <-clara(mds$points, metric='manhattan', k=6)
fitKM <- kmeans(dat[-c(1,2)], centers= k ,iter.max=2000,nstart=50)
# fitPAM <-pam(mds$points, metric='manhattan', k=6)
fitSOM <- som(as.matrix(dat[-c(1,2)]), grid= somgrid(3,2,'hexagonal'))

dis <- vegdist(dat[-c(1,2)], method='manhattan')
clus <- hclust(dis, method='ward.D2')
grp <- cutree(clus, k)

fitHier <- grp

clusters <- data.frame( TAI_ID1 = dat$TAI_ID1, # codes
                        #clara = as.vector(fitClara$clustering ),
                        kmean = as.vector(fitKM$cluster),
                        #PAM = as.vector(fitPAM$clustering),
                        SOM = as.vector(fitSOM$unit.classif),
                        hierarchical = as.vector(fitHier))
display.brewer.all()
levelCols <- brewer.pal(k, "Set1")



quartz(width=8, height=4, family='Helvetica', pointsize=7)
par(mfcol=c(2,3))

for (i in 2:4){
  
  # Plot MDS with grouping according with each clustering algorithm
  ordiplot(mds, type='none', main=colnames(clusters)[i])
  points(mds$points, cex=1, lwd=1.5, 
         col= levelCols[clusters[,i]]) #volta.shp@data[,i]
  ordihull(mds, groups= clusters[,i], label = TRUE, cex=0.7, 
           col="purple", lty=1)
  #plot(ef1, p.max=0.05, col='blue', cex=0.5)
  #plot(ef2, p.max=0.05, col='purple', cex=0.5)
  
  
  # Plot map with resulting clusters
  plot(volta.shp, col= levelCols[clusters[,i]],lwd=0.1)
  
}


##############
## maps
##############
names(dat)

indicesU <- c(3:11)
indicesR <- c(12:16)
indicesB <- c(17:24)
indicesI <- c(25:34)
indicesALL <- c(indicesU, indicesR, indicesB, indicesI)

noALL <- length(indicesALL)


provNames <- dat$ADM_2#volta.shp@data[2]

rownames(dataALL) <- provNames[,1]

# make colnames nice
# varNames <- c("Population trend", ...)
# colnames(dataALL) <- varNames

uESCols <- brewer.pal(length(indicesU), "Reds")
rESCols <- brewer.pal(length(indicesR), "Greys")
bESCols <- brewer.pal(length(indicesB), "PuBu")
iESCols <- brewer.pal(length(indicesI), "YlGn")
esFlowerCol <- c(uESCols, rESCols, bESCols, iESCols)

rankALL<-order(colSums(dataALL),decreasing=TRUE)

quartz(width=13,height=7.5, family='Helvetica', pointsize=12)
par(mfrow=c(4,ceiling(noALL/4)), mai = c(.02, 0.2, 0.2, 0.2))
for(i in 1:dim(dataALL)[2]) {
  esCols <-levelCols[findInterval(dataALL[,i], colBreaks, all.inside=TRUE)] # set colours
  esCols[is.na(esCols)]<-"red"
  plot(volta.shp, col= esCols, forcefill=TRUE,lwd=0.1)
  title(colnames(dataALL)[i], cex=1.5)
}
plot.new()
legend(x="center", legend= paste(colBreaks[1:(length(colBreaks)-1)],"-", colBreaks[2:length(colBreaks)]), cex=1.2,fill= levelCols,title="Level")






######################
### The cube
######################

cube <- function(s1, s2, s3, s4){ # each side of the cube is s_
  mod1 <- metaMDS(s1, distance = 'manhattan', trymax = 1000)
  ef1 <- envfit(mod1, s2, permu=999)
  ef2 <- envfit(mod1, s3, permu=999)
  ef3 <- envfit(mod1, s4, permu=999)
  
  # ## plot
  # plot(mod1, type='p', display='sites', cex=0.8)
  # plot(ef1, p.max=0.05, col='blue', cex=0.8)
  # plot(ef2, p.max=0.05, col='purple', cex=.8)
  # plot(ef3, p.max=0.05, col='grey', cex=.8)
  
  return(list(mod1, ef1, ef2, ef3))
}



int.side <- cube(s1 = interact[-c(1,2)], s2 = resource[-c(1,2)], s3= biophys[-c(1,2)], s4 = users[-c(1,2)])
bio.side <- cube(s1 = biophys[-c(1,2)] , s2 = env.resys, s3= dat.int[-1], s4 = env.social)
res.side <- cube(s1 = env.resys , s2 =  env.bio, s3= dat.int[-1], s4 = env.social)
soc.side <- cube(s1 = env.social , s2 = env.resys, s3= dat.int[-1], s4 =  env.bio)


# create colors
levelCols <- brewer.pal(k, "Set1") # check for better colours with alpha, k = 7

## an especial plot function that does all at once
plot.cube <- function (x, main, c1, c2, c3, sub, ... ){ #takes a cube object
  c <- c("purple",c1, c2, c3) # will never use purple on loop
  for (i in 2:4){
    ## plot
    ordiplot(x[[1]], type='none', main= paste( main, "vs", sub[i], sep = " "),
             xlim = c(-1.2,1.2), ylim = c(-1,1))
    points(x[[1]]$points, cex=0.7, lwd=0.8, 
           col= levelCols[fitKM$cluster])
    
    ordihull(x[[1]], groups= fitKM$cluster, label = FALSE, cex=0.2,
             col= levelCols, lty=1, draw = 'polygon')
    
    ## Plot environmental fitting
    plot(x[[i]], p.max=0.05, col= c[i], cex=0.5, arrow.mul = 1)
    # plot(x[[3]], p.max=0.05, col= c2, cex=.5)
    # plot(x[[4]], p.max=0.05, col= c3, cex=.5)
  }
}

quartz(width = 7.5, height = 2.5)
par(mfrow = c(1,3), mai = c(0.3,0.3,0.3,0.3))
plot.cube (int.side, main = "Interactions", sub = c( " ","Resource", "Biophysic", "Users"),
           c1 = "gray25", # resource
           c2 = 'blue', # biophysic
           c3 = 'red') # users






