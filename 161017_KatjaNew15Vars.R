## load libraries
library  ('gdata')
library ('dplyr')
library ('reshape')
library (corrgram)
library(vegan)
library (ggplot2)

# load libraries for mapping
library(maptools)
library(rgeos)
library(RColorBrewer)

# load libraries for clustering
library (vegan)
# library(rgl)
# library(cluster)
library(NbClust)
library(clValid)
# library(MASS)
library(kohonen)


## Functions
setwd("~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha")
# load map for visualizations and data
volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp")

##### Useful functions for later:

### Select only districts on the Volta Basin
volta.only <- function(x){
  x <- x [ is.element (x$TAI_ID1, volta.shp@data$TAI_ID1 ), ]
  x <- droplevels(x)
}

### Normalizing by scaling everything to 0:1
rescale_hw <- function (x){ # This is Hadley Wickman function from his book R for DataScience
  rng <- range(x, na.rm= TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

### Correct the numeric format from excel files
correct.num <- function (x){
  l <- length(x)
  x[3:l] <- apply (x[3:l], 2, function (x){as.numeric(x)} )
  return(x)
}

## Data
## Read data
#### New file with raw values from Katja: 160901 / 161017
file <- 'TAI_Variables_1610_2.xlsx'
sn <- sheetNames(xls=file)

dat <- read.xls (file, sheet = 2)
str(dat)


## Different tries with distance
d <- vegdist (dat[-1], method = "mahalanobis", tol= 10e-20)
# d <- mahalanobis(dat[-1], center = c(0,0), cov = cov(dat[-1]), tol = 10e-20)
# d <- dist (dat[-1], method = 'euclidean', diag = F)


# Validating number of clusters
# Number of clusters with NdClust: uses 30 different index and compare them to decide the optimal partition
library (NbClust)
# euclidean and manhattan are not good for gradient separation (see help vegdist)
clust_num <- NbClust( data = dat[-1], diss = d, dist = NULL,
                      min.nc = 2, max.nc = 12, method = 'ward.D2', alphaBeale = 0.1, index = 'all')
#dist = 'manhattan',  # diss = designdist(full [-c(2,23)], '1-J/sqrt(A*B)' )


library (clValid)
## Internal validation
intern <- clValid(obj=as.data.frame(dat[-c(1)]), nClust=c(2:9),
                  clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny','som',
                              'pam', 'sota', 'clara', 'model'),
                  validation='internal')

## Stability validation
stab <- clValid(obj=as.data.frame(dat[-c(1)]), nClust=c(2:9),
                clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny', 'som',
                            'pam', 'sota', 'clara', 'model'),
                validation='stability')

summary (intern)
summary (stab)


## Prepare the clustering result dataset
mds <- metaMDS(dat[-c(1)], distance = 'manhattan', trymax = 1000, verbose = FALSE)
setwd('~/Documents/Projects/TAI/scripts/TAI-Volta')

### Explore the correlation problem
library (corrgram)
quartz(height = 4, width = 4)
## users
corrgram(dat[c(1:9)], type = "data", order = "PCA", lower.panel = panel.cor,
        upper.panel = panel.pts, diag.panel = panel.density, main = 'users')
## interactions
corrgram(dat[23:36], type = "data", order = "PCA", lower.panel = panel.cor,
        upper.panel = panel.pts, diag.panel = panel.density, main = 'interactions')

## biophysical
corrgram(dat[17:21], type = "data", order = "PCA", lower.panel = panel.cor,
        upper.panel = panel.pts, diag.panel = panel.density, main = 'biophysical')
## resource
corrgram(dat[c(10:16, 37)], type = "data", order = "PCA", lower.panel = panel.cor,  upper.panel = panel.pts, diag.panel = panel.density, main = 'resource')
