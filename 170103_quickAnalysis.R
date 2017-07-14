rm(list = ls())
set.seed(12345)
# this file combines code from the 161027 draft and the simplified version of the analysis

## load libraries
library  ('gdata')
library ('readxl')
library ('dplyr')
library ('tidyr')
library ('reshape')
library (corrgram)
library (ggplot2)
# library(GGally)

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
# library(kohonen)
library(factoextra)


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

## Update this with new importing data code
setwd("~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha")
# load map for visualizations and data
volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp")


## Read data
load('~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha/161205_Volta.RData')

#### kcals and dams data
dams <- read_excel(path = 'Volta_Dam_density_KLC.xlsx', sheet = 1)
kcals <- read_excel("crop_kcals.xlsx", sheet = 1)


## Select only volta places
dat <- dat %>% volta.only()

### SES variables for later
#### New file with raw values from Katja: 160901
file <- 'Volta_Vars_raw_160901.xlsx'
sn <- sheetNames(xls=file)


dams$TAI_ID2 <- as.factor(dams$TAI_ID1)
# dat <- left_join(dat, dams, by = c("ADM_2", "TAI_ID2"))

users <- read.xls(file, sheet=2, dec=',')
users <- correct.num(users)
users$TAI_ID1 <- as.factor(users$TAI_ID1)
resource <- read.xls (file, sheet=3, dec=',')
biophys <- resource [c(1,2,11:15)]
resource <- resource [-c(11:15)]
biophys <- correct.num(biophys)
resource <- correct.num(resource)
resource$TAI_ID1 <- as.factor(resource$TAI_ID1)
resource <- left_join(resource, dplyr::select(dams, TAI_ID2, Dams), by = c("TAI_ID1" = "TAI_ID2"))

biophys$TAI_ID1 <- as.factor(biophys$TAI_ID1)
biophys$Aridity <- 1-biophys$Aridity  # Invert the scale so results are easy to interpret.
resource <- resource[-c(9)] #delete dams density
# biophys <- read.xls(file, sheet=3, dec=',')

# Threre is 3 datasets for interactions depending on the sheet in Katjas file
# 4 = kcals per distric area in sq.km
# 5 = kcals_harv.area
# 6 = ratio_harv.area
# 7 = other
# interact <- read.xls(file,sheet=5, dec=',')
# interact <- correct.num(interact)
# interact <- volta.only(interact)

### reduce dataset for simplified analysis

social <- dplyr::select(users, TAI_ID1, External_migration, Regional_migration, Pop_trend)
social <- left_join(social, dplyr::select(resource, TAI_ID1,Market_access), by = "TAI_ID1")
users <- dplyr::select(users, TAI_ID1, Farmers, Urbanization, Literacy, Pop_dens_log, Ratio_children, Ratio_women)
eco <- dplyr::select(biophys, TAI_ID1, Aridity, Mean_temp, Soil_water, Wet_season, Slope75)
res.units <- dplyr::select(resource, TAI_ID1, Cattle_sqkm, Small_ruminant_capita)
res.syst <- dplyr::select(resource, TAI_ID1, Dams)


## only 7 key crops, | crop == 'Cocoyam'| crop == 'Cassava'| crop == 'Ground nuts'
datKeyCrops <- (filter(dat, crop == 'Maize' | crop == 'Millet'| crop == 'Rice'| crop == 'Yam'| crop == 'Sorghum'| crop == 'Cowpea'| crop == 'Soy'  ))

datKeyCrops <- drop.levels(datKeyCrops)

## only years with most complete information (3% NA)
datKeyCrops <- (filter(datKeyCrops, Year > 2001, Year < 2010, Year !=2006))

datKeyCrops$TAI_ID2 <- datKeyCrops$TAI_ID1

## Crop dat is now dat from the ExtracDataTAI file
crop.dat <- datKeyCrops

### The imputation doesn't work because we miss crops in a whole country!! So there is no data to inpute, e.g. Cassava
# crops.impute <- list()
#
# for (i in 1:10) { # 10 crops
#   df1 <- dplyr::filter(crop.dat, crop == levels(crop.dat$crop)[1]) %>%
#     dplyr::select(-prod) %>%
#     tidyr::spread(key = Year, value = area) %>%
#
# }

crop.dat$country <- ifelse(as.numeric(as.character(crop.dat$TAI_ID1)) > 3000, 'GH', 'BF')


crop.dat <- crop.dat %>%
  mutate(sq_prod = sqrt(prod), sq_area = sqrt(area)) %>%
  mutate( #dt_prod = sq_prod - mean(sq_prod), Los detrending tienen que calcularse en el agregado de cada TAI_ID1, no para todos los datos!!!
    # dt_area = sq_area - mean(sq_area),
    log_prod = log1p(prod),
    yield = ifelse(prod == 0 | area == 0, 0, prod/area),
    log_yield = log1p(yield),
    log_area = log1p(area) #,
    # Variance needs to be calculated across years!!!
    # var_yield = var(yield, na.rm = T),
    # var_prod = var(prod, na.rm = T),
    # var_area = var(area, na.rm = T)
  )

# variance needs to be calculated over time

tot.area <- crop.dat %>%
  dplyr::select (TAI_ID2, crop, area, Year) %>%
  group_by ( TAI_ID2, Year) %>%
  summarize (tot_area = sum(area, na.rm = TRUE))

crop.dat <- left_join(crop.dat, tot.area, by = c("TAI_ID2", 'Year'))
crop.dat <- crop.dat %>%
  mutate(prop_cultivated_area = area / tot_area )


### Calculate the 7 year mean
crop.area <- crop.dat %>%
  dplyr::select(TAI_ID2, crop, prop_cultivated_area, Year) %>%
  group_by(TAI_ID2, crop) %>%
  summarize(mean_area = var(prop_cultivated_area, na.rm= TRUE))

## remember we are using log-transformed area

## Now do the same for kcals

# kcals
crops.nam <- tolower(levels(crop.dat$crop))
# crops.nam[2] <- 'taro' # cocoyam is aka. taro
crops.nam[6] <- 'soybean'
# crops.nam[4] <- "groundnut"
crops.nam %in% kcals$CROPNAME

cals.fao <- filter(kcals, CROPNAME %in% crops.nam) %>% dplyr::select(crop = CROPNAME, kcals_fao = kcals.per.ton.fao)

cals.fao$crop <- as.factor(cals.fao$crop)
levels(cals.fao$crop) <- levels(crop.dat$crop)

crop.dat <- left_join(crop.dat, cals.fao, by = c("crop"))
crop.dat <- crop.dat %>%
  mutate(kcal_crop = (prod * kcals_fao)) # I calculate the log below in interact
# remember we are using log-transformed production.

## The commented code aggregates kcals per province regardless of crop. For paper I want crops.
# kcals <- crop.dat %>%
#   dplyr::select(TAI_ID2, crop, kcal_crop, Year) %>%
#   group_by(TAI_ID2, crop) %>%
#   summarize(mean_kcal_yr = mean(kcal_crop, na.rm= TRUE)) %>%
#   group_by(TAI_ID2) %>%
#   summarize(kcals = sum(mean_kcal_yr, na.rm = TRUE))

kcals <- crop.dat %>%
  dplyr::select(TAI_ID2, crop, kcal_crop, Year) %>%
  group_by(TAI_ID2, crop) %>%
  summarize(mean_kcal = mean(kcal_crop, na.rm= TRUE))

### Now you need the raw data of population in order to calculate per capita values
popG <- read_excel('GH_Population_2010_TAI_ID.xlsx', sheet = 1)
popG <- popG[!is.na(popG$Region),]
# str(popG)
popB <- read_excel('BF_CountrySTAT_social.xlsx', sheet = 1)
popB <- popB[!is.na(popB$Total),]
# str(popB)

popG <- dplyr::select(popG, TAI_ID1, Total)
popB <- dplyr::select(popB, TAI_ID1, Total)

pop <- bind_rows(popG, popB) %>%
  volta.only()

rm(popG, popB)
pop <- dplyr::select(pop, TAI_ID2 = TAI_ID1, population = Total)
pop$TAI_ID2 <- as.factor(pop$TAI_ID2)

dat.int <- left_join( pop, kcals, by = 'TAI_ID2')
dat.int <- mutate(dat.int, kcal_cap = (mean_kcal / population) )


# #interaction dataset: select the mean for kcals (log)
interact <- dplyr::select(dat.int, TAI_ID2, crop, mean_kcal) %>%
  tidyr::spread(key = crop, value = mean_kcal) %>%
  dplyr::transmute(TAI_ID1 = TAI_ID2, m_cowpea = log1p(Cowpea), m_maize = log1p(Maize), m_millet = log1p(Millet),
                m_rice = log1p(Rice), m_sorghum = log1p(Sorghum), m_soy = log1p(Soy), m_yam = log1p(Yam))


crop.area <- dplyr::select(crop.area, TAI_ID2, crop, mean_area) %>%
  tidyr::spread( key = crop, value = mean_area) %>%
  dplyr::rename(TAI_ID1 = TAI_ID2, a_cowpea = Cowpea, a_maize = Maize, a_millet = Millet,
                a_rice = Rice, a_sorghum = Sorghum, a_soy = Soy, a_yam = Yam)

var_kcals <- crop.dat %>%
  dplyr::select(TAI_ID1, crop, kcal_crop, Year) %>%
  group_by(TAI_ID1, Year) %>%
  summarize(kcals = sum(kcal_crop, na.rm= TRUE)) %>%
  ungroup() %>% group_by(TAI_ID1) %>%
  summarize(sd_kcals = sd(kcals, na.rm = T)) #sd is on units of the original data = kcals



dfx <- left_join(interact, crop.area)

####### To Do #######
### step missing: combine res.syst with crop_area; variance_kcals.
res.syst <- left_join(res.syst, var_kcals, by = 'TAI_ID1') #%>%
 # left_join(., crop.area)


## ggplot(crop.dat, aes (Year, prop_cultivated_area, color = TAI_ID1)) + geom_line(show.legend = F) + facet_wrap(~crop)


####### Now merge data for clustering, it's ready!
df <- left_join(biophys, users) %>%
  left_join(., social) %>%
  left_join(., res.units) %>%
  left_join(., res.syst) %>%
  left_join(., interact)

str(df)
#### check the distribution of variables
# X dataset for plotting final distributions in ggplot
x <- gather(df[-c(1,2)], key = variable) # df[1] is ADM_2 the names of the provinces
x$variable <- as.factor(x$variable)
g <- ggplot (x, aes(clus, value), group = x) + geom_boxplot(aes(color = clus))
g + facet_wrap(~variable, scales = 'free')

# GGally::ggscatmat(df[3:28], columns = 1:25, color = factor(clus), alpha = 0.5, corMethod = 'pearson')

rm(x)

### Re-scale only variables that are not already between 0:1
# Note this is non-normalized data, original values. After the following line all is "rescaled" to 0-1
df[-c(1,2)] <- apply(df[-c(1,2)], 2, rescale_hw)


############ Clustering
## Using mahalanobis distance help us dealing with colinearity or strong correlations.
d <- vegdist(df[-c(1,2)], method = "mahalanobis", tol= 10e-20)
## Validating number of clusters
## Number of clusters with NdClust: uses 30 different index and compare them to decide the optimal partition
# euclidean and manhattan are not good for gradient separation (see help vegdist)
m <- "ward.D2" # minimize the total within-cluster variance

# J170309: Shall we drop cowpeas due to high correlations?
clust_num <- NbClust( data = df[-c(1,2)], distance = 'maximum',
                      min.nc = 3, max.nc = 9, method = m, alphaBeale = 0.1, index = 'alllong')

# clust_num2 <- eclust(x = df[-c(1,2)], FUNcluster = c("kmeans", "pam", "clara", "fanny", "hclust", "agnes", "diana"), 
#                      k = NULL, k.max = 10, stand = FALSE, graph = F,  hc_metric = "maximum", hc_method = "ward.D2", 
#                      gap_maxSE = list(method = "firstSEmax", SE.factor = 1), nboot = 100, 
#                      verbose = interactive(), seed = 123)

# clust_num_mb <- NbClust( data = df[-c(1,2)], diss = d, distance = NULL,
#                          min.nc = 3, max.nc = 9, method = m, alphaBeale = 0.1, index = 'all')


## Stability validation
stab <- clValid(obj=as.data.frame(df[-c(1,2)]), nClust=c(3:9),
                clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny', 'som',
                            'pam', 'sota', 'clara', 'model'),
                validation='stability')

# Internal validation
intern <- clValid(obj=as.data.frame(df[-c(1,2)]), nClust=c(3:9),
                  clMethods=c('hierarchical', 'kmeans', 'diana', 'fanny','som',
                              'pam', 'sota', 'clara', 'model'),
                  validation='internal')


summary(intern)
summary(stab)

### Now I'm getting 3,6,9 (3 in internal, 3,9 in stability, and 6 in clust_num: write the paper with that.)
# This is the case when I keep area out of the analysis because of correlations with kcals. When included,
# main results is still 6 with 3 in internal, and 4,9 in stability with hierarchical, kmeans, and model.


library (corrgram)
corrgram(dfx, upper.panel = 'panel.pts', lower.panel = "panel.cor" )

## Prepare the clustering result dataset
mds <- metaMDS(df[-c(1,2)], distance = 'manhattan', trymax = 1000, verbose = FALSE)

########################
### Clustering all
########################

quartz(width=8, height= 3, pointsize = 7)
# number of clusters
k <- c(6)

par(mfrow=c(2,6))

for (i in 1:length(k)){
  par(mai = c(0.1, 0.2, 0.1 , 0.1))

  ## Best selected algorithms: hierarchical, k-means and som
  fitKM <- kmeans(df[-c(1,2)], centers= k[i] ,iter.max=2000,nstart=50)
  fitSOM <- som(as.matrix(df[-c(1,2)]), grid= somgrid(3,2,'hexagonal'))
  fitPAM <-pam(mds$points, metric='manhattan', k=k[i])
  dis <- vegdist(df[-c(1,2)], method='manhattan')
  clus <- hclust(dis, method='ward.D2')
  grp <- cutree(clus, k[i])
  fitHier <- grp
  fitClara <- clara(df[-c(1,2)], k = k[i])

  # create a clusters dataframe
  clusters <- data.frame(TAI_ID1 = df$TAI_ID1, # codes
                         Clara = as.vector(fitClara$clustering ),
                         k_means = as.vector(fitKM$cluster),
                         PAM = as.vector(fitPAM$clustering),
                         SOM = as.vector(fitSOM$unit.classif),
                         Hierarchical = as.vector(fitHier),
                         Ward = clust_num$Best.partition
  )

  # create colors
  levelCols <- brewer.pal(k [i], "Set3")

  # plot
  for (j in 2:7){
    # Plot MDS with grouping according with each clustering algorithm
    ordiplot(mds, type='none', main= paste(letters[j-1], colnames( clusters)[j],  sep = '.) '), xlab = NULL, ylab = NULL )
    points(mds$points, cex=1, lwd=1.5,
           col= levelCols[clusters[,j]]) #volta.shp@data[,i]
    ordihull(mds, groups= clusters[,j], label = TRUE, cex=0.7,
             col="purple", lty=1)
    #plot(ef1, p.max=0.05, col='blue', cex=0.5)
    #plot(ef2, p.max=0.05, col='purple', cex=0.5)
  }


  for (j in 2:7){
    par(mai = c(0.2, 0.2, 0.2 , 0.2))
    # Plot map with resulting clusters
    plot(volta.shp, col= levelCols[clusters[,j]],lwd=0.1)
    # main= paste(k[i], colnames( clusters)[j], sep = ' '))

  }
}

# plot(volta.shp, col = levelCols[clust_num_mb$Best.partition], lwd = 0.1)
# setwd("~/Documents/Projects/TAI/figures")
# quartz.save(file = '161125_full_clustering.png', type = 'png', dpi = 300,width=15, height=5, pointsize = 7 )

#############################
#### Visualizations for paper
############################


k <- 6 # optimal number of clusters
avg <- apply (df[-c(1,2)], 2, mean)

dis <- vegdist(df[-c(1,2)], method='manhattan')
clus <- hclust(dis, method='ward.D2')
grp <- cutree(clus, k)
fitHier <- grp

## All the fits were calculated beforehand.
# following Garry's code with k-means
#fitKM <- kmeans(df[-c(1,2)], centers= k ,iter.max=2000,nstart=50)
#fitPAM <-pam(mds$points, metric='manhattan', k = k)

df$clus <- grp

## Colors
clusColors <- c('gray90', 'gray10')
# freq <- order(fitKM$size, decreasing = TRUE)
# freq <- order(fitPAM$clusinfo[,1], decreasing = TRUE)
freq <- order(table(fitHier), decreasing = TRUE)

# colnames(fitKM$centers)

U_Cols <- colorRampPalette(c('yellow', 'orange'), interpolate = "linear", alpha = T) #brewer.pal(length(users) - 2, "Reds") #10
R_Cols <- colorRampPalette(c('tan', 'olivedrab4'), interpolate = "linear", alpha = T)#brewer.pal(length(resource)- 2, "Greys") #9
E_Cols <- colorRampPalette(c('salmon', 'purple4'), interpolate = "linear", alpha = T) #brewer.pal(length(biophys)- 2, "PuBu") # 14
I_Cols <- colorRampPalette(c('steelblue4', 'red4'), interpolate = "linear", alpha = T) #brewer.pal(length(interact)- 1, "YlGn") #5

FlowerCol <- c(E_Cols(5), U_Cols(10), R_Cols(4), I_Cols(7))

## ploting
quartz(width=7, height = 7, pointsize = 8)
par(mfrow = c(4,4), mai = c(.02, 0.2, 0.2, 0.2))
layout (matrix(c(1:12, rep(13,4)), 4,4, byrow = T))

for ( i in 1:k) {
  varCols <- clusColors[(as.vector(fitHier) == freq[i]) + 1]
  plot (volta.shp, col = varCols, lwd = 0.1, main= letters[i], cex.main = 1.5 )

  stars (t (dplyr::filter(df[-c(1,2)], clus == i) %>% dplyr::select(-clus) %>% colMeans() )  ,  len = 0.6, full = TRUE, scale = FALSE,
         radius = TRUE, draw.segments = TRUE, col.segments = FlowerCol)
}





# legend
par( mai = c(1,1,1,1))
leg <- data.frame (leg= rep(1, length(df) -2 ), row.names =  colnames(df[-c(1,2)]))  # dat[-c(23)]
stars (t(leg), len = 1, full = TRUE, scale = FALSE, key.loc = c(2.3, 2.3), locations = c(2.3, 2.3),
       radius = TRUE, draw.segments = TRUE, col.segments = FlowerCol,
       axes = F, plot = T, cex = 0.7)

# quartz.save(file = '161125_SESarchetypes.png', type = 'png', dpi = 200,width=7, height=7, pointsize = 8 )



###### CUBE
resource <- left_join(res.syst, res.units)
social2 <- left_join(social, users)


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



int.side <- cube(s1 = interact[-c(1)], s2 = resource[-c(1)], s3= eco[-c(1)], s4 = social2[-c(1)])
bio.side <- cube(s1 = eco[-c(1)] , s2 =  resource[-c(1)], interact[-c(1)], s4 = social2[-c(1)])
res.side <- cube(s1 =  resource[-c(1)] , s2 =  eco[-c(1)], s3= interact[-c(1)], s4 = social2[-c(1)])
soc.side <- cube(s1 = social2[-c(1)] , s2 =  resource[-c(1)], s3= interact[-c(1)], s4 =  eco[-c(1)])


# create colors
levelCols <- brewer.pal(k, "Set3") # check for better colours with alpha, k = 7

## an especial plot function that does all at once
plot.cube <- function (x, main, c1, c2, c3, sub, ... ){ #takes a cube object
  c <- c("purple",c1, c2, c3) # will never use purple on loop
  for (i in 2:4){
    ## plot
    ordiplot(x[[1]], type='none', main= paste( main, "vs", sub[i], sep = " "), xlab = NULL, ylab = NULL)#,
    #xlim = c(-1.2,1.2), ylim = c(-1,1))
    points(x[[1]]$points, cex=0.7, lwd=0.8,
           col= levelCols[as.vector(fitPAM$clustering)])

    ordihull(x[[1]], groups= as.vector(fitPAM$clustering), label = FALSE, cex=0.2,
             col= levelCols, lty = 0, draw = 'polygon', alpha = 0.25)

    ## Plot environmental fitting
    plot(x[[i]], p.max=0.001, col= c[i], cex=1)
    # plot(x[[3]], p.max=0.05, col= c2, cex=.5)
    # plot(x[[4]], p.max=0.05, col= c3, cex=.5)
  }
}

quartz(width = 5, height = 7, pointsize = 8)
par(mfrow = c(4,3), mai = c(0.2,0.2,0.2,0.2))
# Interactions
plot.cube(int.side, main = "Interactions", sub = c( " ","Resource", "Ecological", "Social"),
          c1 = "gray20", # resource
          c2 = "dodgerblue4", # biophysic
          c3 = "darkred") # users
# Resouce
plot.cube(res.side, main = "Resource", sub = c("", "Ecological", "Interactions", "Social"),
          c1 = "dodgerblue4", c2 = "darkgreen", c3 = "darkred")
# biophysic
plot.cube (bio.side, main = "Ecological", sub = c("", "Resource", "Interactions", "Social"),
           c1 = "gray20", c2 = "darkgreen", c3 = "darkred")
# Users
plot.cube(soc.side, main = "Social", sub = c('', "Resource", "Interactions", "Ecological"),
          c1 = "gray20", c2 = "darkgreen", c3 = "dodgerblue4")


# quartz.save(file = '161125_Cube.png', type = 'png', dpi = 200,width=5, height=7, pointsize = 8 )

# quartz(width = 5, height = 5, pointsize = 8)
# ordiplot(int.side[[1]], type='none' )
# points(int.side[[1]]$points, cex=0.7, lwd=0.8,
#        col= levelCols[as.vector(fitPAM$clustering)])
# ordihull(int.side[[1]], groups= as.vector(fitPAM$clustering), label = FALSE, cex=0.2, col= levelCols, lty = 0, draw = 'polygon', alpha = 0.25)
# plot(int.side[[2]], p.max=0.001, cex=1)
