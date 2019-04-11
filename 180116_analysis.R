## Analysis for the Volta paper
## Juan Rocha
## juan.rocha@su.se

## Here I kept the code that worked droping all intermediate stages. It comes from the draft Rmd document that is taking too long time to compile. So Now I calculate all analysis and figures separately and only import objects there.
# this file combines code from the 161027 draft and the simplified version of the analysis

## Load required libraries
set.seed(12345)


## load libraries
library(tidyverse)
library(forcats)
library(gdata)
library(readxl)
# library (corrgram)
# library(GGally)
library(broom)

# load libraries for mapping
library(maptools)
library(rgeos)
library(RColorBrewer)
# library(ggmap)
# library(grid)
# library(gridExtra)

# load libraries for clustering
library (vegan)
# library(rgl)
# library(cluster)
library(NbClust)
library(clValid)
# library(MASS)
# library(kohonen)
# library(FactoMineR)
# library(factoextra)

# setwd('~/Documents/Projects/TAI/scripts/TAI-Volta')


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
volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp", proj4string = CRS('+init=epsg:4378'))


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
# crops.nam %in% kcals$CROPNAME

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
popG <- read_excel('~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha/GH_Population_2010_TAI_ID.xlsx', sheet = 1)
popG <- popG[!is.na(popG$Region),]
# str(popG)
popB <- read_excel('~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha/BF_CountrySTAT_social.xlsx', sheet = 1)
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

res.syst <- left_join(res.syst, var_kcals, by = 'TAI_ID1') #%>%

### Final dataset for the rest of the analysis
df <- left_join(biophys, users) %>%
  left_join(., social) %>%
  left_join(., res.units) %>%
  left_join(., res.syst) %>%
  left_join(., interact)


### Re-scale only variables that are not already between 0:1
# Note this is non-normalized data, original values. After the following line all is "rescaled" to 0-1
df[-c(1,2)] <- apply(df[-c(1,2)], 2, rescale_hw)

## correct names for final Version
corrected_names <- c(
    "Aridity", "Mean temperature", "Soil water", "Wet season", "Slope 75%", 'Farmers', 'Urbanization','Literacy', "Population density [log]", "Ratio of children", "Ratio of women", "External migration", "Regional migration", "Population trend","Market access", "Cattle", 'Small ruminants', "Dams", "SD Kilocalorie", 'Cowpea', "Maize", "Millet", "Rice", "Sorghum", "Soy", "Yam")
names(df)[3:28] <- corrected_names

############ Clustering
## Using mahalanobis distance help us dealing with colinearity or strong correlations.
d <- vegdist(df[-c(1,2)], method = "mahalanobis", tol= 10e-20)
## Validating number of clusters
## Number of clusters with NdClust: uses 30 different index and compare them to decide the optimal partition
# euclidean and manhattan are not good for gradient separation (see help vegdist)
m <- "ward.D2" # minimize the total within-cluster variance

# J170309: Shall we drop cowpeas due to high correlations?
clust_num <- NbClust( data = df[-c(1,2)], distance = 'maximum',
                      min.nc = 3, max.nc = 9, method = m, alphaBeale = 0.1, index = 'all')

# clust_num_mb <- NbClust( data = df[-c(1,2)], diss = d, distance = NULL,
#                          min.nc = 3, max.nc = 9, method = m, alphaBeale = 0.1, index = 'all')

# library(kohonen)
# Stability & Internal validation
stab <- clValid(
    obj=as.matrix(df[-c(1,2)]), nClust=c(3:9),
    clMethods=c("hierarchical", "kmeans", "diana", "fanny", #"som",
     "model", "sota", "pam", "clara", "agnes"),
    validation=c('stability', "internal"),
    #metric = "manhattan", method = "ward",
    verbose = FALSE
            )

# Internal validation
# intern <- clValid(
#     obj=as.data.frame(df[-c(1,2)]), nClust=c(3:9),
#     clMethods=c("hierarchical", "kmeans", "diana", "fanny", #"som",
#     "model", "sota", "pam", "clara", "agnes"),
#     validation=c('stability', "internal"),
#     #metric = "manhattan", method = "ward",
#     verbose = FALSE)

## Prepare the clustering result dataset
mds <- metaMDS(df[-c(1,2)], distance = 'manhattan', trymax = 1000, verbose = FALSE)

### Extract the results from sensitivity analysis on optimal number of clusters:
clust_results <- clust_num$Best.nc %>% t() %>% as.data.frame() %>% rownames_to_column(var = "Index")
## remember that Beale and Dindex are both graphical methods, one needs to check the graph produced by NbClust and manually add the suggested number of clusters by these methods:
clust_results[23,2] <- 5# Hubert
clust_results[25,2] <- 6# Dindex

### If interested of further processing algorithm selection, here is how to extract the info and convert to data_frame
# slot(stab, "measures") %>%
#     as_tibble() %>%
#     add_column(measure = slot(stab, "measNames")) %>%
#     gather(key = alg_num, value = value, 1:63) %>%
#     separate(alg_num, into=c("clust_number", "algorithm")) %>% mutate(clust_number = as.numeric(clust_number), algorithm = as_factor(algorithm)) %>% filter(clust_number == 6) %>%
#     ggplot(
#         aes(algorithm, value)) +
#         geom_col(fill = "dodgerblue4", alpha = 0.5) +
#         facet_wrap(~measure, scales = "free_x", ncol = 7) + coord_flip() + theme_minimal(base_size=6)

########################
### Clustering all
########################
library(kohonen)

k <- 6

## Best selected algorithms: hierarchical, k-means and som
fitKM <- kmeans(df[-c(1,2)], centers= k ,iter.max=2000,nstart=50)
fitSOM <- som(as.matrix(df[-c(1,2)]), grid= somgrid(3,2,'hexagonal'))
fitPAM <-pam(df[-c(1,2)], metric='manhattan', k=k)
dis <- vegdist(df[-c(1,2)], method='manhattan')
clus <- hclust(dis, method='ward.D2')
grp <- cutree(clus, k)
fitHier <- grp
fitClara <- clara(df[-c(1,2)], k = k)

# create a clusters dataframe
clusters <- data.frame(TAI_ID1 = df$TAI_ID1, # codes
                     Clara = as.vector(fitClara$clustering ),
                     k_means = as.vector(fitKM$cluster),
                     PAM = as.vector(fitPAM$clustering),
                     SOM = as.vector(fitSOM$unit.classif),
                     Hierarchical = as.vector(fitHier),
                     Ward = clust_num$Best.partition
  )

### Cube comparison:

resource <- left_join(res.syst, res.units)
social2 <- left_join(social, users)

######################
### The cube
######################

cube <- function(s1, s2, s3, s4){ # each side of the cube is s_
  mod1 <- metaMDS(s1, distance = 'manhattan', trymax = 1000, verbose = FALSE)
  ef1 <- envfit(mod1, s2, permu=999, verbose = FALSE)
  ef2 <- envfit(mod1, s3, permu=999, verbose = FALSE)
  ef3 <- envfit(mod1, s4, permu=999, verbose = FALSE)

  # ## plot
  # plot(mod1, type='p', display='sites', cex=0.8)
  # plot(ef1, p.max=0.05, col='blue', cex=0.8)
  # plot(ef2, p.max=0.05, col='purple', cex=.8)
  # plot(ef3, p.max=0.05, col='grey', cex=.8)

  return(list(mod1, ef1, ef2, ef3))
}

# correct names for final version

names(eco)[-1] <- corrected_names[1:5]
names(social2)[-1] <- corrected_names[c(12:15, 6:11)]
names(resource)[-1] <- corrected_names[c(18,19,16,17)]
names(interact)[-1] <- corrected_names[20:26]

int.side <- invisible(cube(s1 = interact[-c(1)], s2 = resource[-c(1)], s3= eco[-c(1)], s4 = social2[-c(1)]))
bio.side <- cube(s1 = eco[-c(1)] , s2 =  resource[-c(1)], interact[-c(1)], s4 = social2[-c(1)])
res.side <- cube(s1 =  resource[-c(1)] , s2 =  eco[-c(1)], s3= interact[-c(1)], s4 = social2[-c(1)])
soc.side <- cube(s1 = social2[-c(1)] , s2 =  resource[-c(1)], s3= interact[-c(1)], s4 =  eco[-c(1)])

###################################
# Idea for regression
###################################

kcals_cap <- crop.dat %>%
  dplyr::select(TAI_ID2, crop, kcal_crop, Year) %>%
  group_by(TAI_ID2) %>%
  summarize(mean_kcal = mean(kcal_crop, na.rm= TRUE)) %>%
  left_join(pop) %>%
  mutate(kcal_capita = mean_kcal / population) %>%
  rename(TAI_ID1 = TAI_ID2)

# #interaction dataset: select the mean for kcals (now it's not in log, I conserve raw units in kcals but re-scaled by / 10^6)
interact_2 <- dplyr::select(dat.int, TAI_ID2, crop, mean_kcal) %>%
    mutate(mean_kcal = mean_kcal/10^6) %>% # now units are millions of kcals
    tidyr::spread(key = crop, value = mean_kcal ) %>%
    dplyr::transmute(TAI_ID1 = TAI_ID2, m2_cowpea = Cowpea, m2_maize = Maize, m2_millet = Millet, m2_rice = Rice, m2_sorghum = Sorghum, m2_soy = Soy, m2_yam = Yam)

## calculate again standard deviation of kcals per crop for regression.
#sd is on units of the original data = kcals
var_kcals <- crop.dat %>%
    dplyr::select(TAI_ID1, crop, kcal_crop, Year) %>%
    group_by(TAI_ID1, Year, crop) %>%
    summarize(kcals = sum(kcal_crop, na.rm= TRUE)) %>%
    ungroup() %>% group_by(TAI_ID1, crop) %>%
    summarize(sd_kcals = sd(kcals, na.rm = T)/10^6) %>% ## in millions kcals
    tidyr::spread( key = crop, value = sd_kcals) %>%
    dplyr::rename(sd_cowpea = Cowpea, sd_maize = Maize, sd_millet = Millet,
                  sd_rice = Rice, sd_sorghum = Sorghum, sd_soy = Soy, sd_yam = Yam)

## Here I'm joining the df used for the analysis wihtout normalizing plus few extra variables just calculated for the regression.
df_lm <- left_join(biophys, users) %>%
  left_join(., social) %>%
  left_join(., res.units) %>%
  left_join(., res.syst) %>%
  left_join(., interact) %>%
    left_join(kcals_cap) %>%
    left_join(crop.area) %>%
    left_join(var_kcals) %>%
    left_join(interact_2) %>%
    # add_column(cluster = as.vector(fitHier)) %>%
    as_tibble()

df$cluster <- as.vector(fitHier)

fit <- list()

## purrr way (welcome to the paralel world!):
fit <- map2(select(df_lm, starts_with("m2_")), select(df_lm, starts_with("a_")), ~ lm(.x ~ 0 + .y + Aridity + Mean_temp + Wet_season + Soil_water + Slope75 + Farmers + Literacy + Market_access + Dams, data = df_lm))

# fit[[8]] <- lm(Cattle_sqkm ~ 0  + Aridity + Mean_temp + Wet_season + Soil_water + Slope75 + Farmers + Literacy + Market_access + Dams, data = df_lm)
#
# fit[[9]] <- lm(Small_ruminant_capita ~ 0  + Aridity + Mean_temp + Wet_season + Soil_water + Slope75 + Farmers + Literacy + Market_access + Dams, data = df_lm)

z <- names(select(df_lm, starts_with("m2_")))

df_kc <- fit %>% purrr::map(tidy) %>%
    purrr::map(function(x){x$term[1] <- "area";return(x)})

for(i in seq_along(z)) {df_kc[[i]]$response <- z[[i]] }

df_kc <- df_kc %>% purrr::map(
    function(x) separate(x, col = response, into = c("type", "response"))
)

## fit for sd kcals
fit_sd <-  map2(
    select(df_lm, starts_with("sd_"), -sd_kcals),
    select(df_lm, starts_with("a_")), ~ lm(.x ~ 0 + .y + Aridity + Mean_temp + Wet_season + Soil_water + Slope75 + Farmers + Literacy + Market_access + Dams, data = df_lm))


z <- names(select(df_lm, starts_with("sd_"), -sd_kcals))

df_sd <- fit_sd %>% purrr::map(tidy) %>%
    purrr::map(function(x){x$term[1] <- "area";return(x)})

for(i in seq_along(z)) {df_sd[[i]]$response <- z[[i]] }

df_sd <- df_sd %>% purrr::map(
    function(x) separate(x, col = response, into = c("type", "response"))
)

# df_list
# df_r2 <- data_frame(
#     model = c("model 1", "model 2", "model 3", "model 4"),
#     r_squared = unlist(lapply(out, r.squared))
# )



# bind_rows(
#     bind_rows(df_kc),
#     bind_rows(df_sd)
# ) %>% mutate(term = as_factor(term) %>% fct_rev()) %>%
# filter(term != 'area') %>%
#     ggplot(aes(estimate, term)) +
#         geom_vline(xintercept = 0, color = "grey84", linetype = 2) +
#         geom_point(aes(shape = ifelse(
#             p.value < 0.05, "< 0.05" ,
#                 ifelse(p.value < 0.1, "< 0.1", "> 0.1")
#             )), size = 2, show.legend = TRUE) +
#         scale_shape_manual(name = "p value", values = c(19,7,1)) +
#         geom_errorbarh(aes(xmin = estimate - std.error , xmax = estimate + std.error, height = .25))+
#         #geom_text(data = df_r2, x=-Inf, y=Inf, hjust = 0, vjust = 45, size = 3, aes(label = paste("italic(R) ^ 2 == ", round(r_squared,2))), parse = T) +
#         theme_light(base_size = 6)+ theme(legend.position = "bottom")+
#         facet_grid(response ~ type, scales = "free")

setwd("~/Documents/Projects/TAI/scripts/TAI-Volta")
save.image("180601_Volta.RData")
