## Extract and clean data for Volta Basin
## TAI project
## Data was collected and pre-processed in Excel by Katja Malmborg

## by Juan Carlos Rocha
## juan.rocha@su.se
## Version July 21 2016

# clean workspace
rm(list=ls())

# load libraries for data and ploting
library(tidyr)
library('dplyr')
library ('ggplot2')
library(readxl)

# load libraries for mapping
library(maptools)
library(rgeos)
library(RColorBrewer)


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

# clean repeated columns and standardize names
volta.shp@data <- select(volta.shp@data, -Province_district.y, -Farmers.y, -Aver_yield, -Yield_var,
                         -Women, -Children, -Cattle_den, -Crop_div, Urban ) %>%
  rename (Farmers = Farmers.x, Province_district = Province_district.x, Ratio_female = Ratio_women) %>%
  mutate (Dam_dens = Dams / max(Dams), Aridity = 1- Aridity, Pop_dens_log_norm = log(Pop_density) / max( log(Pop_density))) %>%
  select (-Pop_dens, -Urban, -Pop_density, -Dams) # get rid of colinear variables

## checking for correlations
# summary (volta.shp@data)
# library (corrgram)
# corrgram(select(volta.shp@data, -Province_district, -Region, -ADM_2, -TAI_ID1),
#          order=T, main="Correlations",
#          lower.panel=panel.pts,
#          upper.panel=panel.conf,
#          diag.panel=panel.density)
#
# head(crop)

## J160725: Up to here, the data is saved on the shape file for the volta basing, namely volta.shp@data.
# It contains selected variables for the Ostrom SES framework tiers but not for interactions. All
# variables were 'normalized' or at least scale to 0-1 range. Only pop_density was log-scaled.

# Calculate yields ?


# Now extract the data for interactions based on crop data
dat <- mutate(dat, sq_prod = sqrt(prod))
dat <- mutate(dat, dt_prod = sq_prod - mean(sq_prod))
dat <- mutate(dat, log_prod = log (1+prod))
dat <- mutate(dat, yield = ifelse(prod == 0 | area == 0, 0, prod/area))# Inf values when area = 0
dat <- mutate(dat, log_yield = log (1+yield))
# summary (dat$prod / dat$area)


p <- ggplot (data = dat , aes (log_yield, colour = crop) ) +
  geom_density ( alpha=0.1,  show.legend = T) + facet_wrap (~TAI_ID2, nrow = 10, ncol= 10)
p

## not working with the lines for each facet :(
for (i in 1:length(levels(as.factor(dat$crop)))){
  p <- ggplot (data = filter(dat, crop == levels(as.factor(dat$crop))[i]) , aes (log_prod) ) + #  crop == levels(as.factor(dat$crop))[i]
    geom_density () + #geom_vline(aes(xintercept = mean(sq_prod), color = 'purple'), show.legend = F) +
    # geom_vline(aes(xintercept = median(sq_prod), color = 'orange'), show.legend = F) +
    facet_wrap (~TAI_ID2, nrow = 10, ncol= 10)
  p + ggtitle (levels(as.factor(dat$crop))[i])
  ggsave (filename = paste('cropDensity_', levels(as.factor(dat$crop))[i], '.png', sep=''),
          device = 'png', width= 10, height = 10, units = 'in', dpi = 200 )
}


#########################################
# First summarize / reduce croping data #
#########################################

# calculate the 5yrs means
str(dat)
dat$year <- as.numeric(dat$year); dat$year <- dat$year + 2000
dat$crop <- as.factor(dat$crop)

tot.area <- dat %>%
  select (TAI_ID2, crop, area, year) %>%
  group_by ( crop, year) %>%
  summarize (tot_area = sum(area))

dat <- left_join(dat, tot.area, by = c('crop', 'year'))
dat <- dat%>%
  mutate (prop_cultivated_area = area / tot_area)

rm(tot.area)



delta_5yr <- list()

for (i in 1:7){
  delta_5yr [[i]] <- dat %>%
    filter (year == seq(2001,2008,1)[i]:seq(2005,2012,1)[i]) %>%
    select (TAI_ID2, crop, prop_cultivated_area, year) %>%
    group_by (TAI_ID2, crop) %>%
    summarize (mean_prop_cult_area_5yrs = mean(prop_cultivated_area,  na.rm = T) ) %>%
    mutate (time = i)
}

delta_5yr <- dplyr::bind_rows(delta_5yr) # now you have the change over time of the 5yr window for proportion of cultivated area.
# if you want only the last window
# filter(delta_5yr, time == 7)

# ggplot(data = delta_5yr, aes (y = mean_prop_cult_area_5yrs, x = time, color = crop)) +
#     geom_line() + facet_wrap (~TAI_ID2, nrow = 10, ncol= 10)

## Now do the same for kcals

kcals_5yr <- list()

for (i in 1:7){
  kcals_5yr [[i]] <- dat %>%
    filter (year == seq(2001,2008,1)[i]:seq(2005,2012,1)[i]) %>%
    select (TAI_ID2, crop, prod, year) %>%
    group_by (TAI_ID2, crop) %>%
    summarize (mean_tons_5yrs = mean(prod, na.rm = T) ) %>%
    mutate (time = i)
}

kcals_5yr <- bind_rows(kcals_5yr)


# Compile a dataframe with all data to use.
# kcals
crops.nam <- tolower (levels (kcals_5yr$crop))
crops.nam[2] <- 'taro' # cocoyam is aka. taro
crops.nam %in% kcals$CROPNAME

cals.fao <- filter(kcals, CROPNAME %in% crops.nam)[c(1,9,2:8,10),3]

#interaction dataset on kcals

rescale01 <- function (x){
  rng <- range(x)
  (x - rng[1] / (rng[2] - rng[1]))
}


dat.int <- list ()

for (i in 1:7){
  dat.int [[i]] <- kcals_5yr %>% dplyr::select(TAI_ID2, crop, mean_tons_5yrs, time) %>%
    filter (time == i) %>%
    group_by(TAI_ID2, crop) %>%
    summarise (m= mean (mean_tons_5yrs, na.rm = T)) %>% #median on sq_prod is also possible / add the yield.
    spread (key = crop, value = m)

  # Step 1: kcals first
  dat.int[[i]][-1] <- t(apply(as.matrix(dat.int[[i]][-1]), 1, function(x){x * as.vector(as.matrix(cals.fao))/ 10^6})) ## millions of KCals
  # dat.int # now data is in mean Kcals *10^6

  dat.int [[i]] <- mutate (dat.int[[i]], time = i)

}

dat.int <- bind_rows(dat.int)

# Step 2: normalize (optional but important for MDS)
dat.int <- mutate (dat.int,
                   sum_mean_kcals_5yrs = Cassava + Cocoyam+ Cowpea + Groundnut+ Maize+ Millet+ Plantain+ Rice+Sorghum +Yam ) %>%
  mutate (norm_sum_mean_kcals_5yrs = rescale (sum_mean_kcals_5yrs)) %>%
  transmute (TAI_ID2 = TAI_ID2,
             time = time,
             Cassava = Cassava / sum_mean_kcals_5yrs,
             Cocoyam = Cocoyam / sum_mean_kcals_5yrs,
             Cowpea = Cowpea / sum_mean_kcals_5yrs,
             Groundnut = Groundnut / sum_mean_kcals_5yrs,
             Maize = Maize / sum_mean_kcals_5yrs,
             Millet = Millet / sum_mean_kcals_5yrs,
             Plantain = Plantain / sum_mean_kcals_5yrs,
             Rice = Rice / sum_mean_kcals_5yrs,
             Sorghum = Sorghum / sum_mean_kcals_5yrs,
             Yam = Yam / sum_mean_kcals_5yrs,
             norm_sum_mean_kcals_5yrs= norm_sum_mean_kcals_5yrs
             )

summary(dat.int)
ggplot (data = gather (dat.int[-13], key = crops, value = kcals_5yr_ratio, Cassava:Yam, na.rm = T),
        aes ( kcals_5yr_ratio, color = crops)) + geom_density(position = 'stack')

# dat.int <- mutate(dat.int, TAI_ID1 = as.numeric(as.character(TAI_ID2)))

ggplot ( data = gather(dat.int, key = crops, value = kcals, Cassava:Yam, na.rm = T ) %>%
           mutate (log_kcals = log (1 + kcals)), aes(time,log_kcals, color = crops) ) +
  geom_line(alpha= 0.3) + facet_wrap(~TAI_ID2, ncol =10, nrow = 10)


### I could easily exchange mean by median given the pairs plot (itsn't normal)
## but for now work on 'the cube'
# side interactions

agg.crop <- dat %>%
  dplyr::select(TAI_ID2, crop, prod, year) %>%
  group_by(TAI_ID2, crop) %>%
  summarize ( . , avg = mean(prod), medev = mad(prod), med = median(prod),
              v = var(prod), standev = sd(prod)) %>% # I can use geometric mean or median
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
