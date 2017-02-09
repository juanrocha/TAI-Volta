##left over

## Crop data from original files, not the ones processed by Katja.
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
area <- tidyr::gather(area, 'crop', 'area', 3:12)

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
  tidyr::gather('crop', 'CropProd', 3:12)

crop$Province <- as.factor(crop$Province)
crop$TAI_ID2 <- as.factor(crop$TAI_ID1)
crop$year <- as.factor(crop$year)


## combine two datasets and make sure it doesn't create NAs
crop.dat <- full_join(crop %>% select(TAI_ID2, year, crop, CropProd, TAI_ID1),
                 area %>% select(TAI_ID2, year, crop, area, TAI_ID1),
                 by = c("TAI_ID2", "year", "crop", "TAI_ID1"))
# Now dat has all data of crop production in tons and area in Ha.
str(crop.dat) # 18940 obs;  $ TAI_ID1 : Factor w/ 155 levels

crop.dat <- crop.dat [is.element(crop.dat$TAI_ID2, volta.shp@data$TAI_ID1),]
crop.dat <- droplevels(crop.dat)
str(crop.dat) # 11920 obs;  $ TAI_ID1 : Factor w/ 99 levels Only Volta basin!!

### Rename production and assign country.
names(crop.dat)[4] <- 'prod'



#
# rm(tot.area)
#
# delta_5yr <- list()
#
# for (i in 1:7){
#   delta_5yr [[i]] <- crop.dat %>%
#     filter (Year == seq(2001,2008,1)[i]:seq(2005,2012,1)[i]) %>%
#     select (TAI_ID2, crop, prop_cultivated_area, year) %>%
#     group_by (TAI_ID2, crop) %>%
#     summarize (mean_prop_cult_area_5yrs = mean(prop_cultivated_area,  na.rm = T) ) %>%
#     mutate (time = i)
# }

# delta_5yr <- dplyr::bind_rows(delta_5yr) # now you have the change over time of the 5yr window for proportion of cultivated area.
# if you want only the last window
# filter(delta_5yr, time == 7)


# kcals_5yr <- list()
#
# for (i in 1:7){
#   kcals_5yr [[i]] <- crop.dat %>%
#     filter (year == seq(2001,2008,1)[i]:seq(2005,2012,1)[i]) %>%
#     select (TAI_ID2, crop, prod, year) %>%
#     group_by (TAI_ID2, crop) %>%
#     summarize (mean_tons_5yrs = mean(prod, na.rm = T) ) %>%
#     mutate (time = i)
# }
#
# kcals_5yr <- bind_rows(kcals_5yr)

# Compile a dataframe with all data to use.

# dat.int <- list ()
#
# for (i in 1:7){
#   dat.int [[i]] <- kcals_5yr %>% dplyr::select(TAI_ID2, crop, mean_tons_5yrs, time) %>%
#     filter (time == i) %>%
#     group_by(TAI_ID2, crop) %>%
#     summarise (m = mean (mean_tons_5yrs, na.rm = T)) %>% #median on sq_prod is also possible / add the yield.
#     tidyr::spread(key = crop, value = m)
#
#   # Step 1: kcals first
#   dat.int[[i]][-1] <- t(apply(as.matrix(dat.int[[i]][-1]), 1, function(x){x * as.vector(as.matrix(cals.fao))/ 10^6})) ## millions of KCals
#   # dat.int # now data is in mean Kcals *10^6
#
#   dat.int [[i]] <- mutate (dat.int[[i]], time = i)
#
# }
#
# dat.int <- bind_rows(dat.int)

# This dataset contains the interactions from the last time window, so the kcal_5y_mean
# interact <- filter(dat.int, time == 7)%>%
#     dplyr::rename(TAI_ID1 = TAI_ID2)%>%
#     volta.only()
