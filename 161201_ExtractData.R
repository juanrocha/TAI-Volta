## Extract and clean data for Volta Basin
## TAI project
## Data was collected and cleaned by Katja Malmborg, some files
## are available as data in shape files but need to try different
## normalizations here.

## The difference between this file and the original ExtractData is that I'm 
## using new files from Katja, completed after fieldwork but without assuming
## NA by zeroes. Formats are different (now rows are years and spreadsheets crops for Ghana)

## by Juan Carlos Rocha
## juan.rocha@su.se
## Version March 2016

## Output: Volta.RData to be recall by other scripts

# Clean and load libraries

rm(list=ls())

require ('gdata')
require('dplyr')
require('reshape')

setwd("~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha")

###########
# Area data
###########


# Gahna crop areas.
file <- 'GH_Crop_areas_TAI_ID_161117.xlsx'
sn <- sheetNames(xls= file)

# do the first manual so you have a master file
# areaG <- read.xls(xls= file, sheet=1)
# names(areaG)[4:19] <- paste(names(areaG)[4:19], sn[1], sep='_')

# for (i in 2:7){ #is to 7 because I don't want all excel sheets
# df <- read.xls(xls=file, sheet=i)
# names(df)[4:19] <- paste(names(df)[4:19], sn[i], sep='_')
# areaG <- cbind(areaG, df)
# }

# Note not all sheets have the same column names!
# then keep it on long format and fix it later by deleting columns
areaG <- read.xls(xls= file, sheet=1, na.strings='N/A', dec = ',')
areaG$year <- rep(as.numeric(sn[1]), dim(areaG)[1])
areaG <- areaG[-c(1,2,4,5)] # this sheet has district and region repeated
areaG$TAI_ID1 <- as.factor(areaG$TAI_ID1)
colnames(areaG) <- tolower(colnames(areaG))
areaG[-1] <-  apply(areaG[-1], 2, function(x) as.numeric(as.character(x)))
# x <- read.xls(xls= file, sheet=3)
# x$year <- rep(as.numeric(sn[3]), dim(x)[1])
# names(x) == names(areaG)
# # try
# x2<- full_join(x , areaG)
# rm(x,x2)

for (i in 2:13){ # 13 years
  df <- read.xls(xls=file, sheet=i, na.strings='N/A', dec = ',')
  df$year <- rep(as.numeric(sn[i]), dim(df)[1])
  df <- df[-c(1,2)]
  df$TAI_ID1 <- as.factor(df$TAI_ID1)
  df[-1] <- apply(df[-1], 2, function(x) as.numeric(as.character(x)))
  colnames(df) <- tolower(colnames(df))
  areaG <- full_join(areaG, df)
  print(paste('Iteraction', i, "finished", sep= ' '))
}


# Now areaG has the cropped area per crops in all years we have. Some names refers to the same crop but have different spellings; the 'staple.crop' columns are just the sum, as well as 'total'. To keep the raw data, delete them.

str(areaG)
names(areaG)
areaG <- areaG[,-c(18,26,27)] # delete stapple crops, total and veg

## J161201: I can't do this anymore, there is NA's on the original dataset!!!!
# replace NA with zeroes so the sum works. The original dataset does not have NA's,
# they were created when joining tables with different colnames, spelling problems.

# cbind( areaG[29], replace(areaG[29], is.na(areaG[29]), 0)) # good way to do it
# Here I just repeat the operation above across all numeric columns
# areaG[2:28] <- apply(areaG[2:28], 2, function(x){replace(x, is.na(x), 0)})
# names(areaG)
# areaG[10] <- areaG[10] + areaG[18] # merge ground nuts
# areaG <- areaG[,-18] # delete the extra column
# 
# areaG[12] <- areaG[12] + areaG[18] # soyabean / soy.bean
# areaG <- areaG[,-18]
# 
# areaG[21] <- areaG[21] + areaG[16] + areaG[19] # g..egg, garden.egg, etc
# areaG <- areaG[,-c(16,19)]

# Not all crops were measured all years, delete those that didn't

# areaG <- areaG[,-c(19,20,22:26)]

# rename the crops and use long form dataset agreggated by crop

# colnames(areaG)[4:19] <- c('Maize', 'Rice', 'Cassava','Yam', 'Cocoyam', 'Platain',
#                            'Sorghum','Millet', 'Ground nuts', 'Cowpea', 'Soy', 'Okro',
#                            'Pepper', 'Tomato', 'Year', 'Garden eggs')
#
# areaG <- melt(areaG, id.vars=c('District', 'Region', 'TAI_ID1', 'Year'), measure.vars= names(areaG)[c(4:17,19)], variable_name= 'crop')

#rename tai_id1
colnames(areaG)[1] <- "TAI_ID1"

## without deleting crops or years:
areaG <- melt(areaG, id.vars=c('TAI_ID1', 'year'))


# now the dataset is clean and in long format with year being the long variable.

### Burkina Faso crop areas
file <- 'BF_Crop_areas.xlsx'
sn <- sheetNames(xls= file)

# this file has crops on the sheet names and time lines on the tables.

areaB <- read.xls(xls= file, sheet=1, na.strings='N/A')
areaB$crop <- rep(sn[1], dim(areaB)[1])
areaB <- areaB[-c(1,3)] #get rid of variable names that can cause confusion
areaB$TAI_ID1 <- as.factor(areaB$TAI_ID1) # make it into a facto so there is no confusion when joining


## We'are using average during the last 7 yrs. For BF it is 2006-2012, and for G it is 2002:2005-2007:09. Hence, delete all columns that !2006 onwards
# areaB <- areaB[,-c(4:15)]

for (i in 2:length(sn)){
  df <- read.xls(xls=file, sheet=i, na.strings='N/A')
  df[4:dim(df)[2]] <- apply(df[4:dim(df)[2]], 2, as.numeric)
  df$crop <- rep(sn[i], dim(df)[1])
  df <- df[-c(1,3)]
  df$TAI_ID1 <- as.factor(df$TAI_ID1)
  #df <- df[,-c(4:15)]
  areaB <- full_join(areaB, df)
}

# 2012 has too many NAs 195, while other years have max 45. So I keep instead 2005:2011

# areaB <- areaB[,-c(3,11)] # Note J160401: keep all years and filter after

str(areaB)
summary(areaB)
areaB$crop <- as.factor(areaB$crop)
colnames(areaB)[2:21] <- c(1993:2012)# [3:9] <- c(2005:2011)
# now the dataset needs to be rearranged to copy the Ghana structure


areaB <- melt(areaB, id.vars=c( 'TAI_ID1','crop'), variable_name='Year')

## 5092 NA that seems to come from the original file.

# extra cleaning when using all years
# areaB <- areaB[,-3]
str(areaG)
levels(areaG$variable) <- c('Maize', 'Rice', 'Cassava','Yam', 'Cocoyam', 'Platain',
                            'Sorghum','Millet', 'Ground nuts', 'Cowpea', 'Okro',
                            'Pepper', 'Tomato',  'Garden eggs', 'Soy', 'Potato', 'Sweet potato',
                            'Lettuce', 'Onion', 'Cucumber', 'Cabbage','Watermelon', "Carrot")
names(areaG)[3] <- 'crop'
names(areaG)[2] <- 'Year'

# standardize names and classes
# colnames(areaG)[1] <- colnames(areaB)[1]

# areaG <- areaG[,-2]
# names(areaG)[3] <- 'Year'
areaB$Year <- as.numeric(as.character(areaB$Year))

area <- full_join(areaG, areaB)
area$TAI_ID1 <- as.factor(area$TAI_ID1)
area$crop <- as.factor(area$crop)
summary(area)
str(area)

# If you want to see where the NA are:
with(filter(area, is.na(value)) , table (Year, crop))

# Note: I can drop rice_irr there is only 3 yrs of data.
# now area data is clean on long format for both countries.

# to see the common crops
common <- intersect(levels(areaB$crop),  levels(areaG$crop))




###########
# Production data
###########

# Gahna crop production in tons. Note I'm following the same steps as below but now commenting only when necessary.
file <- 'GH_Crop_production_TAI_ID_161117.xlsx'
sn <- sheetNames(xls= file)


prodG <- read.xls(xls= file, sheet=1, na.strings='N/A', dec = ',')
prodG$year <- rep(as.numeric(sn[1]), dim(prodG)[1])
prodG <- prodG[-c(1,2)]
prodG$TAI_ID1 <- as.factor(prodG$TAI_ID1)
colnames(prodG) <- tolower(colnames(prodG))
prodG[-1] <-  apply(prodG[-1], 2, function(x) as.numeric(as.character(x)))

for (i in 2:13){ #13 years
  df <- read.xls(xls=file, sheet=i, na.strings='N/A', dec = ',')
  df$year <- rep(as.numeric(sn[i]), dim(df)[1])
  df <- df[-c(1,2)]
  df$TAI_ID1 <- as.factor(df$TAI_ID1)
  df[-1] <- apply(df[-1], 2, function(x) as.numeric(as.character(x)))
  colnames(df) <- tolower(colnames(df))
  print(apply(df, 2, is.na) %>% colSums)
  prodG <- full_join(prodG, df)
  print(paste('Iteraction', i, "finished", sep= ' '))
}

# The print command on the loop let me see that there is no NA on the original dataset, they are created when columns names are spelled differnetly.
prodG <- prodG[,-c(24)] # delete  veg_total

# # prodG[2:29] <- apply(prodG[2:29], 2, function(x){replace(x, is.na(x), 0)})
# 
# names(prodG)
# prodG[10] <- prodG[10] + prodG[18] + prodG[22]  # merge ground nuts
# prodG <- prodG[,-c(18,22)] # delete the extra column
# 
# prodG[12] <- prodG[12] + prodG[18] # soyabean / soy.bean
# prodG <- prodG[,-18]
# 
# prodG[16] <- prodG[16] + prodG[20] + prodG[19]+prodG[21] # g..egg, garden.egg, etc
# prodG <- prodG[,-c(19:21)]

# Not all crops were measured all years, delete those that didn't
# prodG <- prodG[,-c(20:25)] # Note: trying with all data...

colnames(prodG)[1:24] <- c( 'TAI_ID1','Maize', 'Rice', 'Cassava','Yam', 'Cocoyam', 'Platain',
                           'Sorghum','Millet', 'Ground nuts', 'Cowpea', 'Okro',
                           'Pepper', 'Tomato', 'Garden eggs','Year',  'Soy', 'Sweet potato',
                           'Lettuce', 'Onion', 'Cucumber', 'Cabbage','Watermelon', 'Carrot') #[4:19] <- c('Maize', 'Rice', 'Cassava','Yam', 'Cocoyam', 'Platain', 'Sorghum','Millet', 'Ground nuts', 'Cowpea', 'Soy', 'Okro', 'Pepper', 'Tomato', 'Garden eggs','Year' )

# prodG <- melt(prodG, id.vars=c('District', 'Region', 'TAI_ID1', 'Year'), measure.vars= names(prodG)[c(4:18)], variable_name= 'crop')
prodG <- melt(prodG, id.vars=c('TAI_ID1', 'Year'), variable_name= 'crop')

### Burkina Faso crop production
file <- 'BF_Crop_production.xlsx'
sn <- sheetNames(xls= file)

prodB <- read.xls(xls= file, sheet=1, na.strings='N/A')
prodB$crop <- rep(sn[1], dim(prodB)[1])
prodB <- prodB[-c(1,3)]
prodB$TAI_ID1 <- as.factor(prodB$TAI_ID1)

# prodB <- prodB[,-c(4:15,24)]

for (i in 2:length(sn)){
  df <- read.xls(xls=file, sheet=i, na.strings='N/A')
  df[4:dim(df)[2]] <- apply(df[4:dim(df)[2]], 2, as.numeric)
  df$crop <- rep(sn[i], dim(df)[1])
  df <- df[-c(1,3)]
  df$TAI_ID1 <- as.factor(df$TAI_ID1)
  print(apply(df, 2, is.na) %>% colSums)
  
  # df <- df[,-c(4:15)]
  prodB <- full_join(prodB, df)
}

# prodB <- prodB[,-c(3,11,13)] # Note J160401: keep all years and filter after
prodB <- prodB[,-c(22,24,25)]

str(prodB)
summary(prodB)
prodB$crop <- as.factor(prodB$crop)
colnames(prodB)[2:21] <- c(1993:2012) #[3:9] <- c(2005:2011)

prodB <- melt(prodB, id.vars=c('TAI_ID1', 'crop'), variable_name='Year')

# standardize names and classes
# colnames(prodG)[1] <- colnames(prodB)[1]
# prodG <- prodG[,-2]
prodB$Year <- as.numeric(as.character(prodB$Year))


# to see the common crops
intersect(levels(prodB$crop),  levels(prodG$crop))

prod <- full_join(prodG, prodB)
# prod$Province <- as.factor(prod$Province)
prod$crop <- as.factor(prod$crop)
prod$TAI_ID1 <- as.factor(prod$TAI_ID1)
summary(prod)
str(prod)

# If you want to see where the NA are:
with(filter(prod, is.na(value)) , table (Year, crop))


# ## with the two datasets
# area.volta <- (area[is.element(area$TAI_ID1, volta.shp@data$TAI_ID1),]) # volta data
#
# prod.volta <- prod [is.element(prod$TAI_ID1, volta.shp@data$TAI_ID1),]
#
names(area)[4] <- "area"
names(prod)[4] <- "prod"
#
# # Due to province names misspellings, just delete the columns and then recover after from map data
# area.volta <- area.volta[,-1]
# prod.volta <- prod.volta[,-1]

dat <- full_join(area, prod)
dat$crop <- as.factor(dat$crop)
# dat$country <- as.factor(dat$country)

summary (dat)
str(dat)

# check the NAs in time, or any other column, just change the column name after $
table(dat$Year [is.na(dat$area)])
table(dat$Year [is.na(dat$prod)])


save("dat", "area", "prod", file='161205_Volta.RData' )
# ###########
# # Population statistics
# ###########
# 
# # for Ghana data from 2010
# file <- 'GH_Population_2010_TAI_ID.xlsx'
# sn <- sheetNames(xls=file)
# 
# 
# popG <- read.xls(xls= file, sheet=1)
# colnames(popG)[1] <- 'District'
# popG <- left_join(popG, y=read.xls(file,sheet=2), by='TAI_ID1')
# popG <- left_join(popG, y=read.xls(file,sheet=3), by='TAI_ID1')
# 
# names(popG)
# #remove duplicated columns
# popG <- popG[,-c(14,15,25,26)]
# 
# colnames(popG)[c(1,2,5, 23)] <- c('District', 'Region', 'Total_habs', 'Total_employ')
# 
# 
# # for Burkina Faso data from 2006
# file <- "BF_Population_2006.xlsx"
# popB <- read.xls(file, sheet=4, dec = ',') # this data sucks!
# 
# # J161118: This way of importing the data does not work for this file. Numbers get imported as factors because of using , as . in numbers. Or, the file uses both formats. I think that's why I eded up using the processed data by Katja. Note however that there is interesting info in these files that can be useful on future analysis.
# 
# 
# 
# ## Katja did a file with summary info for the bundling exercise. Use pop and other stats from such file.
# file <- 'Volta_Bundling_prep.xlsx'
# users <- read.xls(file, sheet=1, dec=',')
# users[4:16] <- apply(users[4:16], 2, function (x) {as.numeric(x)} )
# 
# resource <- read.xls (file, sheet=2, dec=',')
# biophys <- read.xls(file, sheet=3, dec=',')
# interact <- read.xls(file,sheet=4, dec=',')
# 
# # the problem with the sheets crop diversity from Katja is that she aggregates
# # summing over the years (I assumed) and temporal variablity is lost. However,
# # she has more crops on her tables and don't know where the info comes from. From
# # the original datasets (original from her), there is no fonio, sweet potatos,
# # cotton, etc in both countries. I can't say if she has the data or if she simply
# # fill up with zero values.
# 
# getwd()
# save( 'biophys', 'interact', 'resource','users',  'dat', 'volta.shp', "datKeyCrops" ,
#       file='160414_Volta.RData')
# 
# ## exploratory graphics
# require (ggplot2)
# 
# g <- ggplot(data = subset(area, crop == c('Rice' , 'Rice_irr')), aes(x=Year, y=value), color=crop) +
#   geom_line(position='identity', aes(group=crop)) +
#   facet_wrap( ~ Province, ncol=8)
# 
# 
# #### New file with raw values from Katja: 160901
# 
# file <- 'Volta_Vars_raw_160901.xlsx'
