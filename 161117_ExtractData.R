## Extract and clean data for Volta Basin
## TAI project
## Data was collected and cleaned by Katja Malmborg.

## J161117: Previous version has bugs and produces unreliable results - the data I see in the console does not correspond in N with the data I see on the original excel files. I did the corrections on the original file

## by Juan Carlos Rocha
## juan.rocha@su.se
## Version November 2016

## Output: Volta.RData to be recall by other scripts


rm(list = ls())


# packages needed

# library('readxl')
library('dplyr')
library('tidyr')
# library('reshape')

setwd("~/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha")

###########
# Area data
###########


# Gahna crop areas in Hectares
file <- 'GH_Crop_areas_TAI_ID.xlsx'
sn <- gdata::sheetNames(xls= file)

areaG <- read_excel(file, sheet=1)
areaG$year <- rep(as.numeric(sn[1]), dim(areaG)[1])
