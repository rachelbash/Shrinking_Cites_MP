# The FRS RAW files were downloaded from
# https://echo.epa.gov/tools/data-downloads#downloads
# on 21.06.2019
# The facilities and system linkage files are filtered here for only
# NPDES and Pennsylvania data

## set working directory to folder containing frs downloaded csv files

setwd("~/Duke/Masters Project/Temporary_Data_Folder/frs_downloads")

## tidyverse
library(tidyverse)

## read in country facilities
Facilities <- read.csv('FRS_FACILITIES.csv', header = T)

## select PA Facilities using filter()
PA_Facilities <- filter(Facilities, FAC_STATE == "PA")
summary(PA_Facilities$FAC_STATE)

## read in program links
Program_Links <- read.csv('FRS_PROGRAM_LINKS.csv', header = T)
## filter for only PA, then only NPDES
Program_Links_PA_NPDES <- Program_Links %>%
  filter(PGM_SYS_ACRNM == "NPDES") %>%
  filter(STATE_CODE == "PA")

## Do not actually need Facility Data, just the Program Links
## However, write PA files to csv files anyway
write.csv(Program_Links_PA_NPDES, file = "FRS_PROGRAM_LINKS_NPDES_PA.csv")
write.csv(PA_Facilities, file = "FRS_FACILITIES_PA.csv")
