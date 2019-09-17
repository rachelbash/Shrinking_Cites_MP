#librarys
library(jsonlite); library(tidyverse); library(lubridate)
library(rvest); library(dplyr); library(purrr); library(stringr)

baseURL = 'https://data.epa.gov/efservice/WATER_SYSTEM/STATE_CODE/'
fileType = '/EXCEL'

stateCode = c('AL','AK','AR','AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA',
              'KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM',
              'NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA',
              'WV','WI','WY') #rb-just make it PA

# This is loads in the locations and the urls for other data accessible via web services
i=1
projectsURL = paste0(baseURL,stateCode[i],fileType)
df <- read.csv(url(projectsURL))

#convert all columns into characters instead of factors. Removes error in loop 
#and converting variables to NA
df <- df %>% mutate_all(as.character)
str(df)

#Loop through the rest of the states #rb-dont’ need to do this loop!
for (i in 2:length(stateCode)){
  projectsURL = paste0(baseURL,stateCode[i],fileType)
  foo <- read.csv(url(projectsURL))
  foo <- foo %>% mutate_all(as.character)
  
  df <- rbind(df, foo)
  print(stateCode[i])
}

#rename headers
colnames(df) <- c("PWSID","PWS_NAME","NPM_CANDIDATE","PRIMACY_AGENCY_CODE","EPA_REGION","SEASON_BEGIN_DATE","SEASON_END_DATE","PWS_ACTIVITY_CODE","PWS_DEACTIVATION_DATE",
                  "PWS_TYPE_CODE","DBPR_SCHEDULE_CAT_CODE","CDS_ID","GW_SW_CODE","LT2_SCHEDULE_CAT_CODE","OWNER_TYPE_CODE","POPULATION_SERVED_COUNT","POP_CAT_2_CODE",
                  "POP_CAT_3_CODE","POP_CAT_4_CODE","POP_CAT_5_CODE","POP_CAT_11_CODE","PRIMACY_TYPE","PRIMARY_SOURCE_CODE","IS_GRANT_ELIGIBLE_IND","IS_WHOLESALER_IND",
                  "IS_SCHOOL_OR_DAYCARE_IND","SERVICE_CONNECTIONS_COUNT","SUBMISSION_STATUS_CODE","ORG_NAME","ADMIN_NAME","EMAIL_ADDR","PHONE_NUMBER","PHONE_EXT_NUMBER","FAX_NUMBER","ALT_PHONE_NUMBER",
                  "ADDRESS_LINE1","ADDRESS_LINE2","CITY_NAME","ZIP_CODE","COUNTRY_CODE","STATE_CODE","SOURCE_WATER_PROTECTION_CODE","SOURCE_PROTECTION_BEGIN_DATE",
                  "OUTSTANDING_PERFORMER","OUTSTANDING_PERFORM_BEGIN_DATE","CITIES_SERVED","COUNTIES_SERVED","X")

#where do primacy codes and state codes not match
zt <- subset(df, PRIMACY_AGENCY_CODE != STATE_CODE)
#sometimes the owner of the system (state code) is different from where the system is physically located (primacy agency code)... for actual location go with primacy agency code

df$PWSID_STATE <- substr(df$PWSID,0,2)
table(subset(df,PWS_TYPE_CODE=="CWS")$PWSID_STATE); #some PWS are owned by EPA regions (mostly tribal)

#remove columns that are not needed
table(df$COUNTRY_CODE); #2 are from Canada so keep those
df2 <- df %>% dplyr::select(-c(ADMIN_NAME, EMAIL_ADDR, PHONE_NUMBER, ADDRESS_LINE1, ADDRESS_LINE2, POP_CAT_2_CODE, POP_CAT_3_CODE, POP_CAT_4_CODE, 
                               POP_CAT_5_CODE, POP_CAT_11_CODE, X, LT2_SCHEDULE_CAT_CODE, DBPR_SCHEDULE_CAT_CODE, FAX_NUMBER, PHONE_EXT_NUMBER, ALT_PHONE_NUMBER))
#NPM_CANDIDATE = I think a candidate for grants from the national program?
#Remove lt2_schedule_cat_code because in theory all systems now implement. Same with DBPR SCHEDULE
#multiple cities / counties are split by a comma

cws <- subset(df2, PWS_TYPE_CODE == "CWS")
ntncws <- subset(df2, PWS_TYPE_CODE == "NTNCWS")
tncws <- subset(df2, PWS_TYPE_CODE == "TNCWS")

#
Subset cws by PWSID_STATE==”PA” and then loop through by PWSID to pull data together.
