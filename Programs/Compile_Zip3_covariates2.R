library(here)
library(tidyverse)
library(lcmm)
library(tidyr)
library(renv)
library(dplyr)
library(rlang)
library(LCTMtools)
library(ellipsis)
library(haven)
library(ggasym)

#######################################################################################################
# 2015-2019 American Community Survey
# data source: https://www.nhgis.org/
#######################################################################################################

#Race -------------------------------------------------------------------------------------------------

## ALUCE001: Total
## ALUDE004: Population of one race: Black or African American
## ALUDE013: Population of two or more races: Population of two races: White; Black or African American
## ALUDE016: Population of two or more races: Population of two races: Black or African American; American Indian and Alaska Native
## ALULE003: Hispanic or Latino


fn=list.files(here("Data","ZIP3_Covar","Race"))
dat1=read.csv(here("Data","ZIP3_Covar","Race",fn[grepl("csv",fn)]))%>%mutate(zip3=as.numeric(substr(sprintf("%05d", ZCTA5A),1,3)))%>%arrange(zip3)%>%
    mutate(Total=ALUCE001,
           Black=ALUDE004+ALUDE013+ALUDE016,
           Hispanic=ALULE003)%>%
    select(zip3,Total,Black,Hispanic)%>%
    group_by(zip3)%>%
    summarise(across(everything(),~ sum(.x, na.rm = TRUE)))%>%
    mutate(zip3_black_prop=Black/Total,
           zip3_hisp_prop=Hispanic/Total)%>%
    select(zip3,zip3_black_prop,zip3_hisp_prop)


head(dat1)
#Education, insurance, poverty------------------------------------------------------------------------
#AMA0E001:     Total
#AMA0E009:     Male: 18 to 24 years: Bachelor's degree
#AMA0E010:     Male: 18 to 24 years: Graduate or professional degree
#AMA0E017:     Male: 25 to 34 years: Bachelor's degree
#AMA0E018:     Male: 25 to 34 years: Graduate or professional degree
#AMA0E025:     Male: 35 to 44 years: Bachelor's degree
#AMA0E026:     Male: 35 to 44 years: Graduate or professional degree
#AMA0E033:     Male: 45 to 64 years: Bachelor's degree
#AMA0E034:     Male: 45 to 64 years: Graduate or professional degree
#AMA0E041:     Male: 65 years and over: Bachelor's degree
#AMA0E042:     Male: 65 years and over: Graduate or professional degree

#AMA0E050:     Female: 18 to 24 years: Bachelor's degree
#AMA0E051:     Female: 18 to 24 years: Graduate or professional degree
#AMA0E058:     Female: 25 to 34 years: Bachelor's degree
#AMA0E059:     Female: 25 to 34 years: Graduate or professional degree
#AMA0E066:     Female: 35 to 44 years: Bachelor's degree
#AMA0E067:     Female: 35 to 44 years: Graduate or professional degree
#AMA0E074:     Female: 45 to 64 years: Bachelor's degree
#AMA0E075:     Female: 45 to 64 years: Graduate or professional degree
#AMA0E082:     Female: 65 years and over: Bachelor's degree
#AMA0E083:     Female: 65 years and over: Graduate or professional degree

# AMBSE010:    Income in the past 12 months below poverty level: Male: 18 to 24 years
# AMBSE011:    Income in the past 12 months below poverty level: Male: 25 to 34 years
# AMBSE012:    Income in the past 12 months below poverty level: Male: 35 to 44 years
# AMBSE013:    Income in the past 12 months below poverty level: Male: 45 to 54 years
# AMBSE014:    Income in the past 12 months below poverty level: Male: 55 to 64 years
# AMBSE015:    Income in the past 12 months below poverty level: Male: 65 to 74 years
# AMBSE016:    Income in the past 12 months below poverty level: Male: 75 years and over
# AMBSE024:    Income in the past 12 months below poverty level: Female: 18 to 24 years
# AMBSE025:    Income in the past 12 months below poverty level: Female: 25 to 34 years
# AMBSE026:    Income in the past 12 months below poverty level: Female: 35 to 44 years
# AMBSE027:    Income in the past 12 months below poverty level: Female: 45 to 54 years
# AMBSE028:    Income in the past 12 months below poverty level: Female: 55 to 64 years
# AMBSE029:    Income in the past 12 months below poverty level: Female: 65 to 74 years
# AMBSE030:    Income in the past 12 months below poverty level: Female: 75 years and over

# AMBSE039:    Income in the past 12 months at or above poverty level: Male: 18 to 24 years
# AMBSE040:    Income in the past 12 months at or above poverty level: Male: 25 to 34 years
# AMBSE041:    Income in the past 12 months at or above poverty level: Male: 35 to 44 years
# AMBSE042:    Income in the past 12 months at or above poverty level: Male: 45 to 54 years
# AMBSE043:    Income in the past 12 months at or above poverty level: Male: 55 to 64 years
# AMBSE044:    Income in the past 12 months at or above poverty level: Male: 65 to 74 years
# AMBSE045:    Income in the past 12 months at or above poverty level: Male: 75 years and over
# AMBSE053:    Income in the past 12 months at or above poverty level: Female: 18 to 24 years
# AMBSE054:    Income in the past 12 months at or above poverty level: Female: 25 to 34 years
# AMBSE055:    Income in the past 12 months at or above poverty level: Female: 35 to 44 years
# AMBSE056:    Income in the past 12 months at or above poverty level: Female: 45 to 54 years
# AMBSE057:    Income in the past 12 months at or above poverty level: Female: 55 to 64 years
# AMBSE058:    Income in the past 12 months at or above poverty level: Female: 65 to 74 years
# AMBSE059:    Income in the past 12 months at or above poverty level: Female: 75 years and over

# AMLLE009:    Male: 19 to 25 years
# AMLLE011:    Male: 19 to 25 years: No health insurance coverage
# AMLLE012:    Male: 26 to 34 years
# AMLLE014:    Male: 26 to 34 years: No health insurance coverage
# AMLLE015:    Male: 35 to 44 years
# AMLLE017:    Male: 35 to 44 years: No health insurance coverage
# AMLLE018:    Male: 45 to 54 years
# AMLLE020:    Male: 45 to 54 years: No health insurance coverage
# AMLLE021:    Male: 55 to 64 years
# AMLLE023:    Male: 55 to 64 years: No health insurance coverage
# AMLLE024:    Male: 65 to 74 years
# AMLLE026:    Male: 65 to 74 years: No health insurance coverage
# AMLLE027:    Male: 75 years and over
# AMLLE029:    Male: 75 years and over: No health insurance coverage
# AMLLE037:    Female: 19 to 25 years
# AMLLE039:    Female: 19 to 25 years: No health insurance coverage
# AMLLE040:    Female: 26 to 34 years
# AMLLE042:    Female: 26 to 34 years: No health insurance coverage
# AMLLE043:    Female: 35 to 44 years
# AMLLE045:    Female: 35 to 44 years: No health insurance coverage
# AMLLE046:    Female: 45 to 54 years
# AMLLE048:    Female: 45 to 54 years: No health insurance coverage
# AMLLE049:    Female: 55 to 64 years
# AMLLE051:    Female: 55 to 64 years: No health insurance coverage
# AMLLE052:    Female: 65 to 74 years
# AMLLE054:    Female: 65 to 74 years: No health insurance coverage
# AMLLE055:    Female: 75 years and over
# AMLLE057:    Female: 75 years and over: No health insurance coverage


fn=list.files(here("Data","ZIP3_Covar","Edu_Pov_Ins"))
dat2=read.csv(here("Data","ZIP3_Covar","Edu_Pov_Ins",fn[grepl("csv",fn)]))%>%mutate(zip3=as.numeric(substr(sprintf("%05d", ZCTA5A),1,3)))%>%arrange(zip3)%>%
  mutate(Total_Edu=AMA0E001,
         Bach_above=AMA0E009+AMA0E010+AMA0E017+AMA0E018+AMA0E025+AMA0E026+AMA0E033+AMA0E034+AMA0E041+AMA0E042+AMA0E050+AMA0E051+AMA0E058+AMA0E059+AMA0E066+AMA0E067+AMA0E074+AMA0E075+AMA0E082+AMA0E083,
         
         Above_Pov=AMBSE039+AMBSE040+AMBSE041+AMBSE042+AMBSE043+AMBSE044+AMBSE045+
                   AMBSE053+AMBSE054+AMBSE055+AMBSE056+AMBSE057+AMBSE058+AMBSE059,
         Below_Pov=AMBSE010+AMBSE011+AMBSE012+AMBSE013+AMBSE014+AMBSE015+AMBSE016+
                   AMBSE024+AMBSE025+AMBSE026+AMBSE027+AMBSE028+AMBSE029+AMBSE030,
         Total_Pov=Above_Pov+Below_Pov,
         
         Total_Unins=AMLLE009+AMLLE012+AMLLE015+AMLLE018+AMLLE021+AMLLE024+AMLLE027+AMLLE037+AMLLE040+AMLLE043+AMLLE046+AMLLE049+AMLLE052+AMLLE055,
         Uninsur=AMLLE011+AMLLE014+AMLLE017+AMLLE020+AMLLE023+AMLLE026+AMLLE029+AMLLE039+AMLLE042+AMLLE045+AMLLE048+AMLLE051+AMLLE054+AMLLE057)%>%
  select(zip3,Total_Edu,Bach_above,Total_Pov,Below_Pov,Total_Unins,Uninsur)%>%
  group_by(zip3)%>%
  summarise(across(everything(),~ sum(.x, na.rm = TRUE)))%>%
  mutate(zip3_bachelor_prop=Bach_above/Total_Edu,
         zip3_pov_prop=Below_Pov/Total_Pov,
         zip3_unins_prop=Uninsur/Total_Unins)%>%
  select(zip3,zip3_bachelor_prop,zip3_pov_prop,zip3_unins_prop)

zip3_dat=merge(dat1,dat2,by="zip3")

prep_dat=readRDS(here('Data','ZIP3_Covar','archive','zip3_covariates'))%>%select(zip3,zip3_prep_density)

zip3_dat_final=merge(zip3_dat,prep_dat,all.x=TRUE,all.y=TRUE,by="zip3")

# for (i in prep_dat$zip3){
#   if(!i %in% zip3_dat$zip3){
#     print(i)
#   }
# }

saveRDS(zip3_dat_final,here('Data','ZIP3_Covar','zip3_covariates_2019'))
