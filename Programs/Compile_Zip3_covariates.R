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

#ZIP-3 covariates -------------------------------------------------------------------------------------------------
fn=list.files(here("Data","ZIP3_Covar"))
fn=fn[! fn %in% c('raw_zip5_2012_2016.sas7bdat','zip3_covariates')]

zip3_dat=list()
for(i in 1:length(fn)){
  zd=read_sas(here("Data","ZIP3_Covar",fn[i]))%>%data.frame()
  print(dim(zd))
  zip3_dat[[i]]=zd
}

zip3_dat=zip3_dat%>%reduce(full_join,by="zip3")%>%mutate(zip3_black_prop=Black/denom_race,
                                                         zip3_hisp_prop=Hispanic/denom_race,
                                                         zip3_pov_prop=pov_sum/tot_pov_known,
                                                         zip3_unins_prop=unins_sum/tot_ins_known,
                                                         zip3_prep_density=prepclinics/total_13plus*100000)%>%
  dplyr::select(zip3,zip3_black_prop,zip3_hisp_prop,zip3_pov_prop,zip3_unins_prop,zip3_prep_density)

zip3_edu_dat=read_sas(here("Data","ZIP3_Covar","raw_zip5_2012_2016.sas7bdat"))%>%
  dplyr::select(ZCTA5A,educ_denom,bachelor,masters,doctorate)%>%
  mutate(zip3=as.numeric(substr(ZCTA5A,1,3)))%>%
  dplyr::select(-ZCTA5A)%>%group_by(zip3)%>%
  summarise(across(everything(),~ sum(.x, na.rm = TRUE)))%>%data.frame()

zip3_edu_dat$zip3_bachelor_prop=(zip3_edu_dat$bachelor+zip3_edu_dat$masters+zip3_edu_dat$doctorate)/zip3_edu_dat$educ_denom

zip3_dat2=merge(x=zip3_dat,y=zip3_edu_dat,by="zip3",all.x=TRUE)%>%dplyr::select(-c(educ_denom,bachelor,masters,doctorate))

# zip3_dat$zip3_prep_density%>%quantile(.,prob=c(0.333, 0.667),na.rm=TRUE)
# is.na(zip3_dat$zip3_pov_prop)%>%sum()
#zip3_dat2$zip3_bachelor_prop%>%quantile(.,prob=c(0.25, 0.50,0.75),na.rm=TRUE)

saveRDS(zip3_dat2,here('Data','ZIP3_Covar','zip3_covariates'))