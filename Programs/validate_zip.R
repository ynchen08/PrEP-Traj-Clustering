rm(list=ls())
#load libraries
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
library(VGAM)
library(glmnet)
library(nnet)


#Specify user
User="Emory"
if(User=="Emory"){
  Folder="fake"
} else{
  Folder="real"
}




## Index object for linking original and analysis ID-----------------------------------
sampled_ID_index=read.csv(here("Export",Folder,"Sampled_ID_index.csv"))

head(sampled_ID_index)

# Append covariate dataset with predicted group membership ----------------------------------------------------------
mod=readRDS(here("Export",Folder,"GBTM_data4k_rep20maxit10"))
final.mod=mod[[4]]
Pred.Class=sampled_ID_index%>%arrange(ID)
Pred.Class$Class=final.mod$pprob$class
covar=read.csv(here('Data',Folder,'PersonVars_sampled.csv'))%>%rename(ID_orig=ID)
covar_dat=merge(x=covar,y=Pred.Class,by='ID_orig',all.x=TRUE)%>%mutate(Dur_subopt_cat2=case_when(Dur_subopt<13~1,
                                                                                                 Dur_subopt<26~2,
                                                                                                 Dur_subopt<39~3,
                                                                                                 Dur_subopt<52~4,
                                                                                                 Dur_subopt<65~5,
                                                                                                 Dur_subopt<78~6,
                                                                                                 Dur_subopt<91~7,
                                                                                                 TRUE~8))

zip3_dat=readRDS(here('Data','ZIP3_Covar','zip3_covariates_2019'))
zipvn=colnames(zip3_dat)[-1]
z=read.csv(here("Data","ZIP3_Covar","Race","nhgis0003_ds244_20195_zcta.csv"))


#simulate ZIP5-----------------------------------------------------------------------------------------------------
zip5=z$ZCTA5A%>%unique()
set.seed(10)
covar_dat$zip5=sample(zip5, nrow(covar_dat),replace = TRUE)
covar_dat=covar_dat%>%mutate(zip3=as.numeric(substr(sprintf("%05d", zip5),1,3)))

#merge with zip3 level covariate table------------------------------------------------------------------------------
covar_dat_merged_all=merge(x=covar_dat,y=zip3_dat,by="zip3",all.x=TRUE)

covar_dat_merged=covar_dat_merged_all%>%select(ID_orig,zip5,zip3,zipvn)

##check whether the merge is correct
zip3_check=quantile(covar_dat$zip3,probs=seq(0,1,length.out=20))
    
check_covar=covar_dat_merged%>%select(ID_orig,zip5,zip3,zip3_black_prop,zip3_pov_prop)%>%arrange(zip3)%>%subset(zip3 %in% zip3_check)
zip3_dat%>%subset(zip3 %in% zip3_check)%>%select(zip3,zip3_black_prop,zip3_pov_prop)%>%print()
print(check_covar)

#descriptive stats on zip-3 variables------------------------------------------------------------------------------
mu=covar_dat_merged%>%dplyr::select(-c(zip3,zip5,ID_orig))%>%summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))%>%t()%>%data.frame()
colnames(mu)="Mean"
sd=covar_dat_merged%>%dplyr::select(-c(zip3,zip5,ID_orig))%>%summarise(across(everything(), ~ sd(.x, na.rm = TRUE)))%>%t()%>%data.frame()
colnames(sd)="SD"
Q=covar_dat_merged%>%dplyr::select(-c(zip3,zip5,ID_orig))%>%summarise(across(everything(), ~ quantile(.x, probs=c(0.5,0.25,0.75,0,1),na.rm = TRUE)))%>%t()%>%data.frame()
colnames(Q)=c("Median","Q1","Q3","Min","Max")

check_discriptive=cbind(mu,sd,Q)%>%round(4)
print(check_discriptive)

#Univariate association---------------------------------------------------------------------------------------------
covar_dat_merged=merge(x=covar_dat,y=zip3_dat,by="zip3",all.x=TRUE)%>%select(ID_orig,zip5,zip3,zipvn,Class)

## 5% increment variables
covar_dat_merged$zip3_black_prop2 = covar_dat_merged$zip3_black_prop/5
covar_dat_merged$zip3_bachelor_prop2 = covar_dat_merged$zip3_bachelor_prop/5
covar_dat_merged$zip3_hisp_prop2 = covar_dat_merged$zip3_hisp_prop/5
covar_dat_merged$zip3_pov_prop2 = covar_dat_merged$zip3_pov_prop/5
covar_dat_merged$zip3_unins_prop2 = covar_dat_merged$zip3_unins_prop/5
covar_dat_merged$zip3_prep_density2 = covar_dat_merged$zip3_prep_density/5

zipvn2=gsub(" ","",paste(zipvn,2))

## fit univariate model

MLM_result=list()

for (i in 1:length(zipvn)){
  
  MLM = vglm(as.formula(paste("Class ~", zipvn[i])),  
             data=covar_dat_merged, 
             family=multinomial(refLevel = "2"))
  
  MLM5 = vglm(as.formula(paste("Class ~", zipvn2[i])),  
             data=covar_dat_merged, 
             family=multinomial(refLevel = "2"))
  
  
  MLM_coef1=summary(MLM)%>%coef()%>%data.frame()%>%rename(SE=Std..Error,p=Pr...z..)
  MLM_coef1_5=summary(MLM5)%>%coef()%>%data.frame()%>%rename(SE=Std..Error,p=Pr...z..)
  rownames(MLM_coef1_5)[-c(1,2,3)]=paste0(rownames(MLM_coef1_5)[-c(1,2,3)],"(by 5%)")
  MLM_coef=rbind(MLM_coef1,MLM_coef1_5[-c(1,2,3),])
  MLM_coef$Parameter=rownames(MLM_coef)

  MLM_coef2=MLM_coef%>%mutate(OR=exp(Estimate)%>%round(4),
                               CI_LB=exp(Estimate-1.96*SE)%>%round(4),
                               CI_UB=exp(Estimate+1.96*SE)%>%round(4),
                               p_value=p%>%round(4))%>%
            select(Parameter,OR,CI_LB,CI_UB,p_value)
  rownames(MLM_coef2)=NULL
  
  MLM_result[[i]]=MLM_coef2[-c(1,2,3),]
}

saveRDS(MLM_result,here("Export",Folder,"Univar_zip3_assoc"))

readRDS(here("Export",Folder,"Univar_zip3_assoc"))


print(MLM_result)

# multivariate association -------------------------------------------------------


###############################################################################
#assess association with tertile
cutoffs=covar_dat_merged_all%>%select(zipvn)%>%summarize(across(everything(), ~ quantile(.x, probs=c(0.333,0.667),na.rm = TRUE)))


for (i in 1:length(zipvn)){
  covar_dat_merged_all[[paste0(zipvn[i],"_tert")]]=
    case_when(is.na(covar_dat_merged_all[[zipvn[i]]])~NA_real_,
              covar_dat_merged_all[[zipvn[i]]]<=cutoffs[[zipvn[i]]][1]~0,
              covar_dat_merged_all[[zipvn[i]]]<=cutoffs[[zipvn[i]]][2]~1,
              TRUE~2)%>%factor()%>%relevel(ref="1")
  
  print(table(covar_dat_merged_all[[paste0(zipvn[i],"_tert")]], useNA = 'always')%>%prop.table())
}


covar_dat_tert=covar_dat_merged_all%>%select(ID_orig,zip5,zip3,paste0(zipvn,"_tert"),Class)


MLM_result_tert=list()

for (i in 1:length(zipvn)){
  
  MLM = vglm(as.formula(paste("Class ~", paste0(zipvn[i],"_tert"))),  
             data=covar_dat_tert, 
             family=multinomial(refLevel = "2"))

  
  MLM_coef=summary(MLM)%>%coef()%>%data.frame()%>%rename(SE=Std..Error,p=Pr...z..)
  MLM_coef$Parameter=rownames(MLM_coef)
  
  MLM_coef2=MLM_coef%>%mutate(OR=exp(Estimate)%>%round(4),
                              CI_LB=exp(Estimate-1.96*SE)%>%round(4),
                              CI_UB=exp(Estimate+1.96*SE)%>%round(4),
                              p_value=p%>%round(4))%>%
    select(Parameter,OR,CI_LB,CI_UB,p_value)
  rownames(MLM_coef2)=NULL
  
  MLM_result_tert[[i]]=MLM_coef2[-c(1,2,3),]
}

saveRDS(MLM_result_tert,here("Export",Folder,"Univar_zip3_tert_assoc"))



ZIP3_pack=list("sampled_zip3_descript"=check_discriptive,
               "Univar_MLM_zip3_cont"=MLM_result,
               "Univar_MLM_zip3_tert"=MLM_result_tert)

saveRDS(ZIP3_pack,here("Export",Folder,"ZIP3_pack_toEnok"))

readRDS(here("Export",Folder,"ZIP3_pack_toEnok"))

saveRDS(covar_dat_merged_all, here("Export",Folder,"covar_wZIP3"))

