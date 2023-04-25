rm(list=ls())

#Prepare working environment ---------------------------------------------------
#set your working directory
setwd("C:/Users/yche465/Desktop/AIM 1/Codes/PrEP-Traj-Clustering/")
#load relevant R project environment
renv::load(getwd())

#install packages if needed
#install.packages("lcmm")
#install.packages('tidyr')
#install.packages('dplyr')
#install.packages("rlang")
#install.packages("devtools")
#renv::install("hlennon/LCTMtools")
#install.packages('here')
#install.packages('ellipsis')
#install.packages(haven)
#install.packages(ggasym)

#Restore relevant packages
# renv::dependencies()
# renv::snapshot()
renv::restore()

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


#Specify user
User="Emory"
if(User=="Emory"){
  Folder="fake"
} else{
  Folder="real"
}

#Load appended original dataset------------------------------------------------
covar=read.csv(here('Data',Folder,'PersonVars_appended.csv'))
# covar=read.csv(here('Data',Folder,'PersonVars_sampled.csv')) - import sample personal variable dataset

covar=covar%>%rename(ID_orig=ID)%>%mutate(Dur_subopt_cat2=case_when(Dur_subopt<13~1,
                                                                    Dur_subopt<26~2,
                                                                    Dur_subopt<39~3,
                                                                    Dur_subopt<52~4,
                                                                    Dur_subopt<65~5,
                                                                    Dur_subopt<78~6,
                                                                    Dur_subopt<91~7,
                                                                    TRUE~8))


# Merge Zip3 info ------------------------------------------------------------------
zip3_dat=readRDS(here('Data','ZIP3_Covar','zip3_covariates'))
zipvn=colnames(zip3_dat)[-1]
# set.seed(10234)  - simulate ZIP3 (not needed for real data!!!)
# covar$zip3=sample(zip3_dat$zip3,nrow(covar),replace=TRUE) - simulate ZIP3 (not needed for real data!!!)
covar_dat=merge(x=covar,y=zip3_dat,by="zip3",all.x=TRUE)%>%arrange(ID_orig)

#Descriptive stats ----------------------------------------------------------------
cat_vars=c("Dur_subopt_cat2","Average_copay_cat")
cont_vars=c(zipvn)

## Categorical covariates----------------------------------------------------------
DESCRIP.DAT_CAT=data.frame()
for (i in cat_vars){
  n=covar_dat[[i]]%>%table()%>%as.data.frame()
  perc=covar_dat[[i]]%>%table()%>%prop.table()%>%as.data.frame()
  descrip.dat=data.frame(Var_Name=rep(i,nrow(n)),Level=as.numeric(rownames(n)),nmiss=sum(is.na(covar_dat[[i]])),N.Mean=as.numeric(n[,2]),Perc.SD=as.numeric(perc[,2]))
  DESCRIP.DAT_CAT=rbind(DESCRIP.DAT_CAT,descrip.dat)
}

## Continuous covariates-----------------------------------------------------------
mu=covar_dat%>%dplyr::select(all_of(cont_vars))%>%summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))%>%t()%>%data.frame()
sd=covar_dat%>%dplyr::select(all_of(cont_vars))%>%summarise(across(everything(), ~ sd(.x, na.rm = TRUE)))%>%t()
descrip.dat_cont=cbind("N.Mean"=mu[,1],"Perc.SD"=sd[,1])

DESCRIP.DAT_CONT=data.frame(Var_Name=rownames(descrip.dat_cont),Level=NA_real_,descrip.dat_cont)
row.names(DESCRIP.DAT_CONT)=NULL

nmiss_cont=covar_dat%>%dplyr::select(all_of(cont_vars))%>%summarise(across(everything(), ~ sum(is.na(.x))))%>%
  t()%>%data.frame()
colnames(nmiss_cont)="nmiss"
nmiss_cont$Var_Name=row.names(nmiss_cont)

DESCRIP.DAT_CONT=merge(DESCRIP.DAT_CONT,nmiss_cont,by="Var_Name",all.x=TRUE)

DESCRIP.DAT=bind_rows(DESCRIP.DAT_CAT,DESCRIP.DAT_CONT)

write.csv(DESCRIP.DAT,here("Export",Folder,"Supp_DescripStat_Orig.csv"))
# write.csv(DESCRIP.DAT,here("Export",Folder,"Supp_DescripStat_Sampled.csv")) - Export supplemental descriptive stats for sample dataset
