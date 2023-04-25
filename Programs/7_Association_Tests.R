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
#install.packages('haven')
#install.packages('ggasym')
#install.packages('VGAM')

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
#####################################################################################################################
#Prepare wide-format dataset for model fitting

## Index object for linking original and analysis ID-----------------------------------
sampled_ID_index=read.csv(here("Export",Folder,"Sampled_ID_index.csv"))%>%rename(ID=ID_new)

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

zip3_dat=readRDS(here('Data','ZIP3_Covar','zip3_covariates'))

zipvn=colnames(zip3_dat)[-1]

covar_dat=merge(x=covar_dat,y=zip3_dat,by="zip3",all.x=TRUE)

###########################################################################################################################
#Fit multinomial logit model to assess the associations with assigned group membership -----------------------------------

covar_dat$Age_at_init_cat=covar_dat$Age_at_init_cat-1

covar_dat$Group=relevel(factor(covar_dat$Class),ref="2")

covar_dat2=covar_dat%>%arrange(ID)%>%select(Group,Class,Age_at_init_cat, Primary_payer, Average_copay_cat, Pharmacy_type,
                              zip3_black_prop, zip3_hisp_prop, zip3_bachelor_prop, zip3_pov_prop, zip3_unins_prop, zip3_prep_density)

covar_dat2$Age_at_init_cat=factor(covar_dat2$Age_at_init_cat)
covar_dat2$Average_copay_cat=factor(covar_dat2$Average_copay_cat)
covar_dat2$Primary_payer=factor(covar_dat2$Primary_payer)
covar_dat2$Pharmacy_type=factor(covar_dat2$Pharmacy_type)

covar_dat2 = covar_dat2[complete.cases(covar_dat2),]


#commercial payer = 1
#traditional retail pharmacy =?

MLM = vglm(Class ~. ,  
           data=covar_dat2[, -1], 
           family=multinomial(refLevel = "2"))
summary(MLM)

MLM2=multinom(Group~.,data=covar_dat2[,-2])
summary(MLM2)

MLM.mod=list(MLM,MLM2)

saveRDS(MLM.mod, here("Export",Folder,"MLM_mod"))


#Fit lasso regression-------------------------------------------------------------------------------
packages <- c("data.table","skimr","glmnet", "car")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}


dumm_vars=covar_dat2%>%select(c(Age_at_init_cat,Primary_payer))%>%model.matrix(~.-1,data=.)%>%as.matrix()
orig_vars=apply(covar_dat2%>%select(Pharmacy_type,Average_copay_cat,contains("zip"))%>%as.matrix(),2,as.numeric)
orig_vars[,"Average_copay_cat"]=orig_vars[,"Average_copay_cat"]-1



covariates=cbind(dumm_vars[,-1],orig_vars)
colnames(covariates)

outcome=covar_dat2$Class


set.seed(12352)
LASSOfit <- cv.glmnet(covariates, 
                      outcome, 
                      family = "multinomial",
                      type.multinomial="grouped",
                      type.measure = "deviance",
                      alpha = 1)
best_lambda <- LASSOfit$lambda.min

plot(LASSOfit)

LASSO_model <- glmnet(covariates, 
                      outcome, 
                      family = "multinomial",
                      type.multinomial="grouped",
                      type.measure="deviance",
                      alpha = 1,
                      lambda = best_lambda)

saveRDS(list(LASSOfit,LASSO_model),here("Export",Folder,"LassoMods"))

