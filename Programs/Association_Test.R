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


## Index object for linking original and analysis ID-----------------------------------
sampled_ID_index=read.csv(here("Export",Folder,"Sampled_ID_index.csv"))%>%rename(ID=ID_new)

head(sampled_ID_index)

#Import input data (long format) --------------------------------------------------------------
SP_long=readRDS(here("Data",Folder,"SP_long_4k"))
head(SP_long)

# Append covariates to the long dataset -----------------------------------------------------

SP_long2=merge(x=SP_long,y=sampled_ID_index,by="ID",all.x=TRUE)
SP_long3=merge(SP_long2,y=covar,by="ID_orig",all.x=TRUE)%>%dplyr::select(ID,Week,Protect,Age_at_init_cat,Average_copay_cat,Primary_payer,Pharmacy_type)%>%arrange(ID)

head(SP_long3)
min(unique(SP_long3$ID)==1:4000)

#Run 1-group latent class trajectory model ---------------------------------------------------
init_mod=lcmm(Protect~1+Week+I(Week^2), subject='ID',ng=1,data=SP_long3, link="thresholds", maxiter=200)  


# Implement K=2~6 GBTM--------------------------------------------------------------------------
source(here("Programs","Helper_Functions_GBTM.R"))

num_rep=20
num_maxit=10
Ncore=detectCores()-1
k=4

t1=Sys.time()
cl=makeCluster(Ncore-1)
clusterExport(cl,list("k",'lcmm'),environment())
mod=gridsearch(rep = num_rep, maxiter = num_maxit, minit = init_mod,
               lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                    subject='ID',
                    # classmb =~Age_at_init_cat+Average_copay_cat+Primary_payer+Pharmacy_type,
                    ng=k,data=SP_long3, link="thresholds",nwg=FALSE),cl=cl)

mod_assoc=gridsearch(rep = num_rep, maxiter = num_maxit, minit = init_mod,
                     lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                          subject='ID',
                          classmb =~Age_at_init_cat+Average_copay_cat+Primary_payer+Pharmacy_type,
                          ng=k,data=SP_long3, link="thresholds",nwg=FALSE),cl=cl)
stopCluster(cl)
t2=Sys.time()

RunTime=difftime(t2,t1,units ='mins')


saveRDS(mod_assoc,here("Export",Folder,"GBTM_assoc"))
# mod=readRDS(here("./Export/GBTM_data4k_rep20maxit10"))


Modfit_assoc=GBTM_stat(mod_assoc)

summary(mod[[3]])
summary(mod_assoc[[3]])
parameters(mod[[2]])

mod[[2]]$conv

mod_assoc[[2]]$conv


