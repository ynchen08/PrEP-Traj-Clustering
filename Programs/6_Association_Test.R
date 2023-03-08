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

#Restore relevant packages
  # renv::dependencies()
  # renv::snapshot()
renv::restore()

#load libraries 
  library(lcmm)
  library(tidyr)
  library(renv)
  library(dplyr)
  library(rlang)
  library(LCTMtools)
  library(here)
  library(ellipsis)

#Import input data --------------------------------------------------------------
SeroProtect=read.delim(here("./Data/SeroProtect_4k.txt"),sep=",",header=FALSE)
colnames(SeroProtect)=c("ID",sapply(1:103, function(i){
  paste0("Protect",i)
}))

orig_ID=SeroProtect$ID #create original ID
SeroProtect['ID']=1:nrow(SeroProtect)  #create new analysis specific ID
new_ID=SeroProtect$ID

## Index object for linking original and analysis ID
sampled_ID_index=data.frame(ID_orig=orig_ID,
                    ID=new_ID)

# write.csv(sampled_ID_index, here("Export","Sampled_ID_index.csv"), row.names=FALSE)

#Convert wide to long format (with analysis ID) --------------------------------------------
SP_long=SeroProtect%>%tidyr::gather(., Week, Protect,Protect1:Protect103, factor_key=TRUE)
SP_long$Week=gsub("Protect","",SP_long$Week)%>%as.numeric()
SP_long=SP_long%>%arrange(ID,Week)
##scale down the time variable to facilitate model convergence
SP_long$Week=SP_long$Week/10  

# saveRDS(SP_long,here("./Data/SP_long_4k"))


# Append covariates to the long dataset -----------------------------------------------------
covar=read.csv(here('Data','PersonVars_sampled.csv'))%>%rename(ID_orig=ID)
SP_long2=merge(x=SP_long,y=sampled_ID_index,by="ID",all.x=TRUE)
SP_long3=merge(SP_long2,y=covar,by="ID_orig",all.x=TRUE)%>%dplyr::select(ID,Week,Protect,Age_at_init_cat,Average_copay_cat,Primary_payer,Pharmacy_type)


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


saveRDS(mod_assoc,here("./Export/GBTM_fake_assoc"))
# mod=readRDS(here("./Export/GBTM_data4k_rep20maxit10"))


Modfit_assoc=GBTM_stat(mod_assoc)

summary(mod[[3]])
summary(mod_assoc[[3]])
parameters(mod[[2]])

mod[[2]]$conv

mod_assoc[[2]]$conv


realmod=readRDS(here("Export","real","GBTM_data4k_rep20maxit10"))

mod[[3]]$best

num_param=mod[[2]]$best%>%length()
UT=mod[[2]]$V

vcov=matrix(0,num_param,num_param)

vcov[upper.tri(vcov,diag=TRUE)]=UT

f_vcov=vcov+t(vcov)
diag(f_vcov)=diag(f_vcov)-diag(vcov)






