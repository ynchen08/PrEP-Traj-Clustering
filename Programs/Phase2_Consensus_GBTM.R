rm(list=ls())

#Restore relevant packages-------------------------------------------------------
# renv::dependencies()
# renv::snapshot()
renv::restore()

#import packages and data ----------------------------------------------------------------------------
library(here)
library(utils)
library(lcmm)
library(tidyr)
library(renv)
library(dplyr)
library(rlang)
library(LCTMtools)
library(here)
library(ellipsis)

SP_wide=read.delim(here("./Data/SeroProtect_4k.txt"),sep=",",header=FALSE)
colnames(SeroProtect)=c("ID",sapply(1:103, function(i){
  paste0("Protect",i)
}))


#Implement iterative resampling and model fitting (for K=2 as an example)--------------------------------------------
source(here("Programs","Helper_Functions_GBTM.R"))

T1=Sys.time()

RUN1=list()
for (i in 1:5){
  RUN1[[i]]=resamp.modelfit(inputdata = SP_wide,
                iter=i,
                rep = 20,
                maxiter = 10, 
                m=lcmm(fixed=Protect~1+Week+I(Week^2),
                       random=~-1, 
                       mixture=~1+Week+I(Week^2),
                       subject='ID',
                       ng=2, 
                       link="thresholds",
                       nwg=FALSE))
}

saveRDS(RUN1,here("Export","paraRUN1"))

T2=Sys.time()
TD1=difftime(T2,T1, units="mins")

#Compute consensus probabilities -------------------------------------------------------------
RUN1=readRDS(here("Export","paraRUN1"))

T3=Sys.time()
# cl=makeCluster(detectCores()-1)
# clusterExport(cl,ls(),envir = environment())
# CONSPROB_ALL=parLapply(cl,1:length(RUN1),consensus_prob)
# stopCluster(cl)

CONSPROB_ALL=lapply(1:length(RUN1),consensus_prob)

CONSPROB_ALL2=CONSPROB_ALL[[1]]
for (i in 2:length(CONSPROB_ALL)){
  CONSPROB_ALL2=rbind(CONSPROB_ALL2,CONSPROB_ALL[[i]])
}

CONSPROB=CONSPROB_ALL2%>%dplyr::arrange(ID1,ID2)%>%
                         group_by(ID1,ID2)%>%
                         summarise(consensus_prob=mean(same.group.prob),N_per_pair=n())
T4=Sys.time()
TD2=difftime(T2,T1, units="mins")

#Plot consensus probabilities ------------------------------------------------------------------------
Consensus.Probabilities=CONSPROB$consensus_prob
hist(Consensus.Probabilities)






















