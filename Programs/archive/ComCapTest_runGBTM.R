rm(list=ls())

#Restore relevant packages
renv::dependencies()
renv::restore()
renv::snapshot()
#install.packages("lcmm")
library(lcmm)
# install.packages('tidyr')
library(tidyr)
library(renv)
# install.packages('dplyr')
library(dplyr)
# install.packages("rlang")
library(rlang)
# install.packages("devtools")
# renv::install("hlennon/LCTMtools")
library(LCTMtools)
# install.packages('here')
library(here)
# install.packages('ellipsis')
library(ellipsis)

#Import input data
SeroProtect=read.delim(here("./Export/SeroProtect.txt"),sep=",",header=FALSE)
colnames(SeroProtect)=c("ID",sapply(1:103, function(i){
  paste0("Protect",i)
}))

#Boostrapped to 4,000 data points
set.seed(103)
index <- sample(1:nrow(SeroProtect), 4000, replace=T)
SeroProtect <- SeroProtect[index,]
SeroProtect$ID=1:dim(SeroProtect)[1]

saveRDS(SeroProtect,here("./Export/SP_wide"))

#Convert wide to long format
SP_long=SeroProtect%>%tidyr::gather(., Week, Protect,Protect1:Protect103, factor_key=TRUE)
SP_long$Week=gsub("Protect","",SP_long$Week)%>%as.numeric()
SP_long=SP_long%>%arrange(ID,Week)
##scale down the time variable to facilitate model convergence
SP_long$Week=SP_long$Week/10  

saveRDS(SP_long,here("./Export/SP_long"))


#Run 1-group latent class trajectory model
mod=list()
mod[[1]]=lcmm(Protect~1+Week+I(Week^2), subject='ID',ng=1,data=SP_long, link="thresholds", maxiter=200)  


#Set up function for obtaining statistics related to GBTM

GBTM_stat=function(lcmm_mod){
  ##BIC and % of class membership
  for (k in 1:length(lcmm_mod)){
    bf=lcmm_mod[[k]]%>%summarytable()%>%as.data.frame()
    if (k==1){
      BaseFit=bf
    } else{
      BaseFit=bind_rows(BaseFit,bf)
    }
  }
  rownames(BaseFit)=NULL
  ##Average posterior probabilities
  for (k in 2:length(lcmm_mod)){
    appa=LCTMtoolkit(lcmm_mod[[k]])$appa
    rownames(appa)=NULL
    colnames(appa)=c(sapply(1:k, function(i){
      paste0("APPA_C",i)
    }))
    if (k==2){
      APPA=appa
    } else {
      APPA=bind_rows(APPA,appa)
    }
  }
  APPA=c(1,rep(NA,dim(APPA)[2]-1))%>%rbind(., APPA)
  
  ## Odds of correct classification
  for (k in 2:length(lcmm_mod)){
    occ=LCTMtoolkit(lcmm_mod[[k]])$occ
    rownames(occ)=NULL
    colnames(occ)=c(sapply(1:k, function(i){
      paste0("OCC_C",i)
    }))
    if (k==2){
      OCC=occ
    } else {
      OCC=bind_rows(OCC,occ)
    }
  }
  OCC=rep(NA,dim(OCC)[2])%>%rbind(., OCC)
  
  FitTab=cbind(BaseFit,APPA,OCC)
  return(FitTab)
}



##############################################################################
# REP=50 & MAXITER = 30
##############################################################################
  Ncore=detectCores()
  print(Ncore)
  
  num_rep=50
  num_maxit=30
  
  Time_rep50maxit30=rep(NA,6)
  
  for (k in 2:6){
    cat("K=",k)
    t1=Sys.time()
    cl=makeCluster(Ncore-1)
    clusterExport(cl,list("k",'lcmm'),environment())
    mod[[k]]=gridsearch(rep = num_rep, maxiter = num_maxit, minit = mod[[1]],
                        lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                             subject='ID',ng=k,data=SP_long, link="thresholds",nwg=FALSE),cl=cl)
    stopCluster(cl)
    t2=Sys.time()
    Time_rep50maxit30[k]=difftime(t2,t1,units ='mins')
  }
  
  saveRDS(mod,here("./Export/GBTM_mods_rep50maxit30"))
  mod=readRDS(here("./Export/GBTM_mods_rep50maxit30"))
  
  FitTab_rep50maxit30=GBTM_stat(mod)
  
  ExportStuff=list(Time_rep50maxit30,FitTab_rep50maxit30)
  saveRDS(ExportStuff,here("./Export/ExportStats_rep50maxit30"))
  
  
  ##############################################################################
  # REP=20 & MAXITER = 10
  ##############################################################################
  
  
  num_rep=20
  num_maxit=10
   
  Time_rep20maxit10=rep(NA,6)
  mod[[2]]=mod[[3]]=mod[[4]]=mod[[5]]=mod[[6]]=NULL
  
  for (k in 2:6){
    cat("K=",k)
    t1=Sys.time()
    cl=makeCluster(Ncore-1)
    clusterExport(cl,list("k",'lcmm'),environment())
    mod[[k]]=gridsearch(rep = num_rep, maxiter = num_maxit, minit = mod[[1]],
                        lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                             subject='ID',ng=k,data=SP_long, link="thresholds",nwg=FALSE),cl=cl)
    stopCluster(cl)
    t2=Sys.time()
   Time_rep20maxit10[k]=difftime(t2,t1,units ='mins')
 }

  saveRDS(mod,here("./Export/GBTM_mods_rep20maxit10"))
  mod=readRDS(here("./Export/GBTM_mods_rep20maxit10"))
  
  FitTab_rep20maxit10=GBTM_stat(mod)
  
  ExportStuff=list(Time_rep20maxit10,FitTab_rep20maxit10)
  saveRDS(ExportStuff,here("./Export/ExportStats_rep20maxit10"))
  
  
  
    
    
  # 
  # ##Plot estimated mean trajectory
  # datnew=data.frame(Week = seq(1, 103)/10)
  # par(mfrow=c(3,2),oma=c(1,1,1.5,1)+0.1,mar=c(4,4,3,1)+0.1)
  # 
  # for (k in 1:length(mod)){
  #   Legend=c(sapply(1:k, function(i){
  #     paste0("Group ",i)
  #   }))
  #   plotpred=predictY(mod[[k]],datnew,var.time="Week", draws=TRUE)
  #   plot(plotpred,lty=1, xlab="Week", ylab="Probability", shades=TRUE, xaxt="n", yaxt="n",legend=NULL,main=paste0("K=",k))
  #   legend("topright",Legend,cex=0.5,lty=1, col=1:k,bty = "n")
  #   axis(1, at =seq(0, 103, by=10)/10,label=seq(0, 103, by=10))
  #   axis(2,at=seq(0,1,by=0.1), label=seq(0,1,by=0.1),las=1)
  # }
  # mtext("Group-specific predicted trajectory of PrEP sero-protection", side=3,line = 0,outer = TRUE)  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 

