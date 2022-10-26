




###############################################################################################



T1=Sys.time()
inputdata=SP_wide
iter=1
rel.size=0.8
num_maxit=10
num_rep=20

set.seed(iter)
index= sample(1:nrow(inputdata),nrow(inputdata)*rel.size , replace=F)%>%sort()
SP_wide.rs=inputdata[index,]
SP_long.rs=SP_wide.rs%>%tidyr::gather(., Week, Protect,Protect1:Protect103, factor_key=TRUE)
SP_long.rs$Week=gsub("Protect","",SP_long.rs$Week)%>%as.numeric()
SP_long.rs=SP_long.rs%>%arrange(ID,Week)
SP_long.rs$Week=SP_long.rs$Week/10 

mod_init=lcmm(Protect~1+Week+I(Week^2), subject='ID',ng=1,data=SP_long.rs, link="thresholds", maxiter=200)

cl2=makeCluster(detectCores()-1)

clusterExport(cl=cl2,varlist=c(ls(),"lcmm"),envir=environment())

RUN2=gridsearch(rep = num_rep, maxiter = num_maxit, minit = mod_init,
                lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                     subject='ID',ng=2,data=SP_long.rs, link="thresholds",nwg=FALSE),cl=cl2)
stopCluster(cl2)

bf=summarytable(RUN2,RUN2,which=c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))%>%as.data.frame()
bf=bf[2,]

appa=LCTMtoolkit(RUN2)$appa
colnames(appa)=c(sapply(1:2, function(i){
  paste0("APPA_C",i)
}))

occ=LCTMtoolkit(RUN2)$occ
colnames(occ)=c(sapply(1:2, function(i){
  paste0("OCC_C",i)
}))

model.fit=cbind(bf,appa,occ)
pred.class.prob=RUN2$pprob


source(here('Programs','Modified_SummaryFunction.R'),local=TRUE)
sum=summary.mod(RUN2)
model.coef=rbind(sum[[2]],sum[[1]],sum[[3]])

T2=Sys.time()
TD2=difftime(T2,T1, units="mins")



##########################################################

# Step 1: resample 80% of data for 50 times
Resample=function(iter, inputdata,rel.size=0.8){
  set.seed(iter)
  index= sample(1:nrow(inputdata),nrow(inputdata)*rel.size , replace=F)%>%sort()
  SP_wide.rs=inputdata[index,]
  SP_long.rs=SP_wide.rs%>%tidyr::gather(., Week, Protect,Protect1:Protect103, factor_key=TRUE)
  SP_long.rs$Week=gsub("Protect","",SP_long.rs$Week)%>%as.numeric()
  SP_long.rs=SP_long.rs%>%arrange(ID,Week)
  SP_long.rs$Week=SP_long.rs$Week/10 
  return(SP_long.rs)
}
DAT.RS=lapply(1:10,Resample,inputdata=SP_wide)





# Step 2: fit GBTM
consensus.GBTM=function(iter,k, num_rep=20,num_maxit=10){
  library(lcmm)
  library(tidyr)
  library(renv)
  library(dplyr)
  library(rlang)
  library(LCTMtools)
  library(here)
  library(ellipsis)
  
  mod_init=lcmm(Protect~1+Week+I(Week^2), subject='ID',ng=1,data=DAT.RS[[iter]], link="thresholds", maxiter=200)
  
  cl2=makeCluster(detectCores()-1)
  
  clusterExport(cl=cl2,varlist=c(ls(),"lcmm"),envir=environment())
  
  mod=testfunc(rep = num_rep, maxiter = num_maxit, minit = mod_init,
               lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                    subject='ID',ng=k,data=DAT.RS[[iter]], link="thresholds",nwg=FALSE),cl=cl2)
  
  stopCluster(cl2)
  
  # Step 3: aggregate model stats
  bf=summarytable(mod,mod,which=c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))%>%as.data.frame()
  bf=bf[2,]
  
  appa=LCTMtoolkit(mod)$appa
  colnames(appa)=c(sapply(1:k, function(i){
    paste0("APPA_C",i)
  }))
  
  occ=LCTMtoolkit(mod)$occ
  colnames(occ)=c(sapply(1:k, function(i){
    paste0("OCC_C",i)
  }))
  
  model.fit=cbind(bf,appa,occ)
  pred.class.prob=mod$pprob
  
  
  source(here('Programs','Modified_SummaryFunction.R'),local=TRUE)
  sum=summary.mod(mod)
  model.coef=rbind(sum[[2]],sum[[1]],sum[[3]])
  
  return(list("Model fitness"=model.fit,
              "Predicted class membership"=pred.class.prob,
              "Model coefficients"=model.coef))
}

k=2
Ncore=detectCores()
cl=makeCluster(Ncore-1)
clusterExport(cl,c('lcmm',ls()),environment())
test=parLapply(cl=cl,X=1:2,fun=consensus.GBTM)
stopCluster(cl)

test=lapply(1:2,consensus.GBTM,k=2)

debug(testfunc)

debug(consensus.GBTM)
undebug(consensus.GBTM)


for (i in 1:length(selected_K)){
  k=selected_K[i]
  cat("K=",k)
  cl=makeCluster(Ncore-1)
  clusterExport(cl,list("k",'lcmm'),environment())
  stopCluster(cl)
}


# Step 3: Compute pariwise consensus probabilities