#Create a function for gathering internal validity measures for cluster solution --------------
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







# Create a function to implement resampling and model fitting ----------------------------
resamp.modelfit=function (m, rep, maxiter,iter, inputdata,rel.size=0.8) {
        library(lcmm)
        library(tidyr)
        library(renv)
        library(dplyr)
        library(rlang)
        library(LCTMtools)
        library(here)
        library(ellipsis)
        # Step 1: resample 80% of data  --------------------------------------------
        set.seed(iter)
        index= sample(1:nrow(inputdata),nrow(inputdata)*rel.size , replace=F)%>%sort()
        SP_wide.rs=inputdata[index,]
        SP_long.rs=SP_wide.rs%>%tidyr::gather(., Week, Protect,Protect1:Protect103, factor_key=TRUE)
        SP_long.rs$Week=gsub("Protect","",SP_long.rs$Week)%>%as.numeric()
        SP_long.rs=SP_long.rs%>%arrange(ID,Week)
        SP_long.rs$Week=SP_long.rs$Week/10 
        
        # Step 2: model initialization ---------------------------------------------
        mc <- match.call()$m
        
        init.param=as.list(mc[-1])
        init.param$mixture=NULL
        init.param$ng=1
        init.param$maxiter=200
        init.param$data=SP_long.rs
        minit=do.call(as.character(mc[[1]]), init.param)
        
        if (minit$conv != 1) 
          stop("The model minit did not converge")
        
        # Step 3: prepare for GBTM parallization ----------------------------------
        mc$maxiter <- maxiter
        models <- vector(mode = "list", length = rep)
        
        cl=makeCluster(detectCores()-1)
        clusterSetRNGStream(cl)
        
        clusterExport(cl, list("mc", "maxiter", "minit","SP_long.rs","rep","lcmm"),envir = environment())
        
        pck <- .packages()
        dir0 <- find.package()
        dir <- sapply(1:length(pck), function(k) {
            gsub(pck[k], "", dir0[k])
          })
        clusterExport(cl, list("pck", "dir"), envir = environment())
        
        #Step 4: gridsearch random initial values --------------------------------
        clusterEvalQ(cl, sapply(1:length(pck), function(k) {
            require(pck[k], lib.loc = dir[k], character.only = TRUE)
          }))
        cat("Be patient, grid search is running ...\n")
        
        mc$data=SP_long.rs
        models <- parLapply(cl, 1:rep, function(X) {
            mc$B <- substitute(random(minit), parent.frame(n = 2))
            return(do.call(as.character(mc[[1]]),as.list(mc[-1])))
          })
        
        #Step 5: perform final estimation-----------------------------------------
        cat("Search completed, performing final estimation\n")
        stopCluster(cl)
        
        llmodels <- sapply(models, function(x) {
          return(x$loglik)
        })
        kmax <- which.max(llmodels)
        mc$B <- models[[kmax]]$best
        mc$maxiter <- match.call()$m$maxiter
        
        final.est=do.call(as.character(mc[[1]]), as.list(mc[-1]))
        
        #Step 6: aggregate model stats
        bf=summarytable(final.est,final.est,
                        which=c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))%>%
          as.data.frame()
        bf=bf[2,]
        
        appa=LCTMtoolkit(final.est)$appa
        colnames(appa)=c(sapply(1:mc$ng, function(i){
          paste0("APPA_C",i)
        }))
        
        occ=LCTMtoolkit(final.est)$occ
        colnames(occ)=c(sapply(1:mc$ng, function(i){
          paste0("OCC_C",i)
        }))
        
        model.fit=cbind(bf,appa,occ)
        pred.class.prob=final.est$pprob
        
        source(here('Programs','Modified_SummaryFunction.R'),local=TRUE)
        sum=summary.mod(final.est)
        model.coef=rbind(sum[[2]],sum[[1]],sum[[3]])
        
        return(list("Model fitness"=model.fit,
                    "Predicted class membership"=pred.class.prob,
                    "Model coefficients"=model.coef))
}


# Create a function to compute consensus probabilities 

consensus_prob=function(j){
  library(here)
  library(utils)
  library(tidyr)
  library(dplyr)
  library(rlang)
  library(ellipsis)
  
  locateprob=function(i){
    return(as.matrix(predprob[i,-c(1,2)]))
  }
  same.group.prob=function(subject.i,subject.j){
    X=ifelse(subject.i==subject.j,NA,
             rowSums(locateprob(subject.i)*locateprob(subject.j)))
    return(X)
  }
  
  predprob=RUN1[[j]]$`Predicted class membership`
  index=predprob$ID
  index.pair=combn(1:length(index),2)
  ID.pair=index[index.pair]%>%matrix(nrow=2)
  cons.prob.dat=rbind(ID.pair,same.group.prob(index.pair[1,],index.pair[2,]))%>%t()%>%data.frame()
  colnames(cons.prob.dat)=c("ID1","ID2","same.group.prob")
  return(cons.prob.dat)
}