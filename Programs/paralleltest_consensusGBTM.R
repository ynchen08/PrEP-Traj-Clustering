#load library
library(doParallel)
install.packages("doSNOW")
library(doSNOW)
install.packages('future')
library(future)

#####################################################################
#
#######################################################################
source(here('Programs','Consensus_GBTM.R'))

T1=Sys.time()

RUN1=list()
for (i in 1:5){
  RUN1[[i]]=testfunc(inputdata = SP_wide,
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


######################################################################
# Nesting parLapply in future (Error in unserialize(node$con) : error reading from connection)
#######################################################################
if (future::supportsMulticore()) {
  future::plan(future::multicore)
} else {
  future::plan(future::multisession)
}


t3=Sys.time()
par.run=list()
for (i in 1:2){
  par.run[[i]]= future({
                          testfunc(inputdata = SP_wide,
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
                          },seed=TRUE)
}
RUN2=lapply(par.run,FUN=value)
t4=Sys.time()
difftime(t4,t3)







#########################################################

CL=makeCluster(5)

clusterCall(CL,fun=Sys.getpid)

registerDoSNOW(CL)

clusterEvalQ(CL, {
  library(parallel)
  cl=makeCluster(2)
  D=clusterCall(cl,fun=Sys.getpid)
})

clusterEvalQ(CL, {
  stopCluster(cl)
})

stopCluster(CL)
############################################################