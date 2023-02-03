library(here)
# install.packages('gt')
library(gt)
# install.packages('webshot2')
library(webshot2)
# install.packages("matrixStats")
library(matrixStats)
library(here)
library(lcmm)

#Import long format seroproection matrix (used by predictY function as input)
SP_long=readRDS(here("Data","SP_long_4k"))

#Import model object
Mod_rep20maxit10=readRDS(here("./Export/test/GBTM_mods_rep20maxit10"))

#plot predicted trajectories for k=2~6

plotGBTM=function(mod){

  datnew=data.frame(Week = seq(1, 103)/10)
  par(mfrow=c(3,2),oma=c(1,1,1.5,1)+0.1,mar=c(4,4,3,1)+0.1)
  
  for (k in 1:length(mod)){
    Legend=c(sapply(1:k, function(i){
      paste0("Group ",i)
    }))
    plotpred=predictY(mod[[k]],datnew,var.time="Week", draws=2000)
    plot(plotpred,lty=1, xlab="Week", ylab="Probability", shades=TRUE, xaxt="n", yaxt="n",legend=NULL,main=paste0("K=",k))
    legend("topright",Legend,cex=0.5,lty=1, col=1:k,bty = "n")
    axis(1, at =seq(0, 103, by=10)/10,label=seq(0, 103, by=10))
    axis(2,at=seq(0,1,by=0.1), label=seq(0,1,by=0.1),las=1)
  }
  
  obj=deparse(substitute(mod))
  params=regmatches(obj, gregexpr("[[:digit:]]+", obj))[[1]]%>%as.numeric()
  title=sprintf("Predicted trajectories of PrEP sero-protection (Rand.Init=%d, Maxiter=%d)",params[1],params[2])
  mtext(title, side=3,line = 0,outer = TRUE)
}

plotGBTM(Mod_rep20maxit10)

#extract model coefficients

## model coefficents (k sets of model coefficients)
## posterior classification table (N, %)
## mean of posterior probabilities in each class matrix

postprob(Mod_rep20maxit10[[3]])

x=Mod_rep20maxit10[[3]]$pprob
head(x)
x$class

y=SeroProtect[,2:104]

p1=y[which(x$class==1),]%>%as.matrix()%>%colMeans()
p2=y[which(x$class==2),]%>%as.matrix()%>%colMeans()
p3=y[which(x$class==3),]%>%as.matrix()%>%colMeans()

plot(p1)
plot(p3)


head(SeroProtect)