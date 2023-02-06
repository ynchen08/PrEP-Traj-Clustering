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

SeroProtect['ID']=1:4000

#Convert wide to long format-----------------------------------------------------
SP_long=SeroProtect%>%tidyr::gather(., Week, Protect,Protect1:Protect103, factor_key=TRUE)
SP_long$Week=gsub("Protect","",SP_long$Week)%>%as.numeric()
SP_long=SP_long%>%arrange(ID,Week)
##scale down the time variable to facilitate model convergence
SP_long$Week=SP_long$Week/10  

#Run 1-group latent class trajectory model ---------------------------------------------------

init_mod=lcmm(Protect~1+Week+I(Week^2), subject='ID',ng=1,data=SP_long, link="thresholds", maxiter=200)  

# Run 4-group latent class trajectory model ----------------------------------------------------
Ncore=detectCores()-1
cl=makeCluster(Ncore-1)
clusterExport(cl,list('lcmm'),environment())
mod_4g=gridsearch(rep = 20, maxiter = 10, minit = init_mod,
                    lcmm(fixed=Protect~1+Week+I(Week^2),random=~-1, mixture=~1+Week+I(Week^2),
                         subject='ID',ng=4,data=SP_long, link="thresholds",nwg=FALSE),cl=cl)
stopCluster(cl)

#Examine model fit
LCTMtoolkit(mod_4g)

#predicted trajectory plot 
datnew=data.frame(Week = seq(1, 103)/10)
Legend=c(sapply(1:4, function(i){
  paste0("Group ",i)
}))
plotpred=predictY(mod_4g,datnew,var.time="Week", draws=2000)
plot(plotpred,lty=1, xlab="Week", ylab="Probability", shades=TRUE, xaxt="n", yaxt="n",legend=NULL,main="Predicted sero-protected probability trajectories \n by identified group membership per the fitted model")
legend("topright",Legend,cex=0.5,lty=1, col=1:4,bty = "n")

#observed proportion of adherence by group 
y=SeroProtect[,2:104]

pred=mod_4g$pprob
pred$class%>%table()

p1=y[which(pred$class==1),]%>%as.matrix()%>%colMeans()
p2=y[which(pred$class==2),]%>%as.matrix()%>%colMeans()
p3=y[which(pred$class==3),]%>%as.matrix()%>%colMeans()
p4=y[which(pred$class==4),]%>%as.matrix()%>%colMeans()
p5=y[which(pred$class==5),]%>%as.matrix()%>%colMeans()

dat=data.frame(x=1:103, p1,p2,p3,p4,p5)

ggplot(dat,aes(x))+geom_line(aes(y=p1),color="black")+geom_line(aes(y=p2),color="red")+geom_line(aes(y=p3),color="green")+geom_line(aes(y=p4),color="blue")+ggtitle("Observed proportion of PrEP sero-protection by identified group membership")+xlab("Week") + ylab("Probability")
