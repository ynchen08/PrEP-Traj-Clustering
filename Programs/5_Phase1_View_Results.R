rm(list=ls())
#########################################################################################
#Prepare working environment ---------------------------------------------------
#set your working directory
setwd("C:/Users/yche465/Desktop/AIM 1/Codes/PrEP-Traj-Clustering/")
#load relevant R project environment
renv::load(getwd())
#check if any packages from the R project need to be installed
renv::restore()

#install packages if needed
  ##install.packages('gt')
  ##install.packages('webshot2')
  ##install.packages("matrixStats")

#load libraries
library(here)
library(gt)
library(webshot2)
library(matrixStats)
library(here)
library(lcmm)
library(ggplot2)
library(ggpubr)

#import plot theme set-up
source(here("misc/plot_theme.R"))

#Import long format seroproection matrix (used by predictY function as input)
SP_long=readRDS(here("Data","SP_long_4k"))

#Import model object
mod=readRDS(here("./Export/GBTM_data4k_rep20maxit10"))

###########################################################################################################
#plot predicted trajectories of optimal seroprotection probabilities by identified group membership for k=2~6


##create a function to convert predicted traj data from wide to long format
traj_transform=function(pred_traj,K){
  dat_agg=c()
  for (i in 1:K){
    
    if (length(grep("class", colnames(pred_traj)))==0){
      varname_50=colnames(pred_traj)[grep("50", colnames(pred_traj))]
      varname_2.5=colnames(pred_traj)[grep("2.5", colnames(pred_traj))]
      varname_97.5=colnames(pred_traj)[grep("97.5", colnames(pred_traj))]
    } else {
      varname_50=colnames(pred_traj)[grep(paste0("50_class",i), colnames(pred_traj))]
      varname_2.5=colnames(pred_traj)[grep(paste0("2.5_class",i), colnames(pred_traj))]
      varname_97.5=colnames(pred_traj)[grep(paste0("97.5_class",i), colnames(pred_traj))]
    }
  
    dat=cbind(Week=1:103,
              Prop_50=pred_traj[,varname_50],
              Prop_2.5=pred_traj[,varname_2.5],
              Prop_97.5=pred_traj[,varname_97.5],
              Class=rep(i,103))
    dat_agg=rbind(dat_agg,dat)
  }
  dat_agg=data.frame(dat_agg)%>%mutate(Group=factor(Class))
  return(dat_agg)
}


datnew=data.frame(Week = seq(1, 103)/10)
pred_traj_k=list()
PLOT=list()
  
for (k in 1:length(mod)){
    #create model prediction for group trajectory and save as object
    plotpred=predictY(mod[[k]],datnew,var.time="Week", draws=2000)
    plotpred_dat=plotpred$pred
    pred_traj_k[[k]]=plotpred_dat
    
    #transform wide to long format
    plotpred_long=traj_transform(plotpred_dat,k)
    #plot each k-group model
    PLOT[[k]]=ggplot(data=plotpred_long,aes(x=Week,y=Prop_50))+geom_smooth(aes(ymin = Prop_2.5, ymax = Prop_97.5,color=Group,fill=Group), stat = "identity",alpha=0.3)+ggtitle(paste0("K=",k))+scale_x_continuous(limits = c(1,103), expand = c(0, 0))+xlab("Week") + ylab("Probability")+theme_ben()+theme(plot.title = element_text(hjust = 0.5))
}

#arrange k-group plots into one graph
pred_plot=ggarrange(plotlist=PLOT,ncol=2,nrow=ceiling(length(PLOT)/2))
pred_plot=annotate_figure(pred_plot, top = text_grob("Predicted probabilities of optimal PrEP sero-protection by identified group membership", color = "black", face = "bold", size = 16))

#export the predicted prob traj plot
ggexport(pred_plot, filename = here("Figures","Pred_prop_traj_k.png"),width=1200, height=1000)
#export the predicted trajectories as R object
saveRDS(pred_traj_k,here("Export","Pred_sp_probtraj_k"))
  

# Use lcmm plot function to plot the predicted trajectory (retired)
# plot(plotpred,lty=1, xlab="Week", ylab="Probability", shades=TRUE, xaxt="n", yaxt="n",legend=NULL,main=paste0("K=",k))
# legend("topright",Legend,cex=0.5,lty=1, col=1:k,bty = "n")
# axis(1, at =seq(0, 103, by=10)/10,label=seq(0, 103, by=10))
# axis(2,at=seq(0,1,by=0.1), label=seq(0,1,by=0.1),las=1)



###################################################################################################
#plot observed proportion of optimal seroprotection by identified group membership for k=2~6

##Import data matrix
SeroProtect=read.delim(here("./Data/SeroProtect_4k.txt"),sep=",",header=FALSE)
colnames(SeroProtect)=c("ID",sapply(1:103, function(i){
  paste0("Protect",i)
}))
obs_binary_traj=SeroProtect[,2:104]

#plot observed proportion trajectories
Obs_sp_proptraj_k=list()
Obs_sp_proptraj_plot=list()

for (k in 1:length(mod)){
  class=mod[[k]]$pprob$class
  obs_prop_seroprotect=c()
  for(i in sort(unique(class))){
    Prop=obs_binary_traj[which(class==i),]%>%as.matrix()%>%colMeans()
    dat=cbind(Week=1:length(Prop),Prop=Prop,Class=rep(i,length(Prop)))
    obs_prop_seroprotect=rbind(obs_prop_seroprotect,dat)
  }
  obs_prop_seroprotect=data.frame(obs_prop_seroprotect)%>%mutate(Group=factor(Class))
  Obs_sp_proptraj_k[[k]]=obs_prop_seroprotect
  
  Obs_sp_proptraj_plot[[k]]=ggplot(obs_prop_seroprotect,aes(x=Week, y=Prop, colour=Group))+geom_line(linewidth=0.75)+ggtitle(paste0("K=",k))+scale_x_continuous(limits = c(1,103), expand = c(0, 0))+xlab("Week") + ylab("Probability")+theme_ben()+theme(plot.title = element_text(hjust = 0.5))
}

obs_plot=ggarrange(plotlist=Obs_sp_proptraj_plot,ncol=2,nrow=ceiling(length(Obs_sp_proptraj_plot)/2))
obs_plot=annotate_figure(obs_plot, top = text_grob("Observed proportion of optimal PrEP sero-protection by identified group membership", color = "black", face = "bold", size = 16))

## Export observed prop traj plot
ggexport(obs_plot, filename = here("Figures","Obs_prop_traj_k.png"),width=1200, height=1000)
## Export observed prop traj to rds
saveRDS(Obs_sp_proptraj_k, here("Export","Obs_sp_proptraj_k"))

################################################################################################
# Import model fitness statistics 
fit=readRDS(here("./Export/real/Stats_data4k_rep20maxit10"))

fit

#Plot scree plot of log-likeliood

D=data.frame(K=1:6,log_likelihood=fit[[2]]$loglik)
ggplot(data=D,aes(x=K,log_likelihood))+geom_line()+geom_point()+xlab("K") + ylab("Log-likelihood")+scale_x_continuous(breaks=1:6)+theme_ben()+ggtitle("Scree plot of log-likelihoods")+theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,25,0)))

#plot scree plot of BIC
D=data.frame(K=1:6,BIC=fit[[2]]$BIC)
ggplot(data=D,aes(x=K,BIC))+geom_line()+geom_point()+xlab("K") + ylab("BIC")+scale_x_continuous(breaks=1:6)+theme_ben()+ggtitle("Scree plot of BIC")+theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,25,0)))

#plot scree plot of sample-adjusted BIC

llik=fit[[2]]$loglik
npar=fit[[2]]$npm
Da=data.frame(K=1:6,ABIC=  (-2*llik) + ((log((4000 + 2)/24)) * npar))
ggplot(data=Da,aes(x=K,ABIC))+geom_line()+geom_point()+xlab("K") + ylab("aBIC")+scale_x_continuous(breaks=1:6)+theme_ben()+ggtitle("Scree plot of sample size-adjusted BIC")+theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,25,0)))


install.packages("tidyLPA")
library(tidyLPA)

install.packages('mclust')
library(mclust)

llik

calc_lrt(n=4000,
         null_ll=llik[1],
         null_param = npar[1],
         null_classes = 1,
         alt_ll=llik[2],
         alt_param = npar[2],
         alt_classes = 2)

?calc_lrt


#################################################################################################
#Extract model coefficients

num_param=mod[[2]]$best%>%length()
UT=mod[[2]]$V

vcov=matrix(0,num_param,num_param)

vcov[upper.tri(vcov,diag=TRUE)]=UT

f_vcov=vcov+t(vcov)
diag(f_vcov)=diag(f_vcov)-diag(vcov)

