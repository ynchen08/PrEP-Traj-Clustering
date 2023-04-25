library(here)


fit=readRDS(here("Export","real","SA_Stats_data4k_rep20maxit10"))

#import plot theme set-up
source(here("misc/plot_theme.R"))

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

# Lo–Mendell–Rubin likelihood ratio test - originally designed in a study testing the number of components in a normal mixture, and has been widely used in latent profile analysis. Not sure if the model type (probit) would affect its performance?  
LRT_run=length(llik)-1

LRT=data.frame(Test=c("1v2","2v3","3v4","4v5","5v6"),
               aLMR=rep(0,LRT_run),
               df=rep(0,LRT_run),
               p=rep(0,LRT_run))


for (i in 1:LRT_run){
  lrt=calc_lrt(n=4000,
               null_ll=llik[i],
               null_param = npar[i],
               null_classes = i,
               alt_ll=llik[i+1],
               alt_param = npar[i+1],
               alt_classes = (i+1))
  LRT$aLMR[i]=lrt[2]
  LRT$df[i]=lrt[3]
  LRT$p[i]=lrt[4]
}

LRT

x