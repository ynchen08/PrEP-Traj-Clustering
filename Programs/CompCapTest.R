renv::restore()
# install.packages('gt')
library(gt)
# install.packages('webshot2')
library(webshot2)
# install.packages("matrixStats")
library(matrixStats)
library(here)
# renv::dependencies()
# renv::snapshot()

Stats_rep50maxit30=readRDS(here("./Export/ExportStats_rep50maxit30"))
Stats_rep50maxit20=readRDS(here("./Export/ExportStats_rep50maxit20"))
Stats_rep50maxit10=readRDS(here("./Export/ExportStats_rep50maxit10_2"))

Stats_rep35maxit30=readRDS(here("./Export/ExportStats_rep35maxit30"))
Stats_rep35maxit20=readRDS(here("./Export/ExportStats_rep35maxit20"))
Stats_rep35maxit10=readRDS(here("./Export/ExportStats_rep35maxit10_2"))

Stats_rep20maxit30=readRDS(here("./Export/ExportStats_rep20maxit30"))
Stats_rep20maxit20=readRDS(here("./Export/ExportStats_rep20maxit20"))
Stats_rep20maxit10=readRDS(here("./Export/ExportStats_rep20maxit10"))

# Run Time table ---------------------------------------------------------

RunTime=rbind(Stats_rep50maxit30[[1]],
              Stats_rep50maxit20[[1]],
              Stats_rep50maxit10[[1]],
              Stats_rep35maxit30[[1]],
              Stats_rep35maxit20[[1]],
              Stats_rep35maxit10[[1]],
              Stats_rep20maxit30[[1]],
              Stats_rep20maxit20[[1]],
              Stats_rep20maxit10[[1]])

Num.Rand.Init=c(50,50,50,35,35,35,20,20,20)
Max.Iter=rep(c(30,20,10),3)
RunTime2=cbind(Num.Rand.Init,Max.Iter,RunTime[,-1])%>%data.frame()
colnames(RunTime2)=c("Num.Rand.Init","Max.Iter","K=2","K=3","K=4","K=5","K=6")
RunTime=round(RunTime2)


# Cluster quality statistics ---------------------------------------------------------


x=as.matrix((Stats_rep50maxit30[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep50maxit30=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep50maxit20[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep50maxit20=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep50maxit10[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep50maxit10=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep35maxit30[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep35maxit30=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep35maxit20[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep35maxit20=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep35maxit10[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep35maxit10=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep20maxit30[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep20maxit30=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep20maxit20[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep20maxit20=rowMins(x,na.rm = TRUE)

x=as.matrix((Stats_rep20maxit10[[2]][,c('APPA_C1', 'APPA_C2', 'APPA_C3', 'APPA_C4', 'APPA_C5', 'APPA_C6')]))
Min.APPA_rep20maxit10=rowMins(x,na.rm = TRUE)

Min.APPA=cbind(Min.APPA_rep50maxit30,
               Min.APPA_rep50maxit20,
               Min.APPA_rep50maxit10,
               Min.APPA_rep35maxit30,
               Min.APPA_rep35maxit20,
               Min.APPA_rep35maxit10,
               Min.APPA_rep20maxit30,
               Min.APPA_rep20maxit20,
               Min.APPA_rep20maxit10)

Min.APPA=Min.APPA[-1,]
colnames(Min.APPA)=c("rep50maxit30","rep50maxit20","rep50maxit10",
                     "rep35maxit30","rep35maxit20","rep35maxit10",
                     "rep20maxit30","rep20maxit20","rep20maxit10")
row.names(Min.APPA)=c("K=2","K=3","K=4","K=5","K=6")

cq=c('loglik','BIC')
CQ=list()
for (i in 1:length(cq)){
  CQ[[i]]=cbind(Stats_rep50maxit30[[2]][cq[i]],
                Stats_rep50maxit20[[2]][cq[i]],
                Stats_rep50maxit10[[2]][cq[i]],
                Stats_rep35maxit30[[2]][cq[i]],
                Stats_rep35maxit20[[2]][cq[i]],
                Stats_rep35maxit10[[2]][cq[i]],
                Stats_rep20maxit30[[2]][cq[i]],
                Stats_rep20maxit20[[2]][cq[i]],
                Stats_rep20maxit10[[2]][cq[i]])
  colnames(CQ[[i]])=c("rep50maxit30","rep50maxit20","rep50maxit10",
                      "rep35maxit30","rep35maxit20","rep35maxit10",
                      "rep20maxit30","rep20maxit20","rep20maxit10")
  row.names(CQ[[i]])=c("k=1","K=2","K=3","K=4","K=5","K=6")
  names(CQ)[i]=cq[i]
}

CQ[[3]]=Min.APPA
names(CQ)[3]="Min. APPA"


##format tables

runtime_tab=RunTime%>%gt()%>%tab_header(title=md("**GBTM run time across various model optimization parameters (in minutes)**")) %>%cols_label(Num.Rand.Init="Rand.Init")
gtsave(runtime_tab, filename =here("Figures", "runtime_Tab.png"))



loglk_tab=cbind(K=1:6,CQ[[1]])%>%gt()%>%tab_header(title=md("**Log-likelihood across various model optimization parameters**"))%>%tab_spanner(label="Random Initial Values=50", columns=2:4)%>%tab_spanner(label="Random Initial Values=35", columns=5:7)%>%tab_spanner(label="Random Initial Values=20", columns=8:10)%>%cols_label(rep50maxit30="Max.Iter=30",rep50maxit20="Max.Iter=20",rep50maxit10="Max.Iter=10",rep35maxit30="Max.Iter=30",rep35maxit20="Max.Iter=20",rep35maxit10="Max.Iter=10",rep20maxit30="Max.Iter=30",rep20maxit20="Max.Iter=20",rep20maxit10="Max.Iter=10")

gtsave(loglk_tab, filename =here("Figures", "loglk_Tab.png"))


BIC_tab=cbind(K=1:6,CQ[[2]])%>%gt()%>%tab_header(title=md("**BIC across various model optimization parameters**"))%>%tab_spanner(label="Random Initial Values=50", columns=2:4)%>%tab_spanner(label="Random Initial Values=35", columns=5:7)%>%tab_spanner(label="Random Initial Values=20", columns=8:10)%>%cols_label(rep50maxit30="Max.Iter=30",rep50maxit20="Max.Iter=20",rep50maxit10="Max.Iter=10",rep35maxit30="Max.Iter=30",rep35maxit20="Max.Iter=20",rep35maxit10="Max.Iter=10",rep20maxit30="Max.Iter=30",rep20maxit20="Max.Iter=20",rep20maxit10="Max.Iter=10")

gtsave(BIC_tab, filename =here("Figures", "BIC_Tab.png"))


APPA_tab=cbind(K=2:6,CQ[[3]])%>%data.frame()%>%gt()%>%tab_header(title=md("**Minimum average posterior probability across various model optimization parameters**"))%>%tab_spanner(label="Random Initial Values=50", columns=2:4)%>%tab_spanner(label="Random Initial Values=35", columns=5:7)%>%tab_spanner(label="Random Initial Values=20", columns=8:10)%>%cols_label(rep50maxit30="Max.Iter=30",rep50maxit20="Max.Iter=20",rep50maxit10="Max.Iter=10",rep35maxit30="Max.Iter=30",rep35maxit20="Max.Iter=20",rep35maxit10="Max.Iter=10",rep20maxit30="Max.Iter=30",rep20maxit20="Max.Iter=20",rep20maxit10="Max.Iter=10")

gtsave(APPA_tab, filename =here("Figures", "APPA_Tab.png"))


# Compare predicted trajectories-----------------------------------------------------------


#import models
Mod_rep50maxit30=readRDS(here("./Export/GBTM_mods_rep50maxit30"))
Mod_rep50maxit20=readRDS(here("./Export/GBTM_mods_rep50maxit20"))
Mod_rep50maxit10=readRDS(here("./Export/GBTM_mods_rep50maxit10_2"))

Mod_rep35maxit30=readRDS(here("./Export/GBTM_mods_rep35maxit30"))
Mod_rep35maxit20=readRDS(here("./Export/GBTM_mods_rep35maxit20"))
Mod_rep35maxit10=readRDS(here("./Export/GBTM_mods_rep35maxit10_2"))

Mod_rep20maxit30=readRDS(here("./Export/GBTM_mods_rep20maxit30"))
Mod_rep20maxit20=readRDS(here("./Export/GBTM_mods_rep20maxit20"))
Mod_rep20maxit10=readRDS(here("./Export/GBTM_mods_rep20maxit10"))


SP_long=readRDS(here("./Export/SP_long"))

##create function to plot estimated mean trajectory
plotGBTM=function(mod){

  datnew=data.frame(Week = seq(1, 103)/10)
  par(mfrow=c(3,2),oma=c(1,1,1.5,1)+0.1,mar=c(4,4,3,1)+0.1)
  
  for (k in 1:length(mod)){
    Legend=c(sapply(1:k, function(i){
      paste0("Group ",i)
    }))
    plotpred=predictY(mod[[k]],datnew,var.time="Week", draws=TRUE)
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

#plot  predicted trajectories

png(file = here("Figures", "Traj_rep50maxit30.png"), width = 1000, height = 700)
plotGBTM(Mod_rep50maxit30)
dev.off()

png(file = here("Figures", "Traj_rep50maxit20.png"), width = 1000, height = 700)
plotGBTM(Mod_rep50maxit20)
dev.off()

png(file = here("Figures", "Traj_rep50maxit10.png"), width = 1000, height = 700)
plotGBTM(Mod_rep50maxit10)
dev.off()

png(file = here("Figures", "Traj_rep35maxit30.png"), width = 1000, height = 700)
plotGBTM(Mod_rep35maxit30)
dev.off()

png(file = here("Figures", "Traj_rep35maxit20.png"), width = 1000, height = 700)
plotGBTM(Mod_rep35maxit20)
dev.off()

png(file = here("Figures", "Traj_rep35maxit10.png"), width = 1000, height = 700)
plotGBTM(Mod_rep35maxit10)
dev.off()


png(file = here("Figures", "Traj_rep20maxit30.png"), width = 1000, height = 700)
plotGBTM(Mod_rep20maxit30)
dev.off()

png(file = here("Figures", "Traj_rep20maxit20.png"), width = 1000, height = 700)
plotGBTM(Mod_rep20maxit20)
dev.off()

png(file = here("Figures", "Traj_rep20maxit10.png"), width = 1000, height = 700)
plotGBTM(Mod_rep20maxit10)
dev.off()


# 
# ##############################################################################
# #Check if performance statistics object match  
# ############################################################################
# 
# x_50.30=GBTM_stat(Mod_rep50maxit30)
# x_50.20=GBTM_stat(Mod_rep50maxit20)
# x_50.10=GBTM_stat(Mod_rep50maxit10)
# 
# x_35.30=GBTM_stat(Mod_rep35maxit30)
# x_35.20=GBTM_stat(Mod_rep35maxit20)
# x_35.10=GBTM_stat(Mod_rep35maxit10)
# 
# x_20.30=GBTM_stat(Mod_rep20maxit30)
# x_20.20=GBTM_stat(Mod_rep20maxit20)
# x_20.10=GBTM_stat(Mod_rep20maxit10)
# 
# 
# as.matrix(Stats_rep50maxit30[[2]]==x_50.30)%>%c()%>%mean(.,na.rm=TRUE)
# as.matrix(Stats_rep50maxit20[[2]]==x_50.20)%>%c()%>%mean(.,na.rm=TRUE)
# as.matrix(Stats_rep50maxit10[[2]]==x_50.10)%>%c()%>%mean(.,na.rm=TRUE)
# 
# Stats_rep50maxit10[[2]]=x_50.10
# saveRDS(Stats_rep50maxit10, here("./Export/ExportStats_rep50maxit10"))
# 
# as.matrix(Stats_rep35maxit30[[2]]==x_35.30)%>%c()%>%mean(.,na.rm=TRUE)
# as.matrix(Stats_rep35maxit20[[2]]==x_35.20)%>%c()%>%mean(.,na.rm=TRUE)
# as.matrix(Stats_rep35maxit10[[2]]==x_35.10)%>%c()%>%mean(.,na.rm=TRUE)
# 
# Stats_rep35maxit10[[2]]=x_35.10
# saveRDS(Stats_rep35maxit10, here("./Export/ExportStats_rep35maxit10"))
# 
# as.matrix(Stats_rep20maxit30[[2]]==x_20.30)%>%c()%>%mean(.,na.rm=TRUE)
# as.matrix(Stats_rep20maxit20[[2]]==x_20.20)%>%c()%>%mean(.,na.rm=TRUE)
# as.matrix(Stats_rep20maxit10[[2]]==x_20.10)%>%c()%>%mean(.,na.rm=TRUE)
# 
# 
# ###########################################################################
# # re-run rep=50 maxit=10 and rep=35 maxit=10
# ############################################################################
# Ex35.10=readRDS(here('Export','ExportStats_rep35maxit10_2'))
# Ex35.10_=readRDS(here('Export','ExportStats_rep35maxit10'))
# Ex50.10=readRDS(here('Export','ExportStats_rep50maxit10_2'))
# Ex50.10_=readRDS(here('Export','ExportStats_rep50maxit10'))
# 
# Ex35.10[[2]]
# Ex35.10_[[2]]
# 
# Ex50.10[[2]]
# Ex50.10_[[2]]
# 
# m35.10=readRDS(here('Export','GBTM_mods_rep35maxit10_2'))
# m50.10=readRDS(here('Export','GBTM_mods_rep50maxit10_2'))
# 
# x35.10=GBTM_stat(m35.10)
# x50.10=GBTM_stat(m50.10)