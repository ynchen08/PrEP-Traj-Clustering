Stats_rep50maxit30=readRDS(here("./Export/ExportStats_rep50maxit30"))
Stats_rep50maxit20=readRDS(here("./Export/ExportStats_rep50maxit20"))
Stats_rep50maxit10=readRDS(here("./Export/ExportStats_rep50maxit10"))

Stats_rep35maxit30=readRDS(here("./Export/ExportStats_rep35maxit30"))
Stats_rep35maxit20=readRDS(here("./Export/ExportStats_rep35maxit20"))
Stats_rep35maxit10=readRDS(here("./Export/ExportStats_rep35maxit10"))

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

install.packages("matrixStats")
library(matrixStats)

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

################################################################################################################
# Graphs
###################################################################################################################

Mod_rep50maxit30=readRDS(here("./Export/GBTM_mods_rep50maxit30"))
Mod_rep20maxit10=readRDS(here("./Export/GBTM_mods_rep20maxit10"))

plotGBTM=function(mod){
  ##Plot estimated mean trajectory
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
  mtext("Group-specific predicted trajectory of PrEP sero-protection", side=3,line = 0,outer = TRUE)
}

plotGBTM(Mod_rep50maxit30)

plotGBTM(Mod_rep20maxit10)



