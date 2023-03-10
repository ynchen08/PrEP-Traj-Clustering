rm(list=ls())

#Prepare working environment ---------------------------------------------------
#set your working directory
setwd("C:/Users/yche465/Desktop/AIM 1/Codes/PrEP-Traj-Clustering/")
#load relevant R project environment
renv::load(getwd())

#install packages if needed
  #install.packages("lcmm")
  #install.packages('tidyr')
  #install.packages('dplyr')
  #install.packages("rlang")
  #install.packages("devtools")
  #renv::install("hlennon/LCTMtools")
  #install.packages('here')
  #install.packages('ellipsis')
  #install.packages(haven)
  #install.packages(ggasym)

#Restore relevant packages
  # renv::dependencies()
  # renv::snapshot()
renv::restore()

#load libraries
  library(here)
  library(tidyverse)
  library(lcmm)
  library(tidyr)
  library(renv)
  library(dplyr)
  library(rlang)
  library(LCTMtools)
  library(ellipsis)
  library(haven)
  library(ggasym)


#Specify user
User="Emory"
if(User=="Emory"){
  Folder="fake"
} else{
  Folder="real"
}


## Index object for linking original and analysis ID-----------------------------------
sampled_ID_index=read.csv(here("Export",Folder,"Sampled_ID_index.csv"))%>%rename(ID=ID_new)

head(sampled_ID_index)

# Append covariate dataset with predicted group membership ----------------------------------------------------------
mod=readRDS(here("Export",Folder,"GBTM_data4k_rep20maxit10"))
final.mod=mod[[4]]
Pred.Class=sampled_ID_index%>%arrange(ID)
Pred.Class$Class=final.mod$pprob$class
covar=read.csv(here('Data',Folder,'PersonVars_sampled.csv'))%>%rename(ID_orig=ID)
covar_dat=merge(x=covar,y=Pred.Class,by='ID_orig',all.x=TRUE)%>%mutate(Dur_subopt_cat2=case_when(Dur_subopt<13~1,
                                                                                                 Dur_subopt<26~2,
                                                                                                 Dur_subopt<39~3,
                                                                                                 Dur_subopt<52~4,
                                                                                                 Dur_subopt<65~5,
                                                                                                 Dur_subopt<78~6,
                                                                                                 Dur_subopt<91~7,
                                                                                                 TRUE~8))

zip3_dat=readRDS(here('Data','ZIP3_Covar','zip3_covariates'))

zipvn=colnames(zip3_dat)[-1]

covar_dat=merge(x=covar_dat,y=zip3_dat,by="zip3",all.x=TRUE)

#Descriptive statistics by deterministic group membership------------------------------------------------------------

cat_vars=c("Age_at_init_cat","Primary_payer","Pharmacy_type","Average_copay_cat","Num_protect_interval","Num_protect_interval_cat","Dur_subopt_cat2")
cont_vars=c("Age_at_init","Dur_subopt",zipvn)
## Categorical covariates
  DESCRIP.DAT_CAT=data.frame()
  for (i in cat_vars){
    n=table(covar_dat[[i]],covar_dat$Class)%>%as.data.frame.matrix()
    colnames(n)=paste0("N.Mean_G",1:ncol(n))
    perc=table(covar_dat[[i]],covar_dat$Class)%>%prop.table(margin=2)%>%as.data.frame.matrix()
    colnames(perc)=paste0("Perc.SD_G",1:ncol(perc))
    descrip.dat=cbind(n,perc)[,rep(seq_len(ncol(n)),each=2)+c(0,ncol(n))]  
    descrip.dat=cbind("Var_Name"=rep(i,nrow(n)),"Levels"=1:nrow(n),descrip.dat)
    DESCRIP.DAT_CAT=rbind(DESCRIP.DAT_CAT,descrip.dat)
  }
  
  nmiss_cat=covar_dat%>%dplyr::select(all_of(cat_vars),Class)%>%
                        group_by(Class)%>%summarise(across(everything(), ~ sum(is.na(.x))))%>%
                        t()%>%data.frame()
  nmiss_cat$Var_Name=row.names(nmiss_cat)
  colnames(nmiss_cat)[1:4]=paste0("NMiss_G",1:4)
  
  nused_cat=covar_dat%>%dplyr::select(all_of(cat_vars),Class)%>%
                        group_by(Class)%>%summarise(across(everything(), ~ sum(!is.na(.x))))%>%
                        t()%>%data.frame()
  nused_cat$Var_Name=row.names(nmiss_cat)
  colnames(nused_cat)[1:4]=paste0("Nused_G",1:4)
  
  col.list=c("Var_Name", "Levels",
             "Nused_G1","NMiss_G1","N.Mean_G1","Perc.SD_G1",
             "Nused_G2","NMiss_G2","N.Mean_G2","Perc.SD_G2",
             "Nused_G3","NMiss_G3","N.Mean_G3","Perc.SD_G3",
             "Nused_G4","NMiss_G4","N.Mean_G4","Perc.SD_G4")
  
  DESCRIP.DAT_CAT=merge(DESCRIP.DAT_CAT,nused_cat,by="Var_Name",all.x=TRUE)
  DESCRIP.DAT_CAT=merge(DESCRIP.DAT_CAT,nmiss_cat,by="Var_Name",all.x=TRUE)%>%dplyr::select(all_of(col.list))
  
  
## Continuous covariates
  mu=covar_dat%>%dplyr::select(all_of(cont_vars),Class)%>%group_by(Class)%>%summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))%>%t()%>%data.frame()
  colnames(mu)=paste0("N.Mean_G",1:ncol(n))
  sd=covar_dat%>%dplyr::select(all_of(cont_vars),Class)%>%group_by(Class)%>%summarise(across(everything(), ~ sd(.x, na.rm = TRUE)))%>%t()
  colnames(sd)=paste0("Perc.SD_G",1:ncol(n))
  descrip.dat_cont=cbind(mu,sd)[,rep(seq_len(ncol(mu)),each=2)+c(0,ncol(mu))]
  
  DESCRIP.DAT_CONT=data.frame(Var_Name=rownames(descrip.dat_cont)[-1],Levels=NA_real_,descrip.dat_cont[-1,])
  row.names(DESCRIP.DAT_CONT)=NULL
  
  nmiss_cont=covar_dat%>%dplyr::select(all_of(cont_vars),Class)%>%
    group_by(Class)%>%summarise(across(everything(), ~ sum(is.na(.x))))%>%
    t()%>%data.frame()
  nmiss_cont$Var_Name=row.names(nmiss_cont)
  colnames(nmiss_cont)[1:4]=paste0("NMiss_G",1:4)
  
  nused_cont=covar_dat%>%dplyr::select(all_of(cont_vars),Class)%>%
    group_by(Class)%>%summarise(across(everything(), ~ sum(!is.na(.x))))%>%
    t()%>%data.frame()
  nused_cont$Var_Name=row.names(nmiss_cont)
  colnames(nused_cont)[1:4]=paste0("Nused_G",1:4)
  
  DESCRIP.DAT_CONT=merge(DESCRIP.DAT_CONT,nused_cont,by="Var_Name",all.x=TRUE)
  DESCRIP.DAT_CONT=merge(DESCRIP.DAT_CONT,nmiss_cont,by="Var_Name",all.x=TRUE)%>%dplyr::select(all_of(col.list))
  
##Combine continous and categorical stats
  DESCRIP.DAT=rbind(DESCRIP.DAT_CAT,DESCRIP.DAT_CONT)
  DESCRIP.DAT_rec=DESCRIP.DAT%>%mutate(Category=case_when(Var_Name=="Age_at_init_cat" & Levels==1~"18 to 24",
                                                          Var_Name=="Age_at_init_cat" & Levels==2~"25 to 29",
                                                          Var_Name=="Age_at_init_cat" & Levels==3~"30 to 39",
                                                          Var_Name=="Age_at_init_cat" & Levels==4~"40 to 49",
                                                          Var_Name=="Age_at_init_cat" & Levels==5~"50+",
                                                          Var_Name=="Primary_payer" & Levels==1~"Commercial",
                                                          Var_Name=="Primary_payer" & Levels==2~"Government",
                                                          Var_Name=="Primary_payer" & Levels==3~"Cash/Other",
                                                          Var_Name=="Pharmacy_type" & Levels==1~"Community-based specialty pharmacy",
                                                          Var_Name=="Pharmacy_type" & Levels==2~"Traditional retail pharmacy",
                                                          Var_Name=="Average_copay_cat" & Levels==1~"$0",
                                                          Var_Name=="Average_copay_cat" & Levels==2~">$0",
                                                          Var_Name=="Num_protect_interval_cat" & Levels==1~"1 to 3",
                                                          Var_Name=="Num_protect_interval_cat" & Levels==2~"4 to 6",
                                                          Var_Name=="Num_protect_interval_cat" & Levels==3~"7 to 9",
                                                          Var_Name=="Num_protect_interval_cat" & Levels==4~"10+",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==1~"<3 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==2~"3 to 6 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==3~"6 to 9 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==4~"9 to 12 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==5~"12 to 15 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==6~"15 to 18 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==7~"18 to 21 months",
                                                          Var_Name=="Dur_subopt_cat2" & Levels==8~"21 to 24 months"))
  DESCRIP.DAT_rec$Category[which(DESCRIP.DAT_rec$Var_Name=="Num_protect_interval")]=DESCRIP.DAT_rec$Levels[which(DESCRIP.DAT_rec$Var_Name=="Num_protect_interval")]
  
  
  DESCRIP.DAT_final=swap_cols(DESCRIP.DAT_rec,Levels,Category)[,-ncol(DESCRIP.DAT_rec)]
  
  write.csv(DESCRIP.DAT_final, here('Export',Folder,'Descript_by_group.csv'))
  
  View(DESCRIP.DAT_final)
