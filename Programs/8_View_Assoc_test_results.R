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
library(VGAM)
library(glmnet)
library(nnet)
rm(list=ls())

MLM_mod=readRDS(here('Export','real','MLM_mod'))

MLM_coef=summary(MLM_mod[[1]])%>%coef()%>%data.frame()
MLM_coef$Variables=rownames(MLM_coef)
MLM_coef$Estimate[grep("zip3",MLM_coef$Variables)]=MLM_coef$Estimate[grep("zip3",MLM_coef$Variables)]*5
MLM_coef$Std..Error[grep("zip3",MLM_coef$Variables)]=MLM_coef$Std..Error[grep("zip3",MLM_coef$Variables)]*5



MLM_coef=MLM_coef%>%rename(SE=Std..Error,p=Pr...z..)%>%
         mutate(OR=exp(Estimate),
                CI_LB=exp(Estimate-1.96*SE),
                CI_UB=exp(Estimate+1.96*SE))%>%
        select(Variables,OR,CI_LB,CI_UB,p)
rownames(MLM_coef)=NULL

group=c(1,3,4)
for(i in 1:3){
  j=group[i]
  dat=MLM_coef%>%subset(grepl(paste0(":",i),Variables)==TRUE)
  colnames(dat)[2:5]=paste0(colnames(MLM_coef)[2:5],j)
  assign(paste0("MLM_coef", i), dat)
}

MLM=cbind(MLM_coef1,MLM_coef2[2:5],MLM_coef3[2:5])

write.csv(MLM,here('Figures','real','MLM_coef.csv'))

################################################################
Lasso_mod=readRDS(here('Export','real','LassoMods'))[[2]]

c=coef(Lasso_mod)

lasso_coefs=list()
OR_cutoff=2


for (i in c(1,3,4)){
  lasso_coefs[[i]] <- tibble(variable = c[[i]]@Dimnames[[1]],
                  log_odds = (c[[i]]@x-c[[2]]@x),
                  effect_size = factor(as.numeric(abs(log_odds)>log(OR_cutoff))))[-1,] %>%
  arrange(log_odds)
  
}


value=c[[1]]@Dimnames[[1]][-1]

label=c("Age at initiation: 25-29 yrs",
        "Age at initiation: 30-39 yrs",
        "Age at initiation: 40-49 yrs",
        "Age at initiation: 50+ yrs",
        "Primary payer: government",
        "Primary payer: cash/others",
        "Pharmacy type: community-based specialty pharmacy",
        "Monthly average copay:>$0",
        "ZIP3 % Black residents",
        "ZIP3 % Latinx/Hispanic residents",
        "ZIP3 % Bachelor's degree or higher",
        "ZIP3 % poverty",
        "ZIP3 % uninsured",
        "ZIP3 % PrEP provider density")

labeltab=data.frame(label=label,variable=value)

for (i in c(1,3,4)){
  print(i)
  lasso_coefs[[i]]=merge(x=lasso_coefs[[i]],y=labeltab,by="variable")%>%select(label,log_odds,effect_size)%>%arrange(log_odds)
}

  
  ggplot(lasso_coefs[[1]]) +
    geom_col(aes(x = fct_rev(fct_reorder(label, log_odds)),
                 y = log_odds,
                 fill = factor(effect_size, levels = c(0, 1), labels = c("No", "Yes")))) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust=1)) +
    coord_flip() + 
    theme(text = element_text(size = 12)) +
    ylab("Log-odds") +
    xlab("Baseline covariates") +
    scale_fill_discrete(name = paste0("OR>",OR_cutoff))+
    ggtitle("Penalized log-odds differences of baseline covariates on \n predicted group membership (Group 1 vs 2)")
  
  
  
  ggplot(lasso_coefs[[3]]) +
    geom_col(aes(x = fct_rev(fct_reorder(label, log_odds)),
                 y = log_odds,
                 fill = factor(effect_size, levels = c(0, 1), labels = c("No", "Yes")))) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust=1)) +
    coord_flip() + 
    theme(text = element_text(size = 12)) +
    ylab("Log-odds") +
    xlab("Baseline covariates") +
    scale_fill_discrete(name = paste0("OR>",OR_cutoff))+
    ggtitle("Penalized log-odds differences of baseline covariates on \n predicted group membership (Group 3 vs 2)")
  
  
  ggplot(lasso_coefs[[4]]) +
    geom_col(aes(x = fct_rev(fct_reorder(label, log_odds)),
                 y = log_odds,
                 fill = factor(effect_size, levels = c(0, 1), labels = c("No", "Yes")))) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust=1)) +
    coord_flip() + 
    theme(text = element_text(size = 12)) +
    ylab("Log-odds") +
    xlab("Baseline covariates") +
    scale_fill_discrete(name = paste0("OR>",OR_cutoff))+
    ggtitle("Penalized log-odds differences of baseline covariates on \n predicted group membership (Group 4 vs 2)")


