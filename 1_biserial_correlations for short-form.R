# Updated on 25.09.2024 
#Author: Yi-Jhen Wu 
#1. biserial_correlations for short-form (Table 2)
library(ltm)
n <- nrow(Short.form)# sample size
item_names <- colnames(Short.form)
biserial_correlations <- numeric(length(item_names))# 3ß items
  
  for (i in seq_along(item_names)) {
    item <- Short.form[[i]]
    total_score <- rowSums(Short.form) - item
    biserial_correlations[i] <- cor(total_score,item)
  }
  
  names(biserial_correlations) <- item_names
  round(biserial_correlations,2)
  
  #source:https://www.r-bloggers.com/2021/07/point-biserial-correlation-in-r-quick-guide/
  
#2. ratio of the correct responses (Table 2)
round (apply(Short.form,2,sum)/74,2)

#3. four-skill model 
library(NPCD)
setwd("C:/Users/myijwu01/Desktop/Revision_CDM_Short form/Shortform")
Q4 <- read.csv("Q4_09252024.txt",header=F,sep="")
Sd4<-CDM::din(data = Short.form, q.matrix = Q4, conv.crit = 0.01, maxit = 500, rule = "DINA", progress = TRUE) 
summary(Sd4)
absf4S <-modelfit.cor.din(Sd4, jkunits=10)
summary(absf4S)

anova (Sd1,Sd4)
anova (Sd5,Sd4)
#4 four-skill (NPCD package)
#attribute profiles for this examinee
n_fourSkill <- AlphaNP(Short.form, Q4, gate="AND", method="Hamming")
loss_fourSkill <- n_fourSkill$loss.matrix
small_fourSkill <- apply(loss_fourSkill,2,min)# select the smallest values for each person 
mean(small_fourSkill)


#5 five-skill (NPCD package)
#attribute profiles for this examinee
n_fiveSkill <- AlphaNP(Short.form, Q5, gate="AND", method="Hamming")
loss_fiveSkill <- n_fiveSkill $loss.matrix
small_fiveSkill <- apply(loss_fiveSkill,2,min)# select the smallest values for each person 
mean(small_fiveSkill)

#6 one-skill (NPCD package)
#attribute profiles for this examinee
n_oneSkill <- AlphaNP(Short.form, Q1, gate="AND", method="Hamming")
loss_oneSkill <- n_oneSkill $loss.matrix
small_oneSkill <- apply(loss_oneSkill,2,min)# select the smallest values for each person 
mean(small_oneSkill)

#7 six-skill (NPCD package)
n_sixSkill <- AlphaNP(Short.form, Q6, gate="AND", method="Hamming")
loss_sixSkill <- n_sixSkill$loss.matrix
small_sixSkill <- apply(loss_sixSkill,2,min)# select the smallest values for each person 
mean(small_sixSkill )