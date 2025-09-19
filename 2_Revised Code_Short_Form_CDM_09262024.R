##Version 26.09 

# 1. Install package ------------------------------------------------------
library(psych)
library(CDM)
library(NPCD)

# 2. Setting work directory -----------------------------------------------
setwd("C:/Users/myijwu01/Desktop/Revision_CDM_Short form/Shortform")
Short.form <- read.csv("Short-form.txt",header = T, sep="")
Q1 <- as.matrix(read.csv("Q1.txt",header = F, sep=""))
Q5 <- as.matrix(read.csv("Q5.txt",header = F, sep=""))
Q6 <- as.matrix(read.csv("Q6.txt",header = F, sep=""))
Q4 <- as.matrix(read.csv("Q4_09252024.txt",header = F, sep=""))

# 3. Dimensionallity of validation  ----------------------------------------
# One-skill model (code from Iva:RShort-form.R)
Sd1<-CDM::din(data = Short.form, q.matrix = Q1, conv.crit = 0.01, maxit = 500, 
              rule = "DINA", progress = TRUE) 
summary(Sd1)# AIC=2565.753 , BIC=2706.301  (Table 3);Mean of RMSEA item fit: 0.001 
natt_1<- dim(Q1)[2]
M1 <- 2^natt_1# the number of profiles is 2 (Table3)
absf1S <-modelfit.cor.din(Sd1, jkunits=10)
summary(absf1S)#max(X2)=21.287, p=0.002

# five-skill model (code from Iva:RShort-form.R)
Sd5<-CDM::din(data = Short.form, q.matrix = Q5, conv.crit = 0.01, maxit = 500, 
              rule = "DINA", progress = TRUE) 
summary(Sd5)# AIC=2440.935 , BIC=2650.605   (Table 3);Mean of RMSEA item fit: 0.167            
absf5S <-modelfit.cor.din(Sd5, jkunits=10)
summary(absf5S)#max(X2)=9.912, p=0.714
natt_5<- dim(Q5)[2]
M5 <- 2^natt_5# the number of profiles is 32 (Table3)
cdm.est.class.accuracy(Sd5, n.sims=0, version=2)#classification accuracy 
#MLE_patt=0.754 ,MAP_patt=0.889

# six-skill model (code from Iva:RShort-form.R)
Sd6<-CDM::din(data = Short.form, q.matrix = Q6, conv.crit = 0.01, maxit = 500, 
              rule = "DINA", progress = TRUE) 
summary(Sd6)# AIC=2483.729, BIC=2767.129   (Table 3)
absf6S <- modelfit.cor.din(Sd6, jkunits=10)
summary(absf6S)#max(X2)=9.997, p=0.682 (Table 3);Mean of RMSEA item fit: 0.18 
natt_6<- dim(Q6)[2]
M6 <- 2^natt_6# the number of profiles is 64 (Table3)
cdm.est.class.accuracy(Sd6, n.sims=0, version=2)#classification accuracy 
#MLE_patt=0.662 ,MAP_patt=0.872

# four-skill model (code created by Wu)
# why testing four-skill? Reviewer 1 asked if we tested the four-skills model 
# because the very high correlations between differentiation and correction skills
# Wu created the Q-matrix for the four skills
Sd4<-CDM::din(data = Short.form, q.matrix = Q4, conv.crit = 0.01, maxit = 500, 
              rule = "DINA", progress = TRUE) 
summary(Sd4)# AIC=2421.53, BIC=2594.335    (Table 3);Mean of RMSEA item fit: 0.141 
absf4S <- modelfit.cor.din(Sd4, jkunits=10)
summary(absf4S)#max(X2)=9.954, p=0.698 (Table 3)
natt_4<- dim(Q4)[2]
M4 <- 2^natt_4# the number of profiles is 16 (Table3)
cdm.est.class.accuracy(Sd4, n.sims=0, version=2)#classification accuracy 
#MLE_patt=0.760 ,MAP_patt=0.871

#Model comparisons
anova (Sd1,Sd5)
anova (Sd1,Sd6)
anova (Sd5,Sd6)
anova (Sd4,Sd5)

# 4. Non-parametric approach for the loss matrix for Dimensionallity of validation---------------------------
## The results are in Table 4
# One-skill model
Q1_nonH <- AlphaNP(Short.form, Q1, gate="AND", method="Hamming")
lossValue_Q1 <- Q1_nonH$loss.matrix# 2 rows (2 profiles)* 74 columns (74 people)
min_value_Q1 <- apply(lossValue_Q1, 2, min)#get the minimal values across profiles for each person
mean(min_value)# the average of the loss value is 17.82

# five-skill model
Q5_nonH <- AlphaNP(Short.form, Q5, gate="AND", method="Hamming")
lossValue_Q5 <- Q5_nonH$loss.matrix# 32 rows (32profiles)* 74 columns (74 people)
min_value_Q5 <- apply(lossValue_Q5, 2, min)#get the minimal values across profiles for each person
mean(min_value_Q5)# the average of the loss value is 5.90

# six-skill model 
Q6_nonH <- AlphaNP(Short.form, Q6, gate="AND", method="Hamming")
lossValue_Q6 <- Q6_nonH$loss.matrix# 64 rows (64 profiles)* 74 columns (74 people)
min_value_Q6 <- apply(lossValue_Q6, 2, min)#get the minimal values across profiles for each person
mean(min_value_Q6)# the average of the loss value is 5.67

# four-skill model
Q4_nonH <- AlphaNP(Short.form, Q4, gate="AND", method="Hamming")
lossValue_Q4 <- Q4_nonH$loss.matrix# 16 rows (16 profiles)* 74 columns (74 people)
min_value_Q4 <- apply(lossValue_Q4, 2, min)#get the minimal values across profiles for each person
mean(min_value_Q4)# the average of the loss value is 6.19

# 6. HCDM with the CDM package --------------------------------------------
# We decided to the five-skill model. Now, we five hierarchical structures 

#6.1.hierarchical relation among attributes:A4>A5 (H1)(code from Iva:RShort-form.R)
skill_H1<-paste0("A",1:5) 
BH1<-"A4>A5"
sp1<-skillspace.hierarchy(B=BH1,skill.names = skill_H1)
nrow(sp1$skillspace.reduced)# 24 profiles
Sd5H1<- CDM::din(data = Short.form, q.matrix=Q5,skillclasses=sp1$skillspace.reduced)
summary(Sd5H1)# AIC=2424.873, BIC=2616.111 (Table 4);Mean of RMSEA item fit: 0.166 
Sd5H1fit<-modelfit.cor.din(Sd5H1,  jkunits=10)# Assessing model fit 
summary(Sd5H1fit)#max(X2)=9.950, p=0.70

#6.2.hierarchical relation among attributes:A3>A4>A5 (H2)(code from Iva:RShort-form.R)
skill_H2<-paste0("A",1:5) 
BH2<-"A3>A4>A5 "
sp2<-skillspace.hierarchy(B=BH2,skill.names = skill_H2)
nrow(sp2$skillspace.reduced)# 16profiles
Sd5H2<- CDM::din(data = Short.form,q.matrix=Q5,skillclasses=sp2$skillspace.reduced)
summary(Sd5H2)# AIC=2409.736, BIC=2582.541  (Table 4);Mean of RMSEA item fit: 0.158
Sd5H2fit<-modelfit.cor.din(Sd5H2,jkunits=10)# Assessing model fit 
summary(Sd5H2fit)#max(X2)=9.960, p=0.696

#6.3.hierarchical relation among attributes: A2>A3>A4>A5 (H3)(code from Iva:RShort-form.R)
skill_H3<-paste0("A",1:5) 
BH3<-"A2>A3>A4>A5"
sp3<-skillspace.hierarchy(B=BH3,skill.names =skill_H3)
nrow(sp3$skillspace.reduced)# 10 profiles
Sd5H3<- CDM::din(data = Short.form,q.matrix=Q5,skillclasses=sp3$skillspace.reduced)
summary(Sd5H3)# AIC=414.83,BIC=2573.811(Table 4);Mean of RMSEA item fit: 0.116 
Sd5H3fit<-modelfit.cor.din(Sd5H3,  jkunits=10)# Assessing model fit 
summary(Sd5H3fit)#max(X2)=8.027, p=1

#6.4.hierarchical relation among attributes: A1>A2>A3>A4>A5 (H4)(code from Iva:RShort-form.R)
skill_H4<-paste0("A",1:5) 
BH4<-"A1>A2>A3>A4>A5"
sp4<-skillspace.hierarchy(B=BH4,skill.names =skill_H4)
nrow(sp4$skillspace.reduced)# 6 profiles
Sd5H4<- CDM::din(data = Short.form,q.matrix=Q5,skillclasses=sp4$skillspace.reduced)
summary(Sd5H4)# AIC=2471.665, BIC=2621.429(Table 4);Mean of RMSEA item fit: 0.166 
Sd5H4fit<-modelfit.cor.din(Sd5H4,  jkunits=10)# Assessing model fit 
summary(Sd5H4fit)#max(X2)=10.016, p=0.675

## Model comparisons (code from Iva:RShort-form.R)
## The results are in Table 5
anova(Sd5,Sd5H1) # negative chi-saure
anova(Sd5,Sd5H2) 
anova(Sd5,Sd5H3) 
anova(Sd5,Sd5H4) 

anova(Sd5H1, Sd5H2)
anova(Sd5H1, Sd5H3)
anova(Sd5H1, Sd5H4)

anova(Sd5H2, Sd5H3)
anova(Sd5H2, Sd5H4)

anova(Sd5H3, Sd5H4)

# 7. Nonparmatirc function to HCDM (loss values) --------------------------
##AlphaNP function have to be modified. The modified part is the number of profiles
##I added a new parameter in the function-->profile_h
#profile_h means the number of profiles in the HCM


AlphaNP_H <- function (Y, Q,profile_h,gate = c("AND", "OR"), method = c("Hamming", 
                                                                        "Weighted", "Penalized"), wg = 1, ws = 1) 
{
  Y <- as.matrix(Y)
  Q <- as.matrix(Q)
  check <- NULL
  check <- CheckInput(Y, Q)
  if (!is.null(check)) 
    return(warning(check))
  gate <- match.arg(gate)
  method <- match.arg(method)
  nperson <- dim(Y)[1]
  nitem <- dim(Q)[1]
  natt <- dim(Q)[2]
  pattern <- as.matrix(profile_h)
  M <- nrow(pattern)
  Ideal <- matrix(NA, M, nitem)
  for (m in 1:M) {
    for (j in 1:nitem) {
      if (gate == "AND") {
        u <- prod(pattern[m, ]^Q[j, ])
      }
      else if (gate == "OR") {
        u <- 1 - prod((1 - pattern[m, ])^Q[j, ])
      }
      else {
        return(warning("Gate specification not valid."))
      }
      Ideal[m, j] <- u
    }
  }
  if (method == "Hamming") {
    weight <- rep(1, nitem)
    ws <- wg <- 1
  }
  else if (method == "Weighted") {
    p.bar <- apply(Y, 2, mean)
    weight <- 1/(p.bar * (1 - p.bar))
    weight[weight > 1/(0.95 * 0.05)] <- 1/(0.95 * 0.05)
    ws <- wg <- 1
  }
  else if (method == "Penalized") {
    p.bar <- apply(Y, 2, mean)
    weight <- 1/(p.bar * (1 - p.bar))
    weight[weight > 1/(0.95 * 0.05)] <- 1/(0.95 * 0.05)
    if (ws == wg) 
      warning("Penalzing weights for guess and slip are the same --> equivalent with the \"Weighted\" method.")
  }
  else {
    return(warning("Method specification not valid."))
  }
  loss.matrix <- matrix(NA, nrow = M, ncol = nperson)
  est.class <- NULL
  est.pattern <- NULL
  n.tie <- rep(0, nperson)
  for (i in 1:nperson) {
    Y.matrix <- matrix(rep(Y[i, ], M), M, nitem, byrow = TRUE)
    loss <- apply(matrix(rep(weight, M), M, nitem, byrow = TRUE) * 
                    (wg * abs(Y.matrix - Ideal) * Y.matrix + ws * abs(Y.matrix - 
                                                                        Ideal) * (1 - Y.matrix)), 1, sum)
    loss.matrix[, i] <- loss
    min.loss <- which(loss == min(loss))
    if (length(min.loss) != 1) {
      n.tie[i] <- length(min.loss)
      min.loss <- sample(min.loss, 1, prob = rep(1/length(min.loss), 
                                                 length(min.loss)))
    }
    est.class <- c(est.class, min.loss)
  }
  est.pattern <- pattern[est.class, ]
  est.ideal <- Ideal[est.class, ]
  output <- list(alpha.est = est.pattern, est.ideal = est.ideal, 
                 est.class = est.class, n.tie = n.tie, pattern = pattern, 
                 loss.matrix = loss.matrix, method = method, Q = Q, Y = Y)
  class(output) <- "AlphaNP"
  return(output)
}

# Before running the function, need to load CheckInput from NCPD package
CheckInput <- function(response, Q) {
  
  nperson <- nrow(response)
  nitem <- ncol(response)
  nitem.Q <- nrow(Q)
  
  if (nitem != nitem.Q) {
    return("Item numbers in the response matrix and Q-matrix do not agree.")
  }
  
  if (!all(response %in% c(1, 0))) {
    return("The response matrix should have only two values: 1=correct, 0=incorrect.")
  }
  
  if (!all(Q %in% c(1, 0))) {
    return("The Q-matrix should have only two values: 1=attribute is required, 0=attribute is not required.")
  }
  
}

CheckInput(response = Short.form, Q=Q5)

#7.1.hierarchical relation among attributes:A4>A5 (H1): 24 profiles
nH1 <- sp1$skillspace.reduced# 24 profiles
Non_H1 <- AlphaNP_H(Y=Short.form,Q=Q5,profile_h =nH1,gate = "AND",method = "Hamming")
loss_H1 <- Non_H1$loss.matrix
mean(apply(loss_H1,2,min))

#7.2.hierarchical relation among attributes:A3>A4>A5 (H2): 16 profiles
nH2 <- sp2$skillspace.reduced# 16profiles
Non_H2 <- AlphaNP_H(Y=Short.form,Q=Q5,profile_h =nH2,gate = "AND",method = "Hamming")
loss_H2 <- Non_H2$loss.matrix
mean(apply(loss_H2,2,min))

#7.3.hierarchical relation among attributes: A2>A3>A4>A5 (H3): 10 profiles
nH3 <- sp3$skillspace.reduced# 10 profiles
Non_H3 <- AlphaNP_H(Y=Short.form,Q=Q5,profile_h =nH3,gate = "AND",method = "Hamming")
loss_H3 <- Non_H3$loss.matrix
mean(apply(loss_H3,2,min))

#7.4.hierarchical relation among attributes: A1>A2>A3>A4>A5 (H4): 6 profiles
nH4 <- sp4$skillspace.reduced# 6 profiles
Non_H4 <- AlphaNP_H(Y=Short.form,Q=Q5,profile_h =nH4,gate = "AND",method = "Hamming")
loss_H4 <- Non_H4$loss.matrix
mean(apply(loss_H4,2,min))


# 8. Extra statistical information ----------------------------------------
# Reliability for the short form 
alpha(Short.form)#0.81

#Reliability for the long form 
setwd("C:/Users/myijwu01/Desktop/Revision_CDM_Short form/Longform")
Long.form <- read.csv("Longform.txt",header = T, sep="")
alpha(Long.form)#0.86

# 9. Item parameters in Long format ----------------------------------------

setwd("C:/Users/myijwu01/Desktop/Revision_CDM_Short form/Longform")
Q6_LF <- as.matrix(read.csv("Q6_60.txt",header = F, sep=""))
Long.form <- read.csv("Longform.txt",header = T, sep="")
# five-skill model (code from Iva:RShorQ5_LFt-form.R)
Sd6_LF<-CDM::din(data = Long.form, q.matrix = Q6_LF, conv.crit = 0.01, maxit = 500, 
                 rule = "DINA", progress = TRUE) 
summary(Sd6_LF)   
Sd5_LF$item

# 10. Item parameters and item statistics in the five-skills model with  --------
# with the hierarchical structure 
#6.3.hierarchical relation among attributes: A2>A3>A4>A5 (H3)(code from Iva:RShort-form.R)
skill_H3<-paste0("A",1:5) 
BH3<-"A2>A3>A4>A5"
sp3<-skillspace.hierarchy(B=BH3,skill.names =skill_H3)
Sd5H3<- CDM::din(data = Short.form,q.matrix=Q5,skillclasses=sp3$skillspace.reduced)
all_summary <- summary(Sd5H3)
mean(all_summary$IDI)#IDI values
item_S5H3 <- round(Sd5H3$item[,2:6],2)
apply(Sd5H3$item[,2:6],2,mean)

# biserial_correlations for short-form 
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

#ratio of the correct responses 
round (apply(Short.form,2,sum)/74,2)