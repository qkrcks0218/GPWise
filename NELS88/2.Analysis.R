############################################
# Last update : Dec 19 2021
# Code for Section 5
############################################

############################################
# Load Source Files and Packages
############################################

source("../MySL.R")
source("../SSLS.R")

library(dplyr)

############################################
# Superlearner Parameters
############################################

SL.hpara <- list()
SL.hpara$SLL <- 1# c(1,2,3,4,5,6,7,9,10)
# Superlearner basic learning algorithms: 
# 1: GLM
# 2: lasso/ridge
# 3: earth
# 4: GAM
# 5: xgboost
# 6: polynomial spline
# 7: random forest
# 9: gbm
# 10: 1-layer MLP
SL.hpara$MTRY <- c(2,4)                # random forest parameters
SL.hpara$MLPL <- c(2,4)                # number of nodes in MLP
SL.hpara$NMN <- 25                     # gbm parameter
SL.hpara$MLPdecay <- 10^c(-3,-4,-5)    # MLP decay parameter

############################################
# Recommend to use parallel computing across BATCH in 1:100
############################################

for(BATCH in 1:100){
  
  
  CData <- read.csv("NELS88_NoNA.csv")
  SCHID <- CData$SCHID
  
  MS.ID <- sort(sample(unique(CData$SCHID),round(length(unique(CData$SCHID))/2)))
  AS.ID <- setdiff(unique(CData$SCHID),MS.ID)
  
  POS <- data.frame(v=1:dim(CData)[1],SCHID=CData$SCHID)
  
  POS %>% filter(SCHID %in% MS.ID) -> MS.POS 
  POS %>% filter(SCHID %in% AS.ID) -> AS.POS 
  
  CData <- CData[,-dim(CData)[2]]
  
  N <- dim(CData)[1]
  pos.AX <- 2:(dim(CData)[2])
  pos.X <- 3:(dim(CData)[2])
  
  
  
  M <- cbind(as.numeric(CData$Urban==1 & CData$White!=1), # Urban minority
             as.numeric(CData$Urban==0 & CData$White!=1), # Suburban minority
             as.numeric(CData$Urban==1 & CData$White==1), # Urban white
             as.numeric(CData$Urban==0 & CData$White==1)) # Suburban white
  
  
  
  MS.Cl <- MS.POS$v          # Split sample-1
  AS.Cl <- AS.POS$v          # Auxiliary sample
  
  OR.Fit <- OR.Fit.X <- PS.Fit <- list()
  
  OR.Fit$MS <-               # Estimation of E(Y|A,X) using split-sample 1
    MySL(CData[MS.Cl,], 1, pos.AX, Ydist=gaussian(),
         SL.list=SL.hpara$SLL, MTRY=SL.hpara$MTRY, MLPL=SL.hpara$MLPL, NMN=SL.hpara$NMN,MLPdecay=SL.hpara$MLPdecay)
  OR.Fit$AS <-               # Estimation of E(Y|A,X) using split-sample 2
    MySL(CData[AS.Cl,], 1, pos.AX, Ydist=gaussian(),
         SL.list=SL.hpara$SLL, MTRY=SL.hpara$MTRY, MLPL=SL.hpara$MLPL, NMN=SL.hpara$NMN,MLPdecay=SL.hpara$MLPdecay)
  
  OR.Fit.X$MS <-               # Estimation of E(Y|X) using split-sample 1
    MySL(CData[MS.Cl,], 1, pos.X, Ydist=gaussian(),
         SL.list=SL.hpara$SLL, MTRY=SL.hpara$MTRY, MLPL=SL.hpara$MLPL, NMN=SL.hpara$NMN,MLPdecay=SL.hpara$MLPdecay)
  OR.Fit.X$AS <-               # Estimation of E(Y|X) using split-sample 2
    MySL(CData[AS.Cl,], 1, pos.X, Ydist=gaussian(),
         SL.list=SL.hpara$SLL, MTRY=SL.hpara$MTRY, MLPL=SL.hpara$MLPL, NMN=SL.hpara$NMN,MLPdecay=SL.hpara$MLPdecay)
  
  PS.Fit$MS <-               # Estimation of P(A=1|X) using split-sample 1
    MySL(CData[MS.Cl,], 2, pos.X, Ydist=binomial(),
         SL.list=SL.hpara$SLL, MTRY=SL.hpara$MTRY, MLPL=SL.hpara$MLPL, NMN=SL.hpara$NMN,MLPdecay=SL.hpara$MLPdecay)
  PS.Fit$AS <-               # Estimation of P(A=1|X) using split-sample 2 
    MySL(CData[AS.Cl,], 2, pos.X, Ydist=binomial(),
         SL.list=SL.hpara$SLL, MTRY=SL.hpara$MTRY, MLPL=SL.hpara$MLPL, NMN=SL.hpara$NMN,MLPdecay=SL.hpara$MLPdecay)
  
  
  
  OR.EST <- matrix(0,N,4) ; PS.EST <- matrix(0,N,1)
  
  TData <- CData[MS.Cl,-1]
  OR.EST[MS.Cl,3] <-         # E(Y|A,X) estimates in split-sample 2
    predict(OR.Fit$AS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[MS.Cl,-1] ; TData$CH <- 1
  OR.EST[MS.Cl,1] <-         # E(Y|A=1,X) estimates in split-sample 2
    predict(OR.Fit$AS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[MS.Cl,-1] ; TData$CH <- 0
  OR.EST[MS.Cl,2] <-         # E(Y|A=0,X) estimates in split-sample 2
    predict(OR.Fit$AS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[MS.Cl,-(1:2)]
  OR.EST[MS.Cl,4] <-         # E(Y|X) estimates in split-sample 2
    predict(OR.Fit.X$AS,newdata=TData,onlySL=TRUE)$pred
  PS.EST[MS.Cl,] <-          # P(A=1|X) estimates in split-sample 2
    predict(PS.Fit$AS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[AS.Cl,-1]
  OR.EST[AS.Cl,3] <-         # E(Y|A,X) estimates in split-sample 1 
    predict(OR.Fit$MS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[AS.Cl,-1] ; TData$CH <- 1
  OR.EST[AS.Cl,1] <-         # E(Y|A=1,X) estimates in split-sample 1
    predict(OR.Fit$MS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[AS.Cl,-1] ; TData$CH <- 0
  OR.EST[AS.Cl,2] <-         # E(Y|A=0,X) estimates in split-sample 1
    predict(OR.Fit$MS,newdata=TData,onlySL=TRUE)$pred
  
  TData <- CData[AS.Cl,-(1:2)]
  OR.EST[AS.Cl,4] <-         # E(Y|X) estimates in split-sample 1 
    predict(OR.Fit.X$MS,newdata=TData,onlySL=TRUE)$pred
  PS.EST[AS.Cl,] <-          # P(A=1|X) estimates in split-sample 1 
    predict(PS.Fit$MS,newdata=TData,onlySL=TRUE)$pred
  
  SS <- rep(0,N)
  SS[MS.Cl] <- 1
  SS[AS.Cl] <- 2
  
  NData <- cbind(SS,OR.EST,PS.EST,SCHID)
  NData <- data.frame(NData)
  colnames(NData) <- c( "SS",
                        "OR.A1.Est","OR.A0.Est","OR.Est",
                        "ORX.Est","PS.Est",
                        "SCHID")
  write.csv(NData,sprintf("NData_B%0.4d.csv",BATCH),row.names=FALSE)
  
  
}
