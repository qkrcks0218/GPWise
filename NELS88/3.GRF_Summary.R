############################################
# Last update : Dec 19 2021
# Code for Section 5
############################################

############################################
# Load Source Files and Packages
############################################

source("../MySL.R")
source("../SSLS.R")
library(grf)
library(lme4)

############################################
# Read NF
############################################

TNF <- 100
NF <- list()
for(jj in 1:TNF){
  NF[[jj]] <- read.csv(sprintf("NData/NData_B%0.4d.csv",jj))
  print(jj)
}

CData <- read.csv("NELS88_NoNA.csv")
SCHID <- CData$SCHID

NF.Mat <- array(unlist(NF), c(dim(NF[[1]])[1], dim(NF[[1]])[2], TNF))
NF.median <- apply(NF.Mat,c(1:2),median) # 20-42
NF.median <- data.frame(NF.median)
colnames(NF.median) <- colnames(NF[[1]])

NF.Mat <- array(unlist(NF), c(dim(NF[[1]])[1], dim(NF[[1]])[2], TNF))
NF.median <- apply(NF.Mat,c(1:2),median) # 20-42
NF.median <- data.frame(NF.median)
colnames(NF.median) <- colnames(NF[[1]])

N <- dim(CData)[1]
A <- CData$CH
Y <- CData$Y
X <- CData[,-c(1,2,dim(CData)[2])]

M <- cbind(as.numeric(CData$Urban==1 & CData$White!=1), # Urban minority
           as.numeric(CData$Urban==0 & CData$White!=1), # Suburban minority
           as.numeric(CData$Urban==1 & CData$White==1), # Urban white
           as.numeric(CData$Urban==0 & CData$White==1)) # Suburban white
G <- 4


############################################
# Balance: Use GLMM
############################################

No.Adj.T <- No.Adj.T2 <- No.Adj.T3 <- matrix(0,dim(X)[2],TNF)
Adj.T <- Adj.T2 <- Adj.T3 <- matrix(0,dim(X)[2],TNF)
fff <- function(x){sample(x,1)}

randmat <- data.frame(r=1:N,CID=SCHID)
MM <- aggregate(randmat$r~randmat$CID,FUN="length")[,2]

for(jj in 1:TNF){
  No.Adj.X <- A*X/mean(A) - (1-A)*X/mean(1-A)
  Adj.X <- A*X/NF[[jj]]$PS.Est - (1-A)*X/(1-NF[[jj]]$PS.Est)
  
  for(kk in 1:dim(X)[2]){
    No.Adj.T3[kk,jj] <- summary( lmer(No.Adj.X[,kk]~(1|SCHID)) )$coefficients[3]
    Adj.T3[kk,jj] <- summary( lmer(Adj.X[,kk]~(1|SCHID)) )$coefficients[3]
  }
  print(jj)
}


T.stat <- cbind(colnames(X)[1:11]," & ",
                round(apply(No.Adj.T3,1,median),2)[1:11]," & ",
                round(apply(Adj.T3,1,median),2)[1:11]," & " ,
                colnames(X)[11+1:11]," & ",
                round(apply(No.Adj.T3,1,median),2)[11+1:11]," & ",
                round(apply(Adj.T3,1,median),2)[11+1:11]," \\\\ \\hline " )

print(data.frame(T.stat),row.names=FALSE)



# Gender  &  1.46  &  1.85  &   1.3  &   0.62  &  0.76  &  0.53  &      BothParent  &   2.06  &   3.11  &   2.15  &   1.39  &   2.01  &   1.39  \\\\ \\hline 
# Black  &  0.45  &  0.13  &  0.15  &      1  &  1.01  &  0.81  &  ReadingSource1  &   2.89  &   4.66  &   3.39  &   1.52  &   2.72  &    2.1  \\\\ \\hline 
# White  &  1.71  &  2.49  &  1.73  &   0.73  &  1.54  &  1.13  &             GPA  &   1.87  &   3.19  &   2.18  &   1.61  &   2.73  &   2.02  \\\\ \\hline 
# Urban  &  2.32  &  3.07  &  2.19  &   0.55  &  0.71  &  0.54  &           Math8  &   1.74  &   3.13  &   2.19  &   1.58  &   2.65  &    1.9  \\\\ \\hline 
# MotherJobPro  &  2.23  &  3.64  &  2.87  &   0.72  &  1.67  &   1.3  &      Attitude12  &  -1.09  &  -2.55  &  -3.14  &    0.4  &   0.11  &   0.08  \\\\ \\hline 
# FatherJobPro  &  2.11  &  3.24  &  2.38  &   1.17  &   2.3  &   1.9  &      Attitude22  &  -0.01  &  -1.87  &   -0.4  &  -0.53  &  -2.62  &  -0.16  \\\\ \\hline 
# MotherCol  &  1.69  &  3.05  &  2.16  &   0.18  &  0.97  &  0.06  &      Attitude32  &   0.12  &  -0.98  &  -1.02  &   0.33  &  -0.58  &   -0.4  \\\\ \\hline 
# MotherHS  &  0.69  &  1.79  &  1.17  &   0.45  &  1.47  &   0.9  &      Attitude42  &   -1.6  &  -1.78  &  -3.58  &  -0.75  &  -1.38  &  -1.15  \\\\ \\hline 
# FatherCol  &  1.88  &  2.91  &  2.04  &   0.91  &  1.44  &  0.78  &      Attitude52  &  -0.85  &  -2.05  &  -2.43  &  -0.11  &  -0.65  &  -0.64  \\\\ \\hline 
# FatherHS  &  0.27  &  1.13  &  0.69  &  -0.15  &  0.77  &  0.23  &      Attitude62  &  -0.71  &  -3.76  &  -1.26  &   -1.3  &  -3.99  &  -1.56  \\\\ \\hline 
# FamIncome  &  2.46  &  4.18  &  3.53  &   0.54  &  1.25  &  0.98  &       Religion3  &   5.66  &   8.48  &   6.34  &   1.43  &   2.97  &   2.19  \\\\ \\hline 

############################################
# Overlap
############################################

# png("Overlap_NoRegion.png",height=4,width=9,res=500,unit="in")

par(mar=c(3,3,1,1))
H1 <- hist( NF.median$PS.Est[A==1],xlim=c(0,0.6),xlab="",ylim=c(0,20),ylab="",prob=TRUE,
            breaks=seq(0,1,0.01),border=FALSE,col=rgb(1,0,0,0.2),main="")
title(xlab="Estimated PS",ylab="Density",line=2)
par(new=T)
H2 <- hist( NF.median$PS.Est[A==0],xlim=c(0,0.6),xlab="",ylim=c(0,20),ylab="",prob=TRUE,xaxt='n',yaxt='n',
            breaks=seq(0,1,0.01),border=FALSE,col=rgb(0,0,1,0.2),main="")
legend("topright",legend=c("A=1","A=0"),pch=c(15,15),col=c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)))

text(0.2,15,sprintf("Range of the PS under treatment (A=1)\n= [%0.4f,%0.4f]",min(NF.median$PS.Est[A==1]),max(NF.median$PS.Est[A==1])),pos=4)
text(0.2,10,sprintf("Range of the PS under control (A=0)\n= [%0.4f,%0.4f]",min(NF.median$PS.Est[A==0]),max(NF.median$PS.Est[A==0])),pos=4)

# dev.off()   


############################################
# Groupwise Effect
############################################

EFF <- EFF.CL <- matrix(0,TNF,8)
VAR <- VAR.CL <- matrix(0,TNF,4*G^2)

for(jj in 1:TNF){
  PLM.EFF1 <-                              # Groupwise effect estimates with the standard SE
    PLM(M, Y, A, NF[[jj]]$PS.Est, 
        NF[[jj]]$ORX.Est, NF[[jj]]$OR.A1.Est, NF[[jj]]$OR.A0.Est,
        target="ATE")
  PLM.EFF2 <-                              # Groupwise effect estimates with the cluster-robust SE 
    PLM(M, Y, A, NF[[jj]]$PS.Est, 
        NF[[jj]]$ORX.Est, NF[[jj]]$OR.A1.Est, NF[[jj]]$OR.A0.Est,
        target="ATE",CID=SCHID)
  
  EFF[jj,] <- t(PLM.EFF1$Est)
  EFF.CL[jj,] <- t(PLM.EFF2$Est)
  
  VAR[jj,] <- as.numeric(PLM.EFF1$Sigma)
  VAR.CL[jj,] <- as.numeric(PLM.EFF2$Sigma)
  
  print(jj)
}

RES <- (EFF - matrix(apply(EFF,2,median),TNF,2*G,byrow=T))
RES.CL <- (EFF.CL - matrix(apply(EFF.CL,2,median),TNF,2*G,byrow=T))
VAR.NEW <- VAR ; VAR.NEW.CL <- VAR.CL
MM <- matrix(0,2*G,2*G)
diag(MM) <- 1
for(gg in 1:G){
  MM[gg,gg+G] <- MM[G+gg,gg] <- 1
}

############################################
# Median adjustment for the variance
############################################

for(jj in 1:TNF){
  VAR.NEW[jj,] <- (VAR[jj,] + as.numeric(t(t(RES[jj,]))%*%t(RES[jj,])))
  VAR.NEW.CL[jj,] <- VAR.CL[jj,] + as.numeric(t(t(RES.CL[jj,]))%*%t(RES.CL[jj,]))
  print(jj)
}

NORM <- apply(VAR.NEW,1,function(x){norm(((matrix(x,2*G,2*G))))})
NORM.CL <- apply(VAR.NEW.CL,1,function(x){norm(matrix(x,2*G,2*G),type="2")})

PLM.EFF.Median <- PLM.EFF.Median.CL <- PLM.EFF1
PLM.EFF.Median$Est[,1] <- PLM.EFF.Median.CL$Est[,1] <- apply(EFF,2,median)
PLM.EFF.Median$Sigma[1:(2*G),1:(2*G)] <- matrix(VAR.NEW[which.min(abs(median(NORM)-NORM)),],2*G,2*G)
PLM.EFF.Median.CL$Sigma[1:(2*G),1:(2*G)] <- matrix(VAR.NEW.CL[which.min(abs(median(NORM.CL)-NORM.CL)),],2*G,2*G)

############################################
# Combined estimate
############################################

LC <- LinComb(M,PLM.EFF.Median.CL)

############################################
# Causal Forest
############################################

GRF <- causal_forest(X,Y,A,num.trees=100000,clusters=SCHID,tune.parameters="all")
SUMGRF <- cbind( average_treatment_effect(GRF,target.sample="all",subset=M[,1]==1),
                 average_treatment_effect(GRF,target.sample="all",subset=M[,2]==1),
                 average_treatment_effect(GRF,target.sample="all",subset=M[,3]==1),
                 average_treatment_effect(GRF,target.sample="all",subset=M[,4]==1))

############################################
# Result Summary
############################################

EST.M <- rbind( LC$EST, SUMGRF[1,] )
EST.S <- rbind( LC$Sigma, SUMGRF[2,] )

RRR <- cbind( c("$\\widehat{\\bT}_\\SSLS$","$\\widehat{\\bT}_\\EIF$",
                "$\\widehat{\\bT}_{\\widehat{W}}$","$\\widehat{\\bT}_{{\\rm GRF}}$"),
              sprintf(" & %0.3f",EST.M[,1]),sprintf(" & %0.3f",EST.S[,1]),sprintf(" & %0.3f",EST.M[,1]/EST.S[,1]),
              sprintf(" & %0.3f",EST.M[,2]),sprintf(" & %0.3f",EST.S[,2]),sprintf(" & %0.3f",EST.M[,2]/EST.S[,2]),
              sprintf(" & %0.3f",EST.M[,3]),sprintf(" & %0.3f",EST.S[,3]),sprintf(" & %0.3f",EST.M[,3]/EST.S[,3]),
              sprintf(" & %0.3f",EST.M[,4]),sprintf(" & %0.3f",EST.S[,4]),sprintf(" & %0.3f",EST.M[,4]/EST.S[,4]),
              rep("\\\\ \\hline",4))

print(data.frame(RRR),row.names=FALSE)

