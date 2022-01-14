############################################
# Last update : Dec 19 2021
# Code for Section 5
############################################

############################################
# Download Dataset
# 1. Visit the URL: https://nces.ed.gov/OnlineCodebook/Session/Codebook/5189b910-34ae-40ee-b6ca-45c600bc0fee
# 2. Select stata format 
# 3. Download "NELS_88_92_STMEG_V1_0.dta" data
############################################


OData.O <- readstata13::read.dta13("NELS_88_92_STMEG_V1_0.dta")
OData <- OData.O
for(jj in 1:dim(OData)[2]){
    OData[,jj] <- as.numeric( OData[,jj] )
    OData[is.na(OData[,jj]),jj] <- 99
}
OData <- OData[order(OData$STU_ID),]

############################################
# In the analysis, we define clusters for the cluster-robust standard errors to be
# schools that the students attended in 12th;
# To cluster students based on their 12th school, 
# we use 370 12th school-level variables.
# We assign students into the same school 
# if all 370 school-level variables of theirs are identical.
############################################

F2Data <- OData.O[,c("STU_ID", "F2C5",	"F2C6",	"F2C7A",	"F2C7B",	"F2C7C",	"F2C7D1",	"F2C7D2",	"F2C7D3",	"F2C7D4",
                     "F2C7D5",	"F2C7D6",	"F2C7D7",	"F2C7D8",	"F2C7D9",	"F2C7E",	"F2C7F",	"F2C7G",	"F2C8AA",
                     "F2C8AB",	"F2C8AC",	"F2C8AD",	"F2C8AE",	"F2C8AF",	"F2C8AG",	"F2C8BA1",	"F2C8BA2",	"F2C8BA3",
                     "F2C8BA4",	"F2C8BA5",	"F2C8BA6",	"F2C8BA7",	"F2C8BA8",	"F2C8BA9",	"F2C8BB1",	"F2C8BB2",
                     "F2C8BB3",	"F2C8BB4",	"F2C8BB5",	"F2C8BB6",	"F2C8BB7",	"F2C8BB8",	"F2C8BB9",	"F2C8BC1",
                     "F2C8BC2",	"F2C8BC3",	"F2C8BC4",	"F2C8BC5",	"F2C8BC6",	"F2C8BC7",	"F2C8BC8",	"F2C8BC9",
                     "F2C9A",	"F2C9B",	"F2C9C",	"F2C9D",	"F2C9E",	"F2C9F",	"F2C9G",	"F2C9H",	"F2C10A",	"F2C10B",
                     "F2C10C",	"F2C11A",	"F2C11B",	"F2C11C",	"F2C11D",	"F2C11E",	"F2C11F",	"F2C12A",	"F2C12B",	"F2C12C",
                     "F2C12D",	"F2C12E",	"F2C12F",	"F2C13A",	"F2C13B",	"F2C13C",	"F2C13D",	"F2C13E",	"F2C13F",	"F2C13G",
                     "F2C13H",	"F2C14",	"F2C15A",	"F2C15B",	"F2C15C",	"F2C15D",	"F2C15E",	"F2C15F",	"F2C16",	"F2C17A",
                     "F2C17B",	"F2C17C",	"F2C17D",	"F2C17E",	"F2C18A",	"F2C18B",	"F2C18C",	"F2C18D",	"F2C18E",	"F2C18F",
                     "F2C18G",	"F2C18H",	"F2C18I",	"F2C19A",	"F2C19B",	"F2C19C",	"F2C20A",	"F2C20B",	"F2C20C",	"F2C20D",
                     "F2C20E",	"F2C21",	"F2C25B",	"F2C25C",	"F2C25D",	"F2C25E",	"F2C25F",	"F2C25G",	"F2C25H",	"F2C25I",	"F2C25J",	"F2C25K",
                     "F2C26",	"F2C27A",	"F2C27B",	"F2C27C",	"F2C27D",	"F2C27E",	"F2C27F",	"F2C28",	"F2C29A",	"F2C29B",
                     "F2C30",	"F2C31",	"F2C32",	"F2C33",	"F2C34A",	"F2C34B",	"F2C34C",	"F2C35A",	"F2C35B",	"F2C35C",
                     "F2C35D",	"F2C35E",	"F2C35F",	"F2C35G",	"F2C35H",	"F2C35I",	"F2C35J",	"F2C35K",	"F2C35L",	"F2C36A1",
                     "F2C36A2",	"F2C36B1",	"F2C36B2",	"F2C36C1",	"F2C36C2",	"F2C36D1",	"F2C36D2",	"F2C36E1",
                     "F2C36E2",	"F2C36F1",	"F2C36F2",	"F2C36G1",	"F2C36G2",	"F2C36H1",	"F2C36H2",	"F2C36I1",
                     "F2C36I2",	"F2C36J1",	"F2C36J2",	"F2C36K1",	"F2C36K2",	"F2C36L1",	"F2C36L2",	"F2C37L",
                     "F2C37H",	"F2C38",	"F2C39",	"F2C40A",	"F2C40B",	"F2C40C",	"F2C41A",	"F2C41B",	"F2C41C",	"F2C41D",
                     "F2C41E",	"F2C41F",	"F2C42",	"F2C43A",	"F2C43B",	"F2C43C",	"F2C43D",	"F2C43E",	"F2C43F",	"F2C44A",
                     "F2C44B",	"F2C44C",	"F2C45A",	"F2C45B",	"F2C45C",	"F2C45D",	"F2C46",	"F2C47A",	"F2C47B",	"F2C47C",
                     "F2C47D",	"F2C47E",	"F2C47F",	"F2C48A1",	"F2C48A2",	"F2C48A3",	"F2C48A4",	"F2C48A5",	"F2C48B1",
                     "F2C48B2",	"F2C48B3",	"F2C48B4",	"F2C48B5",	"F2C48C1",	"F2C48C2",	"F2C48C3",	"F2C48C4",	"F2C48C5",
                     "F2C49",	"F2C51A",	"F2C51B",	"F2C51C",	"F2C51D",	"F2C52A1",	"F2C52A2",	"F2C52A3",	"F2C52A4",	"F2C52A5",
                     "F2C52A6",	"F2C52B1",	"F2C52B2",	"F2C52B3",	"F2C52B4",	"F2C52B5",	"F2C52B6",	"F2C52C1",	"F2C52C2",
                     "F2C52C3",	"F2C52C4",	"F2C52C5",	"F2C52C6",	"F2C52D1",	"F2C52D2",	"F2C52D3",	"F2C52D4",	"F2C52D5",
                     "F2C52D6",	"F2C52E1",	"F2C52E2",	"F2C52E3",	"F2C52E4",	"F2C52E5",	"F2C52E6",	"F2C52F1",	"F2C52F2",
                     "F2C52F3",	"F2C52F4",	"F2C52F5",	"F2C52F6",	"F2C52G1",	"F2C52G2",	"F2C52G3",	"F2C52G4",	"F2C52G5",
                     "F2C52G6",	"F2C52H1",	"F2C52H2",	"F2C52H3",	"F2C52H4",	"F2C52H5",	"F2C52H6",	"F2C53A",	"F2C53B",
                     "F2C53C",	"F2C53D",	"F2C53E",	"F2C53F",	"F2C53G",	"F2C54A",	"F2C54B",	"F2C54C",	"F2C54D",	"F2C54E",	"F2C54F",
                     "F2C54G",	"F2C54H",	"F2C55",	"F2C56A",	"F2C56B",	"F2C56C",	"F2C56D",	"F2C56E",	"F2C56F",	"F2C56G",	"F2C56H",
                     "F2C56I",	"F2C56J",	"F2C56K",	"F2C56L",	"F2C56M",	"F2C57A",	"F2C57B",	"F2C57C",	"F2C57D",	"F2C57E",	"F2C57F",
                     "F2C57G",	"F2C57H",	"F2C57I",	"F2C57J",	"F2C57K",	"F2C57L",	"F2C57M",	"F2C57N",	"F2C57O",	"F2C57P",	"F2C58A",
                     "F2C58B",	"F2C58C",	"F2C58D",	"F2C58E",	"F2C58F",	"F2C58G",	"F2C58H",	"F2C58I",	"F2C58J",	"F2C58K",	"F2C58L",
                     "F2C58M",	"F2C58N",	"F2C58O",	"F2C59A",	"F2C59B",	"F2C59C",	"F2C59D",	"F2C59E",	"F2C59F",	"F2C59G",	"F2C59H",
                     "F2C59I",	"F2C60A",	"F2C60B",	"F2C60C",	"F2C60D",	"F2C60E",	"F2C60F",	"F2C60G",	"F2C60H",	"F2C61A",	"F2C61B",
                     "F2C61C",	"F2C61D",	"F2C61E",	"F2C62A",	"F2C62B",	"F2C62C",	"F2C62D",	"F2C62E",	"F2C62F",	"F2C63M",	"F2C63D",
                     "F2C63Y",	"F2CRDRQ2")]

F2Data.Numeric <- matrix(0,dim(F2Data)[1],dim(F2Data)[2])
for(gg in 1:dim(F2Data)[2]){
    F2Data.Numeric[,gg] <- as.numeric(F2Data[,gg])
    F2Data.Numeric[is.na(F2Data.Numeric[,gg]),gg] <- 99999
}

F2DataUnique <- unique( F2Data.Numeric[,-1] )
F2DataUnique <- cbind(c(NA,1:(dim(F2DataUnique)[1]-1)),
                      rbind(F2DataUnique[2,],F2DataUnique[-2,]))

OData$F2SCHID <- rep(0,dim(OData.O)[1])
for(jj in 1:dim(F2DataUnique)[1]){
    gap <- F2Data.Numeric[,-1] - matrix( F2DataUnique[jj,(-1)], dim(F2Data.Numeric)[1], dim(F2Data.Numeric)[2]-1, byrow=T )
    OData$F2SCHID[which(apply(abs(gap),1,mean)==0)] <- F2DataUnique[jj,1]
    print(jj)
}

save(OData,file="OData.RData")


############################################
# Data Cleaning
############################################

load("OData.RData")

Data <- OData[OData$G10CTRL1<=5 &             # Non-missing Treatment
                  OData$F22XMIRR<=98 &        # Non-missing Outcome
                  (OData$RACE<=5|
                       OData$F1RACE<=5|
                       OData$F2RACE1<=5) &    # Non-missing Race
                  OData$G8URBAN<=2 &          # Non-missing Location
                  OData$SEX<=2,]              # Non-missing Gender
Data$RACE <- apply(cbind(Data$RACE,
                         Data$F1RACE,Data$F2RACE1),1,min) # Race

Data$F2SCHID[is.na(Data$F2SCHID)] <- 
    Data$SCH_ID[is.na(Data$F2SCHID)]+100000   # Cluster ID

Data <- Data[order( Data$F2SCHID ),]          # Focusing on students with non-missing cluster ID

SCHID <- Data$F2SCHID                         # Cluster ID
Y <- Data$F22XMIRR                            # Outcome: 12th year math score
CH <- as.numeric(Data$G10CTRL1==2)            # Treatment: 10th school = Catholic school

Gender <- as.numeric(Data$SEX==1)             # Gender = Male
Asian <- as.numeric(Data$RACE==1)             # Race = Asian
Hispanic <- as.numeric(Data$RACE==2)          # Race = Hispanic
Black <- as.numeric(Data$RACE==3)             # Race = Black
White <- as.numeric(Data$RACE==4)             # Race = White

Urban <- as.numeric(Data$G8URBAN==1)          # 8th grade location = urban
Suburban <- as.numeric(Data$G8URBAN==2)       # 8th grade location = suburban

MotherJobPro <-                               # Mother's job = professional
    as.numeric(Data$BYS4OCC==6|Data$BYS4OCC==9|
                   Data$BYS4OCC==10|Data$BYS4OCC==11|Data$BYS4OCC==14)
FatherJobPro <-                               # Father's job = professional
    as.numeric(Data$BYS7OCC==6|Data$BYS7OCC==9|
                   Data$BYS7OCC==10|Data$BYS7OCC==11|Data$BYS7OCC==14)
NAMotherJob <- as.numeric(Data$BYS4OCC==98)   # Mother's job = missing
NAFatherJob <- as.numeric(Data$BYS7OCC==98)   # Father's job = missing

FatherEdu <- 
    as.numeric(Data$BYP30==1)*8 + as.numeric(Data$BYP30==2)*9 + as.numeric(Data$BYP30==3)*10 +
    as.numeric(Data$BYP30==4)*12 + as.numeric(Data$BYP30==5)*13 + as.numeric(Data$BYP30==6)*13 + 
    as.numeric(Data$BYP30==7)*14 + as.numeric(Data$BYP30==8)*13 + as.numeric(Data$BYP30==9)*14 + 
    as.numeric(Data$BYP30==10)*14 + as.numeric(Data$BYP30==11)*16 + as.numeric(Data$BYP30==12)*18 + as.numeric(Data$BYP30==13)*18

FatherCol <- as.numeric(FatherEdu>=16)           # Father is a college graduate
FatherHS <- as.numeric(FatherEdu>=12)-FatherCol  # Father is a high school graduate
NAFatherEdu <- as.numeric(Data$BYP30==98)        # Father's education = missing

MotherEdu <- as.numeric(Data$BYP31==1)*8 + as.numeric(Data$BYP31==2)*9 + as.numeric(Data$BYP31==3)*10 +
    as.numeric(Data$BYP31==4)*12 + as.numeric(Data$BYP31==5)*13 + as.numeric(Data$BYP31==6)*13 + 
    as.numeric(Data$BYP31==7)*14 + as.numeric(Data$BYP31==8)*13 + as.numeric(Data$BYP31==9)*14 + 
    as.numeric(Data$BYP31==10)*14 + as.numeric(Data$BYP31==11)*16 + as.numeric(Data$BYP31==12)*18 + as.numeric(Data$BYP31==13)*18

MotherCol <- as.numeric(MotherEdu>=16)           # Mother is a college graduate
MotherHS <- as.numeric(MotherEdu>=12)-MotherCol  # Mother is a high school graduate
NAMotherEdu <- as.numeric(Data$BYP31==98)        # Mother's education = missing

FamIncome <- rep(0,dim(Data)[1])
NAFamIncome1 <- as.numeric(Data$BYFAMINC>90)     # Family income = Missing in 1988
NAFamIncome2 <- as.numeric(Data$F2P74>90)        # Family income = Missing in 1992

FamIncome1 <-                                    # Family income in 1988
    as.numeric(Data$BYFAMINC==1)*0 + as.numeric(Data$BYFAMINC==2)*500 + 
    as.numeric(Data$BYFAMINC==3)*2000 + as.numeric(Data$BYFAMINC==4)*4000 + 
    as.numeric(Data$BYFAMINC==5)*6250 + as.numeric(Data$BYFAMINC==6)*8750 + 
    as.numeric(Data$BYFAMINC==7)*12500 + as.numeric(Data$BYFAMINC==8)*17500 + 
    as.numeric(Data$BYFAMINC==9)*22500 + as.numeric(Data$BYFAMINC==10)*30000 + 
    as.numeric(Data$BYFAMINC==11)*42500 + as.numeric(Data$BYFAMINC==12)*62500 + 
    as.numeric(Data$BYFAMINC==13)*87500 + as.numeric(Data$BYFAMINC==14)*150000 + as.numeric(Data$BYFAMINC==15)*300000

FamIncome2 <-                                    # Family income in 1992
    as.numeric(Data$F2P74==1)*0 + as.numeric(Data$F2P74==2)*500 + 
    as.numeric(Data$F2P74==3)*2000 + as.numeric(Data$F2P74==4)*4000 + 
    as.numeric(Data$F2P74==5)*6250 + as.numeric(Data$F2P74==6)*8750 + 
    as.numeric(Data$F2P74==7)*12500 + as.numeric(Data$F2P74==8)*17500 + 
    as.numeric(Data$F2P74==9)*22500 + as.numeric(Data$F2P74==10)*30000 + 
    as.numeric(Data$F2P74==11)*42500 + as.numeric(Data$F2P74==12)*62500 + 
    as.numeric(Data$F2P74==13)*87500 + as.numeric(Data$F2P74==14)*150000 + as.numeric(Data$F2P74==15)*300000

FamIncome <-                                    # Family income in 1988 or 1992
    as.numeric(NAFamIncome1==0&NAFamIncome2==0)*(FamIncome1+FamIncome2)/2 +
    as.numeric(NAFamIncome1==1&NAFamIncome2==0)*(FamIncome2) +
    as.numeric(NAFamIncome1==0&NAFamIncome2==1)*(FamIncome1) +
    as.numeric(NAFamIncome1==1&NAFamIncome2==1)*0

FamIncome <- (FamIncome-mean(FamIncome))/sd(FamIncome) # Standardized family income
NAFamIncome <- NAFamIncome1*NAFamIncome2               # Family income = missing

BothParent <- as.numeric(Data$BYFCOMP==1)              # Intact family

ReadingSource1 <-                                      # Has reading sources in the home
    as.numeric(Data$BYS35B==1&Data$BYS35C==1&Data$BYS35M==1)


GPA <- Data$BYGRADS                                    # 8th grade GPA
NAGPA <- as.numeric(GPA>4)                             # 8th grade GPA is missing
GPA[NAGPA==1] <- 0

Math8 <- Data$BY2XMIRR                                 # 8th grade math score
NAMath8 <- as.numeric(Math8>=99.95)                    # 8th grade math score is missing
Math8[NAMath8==1] <- 0



Attitude12 <- as.numeric(Data$BYS55A==2)    # Sent to the office more than twice due to misbehaving
Attitude22 <- as.numeric(Data$BYS55B==2)    # Sent to the office more than twice due to school work
Attitude32 <- as.numeric(Data$BYS55C==2)    # Parents were warned more than twice due to attendance
Attitude42 <- as.numeric(Data$BYS55D==2)    # Parents were warned more than twice due to grades
Attitude52 <- as.numeric(Data$BYS55E==2)    # Parents were warned more than twice due to behavior
Attitude62 <- as.numeric(Data$BYS55F==2)    # Parents were warned more than twice due to physical fights

Religion3 <- as.numeric(Data$BYP29==7)      # Religion = Catholic


NO.NA<- which(NAMotherJob+NAFatherJob+
                  NAMotherEdu+NAFatherEdu+
                  NAFamIncome+NAGPA+NAMath8==0)

CData <- data.frame(cbind(Y,CH,
                          Gender,Black,White,
                          Urban,
                          MotherJobPro,FatherJobPro,
                          MotherCol,MotherHS,
                          FatherCol,FatherHS,
                          FamIncome,
                          BothParent,
                          ReadingSource1,
                          GPA,Math8,
                          Attitude12,Attitude22,
                          Attitude32,Attitude42,
                          Attitude52,Attitude62,
                          Religion3,SCHID))[NO.NA,]

CData <- CData[,c("Y","CH",
                  "Gender","Black","White","Urban",
                  "MotherJobPro","FatherJobPro",
                  "MotherCol","MotherHS","FatherCol","FatherHS","FamIncome","BothParent",
                  "ReadingSource1","GPA","Math8",
                  "Attitude12","Attitude22",
                  "Attitude32","Attitude42",
                  "Attitude52","Attitude62",
                  "Religion3","SCHID")]

write.csv(CData,"NELS88_NoNA.csv",row.names=FALSE)
