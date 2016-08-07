library(MASS)

#Load simulated datasets, make functions available
load("toyData.R")
source("functions.R")


#Remove "RA15" from all variable names
names(data) <- substr(names(data), 6, max(nchar(names(data))))


#Make factor variables
source("make_factors.R")


#Make education variable excluding "other" category
data$UDD <- data$Q02
data$UDD[data$UDD=="Anden udd."] <- NA
data$UDD <- factor(data$UDD, ordered=T)


#Define age group variable where the levels "18-24" and "25-34"
#are collapsed and combined with the group "18-34"
data$ALDERGRP2 <- data$ALDERGRP
data$ALDERGRP2[data$ALDERGRP=="18-24" | data$ALDERGRP=="25-34"] <- "18-34" 
data$ALDERGRP2 <- factor(data$ALDERGRP2)
data$ALDERGRP2 <- factor(data$ALDERGRP2, levels(data$ALDERGRP2)[c(4,1,2,3)],
                         ordered=T)


#Define even coarser age group variable
#"young": 18-44 or "old": 45+
data$ALDERGRP3 <- NA
data$ALDERGRP3[data$ALDERGRP2=="18-34" | data$ALDERGRP2=="35-44"] <- "young"
data$ALDERGRP3[data$ALDERGRP2=="45-54" | data$ALDERGRP2=="55+"] <- "old"
data$ALDERGRP3 <- factor(data$ALDERGRP3)


#Construct BMI variable
data$BMI <- data$Q41/(data$Q40/100)^2

#Construct gender segregation variable from dataset
#genderSeg: Percentage women (0 is no women)
jobs <- levels(data$UNDERSTYPE1)
nJobs <- length(jobs)
genderSeg <- rep(NA, nJobs)
for (i in 1:nJobs) {
  genderSeg[i] <- round(mean(as.numeric(data[data$UNDERSTYPE1==jobs[i],
                                             "KOEN"])-1),4)
}
for (i in 1:nrow(data)) {
  data$SEG_KOEN[i] <- genderSeg[which(jobs==data$UNDERSTYPE1[i])]
}


#Split into full dataset and dataset with only interviewed respondents
dataFull <- data
dataAllPart <- data[data$INT_RESULTAT=="Svar",]
data <- dataAllPart[dataAllPart$MAIN_Q01 != "Ikke i arbejde", ]


#Variable: Family status
data$FAM_STATUS <- data$Q45:data$Q46
data$FAM_STATUS <- factor(data$FAM_STATUS, 
                          labels=c("Children, partner", "No children, partner",
                                   "Children, no partner", 
                                   "No children, no partner"))


#Variable: Exercise. Follows recommendations from the Danish Health
#Authority - at least 3.5 hours of exercise/week 
data$EXERCISE <- as.character(data$Q44_01)
data$EXERCISE[data$EXERCISE=="More than 4 h" | data$EXERCISE=="2-4 t"] <- "Active"
data$EXERCISE[data$EXERCISE=="Less than 2 h"] <- "Inactive"
data$EXERCISE <- factor(data$EXERCISE)


#One variable for length of employment (in years, with decimals)
#First: Edit original variables. If year=NA, month!=NA, then year=0 and 
#if month=NA, year!=NA, month=0. 
for (i in 1:nrow(data)) {
  if (is.na(data$Q07_01[i])) { #year missing
    if (!is.na(data$Q07_02[i])) { #month not missing
      data$Q07_01[i] <- 0
    }
  } else { #year not missing
    if (is.na(data$Q07_02[i])) { #month missing 
      data$Q07_02[i] <- 0
    }
  }
}
data$ANS_AAR <- (data$Q07_01*12 + data$Q07_02)/12


#Define variables that classify jobs according to the 
#two dimensions used for stratified sampling:
#type of work and degree of knowledge needed 
#NOTE: "Politi" has been classified by me
data$VIDEN_GRAD <- rep(NA, nrow(data))
data$ARB_TYPE <- rep("", nrow(data))
for (i in 1:nrow(data)) {
  val <- data$UNDERSTYPE1[i]
  if (is.na(val)) {
    data$VIDEN_GRAD[i] <- NA
    data$ARB_TYPE[i] <- NA
  } else {
    if (val %in% c("General office clerks", 
                   "Draughtspersons",
                   "Research and education")) {
      data$ARB_TYPE[i] <- "Knowledge work"
    } 
    if (val %in% c("Health care assistants",
                   "Primary school teachers",
                   "Doctors",
                   "Police officers")) {
      data$ARB_TYPE[i] <- "Client work"
    }
    if (val %in% c("Mail carriers", 
                   "Butchers",
                   "Smiths",
                   "Engineers")) {
      data$ARB_TYPE[i] <- "Production and transportation"
    }
    if (val %in% c("Sales assistants", 
                   "Financial advisors",
                   "Business managers")) {
      data$ARB_TYPE[i] <- "Customer work"
    }
    if (val %in% c("General office clerks",
                   "Health care assistants",
                   "Mail carriers", 
                   "Butchers",
                   "Sales assistants")) {
      data$VIDEN_GRAD[i] <- "Low"
    }
    if (val %in% c("Draughtspersons", 
                   "Primary school teachers",
                   "Smiths",
                   "Financial advisors",
                   "Police officers")) {
      data$VIDEN_GRAD[i] <- "Medium"
    }
    if (val %in% c("Research and education",
                   "Doctors",
                   "Engineers",
                   "Business managers")) {
      data$VIDEN_GRAD[i] <- "High"
    }
  }
}
data$ARB_TYPE <- factor(data$ARB_TYPE)
data$VIDEN_GRAD <- factor(data$VIDEN_GRAD, ordered=T)


##Construct scale variables (simple method)##

#Load scale categories
scaleCat <- t(read.table("scale_cat.txt", header=T, sep="\t",
                         as.is=T))[,2]
scaleCat <- data.frame(var=names(scaleCat), scale=scaleCat)
scaleCat$var <- substr(scaleCat$var, 6, 1000)

scaleCat2 <- scaleCat1 <- scaleCat
scaleCat2$scale <-  paste("S", dStrSep(2, scaleCat$scale), sep="")
scaleCat1$scale <-  paste("S", dStrSep(1, scaleCat$scale), sep="")
scaleCat1[scaleCat1$scale=="SNA", "scale"] <- NA
scaleCat2[scaleCat2$scale=="SNA", "scale"] <- NA

#Construct scale variables (level 2 scales and level 1 scales)
data <- scaleVarGen(data, scaleCat2, 
                    SListCreator(scaleCat2$scale, data=data, simple=T, 
                                 scaleMat=scaleCat2), missAllow=0.5)

data <- scaleVarGen(data, scaleCat1, 
                    SListCreator(scaleCat1$scale, data=data, simple=T, 
                                 scaleMat=scaleCat1), missAllow=0.5)

#data.frame with names and scale values
scaleNames2 <- data.frame(scale=c("S1.1", "S1.2", "S1.3", "S1.4", "S1.5",
                                  "S2.1", "S2.2", "S2.3", "S2.4", "S2.5",
                                  "S2.6", "S2.7", "S2.8", "S3.1", "S3.2", 
                                  "S3.3", "S3.4", "S3.5", "S3.7", 
                                  "S3.8", "S3.9", "S3.10", "S3.11", "S3.12",
                                  "S4.1", "S4.2", "S5.1", "S5.2", "S5.3", 
                                  "S5.4", "S5.5", "S5.6"),
                          names=c("Work pace", "Amount of work", 
                                  "Emotional demands", 
                                  "Demands for hiding emotions",
                                  "Demands for concentration", 
                                  "Influence on work", 
                                  "Influence on working hours",
                                  "Possibilites of evolving",
                                  "Clarity of roles",
                                  "Role conflicts",
                                  "Predictability of work",
                                  "Experienced possibilities of doing primary tasks",
                                  "Experienced degree of irrelevant tasks",
                                  "Social support from colleagues",
                                  "Relations to primary colleagues",
                                  "Trust in colleagues",
                                  "Dedication of employees",
                                  "Quality of leadership",
                                  "Relations to primary leader",
                                  "Social support from primary leader",
                                  "Justice",
                                  "Relation to the place of work",
                                  "Involvement of employees",
                                  "Recognition",
                                  "Did major changes occur in the work place?",
                                  "Involvement of employees in handling major changes",
                                  "Experienced meaning of work",
                                  "Devotion to place of work",
                                  "Enthusiasm",
                                  "Interplay between work life and private life",
                                  "Uncertainty in employment",
                                  "Overall work satisfaction"))
scaleNames1 <- data.frame(scale=c("S1", "S2", "S3", "S4", "S5"),
                          names=c("Demands at work",
                                  "Organization and contents of work",
                                  "Interpersonal relations and management",
                                  "Changes",
                                  "Perceived work situation"))




#MOD/FS-variables
#Group observations by the version of the question they 
#were asked
data$Q11_type <- rep("MISS", nrow(data))
data$Q12_type <- rep("MISS", nrow(data))
data$Q27_type <- rep("MISS", nrow(data))
data$Q28_type <- rep("MISS", nrow(data))

for (i in 1:nrow(data)) {
  if (any(!is.na(data[i, c(paste("Q11_0", 1:9, sep=""), 
                           paste("Q11_", 10:15, sep=""))]))) {
    data$Q11_type[i] <- "ORI"
  } else if (any(!is.na(data[i, c(paste("Q11_0", 1:9, "_MOD", sep=""), 
                                  paste("Q11_", 10:15, "_MOD", sep=""))]))) {
    data$Q11_type[i] <- "MOD"
  }
  if (any(!is.na(data[i, c(paste("Q12_0", 1:9, sep=""))]))) {
    data$Q12_type[i] <- "ORI"
  } else if (any(!is.na(data[i, c(paste("Q12_0", 1:9, "_MOD", sep=""))]))) {
    data$Q12_type[i] <- "MOD"
  }
  if (!is.na(data[i, "Q27_PLACERET_FOERST"])) {
    data$Q27_type[i] <- "FOERST"
  } else if (!is.na(data[i, "Q27"])) {
    data$Q27_type[i] <- "SIDST"
  }
  if (!is.na(data[i, "Q28_PLACERET_FOERST"])) {
    data$Q28_type[i] <- "FOERST"
  } else if (!is.na(data[i, "Q28"])) {
    data$Q28_type[i] <- "SIDST"
  }
}

data$MOD_TYPE <- factor("MISS", levels=c("ORI", "MOD", "MISS"))
data$FS_TYPE <- factor("MISS", levels=c("FOERST", "SIDST", "MISS"))

data$MOD_TYPE[factor(data$Q11_type=="MOD"):
                factor(data$Q12_type=="MOD") != "FALSE:FALSE"] <- "MOD"
data$MOD_TYPE[factor(data$Q11_type=="ORI"):
                factor(data$Q12_type=="ORI") != "FALSE:FALSE"] <- "ORI"


data$FS_TYPE[factor(data$Q27_type=="SIDST"):
               factor(data$Q28_type=="SIDST") != "FALSE:FALSE"] <- "SIDST"
data$FS_TYPE[factor(data$Q27_type=="FOERST"):
               factor(data$Q28_type=="FOERST") != "FALSE:FALSE"] <- "FOERST"


#Load classifications of variables. NOTE: depends on scaleNames2
source("var_classification.R")


#Create aggregated Q11 and Q12 variables
data$Q11 <- data$Q12 <- 0
for (i in 1:nrow(data)) {
  mData <- data[, MODVAR]
  pQ11 <- which(MODVAR=="Q11_01_SAMLET")
  pQ12 <- which(MODVAR=="Q12_01_SAMLET")
  for(j in 1:15) {
    obs11 <- as.character(mData[i, pQ11+j-1]) 
    data$Q11[i] <- data$Q11[i] + identical(obs11,"Never/almost never")*100 + 
      identical(obs11,"Rarely")*75 + identical(obs11,"Sometimes")*50 +
      identical(obs11, "Often")*25
  }
  n11 <- length(na.omit(mData[i, c(pQ11:(pQ11+14))]))
  if (n11 == 0) {
    data$Q11[i] <- NA 
  } else {
    data$Q11[i] <- data$Q11[i]/n11
  }
  for (k in 1:9) {
    obs12 <- as.character(mData[i, pQ12+k-1]) 
    data$Q12[i] <- data$Q12[i] + identical(obs12,"Never/almost never")*100 + 
      identical(obs12,"Rarely")*75 + identical(obs12,"Sometimes")*50 +
      identical(obs12, "Often")*25
  }
  n12 <- length(na.omit(mData[i, c(pQ12:(pQ12+8))]))
  if (n12 == 0) {
    data$Q12[i] <- NA 
  } else {
    data$Q12[i] <- data$Q12[i]/n12
  }
}


#Make depression MDI variable
data$MDI <- rep(NA, nrow(data))
mdiQ <- c("Q31_06", "Q31_07", "Q31_08", "Q31_09", "Q31_10", "Q31_11",
          "Q31_12", "Q31_13", "Q31_14", "Q31_15", "Q31_16", "Q31_17")
depDat <- as.data.frame(sapply(data[, mdiQ], as.numeric))
for (q in mdiQ) {
  depDat[, q] <- 6 - depDat[, q] #reverse scale - 6 is worst symptoms
}
depDat$q89 <- pmax(depDat[, 8], depDat[,9], na.rm=T)
depDat$q1112 <- pmax(depDat[, 11], depDat[, 12], na.rm=T)
for (q in names(depDat)) {
  depDat[, q] <- factor(depDat[, q])
}

data$MDI <- aggrVar(depDat, c(mdiQ[-c(8,9,11,12)], "q89", "q1112"),
                    scores=c(0, (100)/(6-1)*(1:(6-2)), 100),
                    missAllow=0.5)


#STRESS variable (first version)
data$STRESS <- 6 - as.numeric(data$Q33) #high values are bad


#Dataset for assesing mod effects (only web respondents), non-missing
#in FS_TYPE and MOD_TYPE
modData <- data[data$METODE=="Web" & data$MOD_TYPE!="MISS"
                & data$FS_TYPE!="MISS", ]
modData$MOD_TYPE <- factor(modData$MOD_TYPE)
modData$FS_TYPE <- factor(modData$FS_TYPE)


#Construct anData and anData2 in which only complete cases are included.
#anData contains only the variables to be used for modeling (+ the ID 
#variable LOBENR), while anData2 contains all variables
anData <- na.omit(data[, c("LOBENR", useVars2)])
anData2 <- data[data$LOBENR %in% anData$LOBENR,]

#Construct SICK_TOTAL
anData$SICK_TOTAL <- anData$Q38 + anData$Q39
anData2$SICK_TOTAL <- anData2$Q38 + anData2$Q39


#Recode observations with SICK_TOTAL>365 to 365 in SICK_TOTAL and 365-Q38 in Q39
anData[anData$SICK_TOTAL > 365, "Q39"] <- 365 - anData[anData$SICK_TOTAL > 365, c("Q38")]
anData[anData$SICK_TOTAL > 365, "SICK_TOTAL"] <- 365
anData2[anData2$SICK_TOTAL > 365, "Q39"] <- 365 - anData2[anData2$SICK_TOTAL > 365, c("Q38")]
anData2[anData2$SICK_TOTAL > 365, "SICK_TOTAL"] <- 365


#Fix scales such that all scales belonging to one dimension are 
#positively correlated
anData$S2.5 <- 100 - anData$S2.5
anData$S2.8 <- 100 - anData$S2.8
anData$S5.3 <- 100 - anData$S5.3
anData$S5.5 <- 100 - anData$S5.5


#Construct categorical gender segregation variables
anData$SEG_KOEN_CAT <- rep("bmixed", nrow(anData))
anData$SEG_KOEN_CAT[anData$SEG_KOEN < 0.4] <- "alowF"
anData$SEG_KOEN_CAT[anData$SEG_KOEN > 0.6] <- "clowM"

anData$SEG_KOEN_CAT2 <- rep("mixed", nrow(anData))
anData$SEG_KOEN_CAT2[anData2$SEG_KOEN < 0.4] <- "seg"
anData$SEG_KOEN_CAT2[anData$SEG_KOEN > 0.6] <- "seg"

anData2$SEG_KOEN_CAT <- rep("bmixed", nrow(anData))
anData2$SEG_KOEN_CAT[anData2$SEG_KOEN < 0.4] <- "alowF"
anData2$SEG_KOEN_CAT[anData2$SEG_KOEN > 0.6] <- "clowM"

anData2$SEG_KOEN_CAT2 <- rep("mixed", nrow(anData))
anData2$SEG_KOEN_CAT2[anData2$SEG_KOEN < 0.4] <- "seg"
anData2$SEG_KOEN_CAT2[anData2$SEG_KOEN > 0.6] <- "seg"

anData$BMI_CAT <- rep("normal weight", nrow(anData))
anData$BMI_CAT[anData$BMI > 30] <- "obese"
anData$BMI_CAT[anData$BMI < 18.5] <- "underweight"


#Add BMI_CAT to health variables
healthVar <- c(healthVar, "BMI_CAT")


#dummy codings and numerical variables
anData$Q36n <- as.numeric(anData$Q36)
anData$Q37_01n <- as.numeric(anData$Q37_01)
anData$Q37_02n <- as.numeric(anData$Q37_02)
anData$Q30n <- as.numeric(anData$Q30)

anData$Q42.daily <- anData$Q42=="Yes, daily"
anData$Q42.sometimes <- anData$Q42=="Yes, sometimes"
anData$Q42.former <- anData$Q42=="Former smoker"

anData$ALDERGRP2.35 <- anData$ALDERGRP2=="35-44"
anData$ALDERGRP2.45 <- anData$ALDERGRP2=="45-54"
anData$ALDERGRP2.55 <- anData$ALDERGRP2=="55+"

anData$BMI_CAT.obese <- anData$BMI_CAT=="obese"
anData$BMI_CAT.underw <- anData$BMI_CAT=="underweight"

anData$FAM_STATUS.cp <- anData$FAM_STATUS=="Children, partner"
anData$FAM_STATUS.ncp <- anData$FAM_STATUS=="No children, partner"
anData$FAM_STATUS.cnp <- anData$FAM_STATUS=="Children, no partner"

anData$Q03.temp <- anData$Q03=="Temporary position"

anData$Q08.yes <- anData$Q08=="Ja"

anData$VIDEN_GRAD.high <- anData$VIDEN_GRAD=="High"
anData$VIDEN_GRAD.medium <- anData$VIDEN_GRAD=="Medium"

anData$ARB_TYPE.client <- anData$ARB_TYPE=="Client work"
anData$ARB_TYPE.customer <- anData$ARB_TYPE=="Customer work"
anData$ARB_TYPE.knowledge <- anData$ARB_TYPE=="Knowledge work"


#Gender segregation squared
anData$SEG_KOEN2 <- anData$SEG_KOEN^2


#Transform skewed variables
bcMDI <- boxcox(lm(anData$MDI+0.5 ~ 1), plotit=F)
bcMDI.lambda <- round(bcMDI$x[which.max(bcMDI$y)], 1)

bcQ43 <- boxcox(lm(anData$Q43+0.5 ~ 1), plotit=F)
bcQ43.lambda <- round(bcQ43$x[which.max(bcQ43$y)], 1)

bcSICK_TOTAL <- boxcox(lm(anData$SICK_TOTAL+0.5 ~ 1), plotit=F)
bcSICK_TOTAL.lambda <- round(bcSICK_TOTAL$x[which.max(bcSICK_TOTAL$y)], 1)

bcQ30n <- boxcox(lm(anData$Q30n+0.5 ~ 1), plotit=F)
bcQ30n.lambda <- round(bcQ30n$x[which.max(bcQ30n$y)], 1)

bcANS_AAR <- boxcox(lm(anData$ANS_AAR+0.5 ~ 1), plotit=F)
bcANS_AAR.lambda <- round(bcANS_AAR$x[which.max(bcANS_AAR$y)], 1)

bcQ38 <- boxcox(lm(anData$Q38+0.5 ~ 1), plotit=F)
bcQ38.lambda <- round(bcQ38$x[which.max(bcQ38$y)], 1)

anData$MDI_t <- ifelse(rep(bcMDI.lambda==0, nrow(anData)), 
                       log(anData$MDI+0.5), (anData$MDI+0.5)^bcMDI.lambda)
anData$Q43_t <- ifelse(rep(bcQ43.lambda==0, nrow(anData)), 
                       log(anData$Q43+0.5), (anData$Q43+0.5)^bcQ43.lambda)
anData$SICK_TOTAL_t <- ifelse(rep(bcSICK_TOTAL.lambda==0, nrow(anData)), 
                              log(anData$SICK_TOTAL+0.5), 
                              (anData$SICK_TOTAL+0.5)^bcSICK_TOTAL.lambda)
anData$Q30n_t <- ifelse(rep(bcQ30n.lambda==0, nrow(anData)), 
                        log(anData$Q30n+0.5), (anData$Q30n+0.5)^bcQ30n.lambda)
anData$ANS_AAR_t <- ifelse(rep(bcANS_AAR.lambda==0, nrow(anData)), 
                           log(anData$ANS_AAR+0.5), (anData$ANS_AAR+0.5)^bcANS_AAR.lambda)
anData$Q38_t <- ifelse(rep(bcQ38.lambda==0, nrow(anData)), 
                       log(anData$Q38+0.5), (anData$Q38+0.5)^bcQ38.lambda)

anData2$SICK_TOTAL_t <- anData$SICK_TOTAL_t
anData2$MDI_t <- anData$MDI_t
anData2$Q43_t <- anData$Q43_t
anData2$Q30n_t <- anData$Q30n_t
anData2$ANS_AAR_t <- anData$ANS_AAR_t
anData2$Q38_t <- anData$Q38_t


#reverse stress 
anData$STRESS <- 6 - anData$STRESS
anData2$STRESS <- 6 - anData2$STRESS


#Make new anData-dataset with standardized numerical variables
#but SEG_KOEN untransformed for easier interpretation
anDataS <- anData
nAn <- ncol(anDataS)
for (i in 1:nAn) {
  thisVar <- anDataS[, i]
  if (is.numeric(thisVar)) {
    anDataS[, i] <- (thisVar-mean(thisVar))/sd(thisVar)
  }
}
anDataS$SEG_KOEN <- anData$SEG_KOEN
anDataS$SEG_KOEN2 <- anData$SEG_KOEN2


#Labels for plots
labelList <- list(Q42="Smoking",
                  ALDERGRP2="Age",
                  SICK_TOTAL="Sick days",
                  Q30n="Selfassessed health",
                  Q05="Working hours",
                  Q08="Two jobs?",
                  Q03="Type of employment",
                  FAM_STATUS="Family status",
                  ANS_AAR="Duration of employment",
                  Q36n="Ability to work",
                  VIDEN_GRAD="Type of work: Knowledge",
                  ARB_TYPE="Type of work: Objects",
                  SEG_KOEN="Gender segregation",
                  SEG_KOEN2="Gender segergation, sq.",
                  BMI_CAT="BMI obese?",
                  S1="S1.1, ..., S1.5 (5 scales)",
                  S2="S2.1, ..., S2.8 (8 scales)",
                  S3="S3.1, ..., S3.12 (11 scales)",
                  S5="S5.1, ..., S5.5 (5 scales)")


#Dichotomized SICK_TOTAL vaiable
anDataS$SICK_TOTAL.many <- anData$SICK_TOTAL > quantile(anData$SICK_TOTAL)[4]




