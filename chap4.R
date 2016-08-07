library(ggplot2)
theme_set(theme_bw() %+% theme(text = element_text(size=12)))
library(xtable)
library(reshape2)
library(gridExtra)
library(GGally)
library(psych)
source("functions.R")
source("plotting_functions.R")

#####4.2.1: Missingness due to non-response#####

#Reason for being missing
mp1 <- ggplot(dataFull, aes(x=INT_RESULTAT)) +
  geom_bar(fill="#00BFC4") +
  coord_flip() +
  scale_x_discrete(labels=c("Reponse", "No contact", "Reject", "Other missingness",
                            "Language difficulties", "No response by phone",
                            "No phone number", "Contact by phone, but no response",
                            "Unfinished web interview", "Blank questionnarie returned")) +
  xlab("") + 
  ggtitle("Response status")


pdf("missingNonresp.pdf", width=8, height=4)
mp1
dev.off()


#Logistic regression approach
missModel <- glm(INT_RESULTAT=="Svar" ~ UNDERSTYPE1*(KOEN + ALDERGRP2), 
                 dataFull, family="binomial")
drop1(missModel, test="LRT") #test of significance

  #construct prediction data.frame for plotting
predMissFrame <- data.frame(UNDERSTYPE1 = rep(levels(dataFull$UNDERSTYPE1), 8),
                            KOEN = rep(levels(dataFull$KOEN), each=56),
                            ALDERGRP2 = rep(levels(dataFull$ALDERGRP2), 
                                            each=14, 2))

ps <- predict(missModel, predMissFrame, type="response",se.fit=T)

predMissFrame <- cbind(predMissFrame, prediction = ps$fit, se=ps$se.fit)
predMissFrame$KOEN <- as.character(predMissFrame$KOEN)
predMissFrame[predMissFrame$KOEN=="K", "KOEN"] <- "Females"
predMissFrame[predMissFrame$KOEN=="M", "KOEN"] <- "Males"

mp2 <- ggplot(predMissFrame, aes(y=prediction, x=UNDERSTYPE1,
                                 col=UNDERSTYPE1)) +
  geom_point(position=position_dodge(width=2)) +
  geom_errorbar(aes(ymin=prediction-2*se, ymax=prediction+2*se), 
                position=position_dodge(width=2)) +
  facet_grid(ALDERGRP2 ~ KOEN) +
  scale_x_discrete(breaks=NULL) + 
  geom_hline(aes(yintercept=mean(dataFull$INT_RESULTAT=="Svar"))) +
  xlab("") +
  ylab("Probability of being non-missing") +
  theme(legend.position="bottom", legend.title=element_blank())

pdf("missingNRpred.pdf", width=11, height=9)
mp2
dev.off()




#####4.2.2: Partial missingness#####


#Number of variables we wish to use later on
length(useVars)

#Number of respondents with at least partial answers
nrow(data)

#Number of observations if we exclude all respondent with at least 
#one missing value
nrow(na.omit(data[, useVars]))


#Describe how many missing observations we can get rid of if 
#each variable is left out
nUseVars <- length(useVars)
nowFrame <- data.frame(Variable = useVars, n1 = rep(0, nUseVars))

for (i in 1:nUseVars) {
  nowFrame[i, "n1"] <- nrow(na.omit(data[, minusStr(useVars, 
                                                    useVars[i])]))
}

nowFrame$miss1 <- nowFrame$n1 - nrow(na.omit(data[, useVars]))

rmv1 <- nowFrame$Variable[which.max(nowFrame$miss1)]
useVars1 <- minusStr(useVars, rmv1)

nowFrame$n2 <- rep(0, nUseVars)
for (i in 1:nUseVars) {
  if (useVars[i] != rmv1) {
    nowFrame[i, "n2"] <- nrow(na.omit(data[, minusStr(useVars1, 
                                                      useVars[i])]))
  } else {
    nowFrame[i, "n2"] <- NA
  }
}

nowFrame$miss2 <- nowFrame$n2 - nrow(na.omit(data[, useVars1]))

rmv2 <- nowFrame$Variable[which.max(nowFrame$miss2)]
useVars2 <- minusStr(useVars1, rmv2)

nowFrame$n3 <- rep(0, nUseVars)
for (i in 1:nUseVars) {
  if (!(useVars[i] %in% c(as.character(rmv1), as.character(rmv2)))) {
    nowFrame[i, "n3"] <- nrow(na.omit(data[, minusStr(useVars2, 
                                                      useVars[i])]))
  } else {
    nowFrame[i, "n3"] <- NA
  }
}
nowFrame$miss3 <- nowFrame$n3 - nrow(na.omit(data[, useVars2]))

rmv3 <- nowFrame$Variable[which.max(nowFrame$miss3)]
useVars3 <- minusStr(useVars2, rmv3)

nowFrame$n4 <- rep(0, nUseVars)
for (i in 1:nUseVars) {
  if (!(useVars[i] %in% c(as.character(rmv1), as.character(rmv2),
                          as.character(rmv3)))) {
    nowFrame[i, "n4"] <- nrow(na.omit(data[, minusStr(useVars3, 
                                                      useVars[i])]))
  } else {
    nowFrame[i, "n4"] <- NA
  }
}
nowFrame$miss4 <- nowFrame$n4 - nrow(na.omit(data[, useVars3]))

nowFrame
nowFrame[nowFrame$miss1!=0,]

  #output
xtable(nowFrame[nowFrame$miss1!=0, c("Variable", "miss1", "miss2", 
                                     "miss3", "miss4")], digits=0)



#Look at respondents that are missing in any of useVars variables
#placement of questions in the questionnaire
anyMiss <- rep(F, nrow(data))
for (i in 1:nrow(data)) {
  nMiss <- sum(is.na(data[i, useVars2]))
  if (nMiss > 0) anyMiss[i] <- T
}

meltMiss <- melt(is.na(data[anyMiss, rVars[1:198]]))
meltMiss$Var1 <- factor(meltMiss$Var1)
meltMiss$Var2 <- factor(meltMiss$Var2)

pdf("missPatternMap.pdf", width=10, height=6)
ggplot(meltMiss, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_x_discrete(breaks=NULL) +
  scale_y_discrete(breaks=NULL) +
  xlab("Observation") +
  ylab("Question number") +
  scale_fill_manual("Missing?", label=c("No", "Yes"),
                    breaks=c(FALSE, TRUE),
                    values=c("grey90", "#00BFC4")) +
  coord_flip()
dev.off()





#####4.3.2: Modification effects on affeted items#####

#Variables affected by modifications:
#Q11_01 --- Q11_15 (MOD)
#Q12_01 --- Q12_09 (MOD) 
#Q27 (FOERST/SIDST)
#Q28 (FOERST/SIDST)

#Note: Q27 variable contains all obs. Q27_PLACERET_FOERST contains
#web respondents with question placed "FOERST", Q27_PLACERET_SIDST
#contains web respondents with question placed "SIDST"

#Note: Q27_type and Q28_type agree for non-missing obs.:
table(data$Q27_type, data$Q28_type, useNA="always")

#Note: Q11_type and Q12_type agree for non-missing obs.:
table(data$Q11_type, data$Q12_type, useNA="always")
  #Both kinds of modifications cannot agree - not the same 
  #number of obs.


#Create density plots stratified by modification
modp1 <- ggplot(modData, aes(x=Q11, col=MOD_TYPE, lty=FS_TYPE)) +
  geom_line(stat="density", size=1)  +
  ggtitle("Q11") + xlab("") + ylab("") +
  scale_x_continuous(breaks=seq(0, 100, 25), limits=c(0,100)) +
  scale_color_discrete(label=c("Original", "Modified")) +
  scale_linetype_discrete(label=c("First","Last"))

modp2 <- ggplot(modData, aes(x=Q12, col=MOD_TYPE, lty=FS_TYPE)) + 
  geom_line(stat="density", size=1) +
  ggtitle("Q12")+ xlab("") + ylab("") 

modp3 <- ggplot(modData, aes(x=(as.numeric(Q27)-1)*10, 
                             lty=FS_TYPE)) +
  geom_line(stat="density", size=1, adjust=1.4) + 
  ggtitle("Q27")+ xlab("") + ylab("")

modp4 <- ggplot(modData, aes(x=(as.numeric(Q28)-1)*10, 
                             lty=FS_TYPE)) +
  geom_line(stat="density", size=1) +
  ggtitle("Q28")+ xlab("") + ylab("")

gg <- grid_arrange_shared_legend(list(modp1, modp2, modp3, modp4), 
                                 left="Density")
ggsave("modDens.pdf", gg, width=8, height=8)


#Formal test of differences in distributions
modQ11 <- lm(Q11 ~ MOD_TYPE:FS_TYPE-1, modData)
anova(lm(Q11 ~ 1, modData),modQ11)

modQ11a <- lm(Q11 ~ MOD_TYPE-1, modData)
anova(lm(Q11 ~ 1, modData),modQ11a)
summary(modQ11a)

modQ12 <- lm(Q12 ~ MOD_TYPE:FS_TYPE-1, modData)
anova(lm(Q12 ~ 1, modData),modQ12)

modQ12a <- lm(Q12 ~ MOD_TYPE-1, modData)
anova(lm(Q12 ~ 1, modData),modQ12a)

modQ27 <- lm((as.numeric(Q27)-1)*10 ~ MOD_TYPE:FS_TYPE-1, modData)
anova(lm((as.numeric(Q27)-1)*10 ~ 1, modData),modQ27)

modQ27a <- lm((as.numeric(Q27)-1)*10 ~ FS_TYPE-1, modData)
anova(lm((as.numeric(Q27)-1)*10 ~ 1, modData),modQ27a)

modQ28 <- lm((as.numeric(Q28)-1)*10 ~ MOD_TYPE:FS_TYPE-1, modData)
anova(lm((as.numeric(Q28)-1)*10 ~ 1, modData),modQ28)

modQ28a <- lm((as.numeric(Q28)-1)*10 ~ FS_TYPE-1, modData)
anova(lm((as.numeric(Q28)-1)*10 ~ 1, modData),modQ28a)


#Test of significance for each item affected by modifications
nMV <- length(MODVAR)
ps <- rep(NA, nMV)
for(i in 1:nMV) {
  nullMod <- formProducer(paste("as.numeric(", MODVAR[i], ")", 
                                sep=""), "1")
  mod <- lm(update(nullMod, . ~ + MOD_TYPE), modData)
  ps[i] <- anova(lm(nullMod, modData), mod)[2,6]
}

ps <- c(ps, anova(lm(as.numeric(Q27) ~  1, modData),
                  lm(as.numeric(Q27) ~ FS_TYPE, modData))[2,6])
ps <- c(ps, anova(lm(as.numeric(Q28) ~ 1, modData),
                  lm(as.numeric(Q28) ~ FS_TYPE, modData))[2,6])

pFrame <- data.frame(item=c(MODVAR, "Q27", "Q28"), p=ps)

pmvp <- ggplot(pFrame, aes(x=item, y=p, col=p)) +
  geom_point(size=3) +
  coord_flip() +
  scale_x_discrete(limits=rev(pFrame$item),
                   labels=rev(substr(pFrame$item, 1, 6))) +
  geom_hline(yintercept=0.05, col="red", lty="dotted") +
  geom_hline(yintercept=0.1, col="red", lty="dashed") +
  theme(legend.position="none")

pdf("modps.pdf", width=9, height=5)
pmvp
dev.off()

aMODVAR <- pFrame[pFrame$p <= 0.1, "item"]


#Density plots for each scale affected by modifications
nAMV <- length(aMODVAR)
aMVFrame <- melt(modData[, c(as.character(aMODVAR), "FS_TYPE", 
                             "MOD_TYPE")],
                 id=c("FS_TYPE", "MOD_TYPE"))
aMVFrame$value[aMVFrame$value=="Aldrig/næsten aldrig"] <- 1
aMVFrame$value[aMVFrame$value=="Sjældent"] <- 2
aMVFrame$value[aMVFrame$value=="Somme tider"] <- 3
aMVFrame$value[aMVFrame$value=="Ofte"] <- 4
aMVFrame$value[aMVFrame$value=="Altid"] <- 5
aMVFrame$value[aMVFrame$variable=="Q28"] <- as.numeric(
  aMVFrame$value[aMVFrame$variable=="Q28"])
aMVFrame$value <- as.numeric(aMVFrame$value)
aMVFrame$variable <- Vectorize(gsub, "x")("_SAMLET", "", 
                                          aMVFrame$variable)

amvp <- ggplot(aMVFrame, aes(x=value, col=MOD_TYPE, 
                             fill=MOD_TYPE)) +
  geom_bar(data=aMVFrame[aMVFrame$variable!="Q28", ], 
           aes(x=value,y=..prop..), alpha=0.4, position="identity") +
  geom_bar(data=aMVFrame[aMVFrame$variable=="Q28", ], 
           aes(x=value,y=..prop.., fill=FS_TYPE, col=FS_TYPE), 
           alpha=0.4, position="identity") +
  ylab("Relative frequency") +
  xlab("") + 
  facet_wrap(~ variable, scales="free") + 
  scale_fill_discrete(labels=c("Original", "Modified"))  +
  scale_color_discrete(breaks=NULL) 

pdf("modAMVDist.pdf", width=9, height=4)
amvp
dev.off()


#####4.3.3: PCADSC#####

source("pca_functions.R")

useVars3 <- c("KOEN", "Q03.temp", "Q05", "Q08.yes",
              "Q30", "Q36", "Q37_01", "Q37_02", "Q38", "Q39", "Q42",
              "Q43", "MDI", "STRESS", 
              "FAM_STATUS.ncp", "FAM_STATUS.cp", "FAM_STATUS.cnp",
              "ARB_TYPE.client", "ARB_TYPE.customer", "ARB_TYPE.knowledge", 
              "VIDEN_GRAD", "ANS_AAR", "ALDERGRP2", "BMI_CAT.obese", 
              "SEG_KOEN", as.character(scaleNames2$scale[-c(25, 26, 32)]))

modData2 <- cbind(anData, 
                  FS_TYPE=data$FS_TYPE[data$LOBENR %in% anData$LOBENR],
                  MOD_TYPE=data$MOD_TYPE[data$LOBENR %in% anData$LOBENR])
modData2 <- modData2[modData2$LOBENR %in% modData$LOBENR,]
modData2$KOEN <- modData2$KOEN=="K"

FSpca <- pcaObjGen(modData2, useVars3, splitBy="FS_TYPE")
FSpcaPlot <- pcaPlot(FSpca, splitLabels=list(FOERST="Placed first", 
                                             SIDST="Placed last"))

MODpca <- pcaObjGen(modData2, useVars3, splitBy="MOD_TYPE")
MODpcaPlot <- pcaPlot(MODpca, splitLabels=list(MOD="Modified", ORI="Original"))

pdf("modMODpcas2.pdf", width=9, height=12)
MODpcaPlot
dev.off()

pdf("modFSpcas2.pdf", width=9, height=12)
FSpcaPlot
dev.off()


#####4.4: Marginal distributions of modelled variables#####

source("plotting_functions.R")

#Scatter plot matrix for the Work environment variables
wep1 <- myScatPlot(anData[, useVars2[22:26]], itemSensitive=T, scaleCat2, 2)
wep2 <- myScatPlot(anData[, useVars2[27:34]], itemSensitive=T, scaleCat2, 2)
wep3 <- myScatPlot(anData[, useVars2[35:45]], itemSensitive=T, scaleCat2, 2)
wep5 <- myScatPlot(anData[, useVars2[46:50]], itemSensitive=T, scaleCat2, 2)

pdf("wePlotS1.pdf", width=6, height=6)
print(wep1)
dev.off()

pdf("wePlotS2.pdf", width=6, height=6)
print(wep2)
dev.off()

pdf("wePlotS3.pdf", width=6, height=6)
print(wep3)
dev.off()

pdf("wePlotS5.pdf", width=6, height=6)
print(wep5)
dev.off()


#Cronbach's alpha 
set.seed(1234)
alphaFrame <- data.frame(scale=c("S1", "S2", "S3", "S5"),
                         alpha=rep(NA, 4), lwr=rep(NA,4),
                         upr=rep(NA,4))
i <- 1
for (s in list(S1Vars, S2Vars, S3Vars, S5Vars)) { 
  aa <- alpha(anData[, s], check.keys=T, n.iter=999)
  alphaFrame[i, "alpha"] <- aa$total$raw_alpha
  alphaFrame[i, "lwr"] <- aa$boot.ci[1]
  alphaFrame[i, "upr"] <- aa$boot.ci[3]
  i <- i + 1
}
ap <- ggplot(alphaFrame, aes(x=scale, y=alpha)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2, col="blue",
                size=1) +
  coord_flip() +
  ylab(expression(hat(alpha))) +
  xlab("dimension") +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) + 
  scale_x_discrete(labels=c("S1: Demands", "S2: Contents", 
                            "S3: Relations", "S5: Situation"))

allScales <- c(S1Vars, S2Vars, S3Vars, S5Vars)
nScales <- length(allScales)
alphaFrame2 <- data.frame(scale=factor(allScales, levels=allScales, 
                                       ordered=T),
                          dim=c(rep("S1", length(S1Vars)),
                                rep("S2", length(S2Vars)),
                                rep("S3", length(S3Vars)),
                                rep("S5", length(S5Vars))),
                          alpha=rep(NA, nScales),
                          lwr=rep(NA, nScales),
                          upr=rep(NA, nScales))
set.seed(323453)
i <- 1
for (s in allScales) {
  items <- scaleVars(s, level=2, scaleCat2)
  if (length(items) > 2) {  
    aDat <- as.data.frame(lapply(anData2[, items], as.numeric))
    aa <- alpha(aDat, n.iter=999)
    alphaFrame2[i, "alpha"] <- aa$total$raw_alpha
    alphaFrame2[i, "lwr"] <- aa$boot.ci[1]
    alphaFrame2[i, "upr"] <- aa$boot.ci[3]
  }
  i <- i + 1
}

ap2 <- ggplot(alphaFrame2, aes(x=scale, col=dim, y=alpha)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.4, size=1) +
  geom_point(size=2) +
  coord_flip() +
  ylab(expression(hat(alpha))) + 
  xlab("scale") +
  scale_colour_discrete(guide=F) +
  scale_y_continuous(breaks=seq(0.4, 1, 0.1))

pdf("alphaPlot1.pdf", width=8, height=3)
ap
dev.off()

pdf("alphaPlot2.pdf", width=8, height=5)
ap2
dev.off()

cor(as.data.frame(lapply(anData2[, scaleVars("S1.4", level=2, scaleCat2)], 
                         as.numeric)),
    use="complete.obs", method="spearman")
cor(as.data.frame(lapply(anData2[, scaleVars("S3.4", level=2, scaleCat2)], 
                         as.numeric)),
    use="complete.obs", method="spearman")


#Health variable distributions
healthVar <- c("BMI_CAT", "STRESS", "MDI", "Q30", "Q36", 
               "Q38", "Q39", "Q42",
               "Q43", "SICK_TOTAL")
healthPlots <- myDistrPlots(anData, healthVar, 
                            makeCat=c("STRESS", "BMI_CAT"),
                            adjustFunction = function(x){2})

pdf("healthDistrPlot.pdf", width=8, height=10)
marrangeGrob(healthPlots, ncol=3, nrow=4,
             top="")
dev.off()


#Occupational gender segregation and gender distribution
gdp <- ggplot(anData, aes(x=SEG_KOEN, group=KOEN, 
                          fill=KOEN, col=KOEN)) +
  geom_bar(width=0.01, position=position_stack()) +
  ylab("Observations") +
  xlab("Percentage females") +
  scale_fill_manual("Gender", breaks=c("K", "M"),
                    labels=c("Females", "Males"),
                    values=c("#00BFC4","#F8766D")) +
  scale_colour_manual(breaks=NULL,
                      values=c("#00BFC4","#F8766D")) +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits=c(0,1))

pdf("genderDistr.pdf", width=8, height=5)
gdp
dev.off()


#Distributions of structural variables
strVar <- c("Q03", "Q05", "Q08", "ANS_AAR", "FAM_STATUS", 
            "ARB_TYPE", "VIDEN_GRAD", "ALDERGRP2")

strPlots <- myDistrPlots(anData, strVar,
                         adjustFunction=function(x){mean(x)/sd(x)})

pdf("strDistrPlot.pdf", width=8, height=10)
marrangeGrob(strPlots, ncol=2, nrow=4, top="")
dev.off()


#Distributions of transformed variables
transfData <- melt(anData[, c("MDI_t", "Q43_t", "SICK_TOTAL_t", 
                              "Q30n_t", "ANS_AAR_t", "Q38_t", "LOBENR")])
transfData$lambdaVal <- rep(NA, nrow(transfData))
transfData$lambdaVal[which(transfData$variable=="MDI_t")[1]] <- paste("lambda ==", 
                                                                      bcMDI.lambda)
transfData$lambdaVal[which(transfData$variable=="Q43_t")[1]] <- paste("lambda ==", 
                                                                      bcQ43.lambda)
transfData$lambdaVal[which(transfData$variable=="SICK_TOTAL_t")[1]] <- paste("lambda ==", 
                                                                  bcSICK_TOTAL.lambda)
transfData$lambdaVal[which(transfData$variable=="Q30n_t")[1]] <- paste("lambda ==", 
                                                                       bcQ30n.lambda)
transfData$lambdaVal[which(transfData$variable=="ANS_AAR_t")[1]] <- paste("lambda ==", 
                                                                   bcANS_AAR.lambda)
transfData$lambdaVal[which(transfData$variable=="Q38_t")[1]] <- paste("lambda ==", 
                                                                      bcQ38.lambda)

tp <- ggplot(transfData, aes(x=value)) +
  geom_histogram(bins=5, fill="#00BFC4", col="white") +
  geom_text(aes(x=Inf, y=Inf, label=lambdaVal), hjust=1.1, vjust=1.5, 
            na.rm=T, parse=T) +
  xlab("") + ylab("") + 
  facet_wrap(~ variable, scales="free", nrow=3)

pdf("transfPlot.pdf", width=8, height=8)
tp
dev.off()
