library(ggplot2)
theme_set(theme_bw() %+% theme(text = element_text(size=12)))
library(lava)
library(lavaan)
library(reshape2)
library(GGally)
library(gridExtra)
library(psych)
library(splines)
library(microbenchmark)
library(semTools)
source("functions.R")
source("plotting_functions.R")


#####5.1: The univariate approach#####

#Health hypotheses

mST_1 <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[4], ""), 
                          c(allScales, strVar, "KOEN", "SEG_KOEN")), data=anData, 
             family="binomial")

mST_2 <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[3], ""), 
                          c(allScales, strVar, "KOEN", "SEG_KOEN")), data=anData, 
             family="binomial")

mST_3 <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[4], ""), 
                          c("S1", "S2", "S3", "S5", strVar, "KOEN", "SEG_KOEN")), 
             data=anData2, family="binomial")

mST_4 <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[3], ""), 
                          c("S1", "S2", "S3", "S5", strVar, "KOEN", "SEG_KOEN")), 
             data=anData2, family="binomial")

mST_5 <- glm(formProducer("SICK_TOTAL_t", c(allScales, strVar, "KOEN", "SEG_KOEN")), 
             data=anData)

mST_6 <- glm(formProducer("SICK_TOTAL_t", c("S1", "S2", "S3", "S5",strVar, "KOEN", 
                                            "SEG_KOEN")), data=anData2)

mST_1F <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[4], ""), 
                           c(allScales, strVar, "SEG_KOEN")), data=anData, 
              family="binomial", subset=KOEN=="K")

mST_2F <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[3], ""), 
                           c(allScales,  strVar, "SEG_KOEN")),
              data=anData, family="binomial", subset=KOEN=="K")

mST_3F <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[4], ""), 
                           c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="K")

mST_4F <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[3], ""), 
                           c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="K")

mST_5F <- glm(formProducer("SICK_TOTAL_t", c(allScales, strVar, "SEG_KOEN")), 
              data=anData, subset=KOEN=="K")

mST_6F <- glm(formProducer("SICK_TOTAL_t", c("S1", "S2", "S3", "S5", 
                                             strVar, "SEG_KOEN")), 
              subset=KOEN=="K", data=anData2)


mST_1M <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[4], ""), 
                           c(allScales,  strVar, "SEG_KOEN")), 
              data=anData, family="binomial", subset=KOEN=="M")

mST_2M <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[3], ""), 
                           c(allScales,  strVar, "SEG_KOEN")), 
              data=anData, family="binomial", subset=KOEN=="M")

mST_3M <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[4], ""), 
                           c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="M")

mST_4M <- glm(formProducer(paste("SICK_TOTAL>", quantile(anData$SICK_TOTAL)[3], ""), 
                           c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="M")

mST_5M <- glm(formProducer("SICK_TOTAL_t", c(allScales, strVar, "SEG_KOEN")), 
              data=anData, subset=KOEN=="M")

mST_6M <- glm(formProducer("SICK_TOTAL_t", c("S1", "S2", "S3", "S5", 
                                             strVar, "SEG_KOEN")), 
              subset=KOEN=="M", data=anData2)


m38_1 <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[4], ""),
                          c(allScales, strVar, "KOEN", "SEG_KOEN")), data=anData, 
             family="binomial")

m38_2 <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[3], ""),
                          c(allScales, strVar, "KOEN", "SEG_KOEN")), data=anData, 
             family="binomial")

m38_3 <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[4], ""),
                          c("S1", "S2", "S3", "S5", strVar, "KOEN", "SEG_KOEN")), 
             data=anData2, family="binomial")

m38_4 <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[3], ""),
                          c("S1", "S2", "S3", "S5", strVar, "KOEN", "SEG_KOEN")), 
             data=anData2, family="binomial")

m38_5 <- glm(formProducer("-Q38_t", c(allScales, strVar, "KOEN", "SEG_KOEN")), 
             data=anData)

m38_6 <- glm(formProducer("-Q38_t", c("S1", "S2", "S3", "S5", strVar, 
                                      "KOEN", "SEG_KOEN")), data=anData2)


m38_1F <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[4], ""),
                           c(allScales, strVar, "SEG_KOEN")), 
              data=anData, family="binomial", subset=KOEN=="K")

m38_2F <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[3], ""),
                           c(allScales, strVar, "SEG_KOEN")), 
              data=anData, family="binomial", subset=KOEN=="K")

m38_3F <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[4], ""),
                           c("S1", "S2", "S3", "S5", strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="K")

m38_4F <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[3], ""),
                           c("S1", "S2", "S3", "S5", strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="K")

m38_5F <- glm(formProducer("-Q38_t", c(allScales, strVar, "SEG_KOEN")), 
              data=anData, subset=KOEN=="K")

m38_6F <- glm(formProducer("-Q38_t", c("S1", "S2", "S3", "S5", 
                                       strVar, "SEG_KOEN")), 
              subset=KOEN=="K", data=anData2)


m38_1M <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[4], ""),
                           c(allScales, strVar, "SEG_KOEN")),
              data=anData, family="binomial", subset=KOEN=="M")

m38_2M <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[3], ""),
                           c(allScales, strVar, "SEG_KOEN")), 
              data=anData, family="binomial", subset=KOEN=="M")

m38_3M <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[4], ""),
                           c("S1", "S2", "S3", "S5", strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="M")

m38_4M <- glm(formProducer(paste("Q38>", quantile(anData$Q38)[3], ""),
                           c("S1", "S2", "S3", "S5", strVar, "SEG_KOEN")), 
              data=anData2, family="binomial", subset=KOEN=="M")

m38_5M <- glm(formProducer("-Q38_t", c(allScales, strVar, "SEG_KOEN")), 
              data=anData, subset=KOEN=="M")

m38_6M <- glm(formProducer("-Q38_t", c("S1", "S2", "S3", "S5", 
                                       strVar, "SEG_KOEN")), 
              subset=KOEN=="M", data=anData2)

mST_1_s <- summary(mST_1)$coef
mST_2_s <- summary(mST_2)$coef
mST_3_s <- summary(mST_3)$coef
mST_4_s <- summary(mST_4)$coef
mST_5_s <- summary(mST_5)$coef
mST_6_s <- summary(mST_6)$coef
mST_1F_s <- summary(mST_1F)$coef
mST_2F_s <- summary(mST_2F)$coef
mST_3F_s <- summary(mST_3F)$coef
mST_4F_s <- summary(mST_4F)$coef
mST_5F_s <- summary(mST_5F)$coef
mST_6F_s <- summary(mST_6F)$coef
mST_1M_s <- summary(mST_1M)$coef
mST_2M_s <- summary(mST_2M)$coef
mST_3M_s <- summary(mST_3M)$coef
mST_4M_s <- summary(mST_4M)$coef
mST_5M_s <- summary(mST_5M)$coef
mST_6M_s <- summary(mST_6M)$coef

m38_1_s <- summary(m38_1)$coef
m38_2_s <- summary(m38_2)$coef
m38_3_s <- summary(m38_3)$coef
m38_4_s <- summary(m38_4)$coef
m38_5_s <- summary(m38_5)$coef
m38_6_s <- summary(m38_6)$coef
m38_1F_s <- summary(m38_1F)$coef
m38_2F_s <- summary(m38_2F)$coef
m38_3F_s <- summary(m38_3F)$coef
m38_4F_s <- summary(m38_4F)$coef
m38_5F_s <- summary(m38_5F)$coef
m38_6F_s <- summary(m38_6F)$coef
m38_1M_s <- summary(m38_1M)$coef
m38_2M_s <- summary(m38_2M)$coef
m38_3M_s <- summary(m38_3M)$coef
m38_4M_s <- summary(m38_4M)$coef
m38_5M_s <- summary(m38_5M)$coef
m38_6M_s <- summary(m38_6M)$coef


nPars <- length(names(coef(mST_1)))
nPars2 <- length(names(coef(mST_3)))

sdaysFrame <- data.frame(model=c(rep(c("SICK_TOTAL,\n 75% quantile cutoff", 
                                       "SICK_TOTAL,\n median cutoff", 
                                       "SICK_TOTAL,\n transformed", 
                                       "Sickness absence,\n 75% quantile cutoff", 
                                       "Sickness absence,\n median cutoff",
                                       "Sickness absence,\n transformed"), each=nPars),
                                 rep(c("SICK_TOTAL,\n 75% quantile cutoff", 
                                       "SICK_TOTAL,\n median cutoff", 
                                       "SICK_TOTAL,\n transformed",
                                       "Sickness absence,\n 75% quantile cutoff", 
                                       "Sickness absence,\n median cutoff",
                                       "Sickness absence,\n transformed"), 
                                     each=(nPars-1), 2)),
                         est=c(mST_1_s[, 1], mST_2_s[, 1], mST_5_s[, 1], 
                               m38_1_s[, 1], m38_2_s[, 1], m38_5_s[, 1],
                               mST_1F_s[, 1], mST_2F_s[, 1], mST_5F_s[, 1], 
                               m38_1F_s[,1], m38_2F_s[,1], m38_5F_s[, 1],
                               mST_1M_s[, 1], mST_2M_s[, 1], mST_5M_s[, 1], 
                               m38_1M_s[,1], m38_2M_s[,1], m38_5M_s[, 1]),
                         sig = (c(mST_1_s[, 4], mST_2_s[, 4], mST_5_s[, 4], 
                                  m38_1_s[, 4], m38_2_s[, 4], m38_5_s[, 4],
                                  mST_1F_s[, 4], mST_2F_s[, 4], mST_5F_s[, 4], 
                                  m38_1F_s[, 4], m38_2F_s[, 4], m38_5F_s[, 4],
                                  mST_1M_s[, 4], mST_2M_s[, 4], mST_5M_s[, 4], 
                                  m38_1M_s[, 4], m38_2M_s[, 4], m38_5M_s[, 4]) < 0.05),
                         var = c(rep(names(coef(mST_1)), 6), 
                                 rep(names(coef(mST_1F)), 6*2)),
                         gender = c(rep("all", 6*nPars), 
                                    rep(c("F", "M"), each=6*(nPars-1))))
sdaysFrame$posest <- factor(sdaysFrame$est > 0)


sdaysFrame2 <- data.frame(model=c(rep(c("SICK_TOTAL,\n 75% quantile cutoff", 
                                        "SICK_TOTAL,\n median cutoff", 
                                        "SICK_TOTAL,\n transformed", 
                                        "Sickness absence,\n 75% quantile cutoff", 
                                        "Sickness absence,\n median cutoff",
                                        "Sickness absence,\n transformed"), each=nPars2),
                                  rep(c("SICK_TOTAL,\n 75% quantile cutoff", 
                                        "SICK_TOTAL,\n median cutoff", 
                                        "SICK_TOTAL,\n transformed", 
                                        "Sickness absence,\n 75% quantile cutoff", 
                                        "Sickness absence,\n median cutoff",
                                        "Sickness absence,\n transformed"), 
                                      each=(nPars2-1), 2)),
                          est=c(mST_3_s[, 1], mST_4_s[, 1], mST_6_s[, 1],
                                m38_3_s[, 1], m38_4_s[, 1], m38_6_s[, 1], 
                                mST_3F_s[, 1], mST_4F_s[, 1], mST_6F_s[, 1], 
                                m38_3F_s[,1], m38_4F_s[,1], m38_6F_s[, 1],
                                mST_3M_s[, 1], mST_4M_s[, 1], mST_6M_s[, 1], 
                                m38_3M_s[,1], m38_4M_s[,1], m38_6M_s[, 1]),
                          sig = (c(mST_3_s[, 4], mST_4_s[, 4], mST_6_s[, 1], 
                                   m38_3_s[, 4], m38_4_s[, 4], m38_6_s[, 4],
                                   mST_3F_s[, 4], mST_4F_s[, 4], mST_6F_s[, 1], 
                                   m38_3F_s[, 4], m38_4F_s[, 4], m38_6F_s[, 4],
                                   mST_3M_s[, 4], mST_4M_s[, 4], mST_6M_s[, 1], 
                                   m38_3M_s[, 4], m38_4M_s[, 4], m38_6M_s[, 4]) < 0.05),
                          var = c(rep(names(coef(mST_3)), 6), 
                                  rep(names(coef(mST_3F)), 6*2)),
                          gender = c(rep("all", 6*nPars2), 
                                     rep(c("F", "M"), each=6*(nPars2-1))))
sdaysFrame2$posest <- factor(sdaysFrame2$est > 0)

sp1 <- ggplot(sdaysFrame[sdaysFrame$var %in% c(allScales), ], 
              aes(x=var, y=posest:gender, col=sig, shape=gender)) +
  geom_point(size=2.5) +
  coord_flip() +
  facet_grid(~ model) +
  scale_y_discrete(breaks=c("FALSE:F", "TRUE:F"),
                   labels=c(expression(beta <= 0), expression(beta > 0)), 
                   name="") +
  scale_colour_discrete(name="Significant", labels=c("No", "Yes")) +
  scale_shape_discrete(name="Gender", labels=c("Both", "Female", "Male")) + 
  xlab("Scale") +
  ggtitle("Models with level 2 scales") +
  geom_vline(xintercept=5.5, col="darkgrey") +
  geom_vline(xintercept=13.5, col="darkgrey") +
  geom_vline(xintercept=24.5, col="darkgrey") +
  geom_hline(yintercept=3.5, linetype=2) +
  theme(text = element_text(size=12))

sp2 <- ggplot(sdaysFrame2[sdaysFrame2$var %in% c("S1", "S2", "S3", "S5"), ], 
              aes(x=var, y=posest:gender, col=sig, shape=gender)) +
  geom_point(size=2.5) +
  coord_flip() +
  facet_grid(~ model) +
  scale_y_discrete(breaks=c("FALSE:F", "TRUE:F"),
                   labels=c(expression(beta <= 0), 
                            expression(beta > 0)), name="") +
  scale_colour_discrete(name="Significant", labels=c("No", "Yes")) +
  scale_shape_discrete(name="Gender", 
                       labels=c("Both", "Female", "Male")) + 
  xlab("Scale") +
  ggtitle("Models with level 1 scales") +
  geom_hline(yintercept=3.5, linetype=2) +
  theme(text = element_text(size=12))


pdf("sickdPlot1.pdf", width=10, height=6)
sp1
dev.off()

pdf("sickdPlot2.pdf", width=10, height=3)
sp2
dev.off()

#MDI
mMDI_1 <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[4], ""), 
                           c(allScales, strVar, "KOEN", "SEG_KOEN")), 
              data=anData, family="binomial")

mMDI_2 <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[3], ""), 
                           c(allScales, strVar, "KOEN", "SEG_KOEN")), 
              data=anData, family="binomial")

mMDI_3 <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[4], ""), 
                           c("S1", "S2", "S3", "S5", strVar, "KOEN", 
                             "SEG_KOEN")), 
              data=anData2, family="binomial")

mMDI_4 <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[3], ""), 
                           c("S1", "S2", "S3", "S5", strVar, "KOEN", 
                             "SEG_KOEN")),
              data=anData2, family="binomial")

mMDI_5 <- glm(formProducer("MDI/2>20", c(allScales, strVar, "KOEN", 
                                         "SEG_KOEN")), 
              data=anData, family="binomial")

mMDI_6 <- glm(formProducer("MDI/2>20", c(strVar, "S1", "S2", "S3", 
                                         "S4", "KOEN", "SEG_KOEN")), 
              data=anData2, family="binomial")

mMDI_7 <- glm(formProducer("MDI_t", c(allScales, strVar, "KOEN", 
                                      "SEG_KOEN")), 
              data=anData)

mMDI_8 <- glm(formProducer("MDI_t", c(strVar, "S1", "S2", "S3", 
                                      "S4", "KOEN", "SEG_KOEN")), 
              data=anData2)

mMDI_1F <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[4], ""), 
                            c(allScales, strVar, "SEG_KOEN")), data=anData, 
               family="binomial", subset=KOEN=="K")

mMDI_2F <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[3], ""), 
                            c(allScales,  strVar, "SEG_KOEN")),
               data=anData, family="binomial", subset=KOEN=="K")

mMDI_3F <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[4], ""), 
                            c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
               data=anData2, family="binomial", subset=KOEN=="K")

mMDI_4F <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[3], ""), 
                            c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
               data=anData2, family="binomial", subset=KOEN=="K")

mMDI_5F <- glm(formProducer("MDI/2>20", c(allScales, strVar, "SEG_KOEN")), 
               data=anData, family="binomial", subset=KOEN=="K")

mMDI_6F <- glm(formProducer("MDI/2>20", c(strVar,"S1", "S2", "S3", 
                                          "S4", "SEG_KOEN")), 
               data=anData2, family="binomial", subset=KOEN=="K")

mMDI_7F <- glm(formProducer("MDI_t", c(allScales, strVar, "SEG_KOEN")), 
               data=anData, subset=KOEN=="K")

mMDI_8F <- glm(formProducer("MDI_t", c(strVar, "S1", "S2", "S3", 
                                       "S4", "SEG_KOEN")), 
               data=anData2, subset=KOEN=="K")

mMDI_1M <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[4], ""), 
                            c(allScales,  strVar, "SEG_KOEN")), 
               data=anData, family="binomial", subset=KOEN=="M")

mMDI_2M <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[3], ""), 
                            c(allScales,  strVar, "SEG_KOEN")), 
               data=anData, family="binomial", subset=KOEN=="M")

mMDI_3M <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[4], ""), 
                            c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
               data=anData2, family="binomial", subset=KOEN=="M")

mMDI_4M <- glm(formProducer(paste("MDI>", quantile(anData$MDI)[3], ""), 
                            c("S1", "S2", "S3", "S5",  strVar, "SEG_KOEN")), 
               data=anData2, family="binomial", subset=KOEN=="M")

mMDI_5M <- glm(formProducer("MDI/2>20", c(allScales, strVar, "SEG_KOEN")), 
               data=anData, family="binomial", subset=KOEN=="M")

mMDI_6M <- glm(formProducer("MDI/2>20", c(strVar,
                                          "S1", "S2", "S3", 
                                          "S4", "SEG_KOEN")), 
               data=anData2, family="binomial")

mMDI_7M <- glm(formProducer("MDI_t", c(allScales, strVar, "SEG_KOEN")), 
               data=anData, subset=KOEN=="M")

mMDI_8M <- glm(formProducer("MDI_t", c(strVar, "S1", "S2", "S3", 
                                       "S4", "SEG_KOEN")), 
               data=anData2, subset=KOEN=="M")

mMDI_1_s <- summary(mMDI_1)$coef
mMDI_2_s <- summary(mMDI_2)$coef
mMDI_3_s <- summary(mMDI_3)$coef
mMDI_4_s <- summary(mMDI_4)$coef
mMDI_5_s <- summary(mMDI_5)$coef
mMDI_6_s <- summary(mMDI_6)$coef
mMDI_7_s <- summary(mMDI_7)$coef
mMDI_8_s <- summary(mMDI_8)$coef
mMDI_1F_s <- summary(mMDI_1F)$coef
mMDI_2F_s <- summary(mMDI_2F)$coef
mMDI_3F_s <- summary(mMDI_3F)$coef
mMDI_4F_s <- summary(mMDI_4F)$coef
mMDI_5F_s <- summary(mMDI_5F)$coef
mMDI_6F_s <- summary(mMDI_6F)$coef
mMDI_7F_s <- summary(mMDI_7F)$coef
mMDI_8F_s <- summary(mMDI_8F)$coef
mMDI_1M_s <- summary(mMDI_1M)$coef
mMDI_2M_s <- summary(mMDI_2M)$coef
mMDI_3M_s <- summary(mMDI_3M)$coef
mMDI_4M_s <- summary(mMDI_4M)$coef
mMDI_5M_s <- summary(mMDI_5M)$coef
mMDI_6M_s <- summary(mMDI_6M)$coef
mMDI_7M_s <- summary(mMDI_7M)$coef
mMDI_8M_s <- summary(mMDI_8M)$coef

mildDepCutoff <- mean(anData$MDI/2 < 20)

mdiFrame <- data.frame(model=c(rep(c("MDI,\n 75% quantile cutoff", 
                                     "MDI,\n median cutoff", "MDI,\n transformed"), 
                                   each=nPars),
                               rep(c("MDI,\n 75% quantile cutoff", 
                                     "MDI,\n median cutoff", "MDI,\n transformed"),
                                   each=(nPars-1), 2),
                               rep("Mild depression\n or worse", nPars*3-2)),
                       est=c(mMDI_1_s[, 1], mMDI_2_s[, 1], mMDI_7_s[, 1], 
                             mMDI_1F_s[, 1], mMDI_2F_s[, 1], mMDI_7F_s[, 1], 
                             mMDI_1M_s[, 1], mMDI_2M_s[, 1], mMDI_7M_s[, 1], 
                             mMDI_5_s[, 1], mMDI_5F_s[, 1],
                             mMDI_5M_s[,1]),
                       sig = (c(mMDI_1_s[, 4], mMDI_2_s[, 4], mMDI_7_s[, 4], 
                                mMDI_1F_s[, 4], mMDI_2F_s[, 4], mMDI_7F_s[, 4], 
                                mMDI_1M_s[, 4], mMDI_2M_s[, 4], mMDI_7M_s[, 4], 
                                mMDI_5_s[, 4], mMDI_5F_s[, 4],
                                mMDI_5M_s[,4]) < 0.05),
                       var = c(rep(names(coef(mST_1)), 3), rep(names(coef(mST_1F)), 3*2),
                               names(coef(mST_1)), rep(names(coef(mST_1F)),2)),
                       gender = c(rep("all", 3*nPars), rep(c("F", "M"), each=3*(nPars-1)),
                                  rep("all", nPars), rep(c("F", "M"), each=(nPars-1))))
mdiFrame$posest <- factor(mdiFrame$est > 0)


mdiFrame2 <- data.frame(model=c(rep(c("MDI,\n 75% quantile cutoff", 
                                      "MDI,\n median cutoff", "MDI,\n transformed"), 
                                    each=nPars2),
                                rep(c("MDI,\n 75% quantile cutoff", 
                                      "MDI,\n median cutoff", "MDI,\n transformed"),
                                    each=(nPars2-1), 2),
                                rep("Mild depression\n or worse", nPars2*3-2)),
                        est=c(mMDI_3_s[, 1], mMDI_4_s[, 1],  mMDI_8_s[, 1],
                              mMDI_3F_s[, 1], mMDI_4F_s[, 1], mMDI_8F_s[, 1],
                              mMDI_3M_s[, 1], mMDI_4M_s[, 1], mMDI_8M_s[, 1],
                              mMDI_6_s[, 1], mMDI_6F_s[, 1],
                              mMDI_6M_s[,1]),
                        sig = (c(mMDI_3_s[, 4], mMDI_4_s[, 4], mMDI_8_s[, 4],
                                 mMDI_3F_s[, 4], mMDI_4F_s[, 4], mMDI_8F_s[, 4],
                                 mMDI_3M_s[, 4], mMDI_4M_s[, 4], mMDI_8M_s[, 4],
                                 mMDI_6_s[, 4], mMDI_6F_s[, 4],
                                 mMDI_6M_s[,4]) < 0.05),
                        var = c(rep(names(coef(mST_3)), 3), 
                                rep(names(coef(mST_3F)), 3*2),
                                names(coef(mST_3)), rep(names(coef(mST_3F)),2)),
                        gender = c(rep("all", 3*nPars2), 
                                   rep(c("F", "M"), each=3*(nPars2-1)),
                                   rep("all", nPars2), 
                                   rep(c("F", "M"), each=(nPars2-1))))
mdiFrame2$posest <- factor(mdiFrame2$est > 0)

mp1 <- ggplot(mdiFrame[mdiFrame$var %in% allScales, ], 
              aes(x=var, y=posest:gender, col=sig, shape=gender)) +
  geom_point(size=2.5) +
  coord_flip() +
  facet_grid(~ model) +
  scale_y_discrete(breaks=c("FALSE:F", "TRUE:F"),
                   labels=c(expression(beta <= 0), 
                            expression(beta > 0)), name="") +
  scale_colour_discrete(name="Significant", labels=c("No", "Yes")) +
  scale_shape_discrete(name="Gender", 
                       labels=c("Both", "Female", "Male")) + 
  xlab("Scale") +
  ggtitle("Models with level 2 scales") +
  geom_vline(xintercept=5.5, col="darkgrey") +
  geom_vline(xintercept=13.5, col="darkgrey") +
  geom_vline(xintercept=24.5, col="darkgrey") +
  geom_hline(yintercept=3.5, linetype=2) 


mp2 <- ggplot(mdiFrame2[mdiFrame2$var %in% c("S1", "S2", "S3", "S5"), ], 
              aes(x=var, y=posest:gender, col=sig, shape=gender)) +
  geom_point(size=2.5) +
  coord_flip() +
  facet_grid(~ model) +
  scale_y_discrete(breaks=c("FALSE:F", "TRUE:F"),
                   labels=c(expression(beta <= 0), 
                            expression(beta > 0)), name="") +
  scale_colour_discrete(name="Significant", labels=c("No", "Yes")) +
  scale_shape_discrete(name="Gender", 
                       labels=c("Both", "Female", "Male")) + 
  xlab("Scale") +
  ggtitle("Models with level 1 scales") +
  geom_hline(yintercept=3.5, linetype=2)

pdf("mdiPlot1.pdf", width=10, height=6)
mp1
dev.off()

pdf("mdiPlot2.pdf", width=10, height=3)
mp2
dev.off()


#STRESS
mSTRESS_1 <- glm(formProducer("STRESS>3",
                              c(allScales, strVar, "KOEN", "SEG_KOEN")), 
                 data=anData, family="binomial")

mSTRESS_2 <- glm(formProducer("STRESS", 
                              c(allScales, strVar, "KOEN", "SEG_KOEN")), 
                 data=anData)

mSTRESS_3 <- glm(formProducer("STRESS>3", 
                              c("S1", "S2", "S3", "S5", 
                                strVar, "KOEN", "SEG_KOEN")), 
                 data=anData2, family="binomial")

mSTRESS_4 <- glm(formProducer("STRESS", 
                              c("S1", "S2", "S3", "S5", strVar, 
                                "KOEN", "SEG_KOEN")), 
                 data=anData2)

mSTRESS_1F <- glm(formProducer("STRESS>3", 
                               c(allScales, strVar, "SEG_KOEN")), 
                  data=anData, 
                  family="binomial", subset=KOEN=="K")

mSTRESS_2F <- glm(formProducer("STRESS", 
                               c(allScales,  strVar, "SEG_KOEN")),
                  data=anData, subset=KOEN=="K")

mSTRESS_3F <- glm(formProducer("STRESS>3", 
                               c("S1", "S2", "S3", "S5", 
                                 strVar, "SEG_KOEN")), 
                  data=anData2, family="binomial", subset=KOEN=="K")

mSTRESS_4F <- glm(formProducer("STRESS", 
                               c("S1", "S2", "S3", "S5", 
                                 strVar, "SEG_KOEN")), 
                  data=anData2, subset=KOEN=="K")

mSTRESS_1M <- glm(formProducer("STRESS>3", 
                               c(allScales,  strVar, "SEG_KOEN")), 
                  data=anData, family="binomial", subset=KOEN=="M")

mSTRESS_2M <- glm(formProducer("STRESS", 
                               c(allScales,  strVar, "SEG_KOEN")), 
                  data=anData, subset=KOEN=="M")

mSTRESS_3M <- glm(formProducer("STRESS>3", 
                               c("S1", "S2", "S3", "S5",  
                                 strVar, "SEG_KOEN")), 
                  data=anData2, family="binomial", subset=KOEN=="M")

mSTRESS_4M <- glm(formProducer("STRESS", 
                               c("S1", "S2", "S3", "S5",  
                                 strVar, "SEG_KOEN")), 
                  data=anData2, subset=KOEN=="M")

mSTRESS_1_s <- summary(mSTRESS_1)$coef
mSTRESS_2_s <- summary(mSTRESS_2)$coef
mSTRESS_3_s <- summary(mSTRESS_3)$coef
mSTRESS_4_s <- summary(mSTRESS_4)$coef
mSTRESS_1F_s <- summary(mSTRESS_1F)$coef
mSTRESS_2F_s <- summary(mSTRESS_2F)$coef
mSTRESS_3F_s <- summary(mSTRESS_3F)$coef
mSTRESS_4F_s <- summary(mSTRESS_4F)$coef
mSTRESS_1M_s <- summary(mSTRESS_1M)$coef
mSTRESS_2M_s <- summary(mSTRESS_2M)$coef
mSTRESS_3M_s <- summary(mSTRESS_3M)$coef
mSTRESS_4M_s <- summary(mSTRESS_4M)$coef

stressFrame <- data.frame(model=c(rep(c("Is stress high?", 
                                        "Stress, continuous"), each=nPars),
                                  rep(c("Is stress high?", 
                                        "Stress, continuous"),
                                      each=(nPars-1), 2)),
                          est=c(mSTRESS_1_s[, 1], mSTRESS_2_s[, 1], 
                                mSTRESS_1F_s[, 1], mSTRESS_2F_s[, 1], 
                                mSTRESS_1M_s[, 1], mSTRESS_2M_s[, 1]),
                          sig = (c(mSTRESS_1_s[, 4], mSTRESS_2_s[, 4], 
                                   mSTRESS_1F_s[, 4], mSTRESS_2F_s[, 4], 
                                   mSTRESS_1M_s[, 4], mSTRESS_2M_s[, 4]) < 0.05),
                          var = c(rep(names(coef(mST_1)), 2), 
                                  rep(names(coef(mST_1F)), 2*2)),
                          gender = c(rep("all", 2*nPars), 
                                     rep(c("F", "M"), each=2*(nPars-1))))
stressFrame$posest <- factor(stressFrame$est > 0)

stressFrame2 <- data.frame(model=c(rep(c("Is stress high?", 
                                         "Stress, continuous"), each=nPars2),
                                   rep(c("Is stress high?", 
                                         "Stress, continuous"),
                                       each=(nPars2-1), 2)),
                           est=c(mSTRESS_3_s[, 1], mSTRESS_4_s[, 1], 
                                 mSTRESS_3F_s[, 1], mSTRESS_4F_s[, 1], 
                                 mSTRESS_3M_s[, 1], mSTRESS_4M_s[, 1]),
                           sig = (c(mSTRESS_3_s[, 4], mSTRESS_4_s[, 4], 
                                    mSTRESS_3F_s[, 4], mSTRESS_4F_s[, 4], 
                                    mSTRESS_3M_s[, 4], mSTRESS_4M_s[, 4]) < 0.05),
                           var = c(rep(names(coef(mST_3)), 2), 
                                   rep(names(coef(mST_3F)), 2*2)),
                           gender = c(rep("all", 2*nPars2), 
                                      rep(c("F", "M"), each=2*(nPars2-1))))
stressFrame2$posest <- factor(stressFrame2$est > 0)
stressFrame2$sig <- factor(stressFrame2$sig)

stressp1 <- ggplot(stressFrame[stressFrame$var %in% allScales, ], 
                   aes(x=var, y=posest:gender, col=sig, shape=gender)) +
  geom_point(size=2.5) +
  coord_flip() +
  facet_grid(~ model) +
  scale_y_discrete(breaks=c("FALSE:F", "TRUE:F"),
                   labels=c(expression(beta <= 0), 
                            expression(beta > 0)), name="") +
  scale_colour_discrete(name="Significant", labels=c("No", "Yes")) +
  scale_shape_discrete(name="Gender", 
                       labels=c("Both", "Female", "Male")) + 
  xlab("Scale") +
  ggtitle("Models with level 2 scales") +
  geom_vline(xintercept=5.5, col="darkgrey") +
  geom_vline(xintercept=13.5, col="darkgrey") +
  geom_vline(xintercept=24.5, col="darkgrey") +
  geom_hline(yintercept=3.5, linetype=2)

stressp2 <- ggplot(stressFrame2[stressFrame2$var %in% c("S1", "S2", 
                                                        "S3", "S5"), ], 
                   aes(x=var, y=posest:gender, col=sig, shape=gender)) +
  geom_point(size=2.5) +
  coord_flip() +
  facet_grid(~ model) +
  scale_y_discrete(breaks=c("FALSE:F", "TRUE:F"),
                   labels=c(expression(beta <= 0), 
                            expression(beta > 0)), name="") +
  scale_color_discrete(name="Significant", 
                       labels=c("No", "Yes"), drop=F) +
  scale_shape_discrete(name="Gender", 
                       labels=c("Both", "Female", "Male")) + 
  xlab("Scale") +
  ggtitle("Models with level 1 scales") +
  geom_hline(yintercept=3.5, linetype=2)

pdf("stressPlot1.pdf", width=10, height=6)
stressp1
dev.off()

pdf("stressPlot2.pdf", width=10, height=3)
stressp2
dev.off()


#Gender segregation and health
mGS1 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""), 
                         c(strVar, "KOEN:SEG_KOEN", 
                           "KOEN", "SEG_KOEN",
                           "I(SEG_KOEN^2)", "I(SEG_KOEN^2):KOEN")), 
            data=anData, family="binomial")

mGS2 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""), 
                         c(strVar, "S1", "S2", "S3", "S5", 
                           "KOEN:SEG_KOEN","KOEN", "SEG_KOEN",
                           "I(SEG_KOEN^2)", "I(SEG_KOEN^2):KOEN")), 
            data=anData2, family="binomial")

mGS3 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""), 
                         c(strVar, "KOEN:SEG_KOEN","KOEN", "SEG_KOEN")), 
            data=anData, family="binomial")

mGS4 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""), 
                         c(strVar, "S1", "S2", "S3", "S5", 
                           "KOEN:SEG_KOEN", "KOEN", "SEG_KOEN")), 
            data=anData2, family="binomial")

mGSspl1 <- glm(formProducer(paste("SICK_TOTAL>", 
                                  quantile(anData$SICK_TOTAL)[3], ""), 
                            c(strVar, "KOEN*ns(SEG_KOEN, df=3)")), 
               data=anData, family="binomial")

mGSspl2 <- glm(formProducer(paste("SICK_TOTAL>", 
                                  quantile(anData$SICK_TOTAL)[3], ""), 
                            c(strVar, 
                              "KOEN*ns(SEG_KOEN, df=3)",
                              "S1", "S2", "S3", "S5")), 
               data=anData2, family="binomial")


mGS5 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""),
                         c(strVar, "SEG_KOEN_CAT", 
                           "SEG_KOEN_CAT:KOEN", "KOEN")),
            data=anData, family="binomial")


mGS6 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""),
                         c(strVar, "SEG_KOEN_CAT", 
                           "SEG_KOEN_CAT:KOEN", "KOEN", 
                           "S1", "S2", "S3", "S5")),
            data=anData2, family="binomial")

mGS7 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""),
                         c(strVar, "KOEN", "SEG_KOEN_CAT2", 
                           "SEG_KOEN_CAT2:KOEN")),
            data=anData, family="binomial")


mGS8 <- glm(formProducer(paste("SICK_TOTAL>", 
                               quantile(anData$SICK_TOTAL)[3], ""),
                         c(strVar, "KOEN", "SEG_KOEN_CAT2", 
                           "SEG_KOEN_CAT2:KOEN", 
                           "S1", "S2", "S3", "S5")),
            data=anData2, family="binomial")


GSpredFrame1 <- genPredFrame(anData, strVar, c("KOEN", "SEG_KOEN"), 
                             list(c("M", "K"), seq(0,1,0.01)))
GSpred1 <- predict(mGS1, new=GSpredFrame1, type="response", se.fit=T)
GSpredFrame1 <- cbind(GSpredFrame1, pred=GSpred1$fit, 
                      se=GSpred1$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN")]

GSpredFrame2 <- genPredFrame(anData2, c(strVar, "S1", "S2", "S3", "S5"),
                             c("KOEN", "SEG_KOEN"), 
                             list(c("M", "K"), seq(0,1,0.01)))
GSpred2 <- predict(mGS2, new=GSpredFrame2, type="response", se.fit=T)
GSpredFrame2 <- cbind(GSpredFrame2, pred=GSpred2$fit, 
                      se=GSpred2$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN")]

GSpredFrame3 <- genPredFrame(anData, strVar, c("KOEN", "SEG_KOEN"), 
                             list(c("M", "K"), seq(0,1,0.01)))
GSpred3 <- predict(mGS3, new=GSpredFrame3, type="response", se.fit=T)
GSpredFrame3 <- cbind(GSpredFrame3, pred=GSpred3$fit, 
                      se=GSpred3$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN")]

GSpredFrame4 <- genPredFrame(anData2, c(strVar, "S1", "S2", "S3", "S5"),
                             c("KOEN", "SEG_KOEN"), 
                             list(c("M", "K"), seq(0,1,0.01)))
GSpred4 <- predict(mGS4, new=GSpredFrame4, type="response", se.fit=T)
GSpredFrame4 <- cbind(GSpredFrame4, pred=GSpred4$fit, 
                      se=GSpred4$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN")]

GSpredFrameSpl1 <- genPredFrame(anData, strVar, c("KOEN", "SEG_KOEN"), 
                                list(c("M", "K"), seq(0,1,0.01)))
GSpredSpl1 <- predict(mGSspl1, new=GSpredFrameSpl1, 
                      type="response", se.fit=T)
GSpredFrameSpl1 <- cbind(GSpredFrameSpl1, pred=GSpredSpl1$fit, 
                         se=GSpredSpl1$se.fit)[, c("pred", "se", 
                                                   "KOEN", "SEG_KOEN")]

GSpredFrameSpl2 <- genPredFrame(anData2, c(strVar, "S1", "S2", "S3", "S5"),
                                c("KOEN", "SEG_KOEN"), 
                                list(c("M", "K"), seq(0,1,0.01)))
GSpredSpl2 <- predict(mGSspl2, new=GSpredFrameSpl2, 
                      type="response", se.fit=T)
GSpredFrameSpl2 <- cbind(GSpredFrameSpl2, pred=GSpredSpl2$fit, 
                         se=GSpredSpl2$se.fit)[, c("pred", "se", 
                                                   "KOEN", "SEG_KOEN")]

GSpredFrame5 <- genPredFrame(anData, strVar,
                             c("KOEN", "SEG_KOEN_CAT"), 
                             list(c("M", "K"), 
                                  c("bmixed", "alowF", "clowM")))
GSpred5 <- predict(mGS5, new=GSpredFrame5, type="response", se.fit=T)
GSpredFrame5 <- cbind(GSpredFrame5, pred=GSpred5$fit, 
                      se=GSpred5$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN_CAT")]

GSpredFrame6 <- genPredFrame(anData2, c(strVar, "S1", "S2", "S3", "S5"), 
                             c("KOEN", "SEG_KOEN_CAT"), 
                             list(c("M", "K"), 
                                  c("bmixed", "alowF", "clowM")))
GSpred6 <- predict(mGS6, new=GSpredFrame6, type="response", se.fit=T)
GSpredFrame6 <- cbind(GSpredFrame6, pred=GSpred6$fit, 
                      se=GSpred6$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN_CAT")]

GSpredFrame7 <- genPredFrame(anData, strVar, 
                             c("KOEN", "SEG_KOEN_CAT2"), 
                             list(c("M", "K"), c("seg", "mixed")))
GSpred7 <- predict(mGS7, new=GSpredFrame7, type="response", se.fit=T)
GSpredFrame7 <- cbind(GSpredFrame7, pred=GSpred7$fit, 
                      se=GSpred7$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN_CAT2")]

GSpredFrame8 <- genPredFrame(anData2, c(strVar, "S1", "S2", "S3", "S5"), 
                             c("KOEN", "SEG_KOEN_CAT2"), 
                             list(c("M", "K"), c("seg", "mixed")))
GSpred8 <- predict(mGS8, new=GSpredFrame8, type="response", se.fit=T)
GSpredFrame8 <- cbind(GSpredFrame8, pred=GSpred8$fit, 
                      se=GSpred8$se.fit)[, c("pred", "se", 
                                             "KOEN", "SEG_KOEN_CAT2")]

nGS <- as.numeric(lapply(list(GSpredFrame1, GSpredFrame2, 
                              GSpredFrame3, GSpredFrame4,
                              GSpredFrameSpl1, GSpredFrameSpl2,
                              GSpredFrame5, GSpredFrame6,
                              GSpredFrame7, GSpredFrame8),
                         nrow))

GSFrame1234spl12 <- cbind(rbind(GSpredFrame1, GSpredFrame2, 
                                GSpredFrame3, GSpredFrame4, 
                                GSpredFrameSpl1, GSpredFrameSpl2), 
                          model=c(rep("1", nGS[1]), rep("2", nGS[2]), 
                                  rep("3", nGS[3]), rep("4", nGS[4]),
                                  rep("spl1", nGS[5]), 
                                  rep("spl2", nGS[6])))

GSFrame56 <- cbind(rbind(GSpredFrame5, GSpredFrame6), 
                   model=c(rep("5", nGS[7]), rep("6", nGS[8])))

GSFrame78 <- cbind(rbind(GSpredFrame7, GSpredFrame8), 
                   model=c(rep("7", nGS[9]), rep("8", nGS[10])))


segp1 <- ggplot(GSFrame1234spl12, 
                aes(x=SEG_KOEN, y=pred, col=KOEN,
                    linetype=model)) +
  geom_vline(xintercept=min(anData$SEG_KOEN), col="darkgrey") +
  geom_vline(xintercept=max(anData$SEG_KOEN), col="darkgrey") +
  geom_line(size=1) +
  xlab("Percentage females") +
  ylab("Probability of many sick days") +
  scale_color_discrete(name="Gender", label=c("Female", "Male")) +
  scale_linetype_discrete(name="Model",
                          label=c("Quadratic, \n only structural variables",
                                  "Quadratic, \n all variables",
                                  "Linear, \n only structural variables",
                                  "Linear, \n all variables",
                                  "Cubic splines, \n only structural variables",
                                  "Cubic splines, \n all variables")) +
  ggtitle("Gender segregation as a covariate") +
  theme(legend.key = element_rect(size = 5, color=NA),
        legend.key.size = unit(1.5, 'lines'))

segp2 <- ggplot(GSFrame56, aes(x=SEG_KOEN_CAT, y=pred, col=KOEN,
                               shape=model,
                               group=factor(KOEN):factor(model))) +
  geom_point(size=4) +
  geom_line(size=1) +
  xlab("Gender segregation") +
  ylab("Probability of many sick days") +
  scale_color_discrete(name="Gender", label=c("Female", "Male")) +
  scale_shape_discrete(name="Model", label=c("Only structural variables",
                                             "All variables")) +
  ggtitle("Three-level categorical gender segregation") +
  scale_x_discrete(labels=c("<40 % females", 
                            "40-60 % females", ">60 % females")) +
  theme(legend.key = element_rect(size = 5, color=NA),
        legend.key.size = unit(1.5, 'lines'))

segp3 <- ggplot(GSFrame78, aes(x=SEG_KOEN_CAT2, y=pred, col=KOEN,
                               shape=model, 
                               group=factor(KOEN):factor(model))) +
  geom_point(size=4) +
  geom_line(size=1) +
  xlab("Gender segregation") +
  ylab("Probability of many sick days") +
  scale_color_discrete(name="Gender", label=c("Female", "Male")) +
  scale_shape_discrete(name="Model", label=c("Only structural variables",
                                             "All variables")) +
  ggtitle("Two-level categorical gender segregation") +
  scale_x_discrete(labels=c("Mixed", "Segregated")) +
  theme(legend.key = element_rect(size = 5, color=NA),
        legend.key.size = unit(1.5, 'lines'))


pdf("segPlot1.pdf", width=8, height=4)
segp1
dev.off()

pdf("segPlot2.pdf", width=8, height=4)
segp2
dev.off()

pdf("segPlot3.pdf", width=8, height=4)
segp3
dev.off()


#AICs
aicGS <- as.numeric(lapply(list(mGS1, mGS2, mGS3, mGS4, mGSspl1, 
                                mGSspl2, mGS5, mGS6, mGS7,
                                mGS8), AIC))
aicGSFrame <- data.frame(AIC = aicGS,
                         model = rep(c("Quadratic", "Linear",
                                       "Splines", "Three categories",
                                       "Two categories"), each=2),
                         expl = rep(c("Only structural variables",
                                      "All variables"), 5))

gsaicp <- ggplot(aicGSFrame, aes(x=model, y=AIC, col=expl)) +
  geom_point(size=2) +
  coord_flip() +
  geom_abline(aes(intercept=AIC, slope=0, col=expl), alpha=0.3) +
  scale_color_discrete(name="Explanatory variables") +
  xlab("Gender segregation version")

pdf("segAICplot.pdf", width=8, height=3)
gsaicp
dev.off()


#Gender segregation and work environment
mGSWE1 <- lm(formProducer("S1", c(strVar,
                                  "KOEN*ns(SEG_KOEN, df=3)")),
             anData2)
mGSWE2 <- lm(formProducer("S2", c(strVar, 
                                  "KOEN*ns(SEG_KOEN, df=3)")),
             anData2)
mGSWE3 <- lm(formProducer("S3", c(strVar, 
                                  "KOEN*ns(SEG_KOEN, df=3)")),
             anData2)
mGSWE5 <- lm(formProducer("S5", c(strVar, 
                                  "KOEN*ns(SEG_KOEN, df=3)")),
             anData2)

mGSWEpredFrame <- genPredFrame(anData2, strVar, 
                               c("KOEN", "SEG_KOEN"), 
                               list(c("M", "K"), seq(0,1,0.1)))
mGSWEframe <- rbind(mGSWEpredFrame, mGSWEpredFrame,
                    mGSWEpredFrame, mGSWEpredFrame)
mGSWEframe <- cbind(mGSWEframe, scale=rep(c("S1", "S2", "S3", "S5"),
                                          each=nrow(mGSWEpredFrame)),
                    pred=c(predict(mGSWE1, new=mGSWEpredFrame),
                           predict(mGSWE2, new=mGSWEpredFrame),
                           predict(mGSWE3, new=mGSWEpredFrame),
                           predict(mGSWE5, new=mGSWEpredFrame)))

GSWEp <- ggplot(mGSWEframe, aes(x=SEG_KOEN, y=pred, col=KOEN,
                                linetype=scale)) +
  geom_vline(xintercept = min(anData$SEG_KOEN), col="darkgrey") +
  geom_vline(xintercept = max(anData$SEG_KOEN), col="darkgrey") +
  geom_line(size=1) +
  xlab("Percentage females") +
  ylab("Score") +
  scale_color_discrete(name="Gender", labels=c("Female", "Male")) +
  scale_linetype_discrete(name="Scale") +
  ggtitle("Work environment and gender segregation") 



pdf("GSWEPlot.pdf", width=6, height=4)
GSWEp
dev.off()




######5.2: Structural equation models######

SB <- "Satorra.Bentler"

#Original full sem
mF0 <- lvm()
regression(mF0) <- c(DEMANDS, CONTENTS, RELATIONS, SITUATION) ~ VIDEN_GRAD.high + 
  VIDEN_GRAD.medium + ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
regression(mF0) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
regression(mF0) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, 
                     S2.6, S2.7, S2.8) ~ CONTENTS
regression(mF0) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, 
                     S3.9, S3.10, S3.11, S3.12) ~ RELATIONS
regression(mF0) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
latent(mF0) <- ~ DEMANDS + CONTENTS + RELATIONS + SITUATION
regression(mF0) <- c(SICK_TOTAL_t, Q36n, Q30n_t) ~ HEALTH
regression(mF0) <- STRESS ~ Q05 + Q08.yes + Q03.temp
regression(mF0) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + BMI_CAT.obese +
  ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
  FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
regression(mF0) <- c(MDI_t, STRESS) ~ HEALTH
regression(mF0) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
regression(mF0) <- STRESS ~ ANS_AAR_t
regression(mF0) <- HEALTH ~ Q43_t
latent(mF0) <- ~ HEALTH
regression(mF0) <- c(HEALTH, DEMANDS, CONTENTS, SITUATION, 
                     RELATIONS) ~ SEG_KOEN + SEG_KOEN2
regression(mF0) <- HEALTH ~ DEMANDS + CONTENTS + SITUATION + RELATIONS

mF0.l <- lavaToLavaan(mF0)
mF0.l.fit <- lavaan(mF0.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mF0.l.fit)


#Original WE and HEALTH-models (no effect of GS)
mWE <- lvm()
regression(mWE) <- c(DEMANDS, CONTENTS, RELATIONS, 
                     SITUATION) ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
  ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
regression(mWE) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
regression(mWE) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, S2.8) ~ CONTENTS
regression(mWE) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, 
                     S3.10, S3.11, S3.12) ~ RELATIONS
regression(mWE) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
latent(mWE) <- ~ DEMANDS + CONTENTS + RELATIONS + SITUATION

mH <- lvm()
regression(mH) <- c(SICK_TOTAL_t, Q36n, Q30n_t) ~ HEALTH
regression(mH) <- STRESS ~ Q05 + Q08.yes + Q03.temp
regression(mH) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former +
    BMI_CAT.obese + ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
  FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
regression(mH) <- c(MDI_t, STRESS) ~ HEALTH
regression(mH) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
regression(mH) <- STRESS ~ ANS_AAR_t
regression(mH) <- HEALTH ~ Q43_t
latent(mH) <- ~ HEALTH

mWE.l <- lavaToLavaan(mWE)
mWE.l.fit <- lavaan(mWE.l, anDataS, fixed.x=T, auto.fix.first=T, test="SB")
rmsea(mWE.l.fit)

mH.l <- lavaToLavaan(mH)
mH.l.fit <- lavaan(mH.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mH.l.fit)

mySemPlot(mWE, dotSimplify = F) %>%  export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mWEplot1.pdf")
mySemPlot(mH, dotSimplify=F) %>%  export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mHplot1.pdf")


#WE submodels - start
mD <- lvm()
regression(mD) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
regression(mD) <- DEMANDS ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
  ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
latent(mD) <- ~ DEMANDS
mD.l <- lavaToLavaan(mD)
mD.l.fit <- lavaan(mD.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)

mC <- lvm()
regression(mC) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, 
                    S2.7, S2.8) ~ CONTENTS
regression(mC) <- CONTENTS ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
  ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
latent(mC) <- ~ CONTENTS
mC.l <- lavaToLavaan(mC)
mC.l.fit <- lavaan(mC.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)

mR <- lvm()
regression(mR) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9,
                    S3.10, S3.11, S3.12) ~ RELATIONS
regression(mR) <- RELATIONS ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
  ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
latent(mR) <- ~ RELATIONS
mR.l <- lavaToLavaan(mR)
mR.l.fit <- lavaan(mR.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)

mS <- lvm()
regression(mS) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
regression(mS) <- SITUATION ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
  ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
latent(mS) <- ~ SITUATION
mS.l <- lavaToLavaan(mS)
mS.l.fit <- lavaan(mS.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)

rmsea(mD.l.fit); rmsea(mC.l.fit); rmsea(mR.l.fit); rmsea(mS.l.fit)

miPlot(modindices(mD.l.fit), exclude=c("VIDEN_GRAD.high",
                                       "VIDEN_GRAD.medium", 
                                       "ARB_TYPE.client",
                                       "ARB_TYPE.customer",
                                       "ARB_TYPE.knowledge")) %>%  export_svg %>% charToRaw %>% rsvg::rsvg_pdf("miPlotD1.pdf")
miPlot(modindices(mC.l.fit), exclude=c("VIDEN_GRAD.high",
                                       "VIDEN_GRAD.medium", 
                                       "ARB_TYPE.client",
                                       "ARB_TYPE.customer",
                                       "ARB_TYPE.knowledge")) %>%  export_svg %>% charToRaw %>% rsvg::rsvg_pdf("miPlotC1.pdf")
miPlot(modindices(mR.l.fit), exclude=c("VIDEN_GRAD.high",
                                       "VIDEN_GRAD.medium", 
                                       "ARB_TYPE.client",
                                       "ARB_TYPE.customer",
                                       "ARB_TYPE.knowledge")) %>%  export_svg %>% charToRaw %>% rsvg::rsvg_pdf("miPlotR1.pdf")
miPlot(modindices(mS.l.fit), exclude=c("VIDEN_GRAD.high",
                                       "VIDEN_GRAD.medium", 
                                       "ARB_TYPE.client",
                                       "ARB_TYPE.customer",
                                       "ARB_TYPE.knowledge")) %>%  export_svg %>% charToRaw %>% rsvg::rsvg_pdf("miPlotS1.pdf")


#WE submodels after adding most powerfull missing effects
mD2 <- mD
variance(mD2) <- S1.5 ~ S1.2
mD2.l <- lavaToLavaan(mD2)
mD2.l.fit <- lavaan(mD2.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mD2.l.fit)

mC2 <- mC
variance(mC2) <- S2.1 ~ S2.3
variance(mC2) <- S2.5 ~ S2.8
variance(mC2) <- S2.1 ~ S2.2
variance(mC2) <- S2.3 ~ S2.5
mC2.l <- lavaToLavaan(mC2)
mC2.l.fit <- lavaan(mC2.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mC2.l.fit)

mR2 <- mR
variance(mR2) <- S3.2 ~ S3.1 + S3.3
variance(mR2) <- S3.1 ~ S3.3
variance(mR2) <- S3.10 ~ S3.9 + S3.11
variance(mR2) <- S3.5 ~ S3.7
variance(mR2) <- S3.9 ~ S3.11
variance(mR2) <- S3.7 ~ S3.8
variance(mR2) <- S3.5 ~ S3.8
mR2.l <- lavaToLavaan(mR2)
mR2.l.fit <- lavaan(mR2.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mR.l.fit)
rmsea(mR2.l.fit)

mS2 <- mS
variance(mS2) <- S5.4 ~ S5.2
variance(mS2) <- S5.1 ~ S5.2
mS2.l <- lavaToLavaan(mS2)
mS2.l.fit <- lavaan(mS2.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mS.l.fit)
rmsea(mS2.l.fit)


#Combine into one WE model again
mWE2 <- mWE
variance(mWE2) <- S1.5 ~ S1.2
variance(mWE2) <- S2.5 ~ S2.8
variance(mWE2) <- S2.3 ~ S2.1
variance(mWE2) <- S2.1 ~ S2.2
variance(mWE2) <- S2.3 ~ S2.5
variance(mWE2) <- S3.2 ~ S3.1 + S3.3
variance(mWE2) <- S3.1 ~ S3.3
variance(mWE2) <- S3.10 ~ S3.9 + S3.11
variance(mWE2) <- S3.5 ~ S3.7
variance(mWE2) <- S3.9 ~ S3.11
variance(mWE2) <- S3.7 ~ S3.8
variance(mWE2) <- S3.5 ~ S3.8
variance(mWE2) <- S5.4 ~ S5.2
variance(mWE2) <- S5.2 ~ S5.1
mWE2.l <- lavaToLavaan(mWE2)
mWE2.l.fit <- lavaan(mWE2.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mWE2.l.fit)
rmsea(mWE.l.fit)


#Modify WE model 2
mWE2.l.fit.mi <- modindices(mWE2.l.fit, sort.=T, op="~~")
mWE3 <- mWE2
variance(mWE3) <- CONTENTS ~ RELATIONS
variance(mWE3) <- CONTENTS ~ SITUATION
variance(mWE3) <- RELATIONS ~ SITUATION
mWE3.l <- lavaToLavaan(mWE3)
mWE3.l.fit <- lavaan(mWE3.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mWE3.l.fit)

#HEALTH-model mi
miPlot(modindices(mH.l.fit)) %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("miPlotH1.pdf")
mH2 <- mH
variance(mH2) <- STRESS ~ MDI_t
mH2.l <- lavaToLavaan(mH2)
mH2.l.fit <- lavaan(mH2.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mH2.l.fit)
rmsea(mH.l.fit)

#Combine into full SEM 
mF <- lvm()
regression(mF) <- c(DEMANDS, CONTENTS, RELATIONS, SITUATION) ~ VIDEN_GRAD.high +
  VIDEN_GRAD.medium + ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
regression(mF) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
regression(mF) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, S2.8) ~ CONTENTS
regression(mF) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, 
                    S3.10, S3.11, S3.12) ~ RELATIONS
regression(mF) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
latent(mF) <- ~ DEMANDS + CONTENTS + RELATIONS + SITUATION
variance(mF) <- S1.5 ~ S1.2
variance(mF) <- S2.5 ~ S2.8
variance(mF) <- S2.3 ~ S2.1
variance(mF) <- S2.1 ~ S2.2
variance(mF) <- S2.3 ~ S2.5
variance(mF) <- S3.2 ~ S3.1 + S3.3
variance(mF) <- S3.1 ~ S3.3
variance(mF) <- S3.10 ~ S3.9 + S3.11
variance(mF) <- S3.5 ~ S3.7
variance(mF) <- S5.2 ~ S5.1
variance(mF) <- S5.4 ~ S5.2
variance(mF) <- CONTENTS ~ RELATIONS
variance(mF) <- CONTENTS ~ SITUATION
variance(mF) <- RELATIONS ~ SITUATION

regression(mF) <- c(SICK_TOTAL_t, Q36n, Q30n_t) ~ HEALTH
regression(mF) <- STRESS ~ Q05 + Q08.yes + Q03.temp
regression(mF) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + 
  BMI_CAT.obese + ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
  FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
regression(mF) <- c(MDI_t, STRESS) ~ HEALTH
regression(mF) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
regression(mF) <- STRESS ~ ANS_AAR_t
regression(mF) <- HEALTH ~ Q43_t
latent(mF) <- ~ HEALTH
variance(mF) <- STRESS ~ MDI_t

regression(mF) <- c(HEALTH, DEMANDS, CONTENTS, SITUATION, 
                    RELATIONS) ~ SEG_KOEN + SEG_KOEN2
regression(mF) <- HEALTH ~ DEMANDS + CONTENTS + SITUATION + RELATIONS

mF.l <- lavaToLavaan(mF)
mF.l.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T, test=SB)
rmsea(mF.l.fit)
rmsea(mF0.l.fit)

mF.l.BS.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T,
                      se="bootstrap", test=SB, bootstrap=200, verbose=T)
mySemPlot_central(mF.l.BS.fit)  %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mFCentral.pdf")
rmsea(mF.l.SB.fit)
rmsea(mF.l.BS.fit)


#Prediction models 
mmH <- lvm()
regression(mmH) <- c(SICK_TOTAL_t, Q36n, Q30n_t, MDI_t, 
                     STRESS) ~ HEALTH
variance(mmH) <- STRESS ~ MDI_t
latent(mmH) <- ~ HEALTH
mmH.l <- lavaToLavaan(mmH)
mmH.l.fit <- lavaan(mmH.l, anDataS, fixed.x=T, auto.fix.first=T)
healthPred <- predict(mmH.l.fit)

mmD <- lvm()
regression(mmD) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
variance(mmD) <- S1.2 ~ S1.5
latent(mmD) <- ~ DEMANDS
mmD.l <- lavaToLavaan(mmD)
mmD.l.fit <- lavaan(mmD.l, anDataS, fixed.x=T, auto.fix.first=T)
demandsPred <- predict(mmD.l.fit)

mmC <- lvm()
regression(mmC) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7,
                     S2.8) ~ CONTENTS
variance(mmC) <- S2.5 ~ S2.8
variance(mmC) <- S2.3 ~ S2.1
variance(mmC) <- S2.1 ~ S2.2
variance(mmC) <- S2.3 ~ S2.5
latent(mmC) <- ~ CONTENTS
mmC.l <- lavaToLavaan(mmC)
mmC.l.fit <- lavaan(mmC.l, anDataS, fixed.x=T, auto.fix.first=T)
contentsPred <- predict(mmC.l.fit)

mmR <- lvm()
regression(mmR) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, 
                     S3.9, S3.10, S3.11, S3.12) ~ RELATIONS
variance(mmR) <- S3.2 ~ S3.1 + S3.3
variance(mmR) <- S3.1 ~ S3.3
variance(mmR) <- S3.10 ~ S3.9 + S3.11
variance(mmR) <- S3.5 ~ S3.7
latent(mmR) <- ~ RELATIONS
mmR.l <- lavaToLavaan(mmR)
mmR.l.fit <- lavaan(mmR.l, anDataS, fixed.x=T, auto.fix.first=T)
relationsPred <- predict(mmR.l.fit)

mmS <- lvm()
regression(mmS) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
variance(mmS) <- S5.2 ~ S5.1
variance(mmS) <- S5.4 ~ S5.2
latent(mmS) <- ~ SITUATION
mmS.l <- lavaToLavaan(mmS)
mmS.l.fit <- lavaan(mmS.l, anDataS, fixed.x=T, auto.fix.first=T)
situationPred <- predict(mmS.l.fit)

predData <- cbind(anDataS, HEALTH=healthPred, DEMANDS=demandsPred,
                  SITUATION=situationPred, RELATIONS=relationsPred,
                  CONTENTS=contentsPred)

#Path analysis model
pam <- lvm()
regression(pam) <- c(DEMANDS, CONTENTS, RELATIONS, 
                     SITUATION) ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
  ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
variance(pam) <- CONTENTS ~ RELATIONS
variance(pam) <- CONTENTS ~ SITUATION
variance(pam) <- RELATIONS ~ SITUATION
regression(pam) <- STRESS ~ Q05 + Q08.yes + Q03.temp
regression(pam) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + 
  BMI_CAT.obese + ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
  FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
regression(pam) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
regression(pam) <- STRESS ~ ANS_AAR_t
regression(pam) <- HEALTH ~ Q43_t
regression(pam) <- c(HEALTH, DEMANDS, CONTENTS, SITUATION, 
                     RELATIONS) ~ SEG_KOEN + SEG_KOEN2
regression(pam) <- HEALTH ~ DEMANDS + CONTENTS + SITUATION + RELATIONS
pam.l <- lavaToLavaan(pam)
pam.l.fit <- lavaan(pam.l, predData, fixed.x=T, test=SB)
rmsea(pam.l.fit)

pam.l.BS.fit <- lavaan(pam.l, predData, fixed.x=T, se="bootstrap", 
                       verbose=T, test=BS, bootstrap=200)
mySemPlot_central(pam.l.BS.fit)  %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("pamCentral.pdf")

#computation time comparison
foo1 <- function() {
  mF <- lvm()
  regression(mF) <- c(DEMANDS, CONTENTS, RELATIONS, SITUATION) ~ VIDEN_GRAD.high + 
    VIDEN_GRAD.medium + ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
  regression(mF) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
  regression(mF) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, S2.8) ~ CONTENTS
  regression(mF) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, 
                      S3.10, S3.11, S3.12) ~ RELATIONS
  regression(mF) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
  latent(mF) <- ~ DEMANDS + CONTENTS + RELATIONS + SITUATION
  variance(mF) <- S1.5 ~ S1.2
  variance(mF) <- S2.5 ~ S2.8
  variance(mF) <- S2.3 ~ S2.1
  variance(mF) <- S2.1 ~ S2.2
  variance(mF) <- S2.3 ~ S2.5
  variance(mF) <- S3.2 ~ S3.1 + S3.3
  variance(mF) <- S3.1 ~ S3.3
  variance(mF) <- S3.10 ~ S3.9 + S3.11
  variance(mF) <- S3.5 ~ S3.7
  variance(mF) <- S5.2 ~ S5.1
  variance(mF) <- S5.4 ~ S5.2
  variance(mF) <- CONTENTS ~ RELATIONS
  variance(mF) <- CONTENTS ~ SITUATION
  variance(mF) <- RELATIONS ~ SITUATION
  regression(mF) <- c(SICK_TOTAL_t, Q36n, Q30n_t) ~ HEALTH
  regression(mF) <- STRESS ~ Q05 + Q08.yes + Q03.temp
  regression(mF) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + 
    BMI_CAT.obese + ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
    FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
  regression(mF) <- c(MDI_t, STRESS) ~ HEALTH
  regression(mF) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
  regression(mF) <- STRESS ~ ANS_AAR_t
  regression(mF) <- HEALTH ~ Q43_t
  latent(mF) <- ~ HEALTH
  variance(mF) <- STRESS ~ MDI_t
  regression(mF) <- c(HEALTH, DEMANDS, CONTENTS, SITUATION, 
                      RELATIONS) ~ SEG_KOEN + SEG_KOEN2
  regression(mF) <- HEALTH ~ DEMANDS + CONTENTS + SITUATION + RELATIONS
  mF.l <- lavaToLavaan(mF)
  mF.l.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T)
}
foo2 <- function() {
  mmH <- lvm()
  regression(mmH) <- c(SICK_TOTAL_t, Q36n, Q30n_t, MDI_t, STRESS) ~ HEALTH
  variance(mmH) <- STRESS ~ MDI_t
  latent(mmH) <- ~ HEALTH
  mmH.l <- lavaToLavaan(mmH)
  mmH.l.fit <- lavaan(mmH.l, anDataS, fixed.x=T, auto.fix.first=T)
  healthPred <- predict(mmH.l.fit)
  mmD <- lvm()
  regression(mmD) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
  variance(mmD) <- S1.5 ~ S1.2
  latent(mmD) <- ~ DEMANDS
  mmD.l <- lavaToLavaan(mmD)
  mmD.l.fit <- lavaan(mmD.l, anDataS, fixed.x=T, auto.fix.first=T)
  demandsPred <- predict(mmD.l.fit)
  mmC <- lvm()
  regression(mmC) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, 
                       S2.8) ~ CONTENTS
  variance(mmC) <- S2.5 ~ S2.8
  variance(mmC) <- S2.3 ~ S2.1
  variance(mmC) <- S2.1 ~ S2.2
  variance(mmC) <- S2.3 ~ S2.5
  latent(mmC) <- ~ CONTENTS
  mmC.l <- lavaToLavaan(mmC)
  mmC.l.fit <- lavaan(mmC.l, anDataS, fixed.x=T, auto.fix.first=T)
  contentsPred <- predict(mmC.l.fit)
  mmR <- lvm()
  regression(mmR) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, 
                       S3.10, S3.11, S3.12) ~ RELATIONS
  variance(mmR) <- S3.2 ~ S3.1 + S3.3
  variance(mmR) <- S3.1 ~ S3.3
  variance(mmR) <- S3.10 ~ S3.9 + S3.11
  variance(mmR) <- S3.5 ~ S3.7
  latent(mmR) <- ~ RELATIONS
  mmR.l <- lavaToLavaan(mmR)
  mmR.l.fit <- lavaan(mmR.l, anDataS, fixed.x=T, auto.fix.first=T)
  relationsPred <- predict(mmR.l.fit)
  mmS <- lvm()
  regression(mmS) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
  variance(mmS) <- S5.2 ~ S5.1
  variance(mmS) <- S5.4 ~ S5.2
  latent(mmS) <- ~ SITUATION
  mmS.l <- lavaToLavaan(mmS)
  mmS.l.fit <- lavaan(mmS.l, anDataS, fixed.x=T, auto.fix.first=T)
  situationPred <- predict(mmS.l.fit)
  
  predData <- cbind(anDataS, HEALTH=healthPred, DEMANDS=demandsPred,
                    SITUATION=situationPred, RELATIONS=relationsPred,
                    CONTENTS=contentsPred)
  pam <- lvm()
  regression(pam) <- c(DEMANDS, CONTENTS, RELATIONS, 
                       SITUATION) ~ VIDEN_GRAD.high + VIDEN_GRAD.medium + 
    ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
  variance(pam) <- CONTENTS ~ RELATIONS
  variance(pam) <- CONTENTS ~ SITUATION
  variance(pam) <- RELATIONS ~ SITUATION
  regression(pam) <- STRESS ~ Q05 + Q08.yes + Q03.temp
  regression(pam) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + 
    BMI_CAT.obese + ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
    FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
  regression(pam) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
  regression(pam) <- STRESS ~ ANS_AAR_t
  regression(pam) <- HEALTH ~ Q43_t
  regression(pam) <- c(HEALTH, DEMANDS, CONTENTS, SITUATION, 
                       RELATIONS) ~ SEG_KOEN + SEG_KOEN2
  regression(pam) <- HEALTH ~ DEMANDS + CONTENTS + SITUATION + RELATIONS
  pam.l <- lavaToLavaan(pam)
  pam.l.fit <- lavaan(pam.l, predData, fixed.x=T)
}

bRes <- microbenchmark(foo1(), foo2())


#Measurement invariance investigations
gr.everything <- c("intercepts", "means", "thresholds",
                   "regressions", "residuals","residual.covariances",
                   "loadings", "lv.variances", "lv.covariances")

#note: default is Satorra-Bentler
measurementInvariance(mmH.l, anDataS, group="KOEN", strict=T)
measurementInvariance(mmD.l, anDataS, group="KOEN", strict=T)
measurementInvariance(mmC.l, anDataS, group="KOEN", strict=T)
measurementInvariance(mmR.l, anDataS, group="KOEN", strict=T)
measurementInvariance(mmS.l, anDataS, group="KOEN", strict=T)


#Construct model with measurement model interactions 
gr.free1 <- c("SICK_TOTAL_tL~1", "Q36n~1", "Q30n_t~1",
              "MDI_t~1", "STRESS~1",
              "SICK_TOTAL_t~~SICK_TOTAL_t", 
              "Q36n~~Q36n", "Q30n_t~~Q30n_t",
              "MDI_t~~MDI_t", "STRESS~~STRESS",
              "S5.1~1", "S5.2~1", "S5.3~1", "S5.4~1", "S5.5~1",
              "S5.1~~S5.1", "S5.2~~S5.2", "S5.3~~S5.3", 
              "S5.4~~S5.4","S5.5~~S5.5")

mFgAllFree.l.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T, 
                           meanstructure=T, group="KOEN", test=SB)
rmsea(mFgAllFree.l.fit)

mFg1.l.BS.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T, 
                        meanstructure=T, group="KOEN", 
                        group.equal = gr.everything,
                        group.partial=gr.free1, test=SB, se="bootstrap", 
                        bootstrap=200, verbose=T)
rmsea(mFg1.l.BS.fit)
rmsea(mF.l.fit)

#Add interactions for effects of gender segregation
gr.free2 <- c(gr.free1, paste(c("DEMANDS", "CONTENTS", 
                                "RELATIONS", "SITUATION"),
                              "SEG_KOEN", sep="~"),
              paste(c("DEMANDS", "CONTENTS", "RELATIONS", "SITUATION"),
                    "SEG_KOEN2", sep="~"),
              "HEALTH~SEG_KOEN", "HEALTH~SEG_KOEN2")

mFg2.l.BS.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T, 
                        meanstructure=T, group="KOEN", 
                        group.equal = gr.everything,
                        group.partial=gr.free2, test=SB, se="bootstrap",
                        bootstrap=200)
rmsea(mFg2.l.BS.fit)  

mySemPlot_central(mFg2.l.BS.fit, group="K") %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mFg2Kcent.pdf")
mySemPlot_central(mFg2.l.BS.fit, group="M") %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mFg2Mcent.pdf")

#mFg1 where interactions have been added to work environment effects
gr.free3 <- c(gr.free2, paste("HEALTH", c("DEMANDS", "CONTENTS", 
                                          "RELATIONS", "SITUATION"),
                              sep="~"))
mFg3.l.BS.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T, 
                        meanstructure=T, group="KOEN", 
                        group.equal = gr.everything,
                        group.partial=gr.free3, test=SB, se="bootstrap",
                        bootstrap=200)
rmsea(mFg3.l.SB.fit)
parameterEstimates(mFg3.l.SB.fit)

#Combing all interactions
gr.free4 <- union(gr.free2, gr.free3)
mFg4.l.BS.fit <- lavaan(mF.l, anDataS, fixed.x=T, auto.fix.first=T, 
                        meanstructure=T, group="KOEN", 
                        group.equal = gr.everything,
                        group.partial=gr.free4, test=SB, se="bootstrap",
                        bootstrap=200)
rmsea(mFg4.l.SB.fit)

mySemPlot_central(mFg4.l.BS.fit, group="K") %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mFg4Kcent.pdf")
mySemPlot_central(mFg4.l.BS.fit, group="M") %>% export_svg %>% charToRaw %>% rsvg::rsvg_pdf("mFg4Mcent.pdf")


#Try investigating normality assumption by using other estimation methods
ULS.stuf <- lavaan(mF.l, anDataS, auto.fix.first = T, fixed.x=T,
                   estimator="ULS", verbose=T) #5.5 time cirka
rmsea(ULS.stuf)

pMFuls <- parameterEstimates(ULS.stuf)
pMFml <- parameterEstimates(mF.l.fit)
pMFbs <- parameterEstimates(mF.l.BS.fit)
pMFuls <- pMFuls[!is.na(pMFuls$z),]
pMFml <- pMFml[!is.na(pMFml$z), ]
pMFbs <- pMFbs[!is.na(pMFbs$z), ]

pMFuls$op[pMFuls$op=="~"] <- "%<-%"
pMFuls$op[pMFuls$op=="=~"] <- "%->%"
pMFuls$op[pMFuls$op=="~~"] <- "%<->%"
pMFml$op[pMFml$op=="~"] <- "%<-%"
pMFml$op[pMFml$op=="=~"] <- "%->%"
pMFml$op[pMFml$op=="~~"] <- "%<->%"
pMFbs$op[pMFbs$op=="~"] <- "%<-%"
pMFbs$op[pMFbs$op=="=~"] <- "%->%"
pMFbs$op[pMFbs$op=="~~"] <- "%<->%"

pMFuls$par <- paste(pMFuls$lhs, pMFuls$op, pMFuls$rhs)
pMFml$par <- paste(pMFml$lhs, pMFml$op, pMFml$rhs)
pMFbs$par <- paste(pMFbs$lhs, pMFbs$op, pMFbs$rhs)
pMFuls <- pMFuls[, c("par", "est", "ci.lower", "ci.upper", "se")]
pMFml <- pMFml[, c("par", "est", "ci.lower", "ci.upper", "se")]
pMFbs <- pMFbs[, c("par", "est", "ci.lower", "ci.upper", "se")]
estCompData <- cbind(rbind(pMFuls, pMFml, pMFbs), 
                     estimator=rep(c("ULS", "ML", "ML and bootstrap"), 
                                   each=nrow(pMFuls)))
estCompData <- estCompData[order(estCompData$par),]
estCompData$page <- c(rep(1, 252), rep(2, nrow(estCompData)-252))
labs1 <- parse(text=estCompData[estCompData$page==1, "par"])
labs2 <- parse(text=estCompData[estCompData$page==2, "par"])

ecp1 <- ggplot(estCompData[estCompData$page==1,], 
               aes(x=par, y=est, col=estimator, group=estimator)) +
  geom_point() + 
  geom_errorbar(aes(ymax=ci.upper, ymin=ci.lower)) +
  theme(legend.position="bottom") +
  scale_color_discrete("Estimator") +
  xlab("") +
  ylab("Estimate") +
  coord_flip() +
  scale_x_discrete(labels=labs1, breaks=labs1)

ecp2 <- ggplot(estCompData[estCompData$page==2,], 
               aes(x=par, y=est, col=estimator)) +
  geom_point() +  
  xlab("") + 
  ylab("Estimate") +
  geom_errorbar(aes(ymax=ci.upper, ymin=ci.lower)) +
  coord_flip() +
  theme(legend.position="bottom") +
  scale_x_discrete(labels=labs2, breaks=labs2)

pdf("estCompPlot1.pdf", width=9, height=13)
ecp1
dev.off()


pdf("estCompPlot2.pdf", width=9, height=12)
ecp2
dev.off()


#bootstrap for total effect se
bootTE <- function(model, data, reps=10, verbose=F, mg=F, ...) {
  if (mg) {
    TEs.K <- rep(NA, reps)
    TEs2.K <- rep(NA, reps)
    TEs.M <- rep(NA, reps)
    TEs2.M <- rep(NA, reps)
  } else {
    TEs <- TEs2 <- rep(NA, reps)
  }
  n <- nrow(data)
  for (i in 1:reps) {
    if (verbose) print(paste("iteration: ", i, sep=""))
    bs <- sample(n, n, replace=T)
    bsData <- data[bs,]
    m <- lavaan(model, bsData, ...)
    if (mg) {
      effK <- getGSEff(m, group="K")
      effM <- getGSEff(m, group="M")
      TEs.K[i] <- effK[1]
      TEs2.K[i] <- effK[2]
      TEs.M[i] <- effM[1]
      TEs2.M[i] <- effM[2]
    } else {
      eff <- getGSEff(m)
      TEs[i] <- eff[1]
      TEs2[i] <- eff[2]
    }
  }
  if (mg) {
    return(list(TEs.K = TEs.K, TEs2.K = TEs2.K,
                TEs.M = TEs.M, TEs2.M = TEs2.M))
  } else {
    list(TEs = TEs, TEs2 = TEs2)
  }
}

#Bootstrap total effects
mF.l.bTE <- bootTE(mF.l, anDataS, reps=200, verbose=T, 
                   mg=F, auto.fix.first=T, fixed.x=T)

mFg4.l.bTE <- bootTE(mF.l, anDataS, reps=200, verbose=T, mg=T, 
                     auto.fix.first=T, fixed.x=T, 
                     group="KOEN",group.equal = gr.everything, 
                     group.partial=gr.free4, test=SB)

mFg2.l.bTE <- bootTE(mF.l, anDataS, reps=200, verbose=T, mg=T, 
                     auto.fix.first=T, fixed.x=T, 
                     group="KOEN",group.equal = gr.everything, 
                     group.partial=gr.free2, test=SB)

mFg1.l.bTE <- bootTE(mF.l, anDataS, reps=200, verbose=T, mg=T, 
                     auto.fix.first=T, fixed.x=T, 
                     group="KOEN",group.equal = gr.everything, 
                     group.partial=gr.free1, test=SB)



#Produce TE plots for all models.
#NOTE: X AND Y HAVE OPPOSITE ROLES OF WHAT IS IN
#THE PROJECT
y <- 0.5
xs <- seq(-y, 1-y, 0.001) 

TE.g1.B <- getGSEff(mFg1.l.BS.fit, group="K")
TE.g2.F <- getGSEff(mFg2.l.BS.fit, group="K")
TE.g4.F <- getGSEff(mFg4.l.BS.fit, group="K")
TE.g2.M <- getGSEff(mFg2.l.BS.fit, group="M")
TE.g4.M <- getGSEff(mFg4.l.BS.fit, group="M")
TE.B <- getGSEff(mF.l.BS.fit)


nXs <- length(xs)
TE.g1.B.upper <- TE.g1.B.lower <- rep(NA, nXs)
TE.g2.F.upper <- TE.g2.F.lower <- rep(NA, nXs)
TE.g4.F.upper <- TE.g4.F.lower <- rep(NA, nXs)
TE.g2.M.upper <- TE.g2.M.lower <- rep(NA, nXs)
TE.g4.M.upper <- TE.g4.M.lower <- rep(NA, nXs)
TE.B.upper <- TE.B.lower <- rep(NA,nXs)

for (i in 1:nXs) {
  x <- xs[i]
  bEst.g1.B <- x*mFg1.l.bTE$TEs.K + x^2*mFg1.l.bTE$TEs2.K + 
    2*y*x*mFg1.l.bTE$TEs2.K
  bSd.g1.B <- sd(bEst.g1.B)
  pEst.g1.B <- TE.g1.B[1]*x+TE.g1.B[2]*x^2 + 2*y*x*TE.g1.B[2]
  TE.g1.B.upper[i] <- pEst.g1.B+qnorm(0.975)*bSd.g1.B
  TE.g1.B.lower[i] <- pEst.g1.B-qnorm(0.975)*bSd.g1.B
  
  bEst.g2.F <- x*mFg2.l.bTE$TEs.K + x^2*mFg2.l.bTE$TEs2.K + 
    2*y*x*mFg2.l.bTE$TEs2.K
  bSd.g2.F <- sd(bEst.g2.F)
  pEst.g2.F <- TE.g2.F[1]*x+TE.g2.F[2]*x^2 + 2*y*x*TE.g2.F[2]
  TE.g2.F.upper[i] <- pEst.g2.F+qnorm(0.975)*bSd.g2.F
  TE.g2.F.lower[i] <- pEst.g2.F-qnorm(0.975)*bSd.g2.F
  
  bEst.g4.F <- x*mFg4.l.bTE$TEs.K + x^2*mFg4.l.bTE$TEs2.K + 
    2*y*x*mFg4.l.bTE$TEs2.K
  bSd.g4.F <- sd(bEst.g4.F)
  pEst.g4.F <- TE.g4.F[1]*x+TE.g4.F[2]*x^2 + 2*y*x*TE.g4.F[2]
  TE.g4.F.upper[i] <- pEst.g4.F+qnorm(0.975)*bSd.g4.F
  TE.g4.F.lower[i] <- pEst.g4.F-qnorm(0.975)*bSd.g4.F
  
  bEst.g2.M <- x*mFg2.l.bTE$TEs.M + x^2*mFg2.l.bTE$TEs2.M + 
    2*y*x*mFg2.l.bTE$TEs2.M
  bSd.g2.M <- sd(bEst.g2.M)
  pEst.g2.M <- TE.g2.M[1]*x+TE.g2.M[2]*x^2 + 2*y*x*TE.g2.M[2]
  TE.g2.M.upper[i] <- pEst.g2.M+qnorm(0.975)*bSd.g2.M
  TE.g2.M.lower[i] <- pEst.g2.M-qnorm(0.975)*bSd.g2.M
  
  bEst.g4.M <- x*mFg4.l.bTE$TEs.M + x^2*mFg4.l.bTE$TEs2.M + 
    2*y*x*mFg4.l.bTE$TEs2.M
  bSd.g4.M <- sd(bEst.g4.M)
  pEst.g4.M <- TE.g4.M[1]*x+TE.g4.M[2]*x^2 + 2*y*x*TE.g4.M[2]
  TE.g4.M.upper[i] <- pEst.g4.M+qnorm(0.975)*bSd.g4.M
  TE.g4.M.lower[i] <- pEst.g4.M-qnorm(0.975)*bSd.g4.M
  
  bEst.B <- x*mF.l.bTE$TEs + x^2*mF.l.bTE$TEs2 + 
    2*y*x*mF.l.bTE$TEs2
  bSd.B <- sd(bEst.B)
  pEst.B <- TE.B[1]*x+TE.B[2]*x^2 + 2*y*x*TE.B[2]
  TE.B.upper[i] <- pEst.B+qnorm(0.975)*bSd.B
  TE.B.lower[i] <- pEst.B-qnorm(0.975)*bSd.B
}

TEplotFrame <- data.frame(lwr=c(TE.g2.F.lower, TE.g4.F.lower,
                                TE.g2.M.lower, TE.g4.M.lower,
                                TE.B.lower, TE.g1.B.lower),
                          upr=c(TE.g2.F.upper, TE.g4.F.upper,
                                TE.g2.M.upper, TE.g4.M.upper,
                                TE.B.upper, TE.g1.B.upper),
                          GS=100*rep(xs, 6),
                          est=c(xs*TE.g2.F[1]+xs^2*TE.g2.F[2]+ 
                                  2*y*xs*TE.g2.F[2],
                                xs*TE.g4.F[1]+xs^2*TE.g4.F[2]+ 
                                  2*y*xs*TE.g4.F[2],
                                xs*TE.g2.M[1]+xs^2*TE.g2.M[2]+ 
                                  2*y*xs*TE.g2.M[2],
                                xs*TE.g4.M[1]+xs^2*TE.g4.M[2]+ 
                                  2*y*xs*TE.g4.M[2],
                                xs*TE.g1.B[1]+xs^2*TE.g1.B[2]+ 
                                  2*y*xs*TE.g1.B[2],
                                xs*TE.B[1]+xs^2*TE.B[2]+ 2*y*xs*TE.B[2]),
                          model=c(rep(c("Grouped gender segregation \n effects", 
                                   "Grouped gender segregation \n and work environment effects"), 
                                      each=nXs, 2), 
                           rep(c("Group effects in \n measurement models", "Full model, no groups \n"), 
                                      each=nXs)),
                          gender=c(rep(c("Females", "Males"), each=2*nXs), 
                                   rep("Both genders", 2*nXs)))

cp1 <- ggplot(TEplotFrame, aes(x=GS, y=est, col=model,
                               group=model:gender, fill=model)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, col=NULL), alpha=0.2) +
  geom_line(size=1) +
  theme(legend.position="bottom") +
  facet_wrap(~ gender, nrow=3) +
  scale_color_discrete("") +
  scale_fill_discrete("") + 
  xlab("Change in percentage females relative to 50%") +
  ylab("Change in health score") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))


#Plots for varying levels of y, only mFg4 and mF
ys <- seq(0, 1, 0.1)
nYs <- length(ys)
nXs <- 101
upper <- NULL
lower <- NULL
est <- NULL
allXs <- NULL

for (i in 1:nYs) {
  y <- ys[i]
  xs <- seq(-y, 1-y, 0.01)
  allXs <- c(allXs, rep(xs, each=3))
  for (j in 1:nXs) {
    x <- xs[j]
    
    bEst.F <- x*mFg4.l.bTE$TEs.K + x^2*mFg4.l.bTE$TEs2.K + 
      2*y*x*mFg4.l.bTE$TEs2.K
    bSd.F <- sd(bEst.F)
    pEst.F <- TE.g4.F[1]*x+TE.g4.F[2]*x^2 + 2*y*x*TE.g4.F[2]
    upper <- c(upper, pEst.F+qnorm(0.975)*bSd.F)
    lower <- c(lower, pEst.F-qnorm(0.975)*bSd.F)
    
    bEst.M <- x*mFg4.l.bTE$TEs.M + x^2*mFg4.l.bTE$TEs2.M + 
      2*y*x*mFg4.l.bTE$TEs2.M
    bSd.M <- sd(bEst.M)
    pEst.M <- TE.g4.M[1]*x+TE.g4.M[2]*x^2 + 2*y*x*TE.g4.M[2]
    upper <- c(upper, pEst.M+qnorm(0.975)*bSd.M)
    lower <- c(lower, pEst.M-qnorm(0.975)*bSd.M)
    
    bEst.B <- x*mF.l.bTE$TEs + x^2*mF.l.bTE$TEs2 + 
      2*y*x*mF.l.bTE$TEs2
    bSd.B <- sd(bEst.B)
    pEst.B <- TE.B[1]*x+TE.B[2]*x^2 + 2*y*x*TE.B[2]
    upper <- c(upper, pEst.B+qnorm(0.975)*bSd.B)
    lower <- c(lower, pEst.B-qnorm(0.975)*bSd.B)
    
    est <- c(est, pEst.F, pEst.M, pEst.B)
  }
}

TEyFrame <- data.frame(est=est, lower=lower, upper=upper,
                       Gender=rep(c("Female", "Male", "Both"), nYs*nXs),
                       y=paste("Reference females:", 
                               100*rep(ys, each=nXs*3), "%"),
                       x=100*allXs)
TEyFrame$y <- factor(TEyFrame$y, 
                     levels=paste("Reference females:", 100*ys, "%"), 
                     ordered=T)

cp2 <- ggplot(TEyFrame[TEyFrame$Gender!="Both",], 
              aes(x=x, y=est, col=Gender, fill=Gender)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3, col=NA) +
  geom_line(size=1) +
  facet_wrap(~ y, nrow=4, scales="free") +
  xlab("Change in percentage females") +
  ylab("Change in health score") +
  theme(legend.position = c(0.8, 0.1))

pdf("changep1.pdf", width=8, height=12)
cp1
dev.off()

pdf("changep2.pdf", width=8, height=12)
cp2
dev.off()




#######Plots for discussion chapter##########
xs <- unique(TEplotFrame$GS)
yF <- -0.000001*xs^3  + 0.001*xs 
hm <- length(xs)/2

yM <- - sin(0.03*xs)/10
yF <- c(rep(0, hm), -0.000001*xs[(hm):length(xs)]^3  - 
          0.00003*xs[(hm):length(xs)]^2)

fp <- ggplot(TEplotFrame[TEplotFrame$gender!="Both genders" & 
              TEplotFrame$model!="Grouped gender segregation \n effects",], 
             aes(x=rep(seq(0, 100, 0.1), 2), y=est, col=gender)) +
  geom_line(size=1, alpha=0.5, linetype=2) +
  geom_line(size=1, aes(y=c(yM, yF))) +
  scale_color_discrete("") +
  xlab("Percentage females") +
  ylab("Health score relative to 50% level") 

pdf("ficPlot.pdf", width=8, height=3)
fp
dev.off()

nOcc <- length(levels(anData2$UNDERSTYPE1))
occ <- levels(anData2$UNDERSTYPE1)
gs <- rep(NA, nOcc)
for (i in 1:nOcc) {
  gs[i] <- anData2[anData2$UNDERSTYPE1==occ[i], "SEG_KOEN"][1]
}
GOdata <- data.frame(gs=gs, occ=occ)
GOdata <- GOdata[order(GOdata$gs),]
GOdata$occ <- factor(GOdata$occ, levels=rev(GOdata$occ), ordered=T)


gsp0 <- ggplot(GOdata, aes(x=occ, y=gs)) +
  geom_bar(stat="identity", fill="#00BFC4") + 
  coord_flip() +
  ylab("Percentage females in the occupation") +
  xlab("")

pdf("gsoccPlot.pdf", height=3, width=8) 
gsp0
dev.off()
