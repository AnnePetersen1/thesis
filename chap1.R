library(ggplot2)
theme_set(theme_bw() %+% theme(text = element_text(size=12)))
library(gridExtra)
library(grid)


######1.3.3: U-shapes######

decileLab <- c("0-10", "10-20", "20-30", "30-40", "40-50", 
               "50-60", "60-70", "70-80", "80-90", "90-100")
decileLab2 <- c("0-10", "10-20", "20-30", "30-40", "40-60", 
                "60-70", "70-80", "80-90", "90-100")
cat5Lab <- c("0-20", "20-40", "40-60", "60-80", "80-100")
cat3Lab <- c("0-40", "40-60", "60-100")
cat3Lab2 <- c("0-20", "21-79", "80-100")

#Data and plot for each paper
ledgy <- ggplot(data.frame(x=seq(1,12), y=seq(1:12), 
                           Gender=rep(c("Males", "Females", "Both"), each=4)),
                aes(x=x, y=y, group=Gender, col=Gender)) +
  geom_line(size=1) +
  scale_color_manual(values=c("black", "#F8766D", "#00BFC4"),
                     breaks=c("Males", "Females", "Both"))


mEm <- c(0.2856, 0.3001, 0.1354, 0.0425, 0, -0.1082, 0.0654, 0.1115, 0.1698)
mEf <- mEm + 0.5635 + c(-0.0799, 0.0262, -0.1184, -0.0534, 0, 
                        -0.0036, -0.0761, -0.0893, -0.1442)
masteData <- data.frame(GS=rep(decileLab2, 2), 
                        gender=rep(c("M", "F"), each=9),
                        effect=c(mEm, mEf))

up1 <- ggplot(masteData, aes(x=GS, y=effect, group=gender,
                             col=gender)) +
  geom_line(size=1) +
  geom_point(size=2) +
  xlab("Percentage females") +
  ylab("Log-odds of sickness absence") +
  ggtitle("Mastekaasa 2005") +
  theme(legend.position="none")

bryngData <- data.frame(GS=rep(cat5Lab, 2),
                        gender=rep(c("F", "M"), each=5),
                        effect=c(1.84, 0.97, 1, 1.38, 1.75, 
                                 1.25, 0.90, 1, 1.11, 0.63),
                        lwr=c(0.89, 0.47, 1, 0.79, 1.04, 
                              0.75, 0.51, 1, 0.52, 0.25),
                        upr=c(3.78, 1.99, 1, 2.40, 2.94,
                              2.08, 1.60, 1, 2.36, 1.60))

up2 <- ggplot(bryngData, aes(x=GS, y=effect, group=gender,
                             col=gender, fill=gender)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, col=NA) +
  xlab("Percentage females") +
  ylab("RR of sickness absence") +
  ggtitle("Bryngelson et al. 2011") +
  theme(legend.position="none")

elwerData <- data.frame(GS=cat3Lab,
                        effect=c(1, 1.77, 1.26),
                        lwr=c(1, 1.10, 0.79),
                        upr=c(1, 2.83, 2.0))

up3 <- ggplot(elwerData, aes(x=GS, y=effect, group=1)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, col=NA) +
  xlab("Percentage females") +
  ylab("OR of psychological distress") +
  ggtitle("Elwér et al. 2014") +
  theme(legend.position="none")


tophData <- data.frame(GS=rep(cat3Lab2, 2),
                       gender=rep(c("F", "M"), each=3),
                       effect=c(c(0.060, 0.046, 0) + 20.63, 
                                c(0, 0.002, 0.018) + 18.35))

up4 <- ggplot(tophData, aes(x=GS, y=effect, group=gender,
                            col=gender)) +
  geom_line(size=1) +
  geom_point(size=2) +
  xlab("Percentage females") +
  ylab("Depression score") +
  ggtitle("Tophoven et al. 2015") +
  theme(legend.position="none")

m2Em <- c(1.641, 1.465, 1, 1.253, 1.556)
m2Ef <- m2Em + 1.715 + c(1.062, 1.065, 1, 0.931, 0.742)
maste2Data <- data.frame(GS=rep(cat5Lab, 2),
                         gender=rep(c("M", "F"), each=5),
                         effect=c(m2Em, m2Ef))

up5 <- ggplot(maste2Data, aes(x=GS, y=effect, group=gender,
                              col=gender)) +
  geom_line(size=1) +
  geom_point(size=2) +
  xlab("Percentage females") +
  ylab("OR for sickness absence") +
  ggtitle("Mastekaasa & Melsom 2014") +
  theme(legend.position="none")


upLegend <- g_legend(ledgy)

pdf("uShapeGallery.pdf", width=10, height=12)
grid.arrange(up1, up2, up3, up4, up5, upLegend) 
dev.off()
