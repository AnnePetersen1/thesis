library(ggplot2)
theme_set(theme_bw() %+% theme(text = element_text(size=12)))
library(reshape2)
library(lava)

#Compute standardized PCA loadings and
# cummulated variance contributions from
# a dataset. 
#varCO option is not yet implemented
#vars: variable names that should be included
pcaLoad <- function(data, vars, varCO=NULL) {
  if (is.null(varCO)) {
    varCO <- 1
  }
  n <- length(vars)
  p <- princomp(sapply(na.omit(data[, vars]), as.numeric))
  px <- round(matrix(c(p$loadings), n, 
                     dimnames=list(vars, 1:n)), 4)

  for (i in 1:n) { #standardize
    px[, i] <- abs(px[, i]/sum(abs(px[, i])))
  }
  
  #cummulative variance contributions and 
  #principal component variance constribution
  pcvc <- p$sdev^2
  pcvc <- pcvc/sum(pcvc)
  cpcvc <- cumsum(pcvc)
  cvc <- cumsum(pcvc)
  cvc <- paste(round(cvc*100, 2), "%")
  
  #combine
  pxx <- melt(px)
  pxx$cvc <- pxx$pcvc <- rep(NA, nrow(pxx))
  for (i in 1:n) {
    thisComp <- which(pxx$Var2==i)
    pxx$cvc[thisComp[1]] <- cvc[i]
    pxx$pcvc[thisComp] <- pcvc[i]
  }
  
  colnames(pxx) <- c("var", "comp", "loading", "pcvc", "cvc")
  list(loadings=pxx, nObs=p$n.obs)
}


#Split data, call pcaLoad, return data.frame ready
#for plotting + useful information
#data: Dataset to perform PCA on
#var: Variables (given by names) to include in the PCA
#splitBy: Grouping variable with to levels. The dataset
# is split according to the values in this variable
#covCO: Not yet implemented
pcaObjGen <- function(data, var, splitBy, covCO=NULL) {
  splitLevels <- unique(data[, splitBy])
  data1 <- data[data[, splitBy]==splitLevels[1], ]
  data2 <- data[data[, splitBy]==splitLevels[2], ]
  res1 <- pcaLoad(data1, var)
  res2 <- pcaLoad(data2, var)
  load1 <- res1$loadings
  load2 <- res2$loadings
  n1 <- nrow(load1) 
  n2 <- nrow(load2)
  nObs1 <- res1$nObs
  nObs2 <- res2$nObs
  pcaFrame <- as.data.frame(rbind(load1, load2))
  pcaFrame$group <- c(rep(as.character(splitLevels[1]), n1),
                      rep(as.character(splitLevels[2]), n2))

  list(pcaFrame=pcaFrame, splitBy=splitBy, splitLevels=splitLevels, 
       varNames=var, n1=n1, n2=n2, nObs1=nObs1, nObs2=nObs2)  
}


#Generate PCA plot from pcaObj
#varLabels can be supplied in a named list, e.g. list(var1="joe")
#covCO option is not yet implemented
#splitLabels are labels for the splitting/grouping-variable
# and should also be supplied in a named list.
pcaPlot <- function(pcaObj, varLabels=NULL, covCO=NULL,
                    splitLabels=NULL) {
  splitLevels <- pcaObj$splitLevels
  nCat1 <- pcaObj$nObs1
  nCat2 <- pcaObj$nObs2
  splitBy <- pcaObj$splitBy
  pcaFrame <- pcaObj$pcaFrame

  if (is.null(varLabels)) {
    varLabels <- pcaObj$varNames
  }
  if (is.null(splitLabels)) {
    splitLabels <- splitLevels
  } else {
    sl1 <- splitLabels[[which(names(splitLabels)==splitLevels[1])]]
    sl2 <- splitLabels[[which(names(splitLabels)==splitLevels[2])]]
    splitLabels <- c(sl1, sl2)
  }
  
  facetLabels <- c(paste(splitLabels[1], ", n = ", nCat1, sep=""),
                   paste(splitLabels[2], ", n = ", nCat2, sep=""))
  names(facetLabels) <- splitLevels
  
  ggplot(pcaFrame, aes(x=comp, y=loading, fill=var)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_x_reverse(breaks=c(1, seq(10, 50, 10))) +
    scale_y_continuous(limits=c(0, 1.4), 
                       breaks=c(0, 0.25, 0.5, 0.75, 1)) +
    geom_text(aes(label=cvc), y=1.2, cex=4, na.rm=T) +
    xlab("Principal component") +
    ylab("Standardized loading") +
    theme(legend.position="bottom") +
    facet_wrap(~ group, ncol=2,
             labeller=as_labeller(facetLabels)) + 
    scale_fill_discrete(name=NULL, labels=varLabels)
}


#Simulate data, divide it into two groups and bias 
# one of the groups
#lavaObj: lvm-object from which to simulate
#nObs: Total number of observations
#n1: Number of observations in first group
#n2: Number of observations in second group
#affectDegree: percentage of variables to be biased 
# among the variables in the second group
#biasSize: mean of biasing error terms
#varSize: variance of biasing error terms
#nClusters: Number of clusters in which bias is inflicted.
# Variables in the same cluster reseve the same 
# biasing error term
simMyData <- function(lavaObj=NULL, nObs=NULL, n1=NULL,
                      n2=NULL, biasSize=NULL, affectDegree=NULL,
                      varSize=NULL, nClusters=NULL) {
  if (is.null(nObs) & !is.null(n1) & !is.null(n2)) {
    nObs <- n1 + n2
  }
  if (!is.null(nObs) & is.null(n2)) {
    n2 <- nObs - n1 #obs: check if pos.
  } #more n-checking here
  
  if (is.null(varSize)) varSize <- 0
  if (is.null(biasSize)) biasSize <- 0
  if (is.null(affectDegree)) affectDegree <- 0
  
  if (is.null(lavaObj)) {
    m <- lvm() #m8d
    regression(m) <- c(SICK_TOTAL, Q36n, Q30n) ~ HEALTH
    regression(m) <- STRESS ~ Q05 + Q08.yes + Q03.temp
    regression(m) <- c(DEMANDS, CONTENTS, RELATIONS, SITUATION) ~ SEG_KOEN +
      SEG_KOEN2 + VIDEN_GRAD.high + VIDEN_GRAD.medium + 
      ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
    regression(m) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
    regression(m) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, S2.8) ~ CONTENTS
    regression(m) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, S3.10, S3.11, S3.12) ~ RELATIONS
    regression(m) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
    regression(m) <- HEALTH ~ DEMANDS + CONTENTS + RELATIONS + SITUATION +
      SEG_KOEN + SEG_KOEN2
    regression(m) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + BMI_CAT.obese +
      ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
      FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
    regression(m) <- c(MDI, STRESS) ~ HEALTH
    regression(m) <- ANS_AAR ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
    latent(m) <- ~ HEALTH + DEMANDS + CONTENTS + RELATIONS + SITUATION
    regression(m) <- STRESS ~ ANS_AAR
    
  } else m <- lavaObj
  if (is.null(latent(m))) latent(m) <- "NULLLATVARDROPME"
  data <- simulate(m, nObs)
  data <- data[, which(!(names(data) %in% latent(m)))]
  splitInd <- sample(nObs, n2, replace=F)
  data1 <- data[-splitInd, ]
  data2 <- data[splitInd, ]
  if (affectDegree > 0) {
    nVar <- ncol(data2)
    nBias <- floor(nVar*affectDegree)
    affVar <- sample(nVar, nBias, replace=F)
    groupSize <- floor(length(affVar)/nClusters)
    for (i in 1:nClusters) {  #nBias) {      
      if (i == nClusters) {
        thisGroup <- ((i-1)*groupSize + 1):length(affVar)
      } else thisGroup <- ((i-1)*groupSize + 1):(i*groupSize)
      theseVars <- data2[, thisGroup]
      sign <- sample(c(-1, 1), 1)
      bias <- sign*rnorm(n2, mean = biasSize, sd = sqrt(varSize))
      addBias <- function(x) x + bias
      data2[, thisGroup] <- sapply(theseVars, addBias)
    }
  } 
  data <- rbind(data1, data2)
  data$group <- c(rep("Group 1", n1), rep("Group 2", n2))
  data
}

