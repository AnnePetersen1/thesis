library(ggplot2)
myTheme <- theme_bw() + theme(text = element_text(size=12))
theme_set(myTheme)
library(DiagrammeR)
library(DiagrammeRsvg) #installed using devtools::install_github("rich-iannone/DiagrammeR")
library(magrittr)
library(reshape2)
library(GGally)
library(gridExtra)
library(psych)
source("functions.R")


#Minimalistic plot matrix

#Pairwise plots using loess smoother
smoothLine <- function(data, mapping, ...) {
  p <- ggplot(data=data, mapping=mapping) + 
    geom_smooth(method="loess", se=F, size=0.5, ...) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.border = element_blank()) +
    scale_x_continuous(breaks=NULL) +
    scale_y_continuous(breaks=NULL)
  p
}

#Plot density
#if itemSensitive=T, the bandwidth of the smoother
#is chosen depending on the number of items used
#to construct the scale. This information is found
#using the ScaleMatrix from the scale construction
#and by specifying the scale level, i.e. the
#hierarchical level in the dot-notation
densLine <- function(data, mapping, itemSensitive=F, 
                     scaleMatrix = NULL, 
                     scaleLevel = NULL, ...) {
  mapping$y <- NULL
  adj <- 1
  if (itemSensitive) {
    nItems <- length(scaleVars(paste(mapping$x),
                               level = scaleLevel,
                               scaleMatrix = scaleMatrix))
    adj <- max(1, 1+5/nItems)  
  }
  p <- ggplot(data=data, mapping=mapping) + 
    geom_line(stat="density", size=0.5, adjust=adj) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.border = element_blank()) +
    scale_x_continuous(breaks=NULL) +
    scale_y_continuous(breaks=NULL)
  p
}

#Plot pairwise correlations
myCor <- function(data, mapping, ...) {
  cols <- colorRampPalette(c("white", "lightblue"))(100)
  cols2 <- colorRampPalette(c("black", "blue"))(100)
  x <- data[, as.character(mapping$x)]
  y <- data[, as.character(mapping$y)]
  corxy <- round(cor(x,y), 2)
  ggally_text(label=paste(corxy), mapping=ggplot2::aes(),
              color=cols2[100*abs(corxy)], ...) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.border = element_blank())
}

#Minimalistic scatter plot matrix
myScatPlot <- function(data, itemSensitive = F, scaleMatrix = NULL,
                       scaleLevel = NULL) {
  ggpairs(data, upper=list(continuous=smoothLine),
          lower=list(continuous=myCor), 
          diag=list(continuous=wrap(densLine, itemSensitive = itemSensitive,
                                    scaleMatrix = scaleMatrix,
                                    scaleLevel = scaleLevel))) +
    theme(axis.ticks=element_blank(), axis.line=element_blank(),
          axis.text=element_blank(),
          axis.text.x=element_blank()) 
}


#Plot multiple distributions for data description. Numerical
# variables are plotted using histograms with estimated densities,
# factor variables are plotted using barplots
#var: variable names of variables in data to plot
#labels: string vector in the same order as var
# specifying the name that should be printed for each variable
#makeCont: list of variables that should be forced to be 
# numeric (and plotted with histogram) no matter their type
#makeCat: list of variables that should be forced to be 
# categorical (and plotted using barplot) no matter their type
#catStyle: either "bars" - colored barplot with level labels
# printed on top of the bars
# or "stacked" where one bar of length 1 colored by frequencies
#   within each category is plotted
#adjustFunction: function used to adjust bandwidth for the
# density estimates of the numerical variables
myDistrPlots <- function(data, var, labels=NULL,
                         makeCont=NULL,
                         makeCat=NULL,
                         catStyle="bars",
                         adjustFunction=function(x){1}) {
  if (is.null(labels)) labels <- var
  n <- length(var)
  outLst <- NULL
  for (i in 1:n) {
    thisVar <- var[i]
    if ((is.factor(data[, thisVar]) & !(thisVar %in% makeCont)) |
        thisVar %in% makeCat) {
      if (thisVar %in% makeCat) {
        data[, thisVar] <- factor(data[, thisVar])
      }
      thisTab <- table(anData[, thisVar])
      thisCount <- as.numeric(c(thisTab))
      tpDat <- data.frame(freq=thisCount/sum(thisCount),
                          lab=factor(names(thisTab), levels=levels(data[, thisVar])))
      if (catStyle == "stacked") {
        p <- ggplot(tpDat, aes(x=1, y=freq, fill=lab,
                               label=lab)) +
          geom_bar(stat="identity") +
          geom_text(position="stack", vjust=1) +
          scale_fill_discrete(breaks=NULL) +
          scale_x_continuous(breaks=NULL) +
          xlab("") +
          ylab("Cummulative frequency") +
          ggtitle(labels[i])
      } else {
        p <- ggplot(tpDat, aes(x=lab, y=freq, fill=lab,
                               label=lab)) +
          geom_bar(stat="identity") +
          geom_text(angle=90, aes(y=0),
                    hjust=-0.01, size=3) +
          scale_x_discrete("", breaks=NULL) +
          ylab("Frequency") + 
          scale_fill_discrete(breaks=NULL)+
          ggtitle(labels[i])
      }
    } else {
      if (thisVar %in% makeCont) {
        data[, thisVar] <- as.numeric(as.character(data[, thisVar]))
      }
      p <- ggplot(data, aes_string(x=thisVar)) +
        geom_histogram(mapping=aes(y=..density..),
                       col="white", fill="#00BFC4",
                       bins=20) +
        geom_line(size=1, col="black", stat="density",
                  adjust=adjustFunction(data[, thisVar])) +
        xlab("") + 
        ylab("Density") +
        ggtitle(labels[i])
    }
    outLst <- c(outLst, list(p))
  }
  outLst
}


#Plot the model graph of a lvm or lvmfit object
#ignore: variables to ignore (not plot)
#labels: names list of labels to use instead of variable names
#ignoreGroup: Not yet implemented
#colVar: Not yet implemented
#col: Not yet implemented
#dotSimplify: Use dot-string structure to collapse variables 
# that only differ by their post-dot annotation together. 
# For instance, if TRUE, a.1 and a.2 will be collapsed into a
#orientation: Should be graph by arranged from left to right ("LR")
# or from the top to the bottom? ("TB")
mySemPlot <- function(lavaObj, ignore=NULL, labels=NULL, ignoreGroup=NULL,
                      colVar=NULL, col=F, dotSimplify=T, orientation="LR") {
  M <- lavaObj$M
  V <- lavaObj$cov
  if (dotSimplify) {
    vnames <- dStrSep(1, rownames(M))
    rownames(M) <- colnames(M) <- vnames
    rownames(V) <- colnames(V) <- vnames
    useInds <- !duplicated(rownames(M))
    M <- M[useInds, useInds]
    V <- V[useInds, useInds]
  } else {
    vnames <- Vectorize(gsub, "x")("\\.", "_", x=rownames(M))
    rownames(M) <- colnames(M) <- vnames
    rownames(V) <- colnames(V) <- vnames
  }
  var <- rownames(M)
  V[upper.tri(V, diag=T)] <- 0
  if (!is.null(ignore)) {
    usePlaces <- which(!(var %in% dStrSep(1, ignore)))
  } else usePlaces <- 1:length(var)
  var <- rownames(M)
  latVar <- which(var %in% intersect(latent(lavaObj), var[usePlaces]))
  obsVar <- setdiff(usePlaces, latVar)
  
  arrowVec <- c("dropme")
  for (i in usePlaces) {
    thisVar <- var[i]
    thisMRow <- M[i, ]
    thisVRow <- V[i, ]
    onePlacesM <- intersect(which(thisMRow==1), usePlaces)
    onePlacesV <- intersect(which(thisVRow==1), usePlaces)
    if (length(onePlacesM) > 0) {
      arrowVec <- c(arrowVec, paste(thisVar, var[onePlacesM], sep="->"))
    } 
    if (length(onePlacesV) > 0) {
      arrowVec <- c(arrowVec, paste(thisVar, paste(var[onePlacesV], "[dir=both]"),
                                    sep="->"))
    }
  }
  if (!is.null(labels)) {
    for (i in 1:length(labels)) {
      thisLab <- names(labels[i])
      labPlace <- which(var==thisLab)
      if (length(labPlace)==1) {
        varPlace <- which(var==thisLab)
        var[varPlace] <- paste(var[varPlace], " [label='", labels[[i]], "']", sep="")
      }
    }
  }
  latVar <- var[latVar]
  obsVar <- var[obsVar]
  
  arrows <- paste(arrowVec[-1], collapse=";")
  
  grViz(paste("
              digraph {
              graph [layout = dot, rankdir=", orientation, "]
              node [shape = oval]",
              paste(latVar, collapse=";"),
              "node [shape = box]",
              paste(obsVar, collapse=";"),
              paste("node [shape = oval, color='", thisCol, 
                    "', fontcolor='", thisCol, "']", sep=""),
              colLatVar,
              paste("node [shape = box, color='", thisCol,
                    "', fontcolor='", thisCol, "']", sep=""),
              colObsVar,
              arrows,
              "}", sep="\n"
  ))
}


#Plot modification indices from a lavaan modIndices() call
miPlot <- function(mi, exclude=NULL) {
  if (!is.null(exclude)) {
    mi <- mi[!(mi$lhs %in% exclude) & !(mi$rhs %in% exclude), ]
  }
  cols <- rev(paste("gray", 0:100, sep=""))
  mi <- mi[which(mi$op=="~~"), c("mi", "lhs", "rhs")]
  miMax <- max(mi$mi)
  rel <- round(mi$mi/miMax, 2)*100
  pcol <- cols[rel+1]
  pcol <- paste("[1]: c('", paste(pcol, collapse="','"), "')", sep="")
  lhs <- gsubv("\\.", "_", mi$lhs)
  rhs <- gsubv("\\.", "_", mi$rhs)
  
  nL <- nrow(mi) 
  edges <- rep(NA, nL)
  for (i in 1:nL) {
    edges[i] <- paste(lhs[i], " -> ", rhs[i], 
                      " [dir='both', color=@@1-",i, 
                      ", arrowhead=none, arrowtail=none]", sep="")  
  }
  nodes <- union(lhs, rhs)
  grViz(paste("
              digraph {
              graph [layout = circo]
              node [shape = box]",
              paste(nodes, collapse=";"), "\n",
              paste(edges, collapse=";"),
              "} \n", 
              pcol))
              }

#Plots the central part of my model with annotated parameter 
#estimates and colouring by significance
#Very problem specific, but the structure can easily 
#be translated into a general lavaanObj plotter
mySemPlot_central <- function(lavaanObj, digit=2, sig.level=0.05,
                              group=NULL, allObs=F) {
  if (allObs) {
    box1Shape <- "box"
  } else box1Shape <- "oval"
  plotTitle <- ""
  nGroups <- lavaanObj@Data@ngroups
  pt <- parameterEstimates(lavaanObj)
  if (nGroups > 1) {
    groupCat <- lavaanObj@Data@group.label
    if (is.null(group) | !any(group==groupCat)) {
      stop("The model was fitted using multiple groups, but no valid choice of group was supplied")
    }
    groupNum <- which(groupCat==group)
    pt <- pt[pt$group==groupNum, ]
    plotTitle <- paste(lavaanObj@Data@group, ":", groupCat[groupNum], sep="")
  }
  D.H <- pt$est[which(pt$rhs=="DEMANDS" & pt$lhs=="HEALTH")]
  C.H <- pt$est[which(pt$rhs=="CONTENTS" & pt$lhs=="HEALTH")]
  R.H <- pt$est[which(pt$rhs=="RELATIONS" & pt$lhs=="HEALTH")]
  S.H <- pt$est[which(pt$rhs=="SITUATION" & pt$lhs=="HEALTH")]
  G.H <- pt$est[which(pt$rhs=="SEG_KOEN" & pt$lhs=="HEALTH")]
  G2.H <- pt$est[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="HEALTH")]
  G.D <- pt$est[which(pt$rhs=="SEG_KOEN" & pt$lhs=="DEMANDS")]
  G.C <- pt$est[which(pt$rhs=="SEG_KOEN" & pt$lhs=="CONTENTS")]
  G.R <- pt$est[which(pt$rhs=="SEG_KOEN" & pt$lhs=="RELATIONS")]
  G.S <- pt$est[which(pt$rhs=="SEG_KOEN" & pt$lhs=="SITUATION")]
  G2.D <- pt$est[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="DEMANDS")]
  G2.C <- pt$est[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="CONTENTS")]
  G2.R <- pt$est[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="RELATIONS")]
  G2.S <- pt$est[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="SITUATION")]
  
  pD.H <- pt$pvalue[which(pt$rhs=="DEMANDS" & pt$lhs=="HEALTH")]
  pC.H <- pt$pvalue[which(pt$rhs=="CONTENTS" & pt$lhs=="HEALTH")]
  pR.H <- pt$pvalue[which(pt$rhs=="RELATIONS" & pt$lhs=="HEALTH")]
  pS.H <- pt$pvalue[which(pt$rhs=="SITUATION" & pt$lhs=="HEALTH")]
  pG.H <- pt$pvalue[which(pt$rhs=="SEG_KOEN" & pt$lhs=="HEALTH")]
  pG2.H <- pt$pvalue[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="HEALTH")]
  pG.D <- pt$pvalue[which(pt$rhs=="SEG_KOEN" & pt$lhs=="DEMANDS")]
  pG.C <- pt$pvalue[which(pt$rhs=="SEG_KOEN" & pt$lhs=="CONTENTS")]
  pG.R <- pt$pvalue[which(pt$rhs=="SEG_KOEN" & pt$lhs=="RELATIONS")]
  pG.S <- pt$pvalue[which(pt$rhs=="SEG_KOEN" & pt$lhs=="SITUATION")]
  pG2.D <- pt$pvalue[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="DEMANDS")]
  pG2.C <- pt$pvalue[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="CONTENTS")]
  pG2.R <- pt$pvalue[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="RELATIONS")]
  pG2.S <- pt$pvalue[which(pt$rhs=="SEG_KOEN2" & pt$lhs=="SITUATION")]
  
  valsSemPlotCentral <- round(c(D.H, C.H, R.H, S.H, G.H, G2.H, G.D, G.C, G.R, 
                                G.S, G2.D, G2.C, G2.R, G2.S), digit)
  sigSemPlotCentral <- c(pD.H, pC.H, pR.H, pS.H, pG.H, pG2.H, pG.D, pG.C, pG.R, 
                         pG.S, pG2.D, pG2.C, pG2.R, pG2.S) > sig.level
  sigSemPlotCentral <- ifelse(sigSemPlotCentral, "black", "navyblue")
  
  assign("valsSemPlotCentral", valsSemPlotCentral, .GlobalEnv)
  assign("sigSemPlotCentral", sigSemPlotCentral, .GlobalEnv)
  assign("titleSemPlotCentral", plotTitle, .GlobalEnv)
  
  grViz(paste("
              digraph {
              
              node [shape = ", box1Shape, "]
              Demands; Contents; Relations; Situation; Health
              
              node [shape = box]
              GS [label='Gender seg.']; GS2 [label='Gender seg., squared']
              
              Demands -> Health [label=@@1-1, color=@@2-1, fontcolor=@@2-1, fontsize=10]; 
              Contents -> Health [label=@@1-2, color=@@2-2, fontcolor=@@2-2, fontsize=10]; 
              Relations -> Health [label=@@1-3, color=@@2-3, fontcolor=@@2-3, fontsize=10]; 
              Situation -> Health [label=@@1-4, color=@@2-4, fontcolor=@@2-4, fontsize=10];
              GS -> Health [label=@@1-5, color=@@2-5, fontcolor=@@2-5, fontsize=10];
              GS2 -> Health [label=@@1-6, color=@@2-6, fontcolor=@@2-6, fontsize=10]; 
              GS -> Demands [label=@@1-7, color=@@2-7, fontcolor=@@2-7, fontsize=10];
              GS -> Contents [label=@@1-8, color=@@2-8, fontcolor=@@2-8, fontsize=10];
              GS -> Relations [label=@@1-9, color=@@2-9, fontcolor=@@2-9, fontsize=10];
              GS -> Situation [label=@@1-10, color=@@2-10, fontcolor=@@2-10, fontsize=10];
              GS2 -> Demands [label=@@1-11, color=@@2-11, fontcolor=@@2-11, fontsize=10]; 
              GS2 -> Contents [label=@@1-12, color=@@2-12, fontcolor=@@2-12, fontsize=10]; 
              GS2 -> Relations [label=@@1-13, color=@@2-13, fontcolor=@@2-13, fontsize=10]; 
              GS2 -> Situation [label=@@1-14, color=@@2-14, fontcolor=@@2-14, fontsize=10]
              
              labelloc='t';
              label='';
              }
              
              [1]: valsSemPlotCentral
              [2]: sigSemPlotCentral
              ", sep="")) 
}





