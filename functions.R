#Formula object producer from vector of variable name strings
formProducer <- function(y=NULL, xs, noResp=F, logResp=F) {
  if (noResp) {
    respStr <- paste("~ ", " ")
  } else if (logResp) {
    respStr <- paste(paste("log(", y, "+0.1)", sep=""), "~", " ")
  } else {
    respStr <- paste(y, "~", " ")
  }
  as.formula(paste(respStr, paste(xs, collapse="+"))) 
}

#Remove element from vector of strings by its name
#for several occurances, all are removed
minusStr <- function(xs, remove) {
  n <- length(remove)
  remove_index <- NULL
  for (i in 1:n) {
    remove_index <- c(remove_index, which(xs==remove[i]))
  }
  xs[-remove_index]
}


#Calculate sds of each column in a matrix
colSDs <- function(x) {
  n <- ncol(x)
  out <- rep(NA, n)  
  for (i in 1:n) {
    out[i] <- sd(x[,i])
  }
  out
}


#Function for arranging ggplot2 plots in grid with shared legend 
#from first plot
#NOT DEVELOPED BY ME
grid_arrange_shared_legend <- function(plots, ...) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    ..., legend,
    heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}


#Get legend from ggplot
#NOT DEVELOPED BY ME
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}



#Use identical itemwise on vectors (vectors must have same lengths)
identVec <- function(x,y) {
  n <- length(x)
  if (n!=length(y)) stop("x and y must have the same length")
  outBool <- rep(NA, n)
  for (i in 1:n) {
    outBool[i] <- identical(x[i], y[i])
  }
  outBool 
}

identVec2 <- function(x,y) {
  n <- length(y)
  outBool <- rep(NA, n)
  for (i in 1:n) {
    outBool[i] <- identical(x, y[i])
  }
  outBool 
}


#Construct aggregated variable
#Consider: 
##speed up
##check for same levels in aggVars
aggrVar <- function(data, aggVars, scores=c(0, 25, 50, 75, 100),
                    missAllow=NULL, reverseScores=F) {
  # browser()
  levs <- levels(data[, aggVars[1]])
  n <- nrow(data)
  if (length(levs) != length(scores)) {
    #   browser()
    stop(paste("Mismatch between number of scores (", length(scores),
               ") and number of levels in aggVars (", length(levs), ") in ",
               paste(aggVars), sep=""))
  }
  missAllowUse <- NULL
  nItems <- length(aggVars)
  if (is.null(missAllow)) missAllowUse <- nItems-1 
  if (reverseScores) scores <- rev(scores)
  outVar <- rep(0, n)
  for (i in 1:n) {
    if (is.null(missAllowUse)) {
      missAllowUse <- min(floor(nItems*missAllow), nItems-1)
    }
    iVals <- data[i, aggVars]
    iMiss <- sum(is.na(iVals))
    if (iMiss > missAllowUse) {
      outVar[i] <- NA
    } else {
      for (j in 1:length(iVals)) {
        jVal <- as.character(iVals[j][[1]])
        outVar[i] <- outVar[i] + sum(identVec2(jVal, levs)*scores)
      }
      outVar[i] <- outVar[i]/(length(iVals)-iMiss)
    }
  }
  outVar
}


#Create scale variables from matrix of variable names (first col, name "var")
#and their scale categorization (second col, name "scale")
scaleVarGen <- function(data, scaleMat, scores=c(0,25,50,75,100), 
                        missAllow=NULL, 
                        reverseScores=F) {
  oneScore <- T
  scaleMat$scale <- as.character(scaleMat$scale)
  if (any(!(scaleMat$name %in% names(data)))) {
    stop("Scale names do not match variable names in data")
  }
  if (is.list(scores)) {
    if (any(!(names(scores) %in% scaleMat$scale))) {
      stop("Score names do not match variable names in data")
    }
    oneScore <- F
  }
  uniScales <- as.character(na.omit(unique(scaleMat$scale)))
  nScales <- length(uniScales)
  for (i in 1:nScales) {
    scale <- uniScales[i]
    vars <- scaleMat[identVec2(scale, scaleMat$scale), "var"]
    if (oneScore) {
      useScores <- scores
    } else {
      useScores <- scores[scale][[1]]
    }
     data[, scale] <- aggrVar(data, vars, useScores, missAllow,
                             reverseScores)
  }
  data
}


#Makes names list with scores for each provided scale in "scales" 
#SOMETHING ABOUT GENERAL FUNCTIONALITY
#If simple mode is chosen (using the option simple=T), the function
#find each variable corresponding to a scale in "scales" using the
#scale matrix "ScaleMat" and checks the number of levels of this variable
#in "data". Then a scale is constructed automatically with equal 
#spacing and limits as provided in "scaleLim".
SListCreator <- function(scales, scoreCatPairList=NULL,
                         stdScore=c(0,25,50,75,100), simple=F, 
                         data=NULL, scaleMat=NULL,
                         scaleLim=c(0,100)) {
  scales <- unique(na.omit(scales))
  if (simple) {
    if (is.null(data)) error("Data set must be specified in simple mode")
    if (is.null(scaleMat)) error("Scale matrix must be specified in simple mode")
  }
  specialScore <- names(scoreCatPairList)
  outList <- NULL
  for (i in 1:length(scales)) {
    thisScale <- as.character(scales[i])
    thisScore <- stdScore
    if (thisScale %in% specialScore) {
      thisScore <- scoreCatPairList[as.character(thisScale)][[1]]
    }
    if (simple) {
      var <- as.character(na.omit(scaleMat[scaleMat$scale==thisScale, "var"])[1])
      nLevs <- length(levels(data[, var]))
      a <- scaleLim[1]
      b <- scaleLim[2]
      if (nLevs <= 2) {
        if (nLevs < 2) error(paste("Less than two levels for scale", thisScale))
        thisScore <- c(a,b)
      } else {
        thisScore <- c(a, a + (b-a)/(nLevs-1)*(1:(nLevs-2)), b) 
      }
    }
    outList <- c(outList, setNames(list(thisScore), thisScale))
  }
  outList
}


#Dot seperated string separator - for a dot separated string, e.g. "ab.cda.12", 
#the function returns the n first entries, e.g. for n=2, we have "ab.cda" and 
#for n>=3, we find "ab.cda.12". n=0 returns "". Vectors of strings are allowed.
dStrSep <- function(n, str) {
  nstr <- length(str)
  if (n==0) return(rep("", nstr))
  str <- as.character(str)
  outStr <- rep(NA, nstr)
  for (i in 1:nstr) {
    thisStr <- str[i]
    dotplaces <- c(gregexpr(".", thisStr, fixed=T)[[1]])
    if (identical(dotplaces, as.integer(-1)) | length(dotplaces) < n) {
      outStr[i] <- thisStr
    } else {
      lastChar <- dotplaces[n]-1
      outStr[i] <- substr(thisStr, 1, lastChar)
    }
  }
  outStr
}

#as.name(as.character(x)) shorthand
as.nc <- function(x) as.name(as.character(x))


#Make string list of variables associated with a certain scale
#from scaleCat matrix
scaleVars <- function(scale, level=1, scaleMatrix) {
  scale <- dStrSep(level, scale)
  scaleMatrix[, "scale"] <- as.character(dStrSep(level, 
                                                 scaleMatrix[, "scale"]))
  outVars <- as.character(na.omit(scaleMatrix[scaleMatrix$scale==scale, 
                                              "var"]))
  outVars <- subset(outVars, !(outVars  %in% mfsVars))
  outVars
}


#lava to lavaan model converter
#Note: Does NOT take parameter restrictions and such
#into account as of now
#Note: In lavaan observed variables can have two different
#relationships with latent variable, indistinguishable from
#the DAG. Either, there can be a regression effect:
#OBS ~ LATENT
#or there can be an indicator effect
#LATENT =~ OBS
#This function does not distinguish between these two
#effect types (as I'm not sure if they are actually
#different aside from syntax) and codes both as
#LATENT =~ OBS.
#Note also that lava does not have these two different
#types of relationships and thus implementing a differentiated
#coding of the two will not be straight forward. 
lavaToLavaan <- function(lvmObj, extraExo=NULL, noExoCoVar=T,
                         noExoVar=T) {
  M <- lvmObj$M
  V <- lvmObj$cov
  vars <- rownames(M)
  lvar <- latent(lvmObj)
  obsVar <- vars[!(vars %in% lvar)]
  outStr <- ""
  nVar <- nrow(M)
  V[lower.tri(V)] <- 0 #make upper triangular matrix, ignore lower part
  zeroVec <- rep(0, nVar)
  exoVars <- exogenous(lvmObj)
  if (!is.null(extraExo)) {
    exoVars <- c(exoVars, extraExo)
  }
  
  for (l in lvar) { #start with latent var indicators
    indVars <- vars[which(M[l, ]!=0)]
    indVars <- indVars[indVars %in% obsVar]
    if (length(indVars)==0) {
      stop(paste("Latent variable", l, "has no indicators"))
    }
    newStr <- paste(l, "=~", paste(indVars, collapse=" + "), " \n")
    outStr <- paste(outStr, newStr)
    M[l, indVars] <- 0
  }

  for (i in 1:nVar) {
    thisVar <- vars[i]
    if (!identical(suppressWarnings(as.numeric(M[, i])), zeroVec)) {
      explVars <- which(M[,i]!=0) 
      newStr <- paste(thisVar, "~", paste(vars[explVars], 
                                          collapse=" + "), " \n")
      outStr <- paste(outStr, newStr)
    }
    if (!identical(suppressWarnings(as.numeric(V[, i])), zeroVec)) {
      nzeroVars <- which(V[, i]!=0) 
      for (j in nzeroVars) {
        doCov <- T
          #no var for exogenous variables
        if (noExoVar & thisVar %in% exoVars & vars[j] == thisVar) doCov <- F 
          #no cov for exogenous variables if noExoVar=T
        if (noExoCoVar & (thisVar %in% exoVars | vars[j] %in% exoVars)) doCov <- F 
        if (doCov) {
          newStr <- paste(thisVar, "~~", vars[j], " \n")
          outStr <- paste(outStr, newStr)
        }
      }
    } 
  }
  outStr
}


#Extract rmsea values from lavaan object 
rmsea <- function(x) {
  ms <- c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper")
  if (x@Options$test!="standard") {#| x@Options$se != "standard") {
    ms <- paste(ms, ".scaled", sep="")
  }
  fitmeasures(x, fit.measures=ms)
}


#gsub vectorized in x
gsubv <- function(pattern, replacement, x, ...) {
  Vectorize(gsub, "x")(pattern, replacement, x, ...)
}


#Calculate mode of a variable, no matter if it 
#is categorical or quantitative
Mode <- function(x) {
  if (is.numeric(x)) {
    return(median(x))
  } else {
    xCat <- unique(x)
    xMode <- xCat[which.max(table(x))]
    return(xMode[1])
  }
}

#Create data.frame for prediction where intVars are allowed to vary
#in the values given by the user and all other values are set at
#mode values
#Right now only two intVars are allowed
genPredFrame <- function(data, lockVars, intVars, intVarVals=NULL) {
  lockVars <- lockVars[!(lockVars %in% intVars)]
  n <- length(intVars) #=2
  nIntVarVals <- rep(NA, n)
  for (i in 1:n) {
    vals <- unique(intVarVals[[i]])
    nIntVarVals[i] <- length(vals)
    #    intVarOut <- c(intVarOut, vals)
  }
  
  nobs <- prod(nIntVarVals)
  outData <- data[1:nobs,c(lockVars, intVars)]
  
  outData[, intVars[1]] <- rep(intVarVals[[1]], nobs/nIntVarVals[1])
  if (n > 1) {
    outData[, intVars[2]] <- rep(intVarVals[[2]], each=nobs/nIntVarVals[2])
  }
  
  for (i in 1:length(lockVars)) {
    outData[, lockVars[i]] <- Mode(data[, lockVars[i]])
  }
  outData
}

#Get gender seg estimates from lavaan model
getGSEff <- function(lavaanObj, group=NULL, total=T) {
  par <- parameterEstimates(lavaanObj)
  if (!is.null(group)) {
    groupNum <- which(lavaanObj@Data@group.label == group)
    par <- par[par$group== groupNum, ]
  }
  tG <- par[par$lhs=="HEALTH" & par$rhs=="SEG_KOEN", "est"]
  tG2 <- par[par$lhs=="HEALTH" & par$rhs=="SEG_KOEN2", "est"]
  if (total) {
    tG <- tG + par[par$lhs=="DEMANDS" & par$rhs=="SEG_KOEN", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="DEMANDS", "est"] + 
      par[par$lhs=="CONTENTS" & par$rhs=="SEG_KOEN", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="CONTENTS", "est"] +
      par[par$lhs=="RELATIONS" & par$rhs=="SEG_KOEN", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="RELATIONS", "est"] +
      par[par$lhs=="SITUATION" & par$rhs=="SEG_KOEN", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="SITUATION", "est"] 
    tG2 <- tG2 + par[par$lhs=="DEMANDS" & par$rhs=="SEG_KOEN2", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="DEMANDS", "est"] + 
      par[par$lhs=="CONTENTS" & par$rhs=="SEG_KOEN2", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="CONTENTS", "est"] +
      par[par$lhs=="RELATIONS" & par$rhs=="SEG_KOEN2", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="RELATIONS", "est"] +
      par[par$lhs=="SITUATION" & par$rhs=="SEG_KOEN2", "est"]*
      par[par$lhs=="HEALTH" & par$rhs=="SITUATION", "est"] 
  }
  c(tG, tG2) 
}

#Bootstrap for total effect SE
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