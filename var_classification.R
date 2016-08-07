#Variables all respondents should answer
#Note: We do not use the "SAMLET" versions of the variables with
#this option
rVars <- c("Q01", "Q02", "Q03", "Q05", "Q05A", "Q06", 
           "Q07_01", "Q07_02", "Q08", 
           paste("Q09_0", 1:9, sep=""), 
           paste("Q09_", 10:16, sep=""),
           paste("Q10_0", 1:9, sep=""),
           paste("Q10_", 10:18, sep=""),
           paste("Q11_0", 1:9, "_SAMLET", sep=""),
           paste("Q11_", 10:15, "_SAMLET", sep=""),
           paste("Q12_0", 1:9, "_SAMLET", sep=""),
           paste("Q13_0", 1:9, sep=""),
           paste("Q13_", 10:13, sep=""),
           paste("Q14_0", 1:9, sep=""),
           paste("Q14_", 10:13, sep=""),
           "Q15", "Q16", "Q17", "Q18", "Q19",
           "Q20", paste("Q21_0", 1:9, sep=""),
           paste("Q21_", 10:16, sep=""),
           "Q22_01", "Q22_02", 
           paste("Q24_0", 1:9, sep=""),
           paste("Q24_", 10:14, sep=""),
           paste("Q25_0", 1:9, sep=""),
           paste("Q25_", 10:12, sep=""),
           paste("Q26_0", 1:9, sep=""),
           "Q27", "Q28", "Q29", "Q30", 
           paste("Q31_0", 1:9, sep=""),
           paste("Q31_", 10:20, sep=""),
           paste("Q32_0", 1:3, sep=""),
           "Q33", paste("Q35_0", 1:4, sep=""),
           "Q36", "Q37_01", "Q37_02", "Q38",
           "Q39", "Q40", "Q41", "Q42", "Q43",
           paste("Q44_0", 1:3, sep=""),
           "Q45", "Q46", #structural variables below
           "LOBENR", "UNDERSTYPE1", "KOEN", "ALDERGRP",
           "METODE", "INT_DATO", "INT_RESULTAT",
           "MAIN_Q01", "TIMERTOT", "ANTMISS",
           "ANTUDF", "ANS_AAR",
           "ARB_TYPE", "VIDEN_GRAD")


#Variables associated with "outcome" type variables,
#that is, health, satisfaction, stress etc.
outVars <- c("Q27", "Q30", "Q33", "Q36",
             "Q38", "Q39")

#SD variables
strucVars <- c("UNDERSTYPE1", "KOEN", "ALDERGRP", "METODE", "INT_RESULTAT", "INT_DATO")
strucVars2 <- strucVars[-6]

#MOD/FS-affected variables
MODVAR <-  c(paste("Q11_0", 1:9, "_SAMLET", sep=""), 
             paste("Q11_",10:15, "_SAMLET", sep=""),
             paste("Q12_0", 1:9,"_SAMLET", sep=""))

mfsVars <- c(paste("Q11_0", 1:9, "_MOD", sep=""), 
             paste("Q11_",10:15, "_MOD", sep=""),
             paste("Q12_0", 1:9,"_MOD", sep=""),
             paste("Q11_0", 1:9, sep=""), 
             paste("Q11_",10:15, sep=""),
             paste("Q12_0", 1:9, sep=""),
             "Q27_PLACERET_FOERST", 
             "Q27_PLACERET_SIDST",
             "Q28_PLACERET_FOERST",
             "Q28_PLACERET_SIDST")
             

#Ordinal/quatitatie variables that are not scales nor involved in scale
#constructing and that all respondents should answer
ordVars <- c("KOEN", "Q03", "Q05", "Q05A", "Q08", "Q15", "Q16", "Q17", 
             "Q18", "Q19", "Q20", "Q29", "Q30", "Q33", "Q36", "Q37_01", 
             "Q37_02", "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q45", 
             "Q46", "ALDERGRP2", "ANS_AAR", "VIDEN_GRAD")

useVars <- c("KOEN", "Q03", "Q05", "Q08",
             "Q30", "Q36", "Q37_01", "Q37_02", "Q38", "Q39", "Q42",
             "Q43", "EXERCISE", "MDI", "STRESS", "FAM_STATUS", "ARB_TYPE", 
             "VIDEN_GRAD", "ANS_AAR", "ALDERGRP2", "BMI", "SEG_KOEN",
             as.character(scaleNames2$scale[-c(25, 26)]))

#variables belonging to each dimension
S1Vars <- paste("S1", 1:5, sep=".")
S2Vars <- paste("S2", 1:8, sep=".")
S3Vars <- paste("S3", c(1:5, 7:12), sep=".")
S5Vars <- paste("S5", 1:5, sep=".")
allScales <- c(S1Vars, S2Vars, S3Vars, S5Vars)

#Health variables
healthVar <- c("BMI", "STRESS", "MDI", "Q30", "Q36", 
               "Q37_01", "Q37_02", "Q38", "Q39", "Q42",
               "Q43")


#Level 1 and level 2 scale variable (classification, but depends on
#setup)
l2Vars <- as.character(scaleNames2$scale)
l1Vars <- as.character(scaleNames1$scale)


#Usevars after complete cases analysis
useVars2 <- c("KOEN", "Q03", "Q05", "Q08",
              "Q30", "Q36", "Q37_01", "Q37_02", "Q38", "Q39", "Q42",
              "Q43", "MDI", "STRESS", "FAM_STATUS", "ARB_TYPE", 
              "VIDEN_GRAD", "ANS_AAR", "ALDERGRP2", "BMI", "SEG_KOEN",
              as.character(scaleNames2$scale[-c(25, 26, 32)]))


#structural variables
strVar <- c("Q03", "Q05", "Q08", "ANS_AAR", "FAM_STATUS", 
            "ARB_TYPE", "VIDEN_GRAD", "ALDERGRP2")
