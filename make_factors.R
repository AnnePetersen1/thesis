#Create factor variables, add labels
janej <- c("Yes", "No")
grad <- c("To a very large degree", "To a large degree", "Partly", 
          "To a small degree", "To a very small degree")
hyppig <- c("Always", "Often", "Sometimes", "Rarely", 
            "Never/almost never")

dagl_nej <- c("Yes, daily or almost daily", 
              "Yes, weekly",
              "Yes, monthly",
              "Yes, less than once a month", 
              "No")

jafler_nej <- c("Yes, multiple times", "Yes, once", "No")

hyppig2 <- c("Never", "Almost never", "Rarely", "Sometimes",
             "Often", "Very often", "Always")

hyppig3 <- c("All the time", "Most of the time", 
             "Little more than half the time",
             "Little less than half the time",
             "A small part of the time", "At no time")

hvorgod <- c("Excellent", "Very good", "Good", "Okay",
             "Bad")

pruge <- c("More than 4 h", "2-4 h", "Less than 2 t", "Do not do this")


data$UNDERSTYPE1 <- factor(data$UNDERSTYPE1, 
                           labels=c("General office clerks",
                                    "Draughtspersons",
                                    "Research and education",
                                    "Health care assistants",
                                    "Primary school teachers",
                                    "Doctors",
                                    "Mail carriers",
                                    "Butchers",
                                    "Smiths",
                                    "Engineers",
                                    "Sales assistants",
                                    "Financial advisors",
                                    "Business managers",
                                    "Police officers"))

data$KOEN <- factor(data$KOEN, labels=c("M","K"))

data$ALDERGRP <- factor(data$ALDERGRP, labels=c("18-24", "25-34", 
                                                "35-44", "45-54",
                                                "55+", "18-34"))

data$METODE <- factor(data$METODE, labels=c("Web","Questionnaire",
                                            "No response"))

data$INT_RESULTAT <- factor(data$INT_RESULTAT, 
                            labels=c("Svar", "Ikke truffet", "Nægter",
                                     "Øvrigt bortfald", "Sprogvansk.",
                                     "Ikke kontakt på tlf",
                                     "Ikke fundet tlf",
                                     "Telefonrykket u. svar",
                                     "Uafsluttet webint.",
                                     "Blankt skema"))

data$Q01 <- factor(data$Q01, 
                   labels=c("Ufaglært arbejder eller specialarbejder",
                            "Faglært arbejder",
                            "Funktioær, tjenestemand, lønmodtager",
                            "Ledende funktionær, tjenestemand",
                            "Selvstændig (inkl. selvstændig landmand)",
                            "Medhjælpende ægtefælle",
                            "Studerende i arbejde min. 8 timer/uge",
                            "Elev/lærling",
                            "I aktivering",
                            "Skånejob, flexjob",
                            "Værnepligt",
                            "Langtidssygemeldt (fortsat ansat",
                            "Orlov",
                            "Andet lønnet arbejde",
                            "Under uddannelse",
                            "Langtidssygemeldt",
                            "Hjemmegående",
                            "Arbejdsløs på dagpenge",
                            "På efterløn",
                            "Andet ikke-arbejde",
                            "Uoplyst"))

data$MAIN_Q01 <- factor(data$MAIN_Q01, labels=c("I arbejde",
                                                "Ikke i arbejde",
                                                "Uoplyst"))

data$Q02 <- factor(data$Q02,
                   labels=c("Folkeskole, ingen afg.prøve",
                            "Folkeskole, afg.prøve",
                            "Gymnasial udd.",
                            "Erhvervsudd.",
                            "KVU",
                            "MVU",
                            "LVU",
                            "Anden udd."), ordered=T)

data$Q03 <- factor(data$Q03, labels=c("Permanent position",
                                      "Temporary position"))

data$Q04 <- factor(data$Q04, labels=janej)

data$Q06 <- factor(data$Q06, labels=c("Fast dagarbejde",
                                      "Fast aftenarbejde",
                                      "Fast natarbejde",
                                      "Skiftende, med nat",
                                      "Skiftende, uden nat"))

data$Q08 <- factor(data$Q08, labels=janej)

for (i in which(names(data)=="Q09_01"):which(names(data)=="Q10_18")) {
  data[, i] <- factor(data[, i], labels=grad)
}

for (i in which(names(data)=="Q11_01"):which(names(data)=="Q12_09_SAMLET")) {
  data[, i] <- factor(data[, i], labels=hyppig)
}

for (i in which(names(data)=="Q13_01"):which(names(data)=="Q13_12")) {
  data[, i] <- factor(data[, i], labels=grad)
}

data$Q13_13 <- factor(data$Q13_13, labels=c("Alt for stor",
                                            "For stor", "Passende",
                                            "For lille", 
                                            "Alt for lille"))

for (i in which(names(data)=="Q14_01"):which(names(data)=="Q14_13")) {
  data[, i] <- factor(data[, i], labels=grad)
}

data$Q15 <- factor(data$Q15, labels=dagl_nej, ordered=T)

data$Q16 <- factor(data$Q16, labels=dagl_nej, ordered=T)

data$Q17 <- factor(data$Q17, labels=dagl_nej, ordered=T)

data$Q18 <- factor(data$Q18, labels=dagl_nej, ordered=T)

data$Q19 <- factor(data$Q19, labels=dagl_nej, ordered=T)

data$Q20 <- factor(data$Q20, labels=dagl_nej, ordered=T)

for (i in which(names(data)=="Q21_01"):which(names(data)=="Q21_16")) {
  data[, i] <- factor(data[, i], labels=grad)
}

for (i in which(names(data)=="Q22_01"):which(names(data)=="Q22_02")) {
  data[, i] <- factor(data[, i], labels=jafler_nej)
}

for (i in which(names(data)=="Q23_01"):which(names(data)=="Q25_12")) {
  data[, i] <- factor(data[, i], labels=grad)
}

for (i in which(names(data)=="Q26_01"):which(names(data)=="Q26_09")) {
  data[, i] <- factor(data[, i], labels=hyppig2)
}

data$Q27 <- factor(data$Q27, ordered=T)
data$Q28 <- factor(data$Q28, ordered=T)
data$Q29 <- factor(data$Q29, ordered=T)

data$Q30 <- factor(data$Q30, labels=c("Excellent", "Very good",
                                      "Good", "Less than good", "Bad"),
                   ordered=T)

for (i in which(names(data)=="Q31_01"):which(names(data)=="Q31_20")) {
  data[, i] <- factor(data[, i], labels=hyppig3)
}

for (i in which(names(data)=="Q32_01"):which(names(data)=="Q32_03")) {
  data[, i] <- factor(data[, i], labels=grad)
}

data$Q33 <- factor(data$Q33, label=hyppig, ordered=T)

data$Q34 <- factor(data$Q34, label=c("Arbejde", "Privatliv", "Begge"))

for (i in which(names(data)=="Q35_01"):which(names(data)=="Q35_04")) {
  data[, i] <- factor(data[, i], labels=hyppig)
}

data$Q36 <- factor(data$Q36, ordered=T)

data$Q37_01 <- factor(data$Q37_01, labels=hvorgod, ordered=T)
data$Q37_02 <- factor(data$Q37_02, labels=hvorgod, ordered=T)

data$Q42 <- factor(data$Q42, labels=c("Yes, daily", 
                                      "Yes, sometimes", 
                                      "Former smoker",
                                      "No"), ordered=T)

for (i in which(names(data)=="Q44_01"):which(names(data)=="Q44_03")) {
  data[, i] <- factor(data[, i], labels=pruge[-4])
}

data$Q45 <- factor(data$Q45, labels=janej)
data$Q46 <- factor(data$Q46, labels=janej)


#reverse order of levels for variables such that scales are always in the same
#"direction" from good to bad. 
data$Q11_06 <- factor(data$Q11_06, levels=rev(hyppig))
data$Q11_06_SAMLET <- factor(data$Q11_06_SAMLET, levels=rev(hyppig))
data$Q11_06_MOD <- factor(data$Q11_06_MOD, levels=rev(hyppig))

data$Q24_04 <- factor(data$Q24_04, levels=rev(grad))
