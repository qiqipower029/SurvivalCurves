#----------------------------- Survival Curves -----------------------------#
#----------------------------- Jieqi Tu -----------------------------#

library(tidyverse)
library(flexsurv)
library(survival)
library(MASS)
# Read in dataset
survival = readxl::read_excel("./Data/Actionable-Outcomes Table.v5_GM_3152021.xlsx")
survival$Cencored = ifelse(survival$Cencored == 1, 0, 1)
Surv(survival$`PFS (days)`, survival$Cencored, type='right')

# ------------- Kaplan Meier Curve for PFS------------- #
PFS=survfit(Surv(survival$PFS, survival$Cencored, type='right')~1, conf.type='log')
plot(PFS, conf.int = FALSE, mark.time = TRUE,xlab="Days", ylab="Survival Probability", main = "Kaplan Meier Curve for PFS", cex.lab=1, cex.main=1)


# ------------- Kaplan Meier Curve for OS ------------- #
OS=survfit(Surv(survival$OS, survival$Cencored, type='right')~1, conf.type='log')
plot(OS, conf.int = FALSE, mark.time = TRUE,xlab="Days", ylab="Survival Probability", main = "Kaplan Meier Curve for OS", cex.lab=1, cex.main=1)


# ------------- Log-Rank test ------------- #
# ------ PFS ------ #
survdiff(Surv(survival$PFS, survival$Cencored, type='right')~survival$Variants, data=survival) 

# ------ OS ------ #
survdiff(Surv(survival$OS, survival$Cencored, type='right')~survival$Variants, data=survival) 


# ------------- Cox Model ------------- #
cox_pfs = coxph(Surv(survival$PFS, survival$Cencored, type='right') ~ as.factor(survival$Diagnosis) + as.factor(survival$Variants) + as.factor(survival$Prim_vs_met),data=survival,ties = "breslow") 
summary(cox_pfs)

cox_os = coxph(Surv(survival$OS, survival$Cencored, type='right') ~ as.factor(survival$Diagnosis) + as.factor(survival$Variants) + as.factor(survival$Prim_vs_met),data=survival,ties = "breslow") 
summary(cox_os)

# ------------- Descriptive Statistics ------------- #

