#Checking for Outliers and replacing them with mean
sum(is.na(D))

boxplot(D$Bo_Age)
Bo_Age_OutLiers = boxplot.stats(D$Bo_Age)$out
D$Bo_Age = ifelse(D$Bo_Age%in%Bo_Age_OutLiers,NA,D$Bo_Age)
boxplot(D$Bo_Age)

boxplot(D$Ln_Orig)
Ln_Orig_OutLiers = boxplot.stats(D$Ln_Orig)$out
D$Ln_Orig = ifelse(D$Ln_Orig%in%Ln_Orig_OutLiers, NA, D$Ln_Orig)
D$Ln_Orig[is.na(D$Ln_Orig)] = mean(D$Ln_Orig,na.rm=T)
boxplot(D$Ln_Orig)

boxplot(D$Orig_LTV_Ratio_Pct)
Orig_LTV_Ratio_Pct_OutLiers = boxplot.stats(D$Orig_LTV_Ratio_Pct)$out
D$Orig_LTV_Ratio_Pct = ifelse(D$Orig_LTV_Ratio_Pct%in% Orig_LTV_Ratio_Pct_OutLiers, NA, D$Orig_LTV_Ratio_Pct)
D$Orig_LTV_Ratio_Pct[is.na(D$Orig_LTV_Ratio_Pct)] = mean(D$Orig_LTV_Ratio_Pct,na.rm=T)
boxplot(D$Orig_LTV_Ratio_Pct)

boxplot(D$Credit_score)
Credit_score_OutLiers = boxplot.stats(D$Credit_score)$out
D$Credit_score = ifelse(D$Credit_score%in% Credit_score_OutLiers, NA, D$Credit_score)
D$Credit_score[is.na(D$Credit_score)] = mean(D$Credit_score,na.rm=T)
boxplot(D$Credit_score)

boxplot(D$Tot_mthly_debt_exp)
Tot_mthly_debt_exp_OutLiers = boxplot.stats(D$Tot_mthly_debt_exp)$out
D$Tot_mthly_debt_exp = ifelse(D$Tot_mthly_debt_exp%in% Tot_mthly_debt_exp_OutLiers, NA, D$Tot_mthly_debt_exp)
D$Tot_mthly_debt_exp[is.na(D$Tot_mthly_debt_exp)] = mean(D$Tot_mthly_debt_exp,na.rm=T)
boxplot(D$Tot_mthly_debt_exp)

boxplot(D$Tot_mthly_incm)
Tot_mthly_incm_OutLiers = boxplot.stats(D$Tot_mthly_incm)$out
D$Tot_mthly_incm = ifelse(D$Tot_mthly_incm%in% Tot_mthly_incm_OutLiers, NA, D$Tot_mthly_incm)
D$Tot_mthly_incm[is.na(D$Tot_mthly_incm)] = mean(D$Tot_mthly_incm,na.rm=T)
boxplot(D$Tot_mthly_incm)

boxplot(D$orig_apprd_val_amt)
orig_apprd_val_amt_OutLiers = boxplot.stats(D$orig_apprd_val_amt)$out
D$orig_apprd_val_amt = ifelse(D$orig_apprd_val_amt%in% orig_apprd_val_amt_OutLiers, NA, D$orig_apprd_val_amt)
D$orig_apprd_val_amt[is.na(D$orig_apprd_val_amt)] = mean(D$orig_apprd_val_amt,na.rm=T)
boxplot(D$orig_apprd_val_amt)

boxplot(D$pur_prc_amt)
pur_prc_amt_OutLiers = boxplot.stats(D$pur_prc_amt)$out
D$pur_prc_amt = ifelse(D$pur_prc_amt%in% pur_prc_amt_OutLiers, NA, D$pur_prc_amt)
D$pur_prc_amt[is.na(D$pur_prc_amt)] = mean(D$pur_prc_amt,na.rm=T)
boxplot(D$pur_prc_amt)

boxplot(D$DTI.Ratio)
DTI.Ratio_OutLiers = boxplot.stats(D$DTI.Ratio)$out
D$DTI.Ratio = ifelse(D$DTI.Ratio%in% DTI.Ratio_OutLiers, NA, D$DTI.Ratio)
D$DTI.Ratio[is.na(D$DTI.Ratio)] = mean(D$DTI.Ratio,na.rm=T)
boxplot(D$DTI.Ratio)

boxplot(D$median.income....)
median.income...._OutLiers = boxplot.stats(D$median.income....)$out
D$median.income.... = ifelse(D$median.income....%in% median.income...._OutLiers, NA, D$median.income....)
D$median.income....[is.na(D$median.income....)] = mean(D$median.income....,na.rm=T)
boxplot(D$median.income....)

boxplot(D$X..of.people.in.poverty)
X..of.people.in.poverty_OutLiers = boxplot.stats(D$X..of.people.in.poverty)$out
D$X..of.people.in.poverty = ifelse(D$X..of.people.in.poverty%in% X..of.people.in.poverty_OutLiers, NA, D$X..of.people.in.poverty)
D$X..of.people.in.poverty[is.na(D$X..of.people.in.poverty)] = mean(D$X..of.people.in.poverty,na.rm=T)
boxplot(D$X..of.people.in.poverty)

boxplot(D$LoanValuetoAppraised)
LoanValuetoAppraised_OutLiers = boxplot.stats(D$LoanValuetoAppraised)$out
D$LoanValuetoAppraised = ifelse(D$LoanValuetoAppraised%in% LoanValuetoAppraised_OutLiers, NA, D$LoanValuetoAppraised)
D$LoanValuetoAppraised[is.na(D$LoanValuetoAppraised)] = mean(D$LoanValuetoAppraised,na.rm=T)
boxplot(D$LoanValuetoAppraised)