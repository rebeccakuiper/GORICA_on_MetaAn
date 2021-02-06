if (!require("metafor")) install.packages("metafor")
library(metafor)
#
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function

library(devtools) # Make sure you have Rtools (and a version which is compatable with your R version).
install_github("rebeccakuiper/ICweights")
library(ICweights)
#?IC.weights # This also contains examples of how to use the function
#citation("ICweights") # In case you use this function, please cite it


#######################################


## Example Berkey et al. (1998) ##
# Based on http://www.metafor-project.org/doku.php/analyses:berkey1998
#
#Berkey et al. (1998) describe a meta-analytic multivariate model for the analysis of multiple correlated outcomes. The use of the model is illustrated with results from 5 trials comparing surgical and non-surgical treatments for medium-severity periodontal disease. Reported outcomes include the change in probing depth (PD) and attachment level (AL) one year after the treatment. The effect size measure used for this meta-analysis was the (raw) mean difference, calculated in such a way that positive values indicate that surgery was more effective than non-surgical treatment in decreasing the probing depth and increasing the attachment level. The data are provided in Table I in the article and are stored in the dataset dat.berkey1998 that comes with the metafor package:
data <- dat.berkey1998
data
#(I copy the dataset into data, which is a bit shorter and therefore easier to type further below). 
#
#So, the results from the various trials indicate that surgery is preferable for reducing the probing depth, while non-surgical treatment is preferable for increasing the attachment level.
#
#Since each trial provides effect size estimates for both outcomes, the estimates are correlated. The v1i and v2i values are the variances and covariances of the observed effects. In particular, for each study, variables v1i and v2i form a 2×2 variance-covariance matrix of the observed effects, with the diagonal elements corresponding to the sampling variances of the mean differences (the first for probing depth, the second for attachment level) and the off-diagonal value corresponding to the covariance of the two mean differences.
#
#A multivariate random-effects model can now be used to meta-analyze the two outcomes simultaneously.
V <- bldiag(lapply(split(data[,c("v1i", "v2i")], data$trial), as.matrix))
metaan <- rma.mv(yi, V, mods = ~ outcome - 1, random = ~ outcome | trial, struct="UN", data=data, method="ML")
print(metaan, digits=3)
#
#This is what Berkey et al. (1998) call a multivariate maximum likelihood (MML) random-effects model.1) Note that rma.mv() uses REML estimation by default, so method="ML" must be explicitly requested. The random = ~ outcome | trial part adds random effects for each outcome within each trial to the model. With struct="UN", the random effects are allowed to have different variances for each outcome and are allowed to be correlated.
#
#The results show that the amount of heterogeneity in the attachment level (AL) outcome (i.e., .026) is larger than the amount of heterogeneity in the probing depth (PD) outcome (i.e., .007). 
#Furthermore, the true outcomes appear to correlate quite strongly (i.e., .70). 
#On average, surgery is estimated to lead to significantly greater decreases in probing depth (i.e., .35), but non-surgery is more effective in increasing the attachment level (i.e., -.34).


# GORICA #

#Substract estmates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("Beta_AL", "Beta_PD")
se_est <- metaan$se
VCOV_est <- metaan$vb

# Hypothesis of interest (fictional)
H1.1 <- "Beta_AL < 0; Beta_PD > 0" 

# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H1.1 <- goric(est, VCOV = VCOV_est, H1.1, comparison = "complement", type = "gorica") 
results_H1.1
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1        H1.1   3.912    0.799  -6.226           1.000
#2  complement  -5.063    1.701  13.529           0.000
#---
#  The order-restricted hypothesis ‘H1.1’ has 19482.703 times more support than its complement.
#
# Thus, there is overwhelming (full) support for H1 versus its complement.
# Note that the complement contains "Beta_AL > 0; Beta_PD < 0", but is not equal to it, 
#     since it also contains "Beta_AL < 0; Beta_PD < 0"  and "Beta_AL > 0; Beta_PD > 0".


#Alternatives:

# Hypothesis of interest (fictional)
H1.1 <- "Beta_AL < 0; Beta_PD > 0" 
H1.2 <- "Beta_AL < 0; Beta_PD < 0" 
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H1 <- goric(est, VCOV = VCOV_est, H1.1, H1.2, type = "gorica") 
results_H1
results_H1$relative.gw
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#           model   loglik  penalty  gorica  gorica.weights
#1           H1.1    3.912    0.799  -6.226           0.769
#2           H1.2  -20.393    1.201  43.189           0.000
#3  unconstrained    3.912    2.000  -3.824           0.231
#> results_H1$relative.gw
#                  vs. H1.1    vs. H1.2 vs. unconstrained
#H1.1          1.000000e+00 53732676772      3.323726e+00
#H1.2          1.861065e-11           1      6.185669e-11
#unconstrained 3.008672e-01 16166399474      1.000000e+00


# Hypothesis of interest (fictional)
H2 <- "-Beta_AL > 0.3; Beta_PD > 0.3"
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, H2, comparison = "complement", type = "gorica") 
results_H2
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H2   3.912    0.799  -6.226           0.734
#2  complement   3.799    1.701  -4.195           0.266
#---
#  The order-restricted hypothesis ‘H2’ has  2.760 times more support than its complement.


# Hypothesis of interest (fictional)
H3 <- "Beta_AL < Beta_PD"
# or, if you know the coefficient for AL is negative and want to compare (absolute) strength:
H3 <- "-Beta_AL < Beta_PD"
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H3 <- goric(est, VCOV = VCOV_est, H3, comparison = "complement", type = "gorica") 
results_H3
# Weights based on penalty values
IC.weights(2*results_H3$result[,3])$IC.weights # Note that the penalty is 2*`penalty'
#
# H3 <- "Beta_AL < Beta_PD"
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#
#Results:
#        model   loglik  penalty   gorica  gorica.weights
#1          H3    3.912    1.500   -4.824           1.000
#2  complement  -52.269    1.500  107.538           0.000
#---
#  The order-restricted hypothesis ‘H3’ has 2506253976084463000486028.000 times more support than its complement.
#
# Thus, there is overwhelimg support for H2 versus its complement (i.e., Beta_AL > Beta_PD).
#
# H3 <- "-Beta_AL < Beta_PD"
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H3   3.912    1.500  -4.824           0.500
#2  complement   3.910    1.500  -4.820           0.500
#---
#  The order-restricted hypothesis ‘H3’ has  1.002 times more support than its complement.
#
# Thus, both hypotheses are equally likely. 
# The log likelihood values are nearly the same and thus the weights largely depend on the penalty values.
# Since both hypotheses are of the same size (i.e., have the same penalty), the weights are the same.
# This implies support for the border of the hypotheses: -Beta_AL = Beta_PD (equal absolute strength)



# AIC #
#
# Note: metafor can give IC values as well, but are not helpful here:
#metaan$fit.stats # for model that is estimated
# Metafor cannot create a competing model with constraints on the effect size parameters! 
# It can compare FE vs RE, but that is not of interest here.
# Therefore, I will use GORICA weights (AIC weights) as proxy to Akaike weights.
#
# Hypothesis of interest - now, no order restriction, because AIC is used.
# Set 1
H01 <- "Beta_AL == 0; Beta_PD == 0" # i.e., Beta_AL == 0, Beta_PD == 0
H02 <- "Beta_PD == 0" # i.e., Beta_AL, Beta_PD == 0
H03 <- "Beta_AL == 0" # i.e., Beta_AL == 0, Beta_PD 
# Note: By default, the unconstrained hypothesis is added to the set.
#
# Set 2
H0 <- "-Beta_AL == Beta_PD"
# Note: This can be compared to its complement, but because of the equality,
# the complement will equal the unconstrained hypothesis.
#
# Set 3
# est # Beta_PD = 0.3448392
# sqrt(diag(VCOV_est)) # se(Beta_PD) = 0.04945984
H0_range <- "-Beta_AL > Beta_PD - 0.04945984*0.3448392; -Beta_AL < Beta_PD + 0.04945984*0.3448392"
# Note: This can be compared to the unconstrained hypothesis.
# Current version of the software cannot handle the complement of range restrictions yet.
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_AIC_Set1 <- goric(est, VCOV = VCOV_est, H01, H02, H03, type = "gorica") 
results_AIC_Set1
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_AIC_Set2 <- goric(est, VCOV = VCOV_est, H0, comparison = "complement", type = "gorica") 
results_AIC_Set2
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_AIC_Set3 <- goric(est, VCOV = VCOV_est, H0_range, type = "gorica") 
results_AIC_Set3
# Weights based on penalty values
IC.weights(2*results_AIC_Set3$result[,3])$IC.weights # Note that the penalty is 2*`penalty'
#
# Set 1
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#           model   loglik  penalty   gorica  gorica.weights
#1            H01  -73.975    0.000  147.950           0.000
#2            H02  -20.393    1.000   42.787           0.000
#3            H03   -5.063    1.000   12.127           0.000
#4  unconstrained    3.912    2.000   -3.824           1.000
# From this, it is concluded that the unconstrained hypothesis is the best hypothesis 
# and has even full support. 
# This implies that the other three hypotheses are weak hypotheses.
#
# Set 2
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   3.910    1.000  -5.820           0.731
#2  complement   3.912    2.000  -3.824           0.269
#---
#  The order-restricted hypothesis ‘H0’ has  2.713 times more support than its complement.
#
# Set 3
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#           model  loglik  penalty  gorica  gorica.weights
#1       H0_range   3.912    1.000  -5.824           0.731
#2  unconstrained   3.912    2.000  -3.824           0.269
#---
#  Restriktor message: Since the constraint matrix is not full row-rank, the level probabilities 
#are calculated using mix.weights = "boot" (the default is mix.weights = "pmvnorm").
#For more information see ?restriktor.
# Notably, in case a hypothesis is not a closed convex cone, which is the case for range restrictions,
# the restriction matrix is not full row-rank. 
# In that case, the penalty is calculated using another function (nl, using bootstrap).


#######################################


## Example van Houwelingen et al. (2002) ##
# based on http://www.metafor-project.org/doku.php/analyses:vanhouwelingen2002
#
#The article by van Houwelingen et al. (2002) is a sequel to the introductory article by Normand (1999) on methods for meta-analysis and focuses on more advanced techniques, such as meta-regression and bivariate/multivariate models. The authors mostly use SAS throughout the article for fitting the various models. The analyses are replicated here using R.
#
#In the first part of the article, the models and methods are illustrated with data from 13 studies examining the effectiveness of the Bacillus Calmette-Guerin (BCG) vaccine for preventing tuberculosis (Colditz et al., 1994). The data are provided in Table I (p. 594) and can be loaded with:
data <- dat.colditz1994
data
#(I copy the dataset into 'data', which is a bit shorter and therefore easier to type further below). Variables tpos and tneg in this dataset indicate the number of individuals that were TB positive and negative in the vaccinated (treatment) group, while cpos and cneg indicate the number of individuals that were TB positive and negative in the non-vaccinated (control) group.
#
#We can calculate the individual log odds ratios and corresponding sampling variances with:
#  
data <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=data)
#In addition, we can recode the year variable as in the table with:
data$year <- data$year - 1900
#
#A random-effects model can be fitted (using maximum likelihood estimation) to the same data with:
metaan <- rma(yi, vi, data=data, method="ML")
metaan


# GORICA #
#
#Substract estmates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("LogOdds")
se_est <- metaan$se
VCOV_est <- metaan$vb
#
# Apply GORICA #
#
# Hypothesis of interest (fictional)
H1 <- "LogOdds < 0" 
# 
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results <- goric(est, VCOV = VCOV_est, H1, comparison = "complement", type = "gorica") 
results
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H1   0.807    0.500  -0.615           1.000
#2  complement  -7.885    0.500  16.770           0.000
#---
#  The order-restricted hypothesis ‘H1’ has 5955.893 times more support than its complement.
#
# Thus, there is overwhelming (full) support for H1 versus its complement (i.e., LogOdds > 0).

# In case you want to specify some value:
#
# Hypothesis of interest (fictional)
H2 <- "LogOdds < -0.5" 
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, H2, comparison = "complement", type = "gorica") 
results_H2
# 
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H2   0.807    0.500  -0.615           0.716
#2  complement  -0.117    0.500   1.234           0.284
#---
#  The order-restricted hypothesis ‘H2’ has  2.520 times more support than its complement.
#
# Now, less support of course (note: complement is now 'LogOdds > -0.5').


# AIC #
#
# Hypothesis of interest
H0 <- "LogOdds == 0" 
# 
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H0 <- goric(est, VCOV = VCOV_est, H0, comparison = "complement", type = "gorica") 
results_H0
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0  -7.885    0.000  15.770           0.000
#2  complement   0.807    1.000   0.385           1.000
#---
#  The order-restricted hypothesis ‘H0’ has  0.000 times more support than its complement.
#
# So, no support for H0 and full support for Hunc, 
# but no quantification for (lack of) support for the hypothesis of interest H1 (or H2).


###


# Based on other model: Meta-Regression
#It may be possible to account for (at least part of) the heterogeneity in the treatments effects (i.e., log odds ratios) based on one or more moderator variables (i.e., study characteristics that may influence the size of the effect). Such analyses are typically conducted with meta-regression models, as described in the article. For example, a meta-regression model which includes the absolute latitude of the study location as a predictor of the treatment effect can be fitted with:
metaan <- rma(yi, vi, mods = ~ ablat, data=data, method="ML")
metaan


# GORICA #
#
#Substract estmates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("LogOdds")
se_est <- metaan$se
VCOV_est <- metaan$vb
#
# Apply GORICA #
#
# Hypothesis of interest (fictional)
# When controlling for 'absolute latitude' the log odds are expected to be positive:
H1 <- "LogOdds > 0" 
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results <- goric(est, VCOV = VCOV_est, H1, comparison = "complement", type = "gorica") 
results
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H1   6.815    1.500  -10.631           0.998
#2  complement   0.703    1.500    1.593           0.002
#---
#  The order-restricted hypothesis ‘H1’ has 451.233 times more support than its complement.

# In case you want to specify some value:
#
# Hypothesis of interest (fictional)
H2 <- "LogOdds > 0.3" 
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, H2, comparison = "complement", type = "gorica") 
results_H2
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H2   6.815    1.500  -10.631           0.556
#2  complement   6.592    1.500  -10.184           0.444
#---
#  The order-restricted hypothesis ‘H2’ has  1.251 times more support than its complement.


# AIC #
#
# Hypothesis of interest
H0 <- "LogOdds == 0" 
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H0 <- goric(est, VCOV = VCOV_est, H0, comparison = "complement", type = "gorica") 
results_H0
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   0.703    1.000   0.593           0.006
#2  complement   6.815    2.000  -9.631           0.994
#---
#  The order-restricted hypothesis ‘H0’ has  0.006 times more support than its complement.
#
# So, almost no support for H0 and almost full support for Hunc, 
# but no quantification for (lack of) support for the hypothesis of interest H1 (or H2).


#######################################


## Example Viechtbauer (2007) based on Linde et al. (2005) ##
# Based on http://www.metafor-project.org/doku.php/analyses:viechtbauer2007b
#
#Viechtbauer (2007) is a general article about meta-analysis focusing in particular on random- and mixed-effects (meta-regression) models. An example dataset, based on a meta-analysis by Linde et al. (2005) examining the effectiveness of Hypericum perforatum extracts (St. John's wort) for treating depression, is used in the paper to illustrate the various methods. The data are given in the paper in Table 1 (p. 105).
#
#The dataset can be recreated with:
data <- escalc(measure="RR", ai=ai, ci=ci, n1i=n1i, n2i=n2i, data=dat.linde2005)
data <- data[c(7:10,13:25), c(13:16,18:19,11,6,7,9)]
data$dosage <- (data$dosage * 7) / 1000
rownames(data) <- 1:nrow(data)
data
#Variables ai and ci indicate the number of participants with significant improvements between baseline and the follow-up assessment in the treatment and the placebo group, respectively, variables n1i and n2i are the corresponding group sizes, variable yi is the log of the relative improvement rate (i.e., the improvement rate in the treatment group divided by the improvement rate in the placebo group), vi is the corresponding sampling variance, dosage is the weekly dosage (in grams) of the Hypericum extract used in each study, major indicates whether a study was restricted to participants with major depression or not (1 or 0, respectively), baseline denotes the average score on the Hamilton Rating Scale for Depression (HRSD) at baseline (i.e., before treatment begin), and duration indicates the number of treatment weeks before response assessment. Variables yi and vi are not actually included in the original dataset and were added by means of the escalc() function.
#
#Note that, for illustration purposes, only a subset of the data from the Linde et al. (2005) meta-analysis are actually included in this example. Therefore, no substantive interpretations should be attached to the results of the analyses given below.
#
#Meta-Regression
#The random-effects model assumes that the heterogeneity in the true (log) relative rates is purely random. However, it may actually be the case that differences in the relative rates are (at least in part) systematic and related to study-level variables (moderators), such as the treatment intensity and the severity of the depression in the group of patients being treated. For reasons that are explained in more detail in the article, treatment intensity will be expressed in terms of a single moderator that indicates the total dosage (in grams) administered during the course of each study. We can create this variable with:
#  
data$dosage <- data$dosage * data$duration
#The baseline HRSD score will be used to reflect the severity of the depression in the patients. Since these two variables may interact, their product will also be included in the model. Finally, for easier interpretation, we will also center the variables at (roughly) their means when including them in the model.
#
#We can fit a mixed-effects meta-regression model with these moderators to the data with:
metaan <- rma(yi, vi, mods = ~ I(dosage-34) * I(baseline-20), data=data, method="DL")
metaan
#These are the same results as given in Table 2 on page 113. Therefore, it appears that St. John's wort is more effective for lower baseline HRSD scores (the coefficient is negative, but just misses being significant at α = .05 with p = .06). On the other hand, the total dosage of St. John's wort administered during the course of a study does not appear to be related to the treatment effectiveness (p= .56) and there does not appear to be an interaction between the two moderators (p = .65).


# GORICA #
#
#Substract estmates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("Beta_intrcpt", "Beta_dosage", "Beta_baseline", "Beta_interact")
se_est <- metaan$se
VCOV_est <- metaan$vb
#
# Apply GORICA #
# Hypothesis of interest (fictional)
H1 <- "Beta_baseline < Beta_dosage; Beta_dosage < 0" 
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results <- goric(est, VCOV = VCOV_est, H1, comparison = "complement", type = "gorica") 
results
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H1  13.158    2.737  -20.843           0.679
#2  complement  13.434    3.763  -19.343           0.321
#---
#  The order-restricted hypothesis ‘H1’ has  2.117 times more support than its complement.
#
# Both hypothesis have  fit values that resemble. Therefore, the weights are mainly based on the penalty values.
# This is an indication that there is support for the border/boundary of the hypothesis:
# That is, there is support for "Beta_baseline = Beta_dosage; Beta_dosage = 0".
# Note that this resembles the result with p-values, where all estimates are said to be statistically zero.


# AIC #
#
# Hypothesis of interest
H0 <- "Beta_baseline == Beta_dosage; Beta_dosage == 0" # stating both are equal to 0.
# 
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H0 <- goric(est, VCOV = VCOV_est, H0, comparison = "complement", type = "gorica") 
results_H0
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#
#Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H0   9.255    2.000  -14.510           0.107
#2  complement  13.382    4.000  -18.763           0.893
#---
#  The order-restricted hypothesis ‘H0’ has  0.119 times more support than its complement.
#
# So, not much support for H0 and quite some support for Hunc, 
# but no quantification for (lack of) support for the hypothesis of interest H1.


#######################################


## Example van Raudenbush (2009) ##
# based on http://www.metafor-project.org/doku.php/analyses:raudenbush2009
#
#Raudenbush (2009) is an excellent chapter in The handbook of research synthesis and meta-analysis (2nd ed.) and describes in detail many of the models and methods that are implemented in the rma() function (including the meta-analytic random- and mixed-effects models). The dataset that is used for the illustration of the various models and methods is actually the same that is used in the Raudenbush and Bryk (1985) and provides the results from 19 studies examining how teachers' expectations about their pupils can influence actual IQ levels (Raudenbush, 1984). A reproduction of the analyses described in Raudenbush and Bryk (1985) can be found here.
#
#Here, I will reproduce the results from Raudenbush (2009). The data are provided in Table 16.1 (p. 300) in the chapter and can be loaded with:
data <- dat.raudenbush1985
data
#(I copy the dataset into 'data', which is a bit shorter and therefore easier to type further below). The contents of the dataset are:
#
#Raudenbush (2009) uses REML estimation to fit a random-effects model. Since REML estimation is the default for the rma() function, we can obtain the same results with:
metaan <- rma(yi, vi, data=data, digits=3)
metaan
# these are the same results reported in Table 16.2 (p. 301). In particular, ^τ2 = .019, ^μ = .084, SE[^μ] = .052, and z = 1.62 for the test H0: μ = 0.


# GORICA #
#
#Substract estmates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("mu")
se_est <- metaan$se
VCOV_est <- metaan$vb
#
# Apply GORICA #
# Hypothesis of interest (fictional) 
# wrt population value of (overall) standardized mean difference: mu
H1 <- "mu > 0"
# Comparable to test done in article (which comes down to using the AIC)
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results <- goric(est, VCOV = VCOV_est, H1, comparison = "complement", type = "gorica")
results
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#  model  loglik  penalty  gorica  gorica.weights
#1          H1   2.044    0.500  -3.089           0.788
#2  complement   0.731    0.500  -0.462           0.212
#---
#  The order-restricted hypothesis ‘H1’ has  3.719 times more support than its complement.
# Support for 'mu > 0' versus 'mu < 0' is about 3.7 as strong.


# Alternative: H0 and H1
H0 <- "mu == 0"
set.seed(123) # set seed: to obtain same results when you re-run it 
goric(est, VCOV = VCOV_est, H0, H1, type = "gorica")
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#           model  loglik  penalty  gorica  gorica.weights
#1             H0   0.731    0.000  -1.462           0.216
#2             H1   2.044    0.500  -3.089           0.488
#3  unconstrained   2.044    1.000  -2.089           0.296
#
# Note: H1 and H0 are subsets of Hunc (and H0 is also part of H1).
#
# It is better to combine results of H0 with that of H1 and its complement:
library(devtools) # Make sure you have Rtools (and a version which is compatable with your R version).
install_github("rebeccakuiper/ICweights")
library(ICweights)
?IC.weights
citation("ICweights")
IC.weights(c(results$result[1,4], results_H1$result[,4]), Name_Hypo = c("H0", "H1", "compl_H1"))
#
#$IC.weights
#[1] 0.2589079 0.5840564 0.1570356
#
#$rel.IC.weights
#             vs H0     vs H1 vs compl H1
#H0       1.0000000 0.4432927    1.648721
#H1       2.2558460 1.0000000    3.719261
#compl H1 0.6065307 0.2688706    1.000000
#
# Most support for H1 (which includes H0). 
# H1 2.3 times more supported than H0 and 3.7 times more than its complement (cf. results first analysis).
#
# Note: support for 'mu >= 0' vs 'mu < 0' is 0.2589079 + 0.5840564 vs 0.1570356
IC.weights(c(0.2589079 + 0.5840564, 0.1570356), Name_Hypo = c("mu >= 0", "mu < 0"))
#
#$IC.weights
#[1] 0.4150896 0.5849104
#
#$rel.IC.weights
#        vs mu >= 0 vs mu < 0
#mu >= 0   1.000000 0.7096635
#mu < 0    1.409119 1.0000000
#
# or: support for 'mu > 0' vs 'mu <= 0' is 0.2589079 vs 0.5840564 + 0.1570356
IC.weights(c(0.2589079, 0.5840564 + 0.1570356), Name_Hypo = c("mu > 0", "mu <= 0"))
#
#$IC.weights
#[1] 0.5599828 0.4400172
#
#$rel.IC.weights
#        vs mu > 0 vs mu <= 0
#mu > 0  1.0000000   1.272638
#mu <= 0 0.7857693   1.000000


# AIC #
#
# Hypothesis of interest
H1 <- "mu > 0"
#
# Apply GORICA
set.seed(123) # set seed: to obtain same results when you re-run it
results_H0 <- goric(est, VCOV = VCOV_est, H0, comparison = "complement", type = "gorica") 
results_H0
#
#restriktor (0.2-800): generalized order-restriced information criterion approximation:
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   0.731    0.000  -1.462           0.422
#2  complement   2.044    1.000  -2.089           0.578
#---
#  The order-restricted hypothesis ‘H0’ has  0.731 times more support than its complement.
#
# So, support for H0 just below the support for Hunc, 
# but no quantification for (lack of) support for the hypothesis of interest H1.
