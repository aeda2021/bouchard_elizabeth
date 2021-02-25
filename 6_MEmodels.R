# Class 6: Mixed Effects Models - Feb. 25, 2021
# Turn this exercise sheet in by 
# 1) Going through the code below, filling in any sections that are missing
# 2) If the code asks you to make a figure, be sure to save it by name from this script into your figures/ directory. Name must start with 6_ so that we can find it.
# 3) Answering any questions (labeled Q#) by typing comments in this text (start each line with #)
# 4) Committing this completed script and any figures to your Git repo
# 5) Pushing your commits to Github so that we can see.
# 6) This is due no later than the start of class 7.

# INTRODUCTION
# American Foulbrood (AFB) is an infectious disease affecting the larval stage of honeybees (Apis mellifera) and is the most widespread and destructive of the brood diseases. The causative agent is Paenibacillus larvae and the spore forming bacterium infects queen, drone, and worker larvae. Only the spore stage of the bacterium is infectious to honey bee larvae. The spores germinate into the vegetative stage soon after they enter the larval gut and continue to multiply until larval death. The spores are extremely infective and resilient, and one dead larva may contain billions of spores. 

# Although adult bees are not directly affected by AFB, some of the tasks carried out by workers might have an impact on the transmission of AFB spores within the colony and on the transmission of spores between colonies. When a bee hatches from its cell, its first task is to clean the surrounding cells, and its next task is tending and feeding of larvae. Here, the risk of transmitting AFB spores is particularly great if larvae that succumbed to AFB are cleaned prior to feeding susceptible larvae. 

# Because AFB is extremely contagious, hard to cure, and lethal at the colony level, it is of importance to detect outbreaks, before they spread and become difficult to control. Reliable detection methods are also important for studies of pathogen transmission within and between colonies. Of the available methods, sampling adult bees has been shown the most effective. Hornitzky and Karlovskis (1989) introduced the method of culturing adult honey bees for AFB, and demonstrated that spores can be detected from colonies without clinical symptoms. Recently, culturing of P. larvae from adult honey bee samples has been shown to be a more sensitive tool for AFB screening compared to culturing of honey samples. When samples of adult bees are used, the detection level of P. larvae is closely linked to the distribution of spores among the bees. 

# For this reason, we will model the density of P. larvae with the potential explanatory variables as number of bees in the hive, presence or absence of AFB, and hive identity.


# Read in and examine bee data
# Spobee column has density of P. larvae spores (the bacterium). 
# Hive has the ID of the hive sampled (there are 3 samples/hive)
# Infection has a metric quantifying the degree of infection. We will turn this into yes/no whether infection is present. 
Bees <- read.table(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/Bees.txt'), header=TRUE)
head(Bees)

# make hive a factor
Bees$fhive <- factor(Bees$Hive)

# Make a yes/no infection column (Infection01)
Bees$Infection01 <- Bees$Infection 
Bees$Infection01[Bees$Infection01 > 0] <- 1
Bees$fInfection01 <- factor(Bees$Infection01) # turn this into a factor

# Scale BeesN to improve model convergence (mean 0, standard deviation 1)
Bees$sBeesN <- scale(Bees$BeesN)


# Make a Cleveland dot-chart of spores vs. hive
dotchart(Bees$Spobee, groups = Bees$fhive, xlab='Spores', ylab='Hive ID')


# Q1. Does variance of spore density appear homogenous among hives? Why or why not?

# The variance of spore density does not appear homogenous among hives. For the hives that have many spores present, there is high variance in spore density.
# For the hives that have very few spores, there is little variation in spore density.

# Q2. Try some transformations of the response variable to homogenize the variances (or at least improve it). Which transformation of spore density seems reasonable? Why?

dotchart(log(Bees$Spobee+1), groups = Bees$fhive, xlab='Spores', ylab='Hive ID')
dotchart(sqrt(Bees$Spobee), groups = Bees$fhive, xlab='Spores', ylab='Hive ID')

# The log transformation of spore density seems more reasonable because it makes variance more homogenous among hives. 
# We also added 1 to the values in order to account for 0s in the dataset.


# Q3. Develop a simple linear model for transformed spore density. Include infection (fInfection01), number of bees (sBeesN) and their interaction as explanatory variables. 
# Check for a hive effect by plotting standardized residuals (see the residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show your code and your plots. 
# Do residuals look homogenous among hives?

lm1 <- lm(log(Bees$Spobee+1) ~ fInfection01 + sBeesN + fInfection01:sBeesN, data=Bees) #or fInfection01*sBeesN
res <- residuals(lm1,type='pearson')
plot(res~Bees$fhive)

# The residuals do look relatively homogenous among hives.

# Q4. What are the advantages of including hive as a random effect, rather than as a fixed effect?

# If you use hives as a fixed effect, you have a lot of degrees of freedom. By including hive as a random effect, you save 23 degrees of freedom.
# It also takes into account the nested data structure and repeated measures.

# Apply the Zuur protocol (10-step version outlined here, as used with the barn owl nesting data in Zuur Ch. 5):
# Step 1: Fit and check a "beyond optimal" linear regression (already done above)
# Step 2: Fit a generalized least squares version of the "beyond optimal" model (no need: we will use the linear regression model).

# Q5. Step 3. Choose a variance structure or structures (the random effects). What random effects do you want to try?

# We want to try hive as a random effect. I want to try it as a random intercept and a random slope.

# We will now fit a mixed effects (ME) model. Zuur et al. used the nlme package in R, but Douglas Bates now has a newer package that is widely used and that is called lme4. 
# The benefits of lme4 include greater flexibility in the structure of the random effects, the option to use non-Gaussian error structures (for generalized linear mixed effects models, or GLMMs), and more efficient code to fit models. The main difference between nlme's lme() function and the lmer() function in lme4 is in how random effects are specified:
# model <- lmer(response ~ explanantoryvars + (1|random), data=mydata) # a random intercept model
# model <- lmer(response ~ explanantoryvars + (slope|random), data=mydata) # a random intercept and slope model
# One of the frustrations some people run into is that the lme4 package doesn't provide p-values. This stems from disagreements and uncertainty about how best to calculate p-values. 
# However, if p-values are important to you, approximate p-values can be derived from the lmerTest package

# install.packages('lme4') # if needed
# install.packages('lmerTest') if needed
require(lmerTest)

# Q6. Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package (transformed spore density is response, fInfection01, sBeesN, and interaction are the explanatory variables). Show your code.

beyondoptimal1 <- lmer(log(Bees$Spobee+1) ~ fInfection01 + sBeesN + fInfection01:sBeesN + (1|fhive), data=Bees)
beyondoptimal2 <- lmer(log(Bees$Spobee+1) ~ fInfection01 + sBeesN + fInfection01:sBeesN + (sBeesN|fhive), data=Bees)

# Q7. Step 5. Compare the linear regression and ME model(s) with a likelihood ratio test, including correction for testing on the boundary if needed. Use the anova() command. 
# This will re-fit your lmer model with maximum likelihood, but this is OK (note there are some debates about exactly how to best compare an lm and lmer model). 
# Show your work and the results. Which random effect structure do you choose based on the results?

anova(beyondoptimal2, beyondoptimal1, lm1) #put lm model last so that the code works

# Data: Bees
# Models:
#     lm1: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + fInfection01:sBeesN
# beyondoptimal1: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + fInfection01:sBeesN + 
#     beyondoptimal1:     (1 | fhive)
# beyondoptimal2: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + fInfection01:sBeesN + 
#     beyondoptimal2:     (sBeesN | fhive)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# lm1               5 325.73 337.11 -157.87   315.73                          
# beyondoptimal1    6 253.43 267.09 -120.71   241.43 74.3041  1     <2e-16 ***
# beyondoptimal2    8 254.50 272.72 -119.25   238.50  2.9235  2     0.2318    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Based on the results, I would choose beyondoptimal1 - the random intercept model - because it has the lowest AIC value (253.43). 
# We do not need to correct for testing on the boundary because the p-value of the Likelihood Ratio Test is so small at the 5% confidence level.


# Q8. Step 6. Check the model: plot standardized residuals vs. fitted values and vs. each predictor. (You can get standardized residuals with residuals(yourmodel, type='pearson')). How do they look?
res1 <- residuals(beyondoptimal1, type='pearson')
plot(res1~fitted(beyondoptimal1))
    # It looks like variance declines as fitted values increase. Generally, this figure does not look too bad.
plot(res1~Bees$fInfection01)
    # There is greater variance when infection is absent.
plot(res1~Bees$sBeesN)
    # This figure looks pretty good. The variance is lowest when there are few bees. However, this could be because there are only three samples with few bees.

# Q9. Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a reduced model without the interaction term, also fit with ML. 
# Use anova() to compare the models. Which model do you choose? Why?

beyondoptimal3 <- lmer(log(Bees$Spobee+1) ~ fInfection01 + sBeesN + fInfection01:sBeesN + (1|fhive), data=Bees, REML=FALSE)
beyondoptimal4 <- lmer(log(Bees$Spobee+1) ~ fInfection01 + sBeesN + (1|fhive), data=Bees, REML=FALSE)

anova(beyondoptimal3, beyondoptimal4)

# Data: Bees
# Models:
# beyondoptimal4: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + (1 | fhive)
# beyondoptimal3: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + fInfection01:sBeesN + 
# beyondoptimal3:     (1 | fhive)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# beyondoptimal4    5 251.97 263.36 -120.99   241.97                     
# beyondoptimal3    6 253.43 267.09 -120.71   241.43 0.5471  1     0.4595

# We choose the beyondpotimal4 model - the reduced model without the interaction term - because it has a lower AIC value.
# Additionally, the p-value for the beyondoptomal3 model is high (0.4595), showing that adding the interaction term to the model is not significant at the 5% level.

# Q10. Step 8. Iterate #7 to arrive at the final model. Show your work. What is your final set of fixed effects?

#beyondoptimal4 <- lmer(log(Bees$Spobee+1) ~ fInfection01 + sBeesN + (1|fhive), data=Bees, REML=FALSE)
beyondoptimal5 <- lmer(log(Bees$Spobee+1) ~ sBeesN + (1|fhive), data=Bees, REML=FALSE)
beyondoptimal6 <- lmer(log(Bees$Spobee+1) ~ fInfection01 + (1|fhive), data=Bees, REML=FALSE)
beyondoptimal7 <- lmer(log(Bees$Spobee+1) ~ (1|fhive), data=Bees, REML=FALSE)

anova(beyondoptimal4,beyondoptimal5)
# Data: Bees
# Models:
# beyondoptimal5: log(Bees$Spobee + 1) ~ sBeesN + (1 | fhive)
# beyondoptimal4: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + (1 | fhive)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# beyondoptimal5    4 267.95 277.06 -129.98   259.95                         
# beyondoptimal4    5 251.97 263.36 -120.99   241.97 17.982  1   2.23e-05 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# The term fInfection01 is significant at the 5% level and we should keep it in the model. Also, model beyondoptimal4 has the lower AIC score.

anova(beyondoptimal4,beyondoptimal6)
# Data: Bees
# Models:
# beyondoptimal6: log(Bees$Spobee + 1) ~ fInfection01 + (1 | fhive)
# beyondoptimal4: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + (1 | fhive)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# beyondoptimal6    4 252.84 261.94 -122.42   244.84                       
# beyondoptimal4    5 251.97 263.36 -120.99   241.97 2.8635  1    0.09061 .
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# The likelihood ratio test shows that including the fixed sBeesN term is not significant at the 5% level because the p-value for model beyondoptimal4 is 0.09061.
# There is also some support for the beyondoptimal6 model because the deltaAIC is less than 3. However, I selected beyondoptimal4 as the final model because it has the lowest
# AIC score. As such, my final model includes the fixed effects of both fInfection01 and sBeesN. 

anova(beyondoptimal4,beyondoptimal5, beyondoptimal6, beyondoptimal7)

# Data: Bees
# Models:
#     beyondoptimal7: log(Bees$Spobee + 1) ~ (1 | fhive)
# beyondoptimal5: log(Bees$Spobee + 1) ~ sBeesN + (1 | fhive)
# beyondoptimal6: log(Bees$Spobee + 1) ~ fInfection01 + (1 | fhive)
# beyondoptimal4: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + (1 | fhive)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# beyondoptimal7    3 270.32 277.15 -132.16   264.32                          
# beyondoptimal5    4 267.95 277.06 -129.98   259.95  4.3683  1    0.03661 *  
# beyondoptimal6    4 252.84 261.94 -122.42   244.84 15.1184  0    < 2e-16 ***
# beyondoptimal4    5 251.97 263.36 -120.99   241.97  2.8635  1    0.09061 .  
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Q11. Step 9. Fit the final model with REML. Check assumptions by plotting a histogram of residuals, plotting Pearson standardized residuals vs. fitted values, 
# and plotting Pearson standardized residuals vs. explanatory variables. Are there issues with the model? If so, how might you address them?

modsel <- lmer(log(Bees$Spobee+1) ~ fInfection01 + sBeesN  + (1|fhive), data=Bees, REML=TRUE) #same as beyondoptimal4, except REML

res2 <- residuals(modsel, type='pearson')

hist(res2)
# The histogram of residuals looks relatively normally distributed.

plot(res2~fitted(modsel))
# There appears to be somewhat greater spread in the residuals when fitted values are low compared to when fitted values are high.
# Otherwise, the plot appears relatively homogeneous.

plot(res2~Bees$fInfection01)
# There appears to be greater variance in the residuals when the infection is absent compared to when the infection is present.

plot(res2~Bees$sBeesN)
# This figure looks pretty good. There is the least amount of spread in the residuals when there are few bees. However, this may occur because there are few samples (only three)
# with few bees. Otherwise, there are no clear patterns in the residuals.

# To address issues from model validation, you can: apply a transformation, check for covariates, add more explanatory terms, or try additive mixed modeling.


# Q12. Step 10. Interpret the model. The summary() command is useful here. What have you learned about American Foulbrood? 

summary(modsel)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(Bees$Spobee + 1) ~ fInfection01 + sBeesN + (1 | fhive)
#    Data: Bees
# 
# REML criterion at convergence: 239.3
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.2837 -0.4321  0.1626  0.5108  1.6666 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  fhive    (Intercept) 4.8222   2.1960  
#  Residual             0.6033   0.7767  
# Number of obs: 72, groups:  fhive, 24
# 
# Fixed effects:
#               Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)     4.1352     0.5041 21.0000   8.203 5.50e-08 ***
# fInfection011   6.1487     1.2704 21.0000   4.840 8.76e-05 ***
# sBeesN         -0.7778     0.4768 21.0000  -1.631    0.118    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) fIn011
# fInfectn011 -0.420       
# sBeesN      -0.108  0.257

# There is a strong effect of infection presence (p < 0.05). The slope for infection is 6.1487, which means that the density of P. larvae spores is 6.1487 greater (on the log scale)
# when the infection is present than when the infection is absent. This makes sense because the spore stage is the infectious stage, which allows the infection to spread.
# Hives where the infection is absent have lower densities of spores, indicating that adult bees may have just recently transmitted the spores into the colony or there may
# be no spores. This finding also suggests that hives may be able to resist American Foulbrood when spore density is relatively low, and they may become more 
# susceptible as density increases.

# The number of bees has a weak effect (p = 0.118) on the density of P. larvae spores.
# The slope for number of bees is -0.7778, which means that the more bees there are, the lower the density of P. larvae spores. 
# This finding suggests that higher densities of bees may be able to dilute the density of spores.

# The random intercept is normally distributed with mean 0 and variance 4.82. The random intercept indicates that absolute values differ per hive in a random way. 
# In other words, different hives have different larval densities.


# Q13. Calculate the correlation between observations from the same hive as variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). 
# Given the correlation among observations from the same hive, do you think it's a good use of time to sample each hive multiple times? Why or why not?

4.8222/(4.8222+0.6033)
#0.8888029

# Even though the observations from the same hive are correlated, I think that it is a good use of time to sample each hive multiple times because there is always the
# possibility that one of your samples will be incorrect. If you collect at least 3 samples from each hive, then you can more easily identify if one of the samples is 
# an outlier and should be excluded from the analysis. If only one sample was collected from each hive, you would have no way of knowing if any of your samples is 
# contaminated or a poor representation of the hive.
