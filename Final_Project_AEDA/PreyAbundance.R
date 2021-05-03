# Preliminary analysis of sediment samples collected from sandbar on June 2, 2020.
# This analysis incorporates ALL of the data because I finally finished processing all of the samples.

# clear R's brain - always a good way to start
rm(list = ls())

###### Libraries ######

# Below portion is based on script from Advanced Ecological Data Analysis course.
# That script file is called "data manip vis - exercise.R".
# The goal of that assignment was to learn how to manipulate and visualize data in R
# using the tidyverse packages

# libraries to load for entire exercise
library(tidyverse)
# tidyverse automatically loads all the packages below, just fyi
library(readr)  # reads data files into R studio really easily
library(tidyr)  # tidies data
library(forcats) # deals with factor variables
library(dplyr)  # manipulates data
library(ggplot2)  # graphs stuff
library(stringr) # edits character strings

library(ggfortify) # for autoplot
library(lme4)
library(lmerTest)

require(glmmTMB) # for glmms
require(DHARMa) # for glm model evaluation

## LOAD DATA AND TIDY DATA ####

## Tidying data with TIDYR
# 'tidy' data means that each column is a variable and each row is an observation
# generally, tidying data changes it from wide format (many columns) to long format (many rows)
# tidy format will make analysis and figures a lot easier to code

# Import dataset
setwd("~/Desktop/Tree of Folders/School/Rutgers/Coursework/Adv Eco Data Analysis/github/bouchard_elizabeth/Final_Project_AEDA")
sandbar <- read_csv("Prelim_Data_Sandbar.csv") 

# Look at data
glimpse(sandbar)
summary(sandbar) #reports NAs and levels of categorical variables

# We need to manipulate the data to make columns for the total number of gastropods and clams for each subsample.
# Right now we have separate columns for: "Gastropod", "Empty_gastropod", "Clam" and "Single_clam_valve"
# First, we have to make a row-wise data frame because, although dyplyr is good at performing operations over columns, it's much more tricky over rows.
sandbar <- sandbar %>% rowwise(Subsample_ID)

# Make Gastropod column that is sum of "Gastropod" and "Empty_gastropod"columns - this will estimate number of gastropods that were in the sample upon collection.
# We are assuming that all gastropods (whether or not they contain tissues) were alive at the time of sample collection.
# creating a new variable from existing variables: mutate()
# if you are sure you want to make a change, you can replace your df by naming the mutated version with the same name
summary(sandbar)
sandbar <- mutate(sandbar, Gastropods = sum(c(Gastropod, Empty_gastropod)))

# Same as above but for "Ostracod" and "Empty_ostracod" columns.
sandbar <- mutate(sandbar, Ostracods = sum(c(Ostracod, Empty_ostracod)))

# Make Clam column that is sum of current "Clam", "Empty_oyster", and half of "Single_clam_valve" - this will estimate number of clams that were in the sample upon collection.
# We are assuming all single full valves that we found during processing were live clams at the time of sample collection.
# First, divide "Single_clam_valve" by 2 in order to get estimate for numbers of single valves that might have been full clams at time sample was collected.
sandbar <- mutate(sandbar, Single_clam_valve = (Single_clam_valve/2))
sandbar <- mutate(sandbar, Clams = sum(c(Clam, Empty_oyster, Single_clam_valve)))
sandbar <- mutate(sandbar, Clams = ceiling(Clams)) #Round up everything that ends with .5 to nearest integer (whole number). This helps with AIC.

# Make a new dataframe for tidy dataset.
# Get rid of columns of raw data ("Gastropod", "Empty_gastropod", "Clam", "Single_clam_valve", "Empty_oyster", "Ostracod", "Empty_ostracod") and notes to prevent confusion.
# select() is for selecting columns, ie, variables
# R will keep only the variables you name in select, and drop the rest
# you can also use -name to drop specific columns
tidysandbar <- select(sandbar, -Clam, -Gastropod, -Single_clam_valve, -Empty_gastropod, -Empty_oyster, -Ostracod, -Empty_ostracod, -Notes, -Notes_during_Collection, -Notes_during_Processing)

# tidyr's gather() fixes a common untidyness, termed 'wide format,' in which values of a variable are used as column headers
# it's called 'wide format' because it makes your matrix wide; when you fix it, your data set becomes 'long format'  (ie fewer columns, more rows)
# arguments are gather(data,name of new column that will contain the column headers of the columns you want to get rid of, name of column that will contain all the values that will go with each header, columns to act on) 
tidysandbar <- gather(tidysandbar, Taxa, Abundance, c(12:24))

# check the variable type
glimpse(tidysandbar)

# change variable types to factor
tidysandbar <- mutate(tidysandbar, Point_on_Sandbar=factor(Point_on_Sandbar))
tidysandbar <- mutate(tidysandbar, Inshore_or_Offshore=factor(Inshore_or_Offshore))
tidysandbar <- mutate(tidysandbar, Time_Point=factor(Time_Point))
tidysandbar <- mutate(tidysandbar, Size_Class=factor(Size_Class))
tidysandbar <- mutate(tidysandbar, Sample_ID=factor(Sample_ID))
tidysandbar <- mutate(tidysandbar, Subsample_ID=factor(Subsample_ID))
tidysandbar <- mutate(tidysandbar, Taxa=factor(Taxa))
#check my work
glimpse(tidysandbar)
summary(tidysandbar) # summary also gives the factor levels

#Fix time variable. Right now I have two time columns. Time_Point is categorical and Time is the actual time that the sample was collected in the field.
#I want to create a new column in the dataframe with a time variable (Time_simple) that standardizes the time (i.e. 0 , 15 min, 30 min, and so on).
tidysandbar <- mutate(tidysandbar, Time_simple = Time_Point)
levels(tidysandbar$Time_simple)[levels(tidysandbar$Time_simple)=="T1"] <- "0"
levels(tidysandbar$Time_simple)[levels(tidysandbar$Time_simple)=="T2"] <- "15"
levels(tidysandbar$Time_simple)[levels(tidysandbar$Time_simple)=="T3"] <- "30"
levels(tidysandbar$Time_simple)[levels(tidysandbar$Time_simple)=="T4"] <- "45"
levels(tidysandbar$Time_simple)[levels(tidysandbar$Time_simple)=="T5"] <- "60"
levels(tidysandbar$Time_simple)[levels(tidysandbar$Time_simple)=="T6"] <- "75"
glimpse(tidysandbar) #Time_simple is currently a factor. We want to make it time.
tidysandbar <- mutate(tidysandbar, Time_simple=as.numeric(as.character(Time_simple))) #Make Time_simple numeric to represent the number of minutes since tide.
glimpse(tidysandbar) #Check my work. It worked!

#Check levels of Taxa
levels(tidysandbar$Taxa)
                      
# Exclude unnecessary Taxa: Exoskeleton, Unknown, Unknown Star Organism
# Get rid of these rows using filter()
# remember filter() keeps only the rows you specify, and that != means is not equal to
tidiersandbar <- filter(tidysandbar, Taxa != "Exoskeleton" & Taxa != "Unknown" & Taxa != "Unknown Star Organism")

#Exclude NEMATODES as well because we did not count them for all samples and they are unlikely food resource for red knots.
tidiersandbar <- filter(tidysandbar, Taxa != "Nematode" & Taxa != "Exoskeleton" & Taxa != "Unknown" & Taxa != "Unknown Star Organism")

#Exclude <0.25 mm and 0.25 mm size classes because we did not process them for all samples.
tidiersandbar <- filter(tidiersandbar, Size_Class != "<0.25" & Size_Class != "0.25")

#Exclude extra samples because they are unique - no replicates.
tidiersandbar <- tidiersandbar %>%
    filter(!str_detect(Inshore_or_Offshore, "Extra")) 

#Check data again
glimpse(tidiersandbar)
summary(tidiersandbar)

# DATA EXPLORATION

#Which organisms should I focus on?
tidiersandbar %>%
    group_by(Taxa) %>%
    summarize(TotAbundance = sum(Abundance))
#Well, there are no amphipods, copepods, or foraminifera. 
#The most abundant group overall is annelids!

#Based on my understanding of the literature, I think that the most likely prey resources for rufa red knot in Delaware Bay are: horseshoe crab eggs, clams, gastropods, and annelids.
#For my analysis, I am going to exclude all of the other taxa and group the four groups of prey items into a single group called "Prey."
sandbar_prey <- tidiersandbar[(tidiersandbar$Taxa=="HSC_egg") | (tidiersandbar$Taxa=="Annelid") | (tidiersandbar$Taxa=="Clams") 
                              | (tidiersandbar$Taxa=="Gastropods"),]

sandbar_prey %>%
    group_by(Taxa) %>%
    summarize(TotAbundance = sum(Abundance))
#Annelids most abundance (233), followed by clams (170), horseshoe crab eggs (90), and gastropods (23).

sandbar_prey$Subsample #None of the 2mm, 1mm, or 0.5mm size classes were subsampled so I do not have to make any corrections for this.

sandbar_preytot <-  sandbar_prey %>%
    group_by(Point_on_Sandbar, Inshore_or_Offshore, Time_simple) %>%  #I removed size class and redid analysis.
    summarize(TotAbundance = sum(Abundance))

#Check resulting data
glimpse(sandbar_preytot)
summary(sandbar_preytot) #Shows breakdown of sample sizes, etc.

####STEP 2: DATA PICTURE

#Reminder: what variables am I working with?
summary(sandbar_preytot)

#Check distribution of data
hist(sandbar_preytot$TotAbundance)

ggplot(sandbar_preytot, aes(x = TotAbundance)) +
    geom_histogram()
#holy moly! this is so skewed to the right - not normal distribution. many zeros.
#Could potentially use poisson distribution - counts.

# first, data pictures of the variables separately
ggplot(sandbar_preytot, aes(x = Point_on_Sandbar, y = TotAbundance)) +
    geom_boxplot()
#Distribution of counts of prey items for each of the four points on the sandbar are quite similar.
#They have similar central tendencies as indicated by the medians in the boxplot. Low.
#The range of total abundance is greatest for Point B and least for Points C and D.
#Field observations indicate that gulls were observed foraging at Point B.

ggplot(sandbar_preytot, aes(x = Inshore_or_Offshore, y = TotAbundance)) +
    geom_boxplot()
#Generally, total abundance of prey items is low on both the inshore and offshore sides of the sandbar.
#There is a greater range in the number of prey items from inshore samples compared to offshore samples.
#In general, it looks like the number of prey items count be slightly greater on the inshore side of the sandbar, compared to the offshore side of the sandbar.

ggplot(sandbar_preytot, aes(x = Time_simple, y = TotAbundance)) +
    #geom_point() +
    geom_jitter(width = 0.8, height = 0.8)
#Based on visual assessment of this figure, there does not appear to be a clear difference in total prey abundance based on time during the ebbing tide.

# hint: you can use geom_smooth to get a loess ie flexible ie somewhat wiggly fit line
# this can be useful in eyeballing datasets to see whether a linear fit makes sense
ggplot(sandbar_preytot, aes(x = Time_simple, y = TotAbundance)) +
    #geom_point() +
    geom_jitter(width = 0.8, height = 0.8) +
    geom_smooth(span = 1, se = FALSE)
#Looks flat - I'm guessing that time has no influence on abundance.

ggplot(sandbar_preytot, aes(x = Size_Class, y = TotAbundance)) +
    geom_boxplot()
#It appears that the prey items were most abundance in the 0.5mm size class compared to the 1 and 2 mm size classes.
#The abundance of both larger size classes was generally quite low.

#Check distribution of abundance by taxa

ggplot(sandbar_prey, aes(x = Abundance)) +
    geom_histogram()+
    facet_wrap(~Taxa)
#For each taxon, there are many zeros and the distribution is non-normal and skewed to the right.
#Gastropods and horseshoe crab eggs have more zeros relative the annelids and clams.

#Same data pictures as above but broken down by taxa

ggplot(sandbar_prey, aes(x = Point_on_Sandbar, y = Abundance, color=Taxa)) +
    geom_boxplot()
#It looks like the high range for Point B is driven by HSC eggs.
#Gastropods are generally the least abundant at each Point.
#Annelids and Clams are generally the most abundant, although horseshoe crab eggs are quite abundant at Point B.

ggplot(sandbar_prey, aes(x = Inshore_or_Offshore, y = Abundance, color=Taxa)) +
    geom_boxplot()
#Higher abundance on inshore side of sandbar appears to be driven by annelids, clams, and horseshoe crab eggs. Number of gastropods looks similar on both sides.

ggplot(sandbar_prey, aes(x = Time_Point, y = Abundance, color=Taxa)) +
    geom_boxplot()
ggplot(sandbar_prey, aes(x = Time_simple, y = Abundance, color=Taxa)) +
    geom_jitter(width = 1, height = 1) +
    geom_smooth(span = 1, se = FALSE)
#No clear trend.

ggplot(sandbar_prey, aes(x = Size_Class, y = Abundance, color=Taxa)) +
    geom_boxplot()
#Higher abundance of 0.5mm size class appears to be driven by annelids and clams.

#####   QUESTION 1: Does prey abundance change over time as the tide goes out? exp: time since high tide ####

#Step 1: Data picture

ggplot(sandbar_preytot, aes(x = Time_simple, y = TotAbundance)) +
    #geom_point() +
    geom_jitter(width = 0.8, height = 0.8)
#Based on visual assessment of this figure, there does not appear to be a clear difference in total prey abundance based on time during the ebbing tide.

# hint: you can use geom_smooth to get a loess ie flexible ie somewhat wiggly fit line
# this can be useful in eyeballing datasets to see whether a linear fit makes sense
ggplot(sandbar_preytot, aes(x = Time_simple, y = TotAbundance)) +
    #geom_point() +
    geom_jitter(width = 0.8, height = 0.8) +
    geom_smooth(span = 1, se = FALSE)
#Looks flat - I'm guessing that time has no influence on abundance.

ggplot(sandbar_prey, aes(x = Time_Point, y = Abundance, color=Taxa)) +
    geom_boxplot()
ggplot(sandbar_prey, aes(x = Time_simple, y = Abundance, color=Taxa)) +
    geom_jitter(width = 1, height = 1) +
    geom_smooth(span = 1, se = FALSE)
#No clear trend.

#Step 2. Run Stats test
# Run the lm model and look at the diagnostics, including a histogram of the residuals
time_mod <- lm(TotAbundance ~ Time_simple, data = sandbar_preytot)
hist(time_mod$residuals) # first look at a histogram of the residuals. Non-normal. Right-skewed.
autoplot(time_mod) # autoplot is the ggplot function that automatically generates assumption-checking pictures for you
#Doesn't look good.

#This data does not meet the assumptions of a linear model because:
    #the residuals are not normally distributed. They are skewed to the right and there are many 0s.

#A poisson distribution would be a better choice because we are working with counts.
hist(sandbar_preytot$TotAbundance)
mean(sandbar_preytot$TotAbundance) #11.09333
#A poisson distribution might be appropriate because we are working with count data that is not normally distributed and the mean is relatively low.
#However, no zeros in the data.

#Run the glm
time_glmmod1 <- glm(TotAbundance ~ Time_simple, data = sandbar_preytot, family = poisson)
# remember if you don't specify a link, R uses the default, which is ln(Y) in this case

# look at diagnostic plots 
autoplot(time_glmmod1)
#Residuals vs. Fitted: No clear pattern aside from grouped time variable.
#Normal Q-Q: Does not meet assumption that residuals are normally distributed. Skewed to the right.
#Scale-Location: Meets assumption that variance is similar across range of X values
#Residuals vs. Leverage: This figure is confusing to interpret!

#Does not look much better than linear model.

#Compare linear model to the poisson model (must use same predictor variables and data set)
AIC(time_mod) #361.5879
AIC(time_glmmod1) #516.5729
#The linear model has an AIC score that is much lower than the glm model, provide much more evidence in support of the linear model.

#Step 4. Interpret stats
anova(time_glmmod1)
#deviance shows how much variability in the data is explained by the model...
anova(time_glmmod1, test = "Chisq")
summary(time_glmmod1)
# the null deviance is a measure of all the variation in the data (it's not exactly this but can be interpreted like this)
# the residual deviance is what's left after fitting the model
# thus the variability explained by the model is  1-(residual/null)  #1 minus (residual/null)
# so, the variability explained by the model is
1-(322.74/328.93) #0.01881859
#In other words, the model explains little variability.

#The p-value is higher than I expected (0.013), which I think might suggest overdispersion.
#Check for overdispersion: (residual deviance) / (residual df) should be <2
322.74/46 #7.016087
#EVIDENCE OF OVERDISPERSION!

#Two approaches to handle overdispersion:
# 1) add missing variable
# 2) use a different family that can account for more dispersion

#INCORPORATING RANDOM EFFECT OF POINT ON SANDBAR. First, I tested whether random intercept or random intercept and slope model would
#be better using the lme4 package. I might skip this portion in my presentation and move right onto the analysis using DHARMa.

res <- residuals(time_mod,type='pearson')
plot(res~sandbar_preytot$Point_on_Sandbar)
#residuals look about even among Points on Sandbar

# We will now fit a mixed effects (ME) model using the lme4 package.
# model <- lmer(response ~ explanantoryvars + (1|random), data=mydata) # a random intercept model
# model <- lmer(response ~ explanantoryvars + (slope|random), data=mydata) # a random intercept and slope model

#Fit the ME model(s) with lmer() in the lme4 package 
lme_int <- lmer(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), data = sandbar_preytot)
lme_int_slope <- lmer(TotAbundance ~ Time_simple + (Time_simple|Point_on_Sandbar), data = sandbar_preytot)
AIC(lme_int) #364.7782
AIC(lme_int_slope) #368.7743

res1 <- residuals(lme_int, type='pearson')
plot(res1~fitted(lme_int))
# It looks like variance increases a bit as fitted values increase. Generally, this figure looks pretty good.
plot(res1~sandbar_preytot$TotAbundance)
# This figure looks awful! (I think) The variance increases linearly as total abundance increases.

lme_REML <- lmer(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), data = sandbar_preytot)
AIC(lme_REML) #364.7782

#EVALUATING GLMs and GLMMs using DHARMa

#Evaluate collinearity and stuff I skipped
#check for spatial autocorrelation and pseudoreplication and that determines if we wanna incldue this shit!!! (random effect)

#Fit a GLM model
time_glmmod1 <- glm(TotAbundance ~ Time_simple, data = sandbar_preytot, family = poisson) #ran this one earlier already
summary(time_glmmod1)

#Evaluate the model. Use DHARMa package
#First step is to simulate the randomized quantile residuals. Default is 250 simulations.
res_glm1 <- simulateResiduals(fittedModel = time_glmmod1, plot = F)
plot(res_glm1) #did not converge fully!!! #DOES ON WEDNESDAY!!!!
#DHARMa residual diagnostics detects significant deviation, including overdispersion.
#This is highly overdispersed.

#Test dispersion: confirms overdispersion. The ratio of observed vs. mean simulated variance is greater than 1 with a p-value of <2.2e-16, indicating overdispersion.
testDispersion(res_glm1)

#To account for overdispersion, I add a random effect on Point_on_Sandbar. Now I have a GLMM.
time_glmmod2 <- glmer(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), data = sandbar_preytot, family = poisson) #random intercept model
summary(time_glmmod2)
res_glm2 <- simulateResiduals(fittedModel = time_glmmod2, plot = F)
plot(res_glm2)
#DHARMa residual diagnostics detects significant deviation, including overdispersion.
#This is still highly overdispersed. Not much better than without random effect.

#Next I will try the negative binomial in order to adjust the dispersion.
#Fit a GLMM with the negative binomial distribution.
time_glmmod3 <- glmmTMB(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), data = sandbar_preytot, family = nbinom1)
summary(time_glmmod3)
res_glm3 <- simulateResiduals(fittedModel = time_glmmod3, plot = F) #did not converge fully
plot(res_glm3) #did not converge fully
#This looks much better, BUT DID NOT CONVERGE FULLY!!! #DOES CONVERGE ON WEDNESDAY...
testDispersion(res_glm3)
#The ratio is greater than 1 (1.4648), but the p-value is high (0.168). The p-value is based on the distribution of the simulated standard deviations.

#I want to examine this a little further. What about if we remove the random effect? Is the negative binomial still better?
time_glmmod4 <- glmmTMB(TotAbundance ~ Time_simple, data = sandbar_preytot, family = nbinom1)
summary(time_glmmod4)
res_glm4 <- simulateResiduals(fittedModel = time_glmmod4, plot = F)
plot(res_glm4) #did not converge fully #NOW IT DOES...
#Including the random effect doesn't change much. AND DOESN'T CONVERGE!!!

#Let's leave it in due to study design.

#Now, let's test for zero-inflation of our fave model.
#Zero-inflation is a type of overdisperson in which there are more zeros in your observed data than expected under
#your fitted model. It puts excess variance in the residuals. This situation is common and there are ways to correct it. 
#Test for zero-inflation: compare distribution of expected zeros in the data against observed zeros
testZeroInflation(res_glm3)
#I do not see evidence of zero-inflation, so I decide not to use a zero-inflated model.
#Based on this analysis, I have decided that the model structure I will use is a GLMM with negative binomial and random intercept 
#of Point_on_Sandbar. For Poisson data, zero-inflation tests are often negative, but they show up as underdispersion.
#Thus, it is good that the dispersion test for this model did not suggest underdispersion.

#A better way to differentiate between overdispersion and zero-inflation is to fit a model with zero-inflation and look
#at the parameter estimate for the zero-inflation.
#Fit model with zero-inflation.
zeromod1 <- glmmTMB(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), ziformula = ~1, data = sandbar_preytot, family = nbinom1)
#MODEL CONVERGENCE PROBLEM!!!
summary(zeromod1)
simulationzero <- simulateResiduals(fittedModel = zeromod1)
plot(simulationzero) #looks no better than other <- UPDATE:MODEL CONVERGENEC PROBLEM
#compare AIC to select between models with and without zero-inflation
AIC(time_glmmod3,zeromod1) #basically the same, although model without zero-inflation is better by 2 AIC. So use model without zero-inflation.
#update for this section: MODEL CONVERGENCE PROBLEMS!


#Now I need to check for heteroscedasticity.
#In other words, we are checking whether the level of overdispersion depends on another parameter.
testCategorical(res_glm3,catPred = sandbar_preytot$Time_simple)
#But no evidence of heteroscedasticity, so do not need to account for this in model.
#I should also plot residuals against each of the explanatory variables. I only have one variable though, and the figure looks similar
#to the one above.
plotResiduals(res_glm3, sandbar_preytot$Time_simple)

#Fit an alternative GLMM. Based on preliminary data pictures, I think the null model might actually be best.
#Fit the null model.
time_glmmod5 <- glmmTMB(TotAbundance ~ 1 + (1|Point_on_Sandbar), data = sandbar_preytot, family = nbinom1)
summary(time_glmmod5)
#Being thorough each checking out this model.
res_glm5 <- simulateResiduals(fittedModel = time_glmmod5, plot = T)
plot(res_glm5)
testDispersion(res_glm5)
#CONVERGED AND NO OVERDISPERSION

time_glmmod6 <- glmmTMB(TotAbundance ~ 1, data = sandbar_preytot, family = nbinom1)
summary(time_glmmod6)
res_glm6 <- simulateResiduals(fittedModel = time_glmmod6, plot = T)
plot(res_glm6)
#CONVERGED!!! No OVERDISPERSION!

time_glmmod7 <- glmmTMB(TotAbundance ~ 1, data = sandbar_preytot, family = poisson)
summary(time_glmmod7)
res_glm7 <- simulateResiduals(fittedModel = time_glmmod7, plot = T)
plot(res_glm7)
#OVERDISPERSED! but converged.

time_glmmod8 <- glmmTMB(TotAbundance ~ Time_simple, data = sandbar_preytot, family = nbinom1)
summary(time_glmmod8)
res_glm8 <- simulateResiduals(fittedModel = time_glmmod8, plot = T)
plot(res_glm8)
#DOES NOT CONVERGE!!! So most basic null model must be best. (i'm not sure if this is the appropriate rationale, but I'm gonna wtick with it for now.)

time_glmmod9 <- glmmTMB(TotAbundance ~ 1 + (1|Point_on_Sandbar), data = sandbar_preytot, family = poisson)
summary(time_glmmod9)
res_glm9 <- simulateResiduals(fittedModel = time_glmmod9, plot = T)
plot(res_glm9)
#CONVERGED BUT OVERDISPERSED.

#compare AIC of two plausible models (converged and no overdispersion)
AIC(time_glmmod5,time_glmmod6) #deltaAIC is 2. Pick GLM because it is the simpler model.
#So... based on my decisionmaking, the time_glmmod6 is best.
#In other words, time since high tide is not an important factor determining prey availability.
#To conclude this, might need to compare AIC with model that actually has time as a predictor variable!

#INTERPRET RESULTS. SHOW OUTPUT AND INTERPRET

#Step 5. Plot the model prediction back onto the data
#make prediction dataframe
newdat <- expand.grid(Time_simple=c(0,15,30,45,60,75))
#design matrix (fixed effects)
mm <- model.matrix(delete.response(terms(time_glmmod6)),newdat)
#Linear predictor, back-transform this with the inverse link function
newdat$TotAbundance <- drop(mm %*% fixef(time_glmmod6)[["cond"]])
predvar <- diag(mm %*% vcov(time_glmmod6)[["cond"]] %*% t(mm))
newdat$SE <- sqrt(predvar)
newdat_transform <- mutate(newdat, ci_lwr = TotAbundance - (1.96 * SE), ci_upr = TotAbundance + (1.96 * SE))
newdat_transform <- mutate(newdat_transform, TotAbundance = exp(TotAbundance), ci_lwr = exp(ci_lwr), ci_upr = exp(ci_upr))
#plot
pd <- position_dodge(width = 0.4)
g0 <- ggplot(sandbar_preytot, aes(x=Time_simple, y=TotAbundance))+
    #stat_sum(alpha=0.5,color = "skyblue2", aes(size=..n..))+
    geom_jitter(alpha=0.5, color="skyblue2",size=2,width=2, height=0.1)+
    scale_size_continuous(breaks=1:4,range=c(2,5))
g1 <- g0 + geom_line(data=newdat_transform, position=pd, color="darkorange2") +
    geom_point(data=newdat_transform, shape=17, size=4, position=pd, color="darkorange2") 
#confidence intervals
g2 <- g1 + 
   # geom_linerange(data=newdat_transform, aes(ymin=ci_lwr, ymax=ci_upr),
           #               lwd=2, position=pd, color = "darkorange2", alpha = 0.7) + #alpha controls opacity
    geom_ribbon(data = newdat_transform, aes(ymin = ci_lwr, ymax = ci_upr), fill = "darkorange1", alpha = 0.2) +
    theme_bw() +
    ggtitle('Predictions on the Data and Im making this really long so i can learn how to use this \n thing wow isnt this nifty') +
    xlab("Time Since High Tide (minutes)") + 
    ylab("Total Abundance of Red Knot Prey Items") +
    labs(size = "Sample Size") +
    theme(legend.position = c(0.1,0.825), legend.title = element_text(size = 12),
          legend.text = element_text(color = "black"),
          legend.background = element_rect(fill = "azure2", size=0.5, linetype="solid", 
                                           colour ="azure"),
          legend.key = element_rect(fill="azure2", color = NA),
          plot.title = element_text(color="black", size=14, face="bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="black", size=12, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(color="black", size=12, face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
          panel.background = element_rect(fill = "white"),
         # panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "azure"))+
    scale_x_continuous(breaks=seq(0,75,15)) +
    scale_y_continuous(breaks=seq(0,35,5))
g2

# QUESTION 1, but Testing standardization of predictor variable ####
prey_st <- mutate(sandbar_preytot, Time_simple = (Time_simple - mean(sandbar_preytot$Time_simple)/sd(sandbar_preytot$Time_simple)))

#Fit a GLM model
stmod1 <- glm(TotAbundance ~ Time_simple, data = prey_st, family = poisson) #ran this one earlier already
summary(stmod1)

#Evaluate the model. Use DHARMa package
#First step is to simulate the randomized quantile residuals. Default is 250 simulations.
res_stmod1 <- simulateResiduals(fittedModel = stmod1, plot = F)
plot(res_stmod1) #did not converge fully!!! #DOES ON WEDNESDAY!!!!
#DHARMa residual diagnostics detects significant deviation, including overdispersion.
#This is highly overdispersed.

#Test dispersion: confirms overdispersion. The ratio of observed vs. mean simulated variance is greater than 1 with a p-value of <2.2e-16, indicating overdispersion.
testDispersion(res_glm1)

#To account for overdispersion, I add a random effect on Point_on_Sandbar. Now I have a GLMM.
time_glmmod2 <- glmer(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), data = sandbar_preytot, family = poisson) #random intercept model
summary(time_glmmod2)
res_glm2 <- simulateResiduals(fittedModel = time_glmmod2, plot = F)
plot(res_glm2)
#DHARMa residual diagnostics detects significant deviation, including overdispersion.
#This is still highly overdispersed. Not much better than without random effect.

#Next I will try the negative binomial in order to adjust the dispersion.
#Fit a GLMM with the negative binomial distribution.
time_glmmod3 <- glmmTMB(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), data = sandbar_preytot, family = nbinom1)
summary(time_glmmod3)
res_glm3 <- simulateResiduals(fittedModel = time_glmmod3, plot = F) #did not converge fully
plot(res_glm3) #did not converge fully
#This looks much better, BUT DID NOT CONVERGE FULLY!!! #DOES CONVERGE ON WEDNESDAY...
testDispersion(res_glm3)
#The ratio is greater than 1 (1.4648), but the p-value is high (0.168). The p-value is based on the distribution of the simulated standard deviations.

#I want to examine this a little further. What about if we remove the random effect? Is the negative binomial still better?
time_glmmod4 <- glmmTMB(TotAbundance ~ Time_simple, data = sandbar_preytot, family = nbinom1)
summary(time_glmmod4)
res_glm4 <- simulateResiduals(fittedModel = time_glmmod4, plot = F)
plot(res_glm4) #did not converge fully #NOW IT DOES...
#Including the random effect doesn't change much. AND DOESN'T CONVERGE!!!

#Let's leave it in due to study design.

#Now, let's test for zero-inflation of our fave model.
#Zero-inflation is a type of overdisperson in which there are more zeros in your observed data than expected under
#your fitted model. It puts excess variance in the residuals. This situation is common and there are ways to correct it. 
#Test for zero-inflation: compare distribution of expected zeros in the data against observed zeros
testZeroInflation(res_glm3)
#I do not see evidence of zero-inflation, so I decide not to use a zero-inflated model.
#Based on this analysis, I have decided that the model structure I will use is a GLMM with negative binomial and random intercept 
#of Point_on_Sandbar. For Poisson data, zero-inflation tests are often negative, but they show up as underdispersion.
#Thus, it is good that the dispersion test for this model did not suggest underdispersion.

#A better way to differentiate between overdispersion and zero-inflation is to fit a model with zero-inflation and look
#at the parameter estimate for the zero-inflation.
#Fit model with zero-inflation.
zeromod1 <- glmmTMB(TotAbundance ~ Time_simple + (1|Point_on_Sandbar), ziformula = ~1, data = sandbar_preytot, family = nbinom1)
#MODEL CONVERGENCE PROBLEM!!!
summary(zeromod1)
simulationzero <- simulateResiduals(fittedModel = zeromod1)
plot(simulationzero) #looks no better than other <- UPDATE:MODEL CONVERGENEC PROBLEM
#compare AIC to select between models with and without zero-inflation
AIC(time_glmmod3,zeromod1) #basically the same, although model without zero-inflation is better by 2 AIC. So use model without zero-inflation.
#update for this section: MODEL CONVERGENCE PROBLEMS!


#Now I need to check for heteroscedasticity.
#In other words, we are checking whether the level of overdispersion depends on another parameter.
testCategorical(res_glm3,catPred = sandbar_preytot$Time_simple)
#But no evidence of heteroscedasticity, so do not need to account for this in model.
#I should also plot residuals against each of the explanatory variables. I only have one variable though, and the figure looks similar
#to the one above.
plotResiduals(res_glm3, sandbar_preytot$Time_simple)

#Fit an alternative GLMM. Based on preliminary data pictures, I think the null model might actually be best.
#Fit the null model.
time_glmmod5 <- glmmTMB(TotAbundance ~ 1 + (1|Point_on_Sandbar), data = sandbar_preytot, family = nbinom1)
summary(time_glmmod5)
#Being thorough each checking out this model.
res_glm5 <- simulateResiduals(fittedModel = time_glmmod5, plot = T)
plot(res_glm5)
testDispersion(res_glm5)
#CONVERGED AND NO OVERDISPERSION

time_glmmod6 <- glmmTMB(TotAbundance ~ 1, data = sandbar_preytot, family = nbinom1)
summary(time_glmmod6)
res_glm6 <- simulateResiduals(fittedModel = time_glmmod6, plot = T)
plot(res_glm6)
#CONVERGED!!! No OVERDISPERSION!

time_glmmod7 <- glmmTMB(TotAbundance ~ 1, data = sandbar_preytot, family = poisson)
summary(time_glmmod7)
res_glm7 <- simulateResiduals(fittedModel = time_glmmod7, plot = T)
plot(res_glm7)
#OVERDISPERSED! but converged.

time_glmmod8 <- glmmTMB(TotAbundance ~ Time_simple, data = sandbar_preytot, family = nbinom1)
summary(time_glmmod8)
res_glm8 <- simulateResiduals(fittedModel = time_glmmod8, plot = T)
plot(res_glm8)
#DOES NOT CONVERGE!!! So most basic null model must be best. (i'm not sure if this is the appropriate rationale, but I'm gonna wtick with it for now.)

time_glmmod9 <- glmmTMB(TotAbundance ~ 1 + (1|Point_on_Sandbar), data = sandbar_preytot, family = poisson)
summary(time_glmmod9)
res_glm9 <- simulateResiduals(fittedModel = time_glmmod9, plot = T)
plot(res_glm9)
#CONVERGED BUT OVERDISPERSED.

#compare AIC of two plausible models (converged and no overdispersion)
AIC(time_glmmod5,time_glmmod6)
#####   QUESTION 2: Are there differences in prey abundance between the inshore and offshore side of the sandbars? ####

#Step 1: Data picture

#Reorg data for this analysis such that each row represents a single sample (get rid of size class)
simplesand <-  sandbar_preytot %>%
    group_by(Point_on_Sandbar, Inshore_or_Offshore, Time_simple) %>%
    summarize(TotAbundance = sum(TotAbundance))

ggplot(simplesand, aes(x = Inshore_or_Offshore, y = TotAbundance)) +
    geom_boxplot()
#Based on visual assessment of this figure, there appears to be greater total prey abundance on the inshore side of the sandbar.

sandbar_prey %>% group_by(Point_on_Sandbar, Inshore_or_Offshore, Time_simple, Taxa) %>%
    summarize(TotAbundance = sum(Abundance)) %>%
    ggplot(data=, aes(x = Inshore_or_Offshore, y = TotAbundance, color=Taxa)) +
        geom_boxplot()
#Based on visual assessment of this figure, this trend appears to be driven by annelids, clams, and HSC eggs.
#However, there are more snails on the offshore side of the sandbar. Snails are mobile scavengers, so perhaps they were
#following the tide as it went out.

In <- subset(simplesand, Inshore_or_Offshore == "Inshore")
hist(In$TotAbundance,breaks=seq(0,60,1))

Off <- subset(simplesand, Inshore_or_Offshore == "Offshore")
hist(Off$TotAbundance,breaks=seq(0,60,1))

# FIGURES FOR PRESENTATION!
ggplot(simplesand, aes(x=Inshore_or_Offshore, y=TotAbundance))+
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(alpha=0.5,color = "skyblue2",size=3) +
    theme_bw() +
    ggtitle('Abundance of Potential Red Knot Prey Items on \n the Inshore and Offshore Side of Sandbars') +
    xlab("Side of Sandbar") + 
    ylab("Prey Abundance") +
    theme(plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="black", size=14, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(color="black", size=14, face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.text.x = element_text(color="black", size=12),
          axis.text.y = element_text(color="black", size=12),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "azure")) +
    scale_y_continuous(breaks=seq(0,55,5))
#Boxplots show that Blue dots are data points. 
#Median abundance on inshore side is high (14.5) compared to offshore side (5) of sandbar.
#Inshore side has relatively high spread, suggesting higher variability in prey abundance.

In <- subset(simplesand, Inshore_or_Offshore == "Inshore")
median(In$TotAbundance) #14.5
mean(In$TotAbundance) #16.75
Off <- subset(simplesand, Inshore_or_Offshore == "Offshore")
median(Off$TotAbundance) #5
mean(Off$TotAbundance) #5.4166667

sandbar_prey %>% group_by(Point_on_Sandbar, Inshore_or_Offshore, Time_simple, Taxa) %>%
    summarize(TotAbundance = sum(Abundance)) %>%
    ggplot(data=, aes(x = Inshore_or_Offshore, y = TotAbundance, color=Taxa)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 1)) +
    theme_bw() +
    ggtitle('Abundance of Potential Red Knot Prey Items on \n the Inshore and Offshore Side of Sandbars') +
    xlab("Side of Sandbar") + 
    ylab("Prey Abundance") +
    scale_color_discrete(name="Prey Type", labels = c("Worms", "Clams", "Snails", "HSC Eggs")) +
    theme(plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="black", size=14, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(color="black", size=14, face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.text.x = element_text(color="black", size=12),
          axis.text.y = element_text(color="black", size=12),
          legend.position = c(0.8,0.8), 
          legend.title = element_text(size = 14),
          legend.text = element_text(color = "black", size = 12),
          legend.background = element_rect(fill = "azure", size=0.5, linetype="solid", 
                                           colour ="azure2"),
          legend.key = element_rect(fill="azure", color = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "azure")) +
    scale_y_continuous(breaks=seq(0,55,5))
#Based on visual assessment of this figure, this trend appears to be driven by annelids, clams, and HSC eggs.
#However, there are more snails on the offshore side of the sandbar. Snails are mobile scavengers, so perhaps they were
#following the tide as it went out.
#For each prey type, how many samples contained that type of prey? AKA what are sample sizes?


#Step 2. RUN STATS TESTS

# Run the lm model and look at the diagnostics, including a histogram of the residuals
bar_mod <- lm(TotAbundance ~ Inshore_or_Offshore, data = simplesand)
hist(bar_mod$residuals) # first look at a histogram of the residuals
autoplot(bar_mod) # autoplot is the ggplot function that automatically generates assumption-checking pictures for you
#Doesn't look good.
#This data does not meet the assumptions of a linear model.

#Residuals vs. Fitted: Checks the assumption that a line is an appropriate fit for our data.
                    #There is no clear pattern in the residuals, but there is much greater scatter at right.
#Normal Q-Q: This plot indicates that the assumption that residuals are normally distributed is not met. 
            #This plot indicates that the residuals are skewed to the right.
#Scale-Location: This plot suggests that the data violates the assumption that variance is similar across range of X values. 
    #There is evidence of heteroscedasticity because the variance of residuals is greater at the right side of the plot.
#Residuals vs. Leverage: There may be some influential data point on the inshore side.

#A poisson distribution would be a better choice because we are working with counts.
hist(simplesand$TotAbundance,breaks=seq(0,55,1),main="Histogram of Prey Abundance", xlab = "Prey Abundance")
mean(simplesand$TotAbundance) #11.08333
#A poisson distribution might be appropriate. There are no zeros in the data, but it is count data (bounded at zero) and it is very right skewed.
max(simplesand$TotAbundance)
min(simplesand$TotAbundance) #2

#Run the glm
bar_glm1 <- glm(TotAbundance ~ Inshore_or_Offshore, data = simplesand, family = poisson)
# remember if you don't specify a link, R uses the default, which is ln(Y) in this case

# look at diagnostic plots 
autoplot(bar_glm1)
#The diagnostics plot do not look better than those of the linear model, suggesting that we are still violating the model assumptions.

#Compare linear model to the poisson model (must use same predictor variables and data set)
AIC(bar_mod) #343
AIC(bar_glm1) #376
#The linear model has an AIC score that is much lower than the glm model, suggesting that the linear model is better.

#Interpret stats for the glm
anova(bar_glm1)
#deviance shows how much variability in the data is explained by the model...
anova(bar_glm1, test = "Chisq")
summary(bar_glm1)
# the null deviance is a measure of all the variation in the data (it's not exactly this but can be interpreted like this)
# the residual deviance is what's left after fitting the model
# thus the variability explained by the model is  1-(residual/null)  #1 minus (residual/null)
# so, the variability explained by the model is
#1-() # 
#In other words, the model explains XXX variability.

#Check for overdispersion: (residual deviance) / (residual df) should be <2
# do math here!!!

#EVALUATING GLMs and GLMMs using DHARMa

#We already fit the GLM model above.

#Evaluate the model. Use DHARMa package
#First step is to simulate the randomized quantile residuals. Default is 250 simulations.
res_glmbar1 <- simulateResiduals(fittedModel = bar_glm1, plot = F)
plot(res_glmbar1)
#DHARMa residual diagnostics shows that it is highly overdispersed.
#Test dispersion: confirms overdispersion. The ratio of observed vs. mean simulated variance is greater than 1, indicating overdispersion.
testDispersion(res_glmbar1)

#To account for overdispersion, I add a random effect on Point_on_Sandbar. Now I have a GLMM.
bar_glm2 <- glmer(TotAbundance ~ Inshore_or_Offshore + (1|Point_on_Sandbar), data = simplesand, family = poisson) #random intercept model
summary(bar_glm2)
res_glmbar2 <- simulateResiduals(fittedModel = bar_glm2, plot = F)
plot(res_glmbar2)
testDispersion(res_glmbar2)
#DHARMa residual diagnostics detects significant deviation, including overdispersion.
#This is still highly overdispersed, but better than without random effect.

#Next I will try the negative binomial in order to adjust the dispersion.
#Fit a GLMM with the negative binomial distribution.
bar_glm3 <- glmmTMB(TotAbundance ~ Inshore_or_Offshore + (1|Point_on_Sandbar), data = simplesand, family = nbinom1) #negative binomial
summary(bar_glm3)
res_glmbar3 <- simulateResiduals(fittedModel = bar_glm3, plot = T)
plot(res_glmbar3)
#This looks much better.
testDispersion(res_glmbar3) #no strong evidence of overdispersion! woohoo!
#The ratio is greater than 1 (1.3698), but the p-value is high (0.104). The p-value is based on the distribution of the simulated standard deviations.

#I want to examine this a little further. What about if we remove the random effect? Is the negative binomial still better?
bar_glm4 <- glmmTMB(TotAbundance ~ Inshore_or_Offshore, data = simplesand, family = nbinom1)
summary(bar_glm4)
res_glmbar4 <- simulateResiduals(fittedModel = bar_glm4, plot = F)
plot(res_glmbar4)
testDispersion(res_glmbar4) #Almost significant overdispersion. So random effect is important.
#Including the random effect doesn't change much.

#Now, let's test for zero-inflation of our fave model. MY DATA HAS NO ZEROS (SINCE I ADDED TAXA TOGETHER), SO I'M GOING TO SKIP THIS BECAUSE NOT A PROB.
#Zero-inflation is a type of overdisperson in which there are more zeros in your observed data than expected under
#your fitted model. It puts excess variance in the residuals. This situation is common and there are ways to correct it. 
#Test for zero-inflation: compare distribution of expected zeros in the data against observed zeros
testZeroInflation(res_glmbar3)
#I do not see evidence of zero-inflation, so I decide not to use a zero-inflated model.
#Based on this analysis, I have decided that the model structure I will use is a GLMM with negative binomial and random intercept 
#of Point_on_Sandbar. For Poisson data, zero-inflation tests are often negative, but they show up as underdispersion.
#Thus, it is good that the dispersion test for this model did not suggest underdispersion.

#From Help: The plot shows the expected distribution of zeros against the observed values, 
#the ratioObsSim shows observed vs. simulated zeros. A value < 1 means that the observed data has less zeros than expected, 
#a value > 1 means that it has more zeros than expected (aka zero-inflation). Per default, the function tests both sides.
#So our results indicate that the observed that has less zeros than expected, but p=value is 0.52.

#A better way to differentiate between overdispersion and zero-inflation is to fit a model with zero-inflation and look
#at the parameter estimate for the zero-inflation.
#Fit model with zero-inflation.
zeromod2 <- glmmTMB(TotAbundance ~ Inshore_or_Offshore + (1|Point_on_Sandbar), ziformula = ~1, data = simplesand, family = nbinom1)
summary(zeromod2)
simulationz <- simulateResiduals(fittedModel = zeromod2)
plot(simulationz) #looks no better than other
#compare AIC to select between models with and without zero-inflation
AIC(bar_glm3,zeromod2) 
#deltaAIC is exactly 2 and bar_glm3 is the better model (304.2522)

#Now I need to check for heteroscedasticity.

#Plot standardized residuals vs. fitted
res_bar_glm3 <- residuals(bar_glm3, type='pearson')
plot(res_bar_glm3~fitted(bar_glm3),main="Standardized Residuals vs. Fitted for GLMM", xlab = "Fitted", ylab = "Standardized Residuals")
#Plot standardized residuals vs. the predictor
plot(res_bar_glm3~simplesand$Inshore_or_Offshore,main="Standardized Residuals vs. Predictor for GLMM", xlab = "Predictor", ylab = "Standardized Residuals")

#Fit an alternative GLMM: the null model.
bar_glm5 <- glmmTMB(TotAbundance ~ 1 + (1|Point_on_Sandbar), data = simplesand, family = nbinom1)
summary(bar_glm5)
#Being thorough each checking out this model.
res_glmbar5 <- simulateResiduals(fittedModel = bar_glm5, plot = T)
plot(res_glmbar5)
testDispersion(res_glmbar5)

#Compare models.
AIC(bar_glm3,bar_glm5)
#Model 3 receives much more support (AIC 304) than model 5 (AIC 328), so it looks like the side of the sandbar is important
#for predicting food availability.


#INTERPRET RESULTS. SHOW OUTPUT AND INTERPRET

#Interpret model parameters
summary(bar_glm3)
#The side of the sandbar (inshore vs. offshore) has a strong effect on prey abundance (p-value < 0.001). The slope for the side of the sandbar is -1.0019 (on the log scale),
#indicating that natural log of prey abundance for the offshore side is on average 1.0019 less than that of the inshore side. 
#When I account for the log link, the slope is now 0.3671811, indicating that prey abundance for the offshore side of the sandbar
#is on average a factor of about 0.37 times less than the prey abundance on the inshore side of the sandbar.
exp(-1.0019) #0.3671811
#This finding matches our expectations based on our initial data pictures.
# The random intercept is normally distributed with mean 0 and variance 0.02543. The random intercept indicates that absolute values of abundance differ per site in a random way. 
# In other words, different sites have different prey abundances.

#The negative binomial uses the log link, otherwise known as the natural log (base e).

#Step 5. Plot the model prediction (orange triangle) back onto the data with confidence intervals (orange line) for the fitted value of y
#make prediction dataframe
newdat2 <- expand.grid(Inshore_or_Offshore=c("Inshore","Offshore"))
#design matrix (fixed effects)
mm2 <- model.matrix(delete.response(terms(bar_glm3)),newdat2)
#Linear predictor, back-transform this with the inverse link function
newdat2$TotAbundance <- drop(mm2 %*% fixef(bar_glm3)[["cond"]])
predvar2 <- diag(mm2 %*% vcov(bar_glm3)[["cond"]] %*% t(mm2))
newdat2$SE <- sqrt(predvar2)
newdat_transform2 <- mutate(newdat2, ci_lwr = TotAbundance - (1.96 * SE), ci_upr = TotAbundance + (1.96 * SE))
newdat_transform2 <- mutate(newdat_transform2, TotAbundance = exp(TotAbundance), ci_lwr = exp(ci_lwr), ci_upr = exp(ci_upr))
#plot
pd <- position_dodge(width = 0.4)
g3 <- ggplot(simplesand, aes(x=Inshore_or_Offshore, y=TotAbundance))+
     geom_boxplot(outlier.shape = NA) + #hides outliers
    geom_jitter(alpha=0.5,color = "skyblue2",size=3)
g4 <- g3 +
    geom_point(data=newdat_transform2, shape=17, size=5, position=pd, color="darkorange2") 
#confidence intervals
g5 <- g4 + 
    geom_linerange(data=newdat_transform2, aes(ymin=ci_lwr, ymax=ci_upr),
                  lwd=3, position=pd, color = "darkorange1", alpha = 0.4) + #alpha controls opacity
    theme_bw() +
    ggtitle('Model Predictions with Confidence Intervals Plotted on the Original Data') +
    xlab("Side of Sandbar") + 
    ylab("Prey Abundance") +
    theme(plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="black", size=14, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(color="black", size=14, face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(color="black", size=12),
          axis.text.y = element_text(color="black", size=12),
          # panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "azure")) +
    scale_y_continuous(breaks=seq(0,55,5))
g5

# Thinking about HSC eggs ####
sandbar_prey %>% group_by(Point_on_Sandbar, Inshore_or_Offshore, Time_simple, Taxa) %>%
    summarize(TotAbundance = sum(Abundance)) %>%
    ggplot(data=, aes(x = Inshore_or_Offshore, y = TotAbundance, color=Taxa)) +
    geom_boxplot()

HSC <- subset(sandbar_prey, Taxa == "HSC_egg") %>%
    group_by(Point_on_Sandbar, Inshore_or_Offshore, Time_simple, Taxa) %>%
    summarize(TotAbundance = sum(Abundance)) 

ggplot(data=HSC, aes(x = Inshore_or_Offshore, y = TotAbundance)) +
    geom_boxplot() +
    geom_jitter(alpha=0.5,color = "skyblue2",size=3)+
    scale_y_continuous(breaks=seq(0,35,5))

HSC_In <- subset(HSC, Inshore_or_Offshore == "Inshore")
hist(HSC_In$TotAbundance, breaks=seq(0,35,1), main = "Histogram of HSC egg abundance on inshore side of sandbars")
HSC_In$TotAbundance #1, 2, 13, 33, 30, 2, 2, 1, 1, rest 0

HSC_Off <- subset(HSC, Inshore_or_Offshore == "Offshore")
hist(HSC_Off$TotAbundance, breaks=seq(0,35,1), main = "Histogram of HSC egg abundance on offshore side of sandbars")
HSC_Off$TotAbundance #1, 1, 2, 1, rest 0


#Run linear model
HSC_mod <- lm(TotAbundance ~ Inshore_or_Offshore, data = HSC)
hist(HSC_mod$residuals) # first look at a histogram of the residuals
autoplot(HSC_mod) # autoplot is the ggplot function that automatically generates assumption-checking pictures for you
#Doesn't look good. This data does not meet the assumptions of a linear model.

#A poisson distribution would be a better choice because we are working with counts and MANY zeros.
hist(HSC$TotAbundance)
mean(HSC$TotAbundance) #1.875
#A poisson distribution might be appropriate. There are SO MANY zeros in the data

#EVALUATING GLMs and GLMMs using DHARMa

#Run the glm
hsc_glm1 <- glm(TotAbundance ~ Inshore_or_Offshore, data = HSC, family = poisson)
# remember if you don't specify a link, R uses the default, which is ln(Y) in this case

#Evaluate the model. Use DHARMa package
#First step is to simulate the randomized quantile residuals. Default is 250 simulations.
res_hscglm1 <- simulateResiduals(fittedModel = hsc_glm1, plot = F)
plot(res_hscglm1)
#DHARMa residual diagnostics shows that it is highly overdispersed.
#Test dispersion: confirms overdispersion. The ratio of observed vs. mean simulated variance is greater than 1, indicating overdispersion.
testDispersion(res_hscglm1)

#To account for overdispersion, I add a random effect on Point_on_Sandbar. Now I have a GLMM.
hsc_glm2 <- glmer(TotAbundance ~ Inshore_or_Offshore + (1|Point_on_Sandbar), data = HSC, family = poisson) #random intercept model
summary(hsc_glm2) #model failed to converge. I think this happened because there are so few levels for the random effect.
res_hscglm2 <- simulateResiduals(fittedModel = hsc_glm2, plot = F)
plot(res_hscglm2)
#DHARMa residual diagnostics looks SO MUCH BETTER! But figure out why model failed to converge... Decided to drop the random effect since it didn't converge.

#Next I will try the negative binomial in order to adjust the dispersion.
#Fit a GLMM with the negative binomial distribution.
hsc_glm3 <- glmmTMB(TotAbundance ~ Inshore_or_Offshore + (1|Point_on_Sandbar), data = HSC, family = nbinom1) #negative binomial
summary(hsc_glm3)
res_hscglm3 <- simulateResiduals(fittedModel = hsc_glm3, plot = T)
plot(res_hscglm3)
#Looks pretty good
testDispersion(res_hscglm3) #Almost underdispersed... but p-value is high-sh. Look into this! Possible zero-inflation!!!
#The ratio is less than 1 (0.29786), but the p-value is high (0.544). The p-value is based on the distribution of the simulated standard deviations.

#I want to get rid of the random effect since model 2 didn't converge.
hsc_glm4 <- glmmTMB(TotAbundance ~ Inshore_or_Offshore, data = HSC, family = nbinom1)
summary(hsc_glm4)
res_hscglm4 <- simulateResiduals(fittedModel = hsc_glm4, plot = F)
plot(res_hscglm4) #Looks good.
testDispersion(res_hscglm4) #No overdiserpsion

#Now, let's test for zero-inflation of our fave model.
#Zero-inflation is a type of overdisperson in which there are more zeros in your observed data than expected under
#your fitted model. It puts excess variance in the residuals. This situation is common and there are ways to correct it. 
#Test for zero-inflation: compare distribution of expected zeros in the data against observed zeros
testZeroInflation(res_hscglm3) #ratio: 0.97039, indicates observed data has less zeros than expected. But p-value is 0.88, so not significant.
testZeroInflation(res_hscglm2)
testZeroInflation(res_hscglm4) #no evidence of zero-inflation.
#I do not see evidence of zero-inflation, so I decide not to use a zero-inflated model.
#Based on this analysis, I have decided that the model structure I will use is a GLMM with negative binomial and random intercept 
#of Point_on_Sandbar. For Poisson data, zero-inflation tests are often negative, but they show up as underdispersion.
#Thus, it is good that the dispersion test for this model did not suggest underdispersion.

#From Help: The plot shows the expected distribution of zeros against the observed values, 
#the ratioObsSim shows observed vs. simulated zeros. A value < 1 means that the observed data has less zeros than expected, 
#a value > 1 means that it has more zeros than expected (aka zero-inflation). Per default, the function tests both sides.
#So our results indicate that the observed that has less zeros than expected, but p=value is 0.52.

#A better way to differentiate between overdispersion and zero-inflation is to fit a model with zero-inflation and look
#at the parameter estimate for the zero-inflation.
#Fit model with zero-inflation.
zeromod3 <- glmmTMB(TotAbundance ~ Inshore_or_Offshore, ziformula = ~1, data = HSC, family = nbinom1)
summary(zeromod3)
simulationz3 <- simulateResiduals(fittedModel = zeromod3)
plot(simulationz3) #looks no better than other
#compare AIC to select between models with and without zero-inflation
AIC(hsc_glm4,zeromod3) #same support (HSC_glm4 better by delta AIC 2)

#Compare fave GLM to GLMM (I think I can do this...)
AIC(hsc_glm4, hsc_glm3)
#hsc_glm3 receives significant support (AIC 113.4990) compared to hsc_glm4 (AIC 122.3186)

#Fit an alternative GLMM: the null model.
hsc_glm5 <- glmmTMB(TotAbundance ~ 1 + (1|Point_on_Sandbar), data = HSC, family = nbinom1)
summary(hsc_glm5)
res_hsc_glm5 <- simulateResiduals(fittedModel = hsc_glm5)
plot(res_hsc_glm5)
testDispersion(res_hsc_glm5) #good to go

#Compare models.
AIC(hsc_glm3,hsc_glm5)
#HSC_glm3 (113) receives more support than the null model (AIC 117.5855), suggesting that the side of the sandbar
#is important for HSC egg availability.

#INTERPRET RESULTS. SHOW OUTPUT AND INTERPRET

#Interpet stuff.

#Step 5. Plot the model prediction back onto the data
#make prediction dataframe
newdat3 <- expand.grid(Inshore_or_Offshore=c("Inshore","Offshore"))
#design matrix (fixed effects)
mm3 <- model.matrix(delete.response(terms(hsc_glm3)),newdat3)
#Linear predictor, back-transform this with the inverse link function
newdat3$TotAbundance <- drop(mm3 %*% fixef(hsc_glm3)[["cond"]])
predvar3 <- diag(mm3 %*% vcov(hsc_glm3)[["cond"]] %*% t(mm3))
newdat3$SE <- sqrt(predvar3)
newdat_transform3 <- mutate(newdat3, ci_lwr = TotAbundance - (1.96 * SE), ci_upr = TotAbundance + (1.96 * SE))
newdat_transform3 <- mutate(newdat_transform3, TotAbundance = exp(TotAbundance), ci_lwr = exp(ci_lwr), ci_upr = exp(ci_upr))
#plot
pd <- position_dodge(width = 0.4)
g6 <- ggplot(HSC, aes(x=Inshore_or_Offshore, y=TotAbundance))+
    geom_boxplot() +
    geom_jitter(alpha=0.5,color = "skyblue2",size=3)#, aes(size=..n..))
#scale_size_continuous(breaks=1:4,range=c(2,5))
g7 <- g6 +
    geom_point(data=newdat_transform3, shape=17, size=5, position=pd, color="darkorange2") 
#confidence intervals
g8 <- g7 + 
    geom_linerange(data=newdat_transform3, aes(ymin=ci_lwr, ymax=ci_upr),
                   lwd=3, position=pd, color = "darkorange1", alpha = 0.4) + #alpha controls opacity
    # geom_ribbon(data = newdat_transform2, aes(ymin = ci_lwr, ymax = ci_upr), fill = "darkorange1", alpha = 0.2) +
    theme_bw() +
    ggtitle('Predictions on the Data and Im making this really long so i can learn how to use this \n thing wow isnt this nifty') +
    xlab("Side of Sandbar") + 
    ylab("Total Abundance of Horseshoe Crab Eggs") +
    labs(size = "Sample Size") +
    theme(plot.title = element_text(color="black", size=14, face="bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="black", size=12, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(color="black", size=12, face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
          panel.background = element_rect(fill = "white"),
          # panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "azure")) +
    scale_y_continuous(breaks=seq(0,55,5))
g8


#### THINKING #####

##REASONS I WANT TO WORK WITH POISSON DISTRIBUTION
#I am working with count data.
#There are a lot of zeros.
#Outcome variable is not normally distributed

#Mixed effects model? Nested? Samples collected on both inshore and offshore sides of sandbar at four points. You might expect the samples collected on inshore and offshore sides to be more
#similar at same Point compared to other Points. But they were all collected from the same sandbar so...

#Steps of statistics:
# 1) Data picture
# 2) Run stats test
# 3) Check assumptions
# 4) Interpret stats
# 5) Plot the model back onto the data

#ANOVA: ## ANOVA 
# our anova question: do irises of different species have different sepal widths?

## TWO WAY ANOVA
# our two-way anova question: do flower color and/or size predict the number of insect visits?

