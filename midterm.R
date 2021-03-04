# Elizabeth Bouchard
# March 4, 2021

### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, 
# such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv' contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site 
# within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands


##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

rm(list = ls()) # clear R's brain

#load libraries to be used throughout the midterm
library(tidyr)  # tidies data
library(dplyr)  # manipulates data
library(tidyverse)
library(ggplot2)  # graphs 
library(lubridate)  #deals with dates
library(ggfortify) # ggplot needs this to use autoplot for lm


#getting the data
bees <- read.csv('pinelands_bees.csv')  
lc <- read.csv('land_cover.csv')

#looking at the data to figure out which column to use to join the two dataframes
glimpse(bees)
summary(bees)
glimpse(lc)
summary(lc)
#It looks like the key should be site_name.
#how many sites? 

#Join two dataframes. I chose to use a left join because it keeps all rows of first dataframe, and adds any matching rows of second dataframe.
beedata <- left_join(bees, lc, key = site_name)
beedata #look at the dataframe to make sure the dataframes were joined correctly. There should be 9 variables and the data in each row should line up appropriately.

#Check for NA values
View(beedata)
unique(beedata$genus)
unique(beedata$species)
unique(beedata$sex)
unique(beedata$date)
unique(beedata$round)
unique(beedata$method)
unique(beedata$site_name)
unique(beedata$Nat1600)

#Look at data again to make sure that it is tidy. It appears tidy because each column is a variable and each row is an observation. It is in long format rather than wide format.
glimpse(beedata)
summary(beedata)

#Check variable types and change character variables to factors. Factors are categorical variables with levels. Mutate is used to add a new variable that is created from an existing variable.
glimpse(beedata)
beedata <- mutate(beedata, genus=factor(genus))
beedata <- mutate(beedata, species=factor(species))
beedata <- mutate(beedata, genus_species=factor(genus_species))
beedata <- mutate(beedata, sex=factor(sex))
beedata <- mutate(beedata, round=factor(round))
beedata <- mutate(beedata, method=factor(method))
beedata <- mutate(beedata, site_name=factor(site_name))
summary(beedata) #using summary() to check my work and notice that date is a character

#Change the dates to standard date format (year-month-day) using lubridate with mutate to create a new version of date and replace the old date
beedata <- mutate(beedata, date = mdy(date))
glimpse(beedata) #Check my work

#At this point, I realize that I need to use group_by and summarize to create a new dataframe that has ONLY the variables I need: site, land cover, number of bees collected. Oops!
beesite <- beedata %>%     #make a data frame named beesite. First, take the bee dataframe
    group_by(site_name, Nat1600) %>%    #subset data by site and include land cover
    summarize(bee_number = n())  #then counts the rows (aka number of bees in each site) and name this column bee_number

#Check my work. Make sure there is correct variable types, number of rows and columns.
glimpse(beesite)

## 2 Data picture
# plot the data and figure out what type of model might be best
ggplot(data = beesite, aes(x = Nat1600, y = bee_number)) +
    geom_point() +
    theme_bw()
#Since the question is "Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?", I used
#a scatterplot with Nat1600 on the x axis and the number of bees at each site on the y axis.
#I think regression is appropriate because the question is asking whether a continuous variable is associated with a continuous variable.
#At first glance, it appears that there could be a negative linear relationship between these two variables with the number of bees found
#at a site declining as the percentage of the natural land cover surrounding the site increases.
#Since linearity and homoskedacticity look relatively okay, I am going to try a linear model.
#The slope will probably be negative and the intercept might be ~90.

hist(beesite$bee_number) #Plot a histogram of the number of individual bees collected at each site to check for normal distribution. It appears relatively normal.


## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

bee_mod <- lm (bee_number ~ Nat1600, data = beesite) #run the model
hist(bee_mod$residuals)  #Plot histogram of residuals to check assumption that residuals are normally distributed. Distribution is not ideal, but relatively normal. Somewhat skewed to the right.
autoplot(bee_mod) # Use autoplot to automatically generate assumption-checking pictures.

# The residuals versus fitted values figure checks the assumption that a line is an appropriate fit to the data. The points in this figure do not show a clear pattern.
#However, strange scatter may be due to relatively low sample size. This suggests that a linear fit might be appropriate for our data.
# The Normal Q-Q plot shows that the residuals are somewhat normal, although points on the far right deviate from the dashed line. This supports my initial
#idea that it is skewed to the right.
#The scale-location plot checks the assumption that variance is similar across the range of X values. The vertical spread increases somewhat as fitted values increase,
#suggesting slight heteroskedasticity. It looks reasonable, so I will proceed.
#The residuals vs. leverage plot checks the assumption that the results do not depend on a small proportion of the data. There is one point in the top right section 
#of the plot that may have a slightly greater influence.

#I think these data meet the assumptions for a linear model. 
#Although this is count data (number of bees collected at each site), I will not use a generalized linear model.
#I think the numbers are large enough that we do not need to use a poisson distribution. Also, as mentioned before, 
#the residuals of the response variable appear relatively normally distributed.


## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

summary(bee_mod)
#For every 1 unit increase in the percentage of the natural land cover surrounding the site 
# within a 1600m radius (Nat1600), the number of individual bees collected at a site decreases by about 0.4 bees on average.
#If in reality the slope was 0, the probability of the number of individual bees collected at a site decreases by about 0.4 bees 
#on average for every 1 unit increase in natural land cover would be 0.0293 (p-value).
#As the percentage of natural land cover approaches zero, the number of bees collected per site increases toward about 73 bees.
#If in reality the number was 0 bees per site, the probability of getting the intercept we got (about 73 bees), would be about 1.96e-05 (p-value).
#For the model, the associated F-statistic is 5.3 with a p-value of 0.02927. These values provide information about the ratio of what is explained by the model to what is not explained.
#Using this linear model, percentage of natural land cover appears to predict the number of individual bees collected at a site. 
#As natural land cover increases, the number of individual bees collected at a site tends to decrease.
#Given the initial research question, I am somewhat confident in this analysis. However, I assume that natural land provides bee habitat, so 
#my expectation would be that bee abundance would increase with natural land cover. As such, I do have doubts about this analysis. 
#If I were to perform it again, I would want to incorporate other predictor variables into the analysis.
#For example, I think sampling effort could play a major role in predicting the number of bees collected at a site. If more people were collecting bees at
#certain sites than others, then you would expect more bees to be collected at those sites. I think this variable could potentially be a very important
#predictor for the number of individual bees collected at a single site and it should be accounted for in the future.


# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?


## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want

ggplot(data = beesite, aes(x = Nat1600, y = bee_number)) +
    geom_point() +
    theme_bw()+
    geom_smooth()



## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

#I would add site as a random effect. A random effect accounts for nested data. I think this makes sense for site because bee abundance and the 
#number of bees collected is likely to be more related at each of the individual sites. Incorporating this random effect would allow us to control for 
#this. Advantages for random effects are that we would use fewer degrees of freedom than a fixed effect and it would help control for the nested data structure.


### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.


# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)


modSeldata <- read.csv('modSel.csv') #Load data
glimpse(modSeldata)#Look at data
summary(modSeldata)

# We are working with count data so the two possible error distributions are Poisson and negative binomial.
# Use the fitdist() function from the fitdistrplus package to test Poisson and negative binomial.
#Save the results to objects and then use summary() to get info
library('fitdistrplus')
fitp <- fitdist(modSeldata$observedAbundance,"pois") #fit poisson
summary(fitp)
plot(fitp)
fitnb <- fitdist(modSeldata$observedAbundance, "nbinom") #fit negative binomial
summary(fitnb)

#The best error structure for our data is negative binomial because it has the lowest AIC score (290) compared to the poisson distribution (353).
#AIC takes the number of parameters into account, so it is telling that the change in AIC is so great despite the negative binomial being penalized for
#an extra parameter. Likelihood also supports the conclusion that negative binomial is the appropriate error structure/error distribution. The loglikelihood
#for the negative binomial is -143 (compared to -175 for poisson), which indicates that the probability of observing these data is higher given an underlying
#negative binomial distribution compared to an underlying poisson distribution.


# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines

annualtemp <- glm(observedAbundance ~ meanAnnualTemp, family = "poisson", data = modSeldata)
summertemp <- glm(observedAbundance ~ meanSummerTemp, family = "poisson", data = modSeldata)

annualprecip <- glm(observedAbundance ~ annualPrecipitation, family = "poisson", data = modSeldata)
summerprecip <- glm(observedAbundance ~ summerPrecipitation, family = "poisson", data = modSeldata)

edgedist <- glm(observedAbundance ~ distance2edge, family = "poisson", data = modSeldata)
edgetot <- glm(observedAbundance ~ totalEdge, family = "poisson", data = modSeldata)

# I am comparing these models using the model.sel() function from the MuMIn package:
library('MuMIn')
model.sel(summertemp, annualtemp) #The model includes the main effect of summer temperature better predicts observed abundance given our data and model set. 
                                    #because these two models differ by a deltaAIC greater than 3 with the annualtemp model having the lower AIC score.
model.sel(summerprecip, annualprecip) #The model that includes the main effect of summer precipitation better predicts observed aduncdance given our data and model set (deltaAIC = 3.69).
model.sel(edgedist, edgetot) #The model that includes the total edge better predicts observed abundance given our data and model set (deltaAIC 45.48).






# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

#Run models
null <- glm(observedAbundance ~ 1, family = "poisson", data = modSeldata)
summertemp <- glm(observedAbundance ~ meanSummerTemp, family = "poisson", data = modSeldata)
summerprecip <- glm(observedAbundance ~ summerPrecipitation, family = "poisson", data = modSeldata)
edgetot <- glm(observedAbundance ~ totalEdge, family = "poisson", data = modSeldata)
global <- glm(observedAbundance ~ totalEdge + summerPrecipitation + meanSummerTemp, family = "poisson", data = modSeldata)
edgeprecip <- glm(observedAbundance ~ totalEdge + summerPrecipitation, family = "poisson", data = modSeldata)
edgetemp <- glm(observedAbundance ~ totalEdge + meanSummerTemp, family = "poisson", data = modSeldata)
preciptemp <- glm(observedAbundance ~ summerPrecipitation + meanSummerTemp, family = "poisson", data = modSeldata)

#Make table comparing models based on AIC
model.sel(null, summertemp,summerprecip, edgetot, global, edgeprecip,edgetemp,preciptemp, rank = AIC, rank.args = alist(k = log(nobs(x))))

# There is an 89% probability that the model with total edge and mean summer temperature is the "best" model for predicting observed abundance given our model set and data.
# The delta AIC between the two top models is 4.21, providing substantial support for our "best" model given the model set.

# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

#The results support the hypothesis that total edge and mean summer temperature predict observed abundance of this species. 
#Total edge appears to be slightly more important than mean summer temperature because the magnitude of the coefficient is greater in the model with the lowest AIC score (268.6).
#However, mean summer temperature was present in the four top models, whereas total edge was present in only two of the four top models (ranked according to AIC score).
#I conclude that total edge and mean summer temperature affect the observed abundance of this species. The species is more abundant when mean summer temperature is high
#and total edge is low.
#Summer precipitation appears to be the least important predictor variable. The model with summer preciptation on its own as the predictor variable has
#a lower AIC score than the null model. There is less than 2 deltaAIC between this model and the null, suggesting that summer precipitation (on its own)
#is relatively unimportant.






