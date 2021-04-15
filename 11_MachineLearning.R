#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

a_clean <- amniote_data %>% 
    filter(!is.na(litter_or_clutch_size_n)) %>% 
    #filter(litter_or_clutch_size_n >= 1) %>% 
    mutate(y = log1p(litter_or_clutch_size_n)) %>% 
    filter(Class == "Reptilia") %>%
    select(where(~!all(is.na(.x)))) #keeps columns that aren't all missing values; filtering out columns with all missing data

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.

ggplot(a_clean, aes(x = y, fill=Order)) + 
    geom_histogram()

##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.
preprocesses2 <- preProcess(a_clean, method = 'medianImpute')
p_impute2 <- predict(preprocesses2, a_clean)

cols2=c(7:30,32)
p_impute2_data=p_impute2[,cols2]
p_impute2_data<-p_impute2_data[,-2]

dim(p_impute2_data)
names(p_impute2_data)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(p_impute2_data)){
            hist(p_impute2_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}

log_cols2 <- c(3, 4, 5, 6, 7, 9, 10, 12, 13, 14, 17, 19, 20, 21, 22)

p_impute2_data[, log_cols2] <- log1p(p_impute2_data[, log_cols2])

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(p_impute2_data)){
            hist(p_impute2_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
} #Checking the transformed variable distributions.

#Of the four models we will fit, the input variables will need to be log-transformed for the parametric models: the simple linear model and the elastic net model.


##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?
folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

apriori_formula2 <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
reptiles_m0_lm <- train(apriori_formula2, data = p_impute2_data, method = 'lm', trControl = trcntrl, na.action = na.omit)

plotCV(reptiles_m0_lm)

reptiles_m0_lm

summary(reptiles_m0_lm$finalModel)

#The scatterplots of observed versus predicted values show that neither of the linear models for mammals nor reptiles performs particularly well because points are quite
#scattered from the 1-to-1 line.
#The r2 values (proportion of the variance of the held-out observed values explained by the predictions) indicate that the linear model is slightly better at making predictions for
#mammals (r2~0.34) than reptiles (r2~0.29).

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?

enet_gr2 <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
reptile_m1_enet <- train(y ~ ., data = p_impute2_data, method = 'enet', tuneGrid = enet_gr2, trControl = trcntrl, na.action = na.omit)

plotCV(reptile_m1_enet) #Cross-validation: plot observed vs predicted

reptile_m1_enet$results$Rsquared %>% max #elastic_net_summary - best model fit #1

reptile_m1_enet$results %>%
    ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
    geom_line() +
    geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction') # Plot R2 vs regularization strength

#Using the elastic net does not improve prediction relative to the linear model for this dataset. The Rsquared values for both the linear and elastic net model are 
#about 0.29.

##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

gp_gr2 <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
reptile_m2_gp <- train(y ~ ., data = p_impute2_data, method = 'gaussprRadial', tuneGrid = gp_gr2, trControl = trcntrl, na.action = na.omit)

reptile_m2_gp$results %>% ggplot(aes(sigma, Rsquared)) +  ### Plot R2 vs sigma
    geom_line() + geom_point() + xlab('Sigma')

plotCV(reptile_m2_gp) #Cross-validation: plot observed vs predicted

reptile_m2_gp  #gaussian process summary
reptile_m2_gp$results$Rsquared %>% max

compare_models(reptile_m2_gp,reptiles_m0_lm)

#The r2 values (proportion of the variance of the held-out observed values explained by the predictions) indicate that the Gaussian process model is worse at 
#making predictions for reptiles (maximum r2~0.19) than mammals (maximum r2~0.62). This is confirmed by the scatterplots of observed versus predicted values.
#Points on the reptiles scatterplot are more scattered from the 1-to-1 line, indicating worse model performance. 

#Overall, the Gaussian process model performs worse than the linear model.

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
##What does the number of random predictors selected indicate about interaction depth?

rf_gr2 <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
reptile_m3_rf <- train(y ~ ., data = p_impute2_data, method = 'ranger', tuneGrid = rf_gr2, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

reptile_m3_rf$results %>%   ##Plot # of random predictors and minimum node size vs R2
    ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
    geom_line() +
    geom_point() +
    labs(colour = 'min.node.size')

plotCV(reptile_m3_rf) #Cross-validation: plot observed vs predicted

reptile_m3_rf # Random forest summary
reptile_m3_rf$results$Rsquared %>% max

#The node size selected (5) indicates that there is not much noise in the model relative to the signal. This is because the selected node size is the smallest value tested.
#The number of random predictors selected (24) indicates that there may be relatively high interaction depth, either interactions between covariates or uninformative covariates.

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?

compare_models(reptile_m2_gp, reptile_m3_rf)
compare_models(reptile_m1_enet, reptile_m3_rf)
compare_models(reptiles_m0_lm, reptile_m3_rf)

#Overall, the random forest model (rsquared ~ 0.3955) performed best at predicting litter and clutch size for reptiles.
#The best model for mammals was also the random forest model (rsquared ~0.668).
#The Gaussian model performed the worst for reptiles (rsquared ~ 0.19). However, the linear model was worst for mammals (rsquared~0.34).
#These findings suggest that these methods may not be universal, even though the datasets are similar.

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 

varImp(reptile_m1_enet) #adult_body_mass_g : 100
varImp(reptile_m2_gp) #adult_body_mass_g : 100
varImp(reptile_m3_rf) #adult_body_mass_g :100

#The variable that is most important across models is adult body mass.

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
##What does this say about the likely relationship between litter/clutch size and the best predictor variable?

partial(reptile_m2_gp, pred.var = c('adult_body_mass_g'), plot = TRUE) 
partial(reptile_m3_rf, pred.var = c('adult_body_mass_g'), plot = TRUE)

#Generally, the functional form for the relationship between litter/clutch size and adult body mass for both the Gaussian Process model and the random forest model suggests that 
#predicted litter/clutch size increases for reptiles as adult body mass increases. In other words, adults with larger body mass should produce more young.
#The relationship between litter/clutch size and adult body mass for the random forest model appears more complex - the relationship is not smooth like the Gaussian Process model.
#It's important to note that machine learning performance is relatively poor at extreme values, so we should not draw strong conclusions for adults with very low or
#very high body mass.