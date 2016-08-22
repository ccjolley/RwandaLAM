# Treat stunting as a binary variable and try out various ML
# models (and feature sets) using caret to see how well it 
# can be predicted.

source('R/01_RW_cleanDHS.R') # load and clean hh data
source('R/04_RW_cleanDHS_kids.R') # load Nada's work so far
source('R/simple_plots.R')
# most importantly, this creates the data frames kids_clean 
# and kids_diet

my_kids <- kids_clean %>% 
  mutate(stunted = as.numeric(height_age_zscore < -2))

###############################################################################
# Add binary variable for high-stunting districts
###############################################################################
tmp <- my_kids[,c('cluster_num','stunted')] %>% 
  na.omit() %>%
  mutate(val=stunted) %>% 
  plyr::join(geo_clean,by='cluster_num') %>%
  group_by(district) %>%
  dplyr::summarise(yes=sum(val==1),no=sum(val==0)) 

fisher_pval <- function(i) {
  ft <- rbind(tmp[i,c('yes','no')],
              tmp[-i,c('yes','no')] %>% colSums()) %>%
    as.matrix() %>%
    fisher.test(alternative='greater')
  ft$p.value
}

tmp <- tmp %>%
  mutate(mean=yes/(yes+no),
         pval=sapply(1:nrow(tmp),fisher_pval),
         sig=as.numeric(pval < 0.1/nrow(tmp)))
# # Visualization: average stunting rates all over
# my_kids %>% adm2_map('stunted')
# # just pick out the really high ones
# tmp %>%
#   mutate(val=sig,NAME_2=as.character(district)) %>%
#   make_map(low_color='ivory',high_color='firebrick1')
# tmp %>% filter(sig==TRUE)

# create a new feature corresponding to these four districts
j <- join(kids_clean,geo_clean,by='cluster_num') %>%
  join(tmp,by='district') 
my_kids$stunt_geo <- j$sig  

rm(tmp,j)

###############################################################################
# Add relevant household-level variables
###############################################################################
kids_add_hh <- kids_clean %>%
  dplyr::select(-wealth_index,-cluster_num,-hh_num) %>%
  join(hh_clean,by='cluster_hh_num') %>%
  dplyr::select(num_hh,num_under5,urban,water_source,toilet_type,
                electricity,age_head,share_toilet,
                water_treat:water_treat_settle,has_soap,toilet_clean_dry,
                toilet_clean_urine,toilet_clean_flies,wealth_score,kitchen,
                ag_land,livestock:bull) %>%
  # add binary variables for most prominent
  mutate(water_prot_spring = as.numeric(water_source==41),
         water_standpipe = as.numeric(water_source==13),
         water_spring = as.numeric(water_source==42),
         water_open = as.numeric(water_source==43),
         water_piped = as.numeric(water_source==12),
         water_indoor = as.numeric(water_source==11),
         toilet_pit_slab = as.numeric(toilet_type==22),
         toilet_pit_open = as.numeric(toilet_type==23),
         toilet_vip = as.numeric(toilet_type==21),
         toilet_bush = as.numeric(toilet_type==31),
         toilet_flush = as.numeric(toilet_type < 16)) %>%
  dplyr::select(-water_source,-toilet_type)

my_kids <- cbind(my_kids,kids_add_hh)


my_kids <- my_kids %>% 
  select(-height_age_percentile:-weight_height_zscore, # Drop other biometrics
         -caseid,-midx,-hh_num,-cluster_hh_num,  # Drop cross-ref variables
         -wealth_index)  # wealth_score is more informative


###############################################################################
# Which variables have a lot of missing values? 
###############################################################################

nm <- is.na(my_kids) %>% colSums() %>% sort(decreasing=TRUE)
nm <- nm / nrow(my_kids)
head(nm,15)
# TODO: Something's not right with dietary diversity; check Nada's code

#### What's going on with the missing diet variables?
cor(is.na(my_kids$diet_tubers),is.na(my_kids$diet_milk))
# Missing for the same kids
cor(is.na(my_kids$diet_tubers),my_kids$wealth_score)
# No wealth correlation
my_kids %>% mutate(food_na=is.na(diet_tubers)) %>%
  adm2_map(.,'food_na')
# No obvious geographic pattern
# I think (tentatively) that it's safe to impute these

#### Similar sanity checks on maternal height/age
cor(is.na(my_kids$diet_tubers),is.na(my_kids$mother_height_age_zscore))
# No correlation with missing diet info
cor(my_kids$wealth_score,is.na(my_kids$mother_height_age_zscore))
# No correlation with wealth
my_kids %>% mutate(mom_na=is.na(mother_height_age_zscore)) %>%
  adm2_map(.,'mom_na')
# More common in a few districts, but not a striking pattern
# Impute these as well

# Remove variables that are missing a lot
my_kids <- my_kids %>% select(-has_soap,-kitchen)

###############################################################################
# Strongest 1-to-1 correlations? (Prior to any imputation)
###############################################################################

# TODO: I don't know why this didn't get fixed in 04_RW_cleanDHS_kids.R
my_kids$diet_other_food <- na_if(my_kids$diet_other_food,8)
my_kids$diet_meat <- na_if(my_kids$diet_meat,8)
my_kids$wealth_score <- my_kids$wealth_score/1e5

# First, which variables take only two unique values
num_responses <- data.frame(name=names(my_kids),
                            num=sapply(names(my_kids), function(x) 
                              my_kids[,x] %>% na.omit %>% unique %>% length),
                            stringsAsFactors=FALSE) %>% 
  arrange(desc(num))

binary_vars <- num_responses %>% 
  filter(num==2)
binary_vars$cor <- sapply(binary_vars$name,function(x) 
  cor(my_kids[,x],my_kids$stunted,use='complete.obs'))
binary_vars$odds <- sapply(binary_vars$name,function(x) 
  fisher.test(my_kids[,x],my_kids$stunted)$estimate)
binary_vars$pval <- sapply(binary_vars$name,function(x)
  fisher.test(my_kids[,x],my_kids$stunted)$p.value)
# correct for multiple hypothesis testing
binary_vars$pval <- binary_vars$pval*nrow(binary_vars)
binary_vars <- binary_vars %>% arrange(pval)

# There's a solid negative correlation with urbanization and electricity access,
# as well as various types of water treatment (we'd expect this)
# Some diet components seem to hurt in ways that surprise me: 
# tubers, dark green veggies, legumes/nuts. Dairy seems to help.
# Sex doesn't seem to matter much

# Now look at non-binary variables
scale_vars <- num_responses %>% filter(num>2)
scale_vars$cor <- sapply(scale_vars$name,function(x) 
  cor(my_kids[,x],my_kids$stunted,use='complete.obs'))
scale_vars$est <- sapply(scale_vars$name,function(x) {
  tt <- t.test(my_kids[my_kids$stunted==TRUE,x],
               my_kids[my_kids$stunted==FALSE,x])
  tt$estimate[2] - tt$estimate[1]
})
scale_vars$pval <- sapply(scale_vars$name,function(x) {
  tt <- t.test(my_kids[my_kids$stunted==TRUE,x],
               my_kids[my_kids$stunted==FALSE,x])
  tt$p.value
})
# correct for multiple hypothesis testing
scale_vars$pval <- scale_vars$pval*nrow(scale_vars)


scale_vars <- scale_vars %>% arrange(pval)

# Wealth matters a lot, as do the mother's height and education level
# Age matters, though this might be more complicated
# Dietary diversity helps, as do cows
# Longer preceding birth intervals and earlier birth order both help

###############################################################################
# Use multiple imputation to fill in missing values
###############################################################################

# No sense in imputing stunting; will just muddy the waters later on
my_kids <- my_kids %>% filter(!is.na(stunted))

nrow(na.omit(my_kids)) / nrow(my_kids) # only 34% have no missing values

# split into train/test before imputation
library(caret)
set.seed(12345) # imputation of test set fails for some random seed values
Train <- createDataPartition(my_kids$stunted,p=0.75)[[1]]
training <- my_kids[Train,]
testing <- my_kids[-Train,]

# impute based on everything we know about these kids that might be relevant,
# *except* stunting.
pm <- 1 - diag(ncol(my_kids))
pm[,which(names(my_kids)=='stunting')] <- 0
train_imp <- mice(training,predictorMatrix=pm,seed=123)
test_imp <- mice(testing,predictorMatrix=pm,seed=123)

(complete(train_imp,1) %>% na.omit %>% nrow) / (complete(train_imp,1) %>% nrow)
(complete(test_imp,1) %>% na.omit %>% nrow) / (complete(test_imp,1) %>% nrow)
# now ~59% complete

train_imp1 <- complete(train_imp,1) %>% na.omit 
test_imp1 <- complete(test_imp,1) %>% na.omit 

###############################################################################
# Logistic regression
###############################################################################

fit1 <- glm(stunted ~ .,data=train_imp1) 
# TODO: this is klugy -- find a brief way to make them all numeric
test_imp1$water_treat_boil <- as.numeric(test_imp1$water_treat_boil)
test_imp1$water_treat_bleach <- as.numeric(test_imp1$water_treat_bleach)
test_imp1$water_treat_cloth <- as.numeric(test_imp1$water_treat_cloth)
test_imp1$water_treat_filter <- as.numeric(test_imp1$water_treat_filter)
test_imp1$water_treat_solar <- as.numeric(test_imp1$water_treat_solar)
test_imp1$water_treat_settle <- as.numeric(test_imp1$water_treat_settle)
res1 <- data.frame(actual=test_imp1$stunted,pred=predict(fit1,test_imp1))
# Model is rank deficient -- I may want to take some variables out.
summary(fit1) # highlights a few variables for which estimates weren't made

ggplot(res1,aes(pred,actual)) +
  geom_jitter(color='tomato',size=2,alpha=0.1,width=0,height=0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()
# Pseudo-R2
library(pscl)
pR2(fit1)   # McFadden pseudo-R^2 ~ 0.15
# AUC
library(pROC)
r <- roc(res1$actual,res1$pred)
plot(r) # AUC = 0.68; I'd like to see it closer to 0.8

###############################################################################
# Try stepwise regression to shorten my variable list
###############################################################################
library(MASS)
step1 <- stepAIC(fit1) # good to do this with other imputed sets as well
train_imp2 <- complete(train_imp,2) %>% na.omit 
step2 <- glm(stunted ~ .,data=train_imp2) %>% stepAIC()
train_imp3 <- complete(train_imp,3) %>% na.omit 
step3 <- glm(stunted ~ .,data=train_imp3) %>% stepAIC()
train_imp4 <- complete(train_imp,4) %>% na.omit 
step4 <- glm(stunted ~ .,data=train_imp4) %>% stepAIC()
train_imp5 <- complete(train_imp,5) %>% na.omit 
step5 <- glm(stunted ~ .,data=train_imp5) %>% stepAIC()
# consolidate into a consensus list of variables that appear in step1-5
f <- function(x) x$model %>% names
var_list <- c(f(step1),f(step2),f(step3),f(step4),f(step5)) %>% 
  table %>% 
  as.data.frame %>% 
  arrange(desc(Freq))

form <- var_list$.[var_list$. != 'stunted'] %>% as.character %>% 
  paste(collapse=' + ') %>% paste('stunted ~ ',.) %>% as.formula

fit_new <- glm(form,data=train_imp1)
# no more complaints about rank deficiency
res2 <- data.frame(actual=test_imp1$stunted,pred=predict(fit2,test_imp1))
r <- roc(res2$actual,res2$pred)
plot(r) # AUC = 0.7 -- cutting down on variables actually improves things a little

###############################################################################
# Keep my best model so far up here; put others down in odds and ends
###############################################################################

train_label <- train_imp1 %>%
  mutate(outcome=ifelse(stunted,'T','F') %>% as.factor) %>%
  dplyr::select(-stunted,-cluster_num)
test_label <- test_imp1 %>%
  mutate(outcome=ifelse(stunted,'T','F') %>% as.factor) %>%
  dplyr::select(-stunted,-cluster_num)

library(caret)

ctrl <- trainControl(classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# add some basic pre-processing (no imputation or PCA yet)
rf_model2 <- train(outcome~.,
                  data=train_label,
                  metric='ROC',
                  method='rf',
                  trControl=ctrl,
                  preProc = c("zv","center", "scale"))
rfClasses2 <- predict(rf_model2, newdata = test_label)
confusionMatrix(data = rfClasses2, test_label$outcome)
# accuracy = 0.7419, but my 95% CI still doesn't do it.

# try variable lists from MASS
form_1 <- var_list$.[var_list$. != 'stunted'] %>% as.character %>% 
  paste(collapse=' + ') %>% paste('outcome ~ ',.) %>% as.formula
rf_mass_vars1 <- train(form_1,
                      data=train_label,
                      metric='Accuracy',
                      method='rf',
                      trControl=ctrl,
                      preProc = c("zv","center", "scale"))
rf_mass_classes1 <- predict(rf_mass_vars1, newdata = test_label)
confusionMatrix(data = rf_mass_classes1, test_label$outcome)
# accuracy = 0.7419; same accuracy as before with fewer variables
# adding more variables doesn't seem to help, but in the end I might
# replace some of these with others that provide more interpretability

# Things I tried that didn't help:
#    removing some preprocessing steps
#    adjusting model parameters with tuneLength=20
#    combine water source, toilet type, toilet cleanliness, cow-related 
#      into single variables






#    combine toilet cleanliness variables into a single index
train_label %>% filter(toilet_clean_dry==1) %>% summarise(x=mean(outcome=='T')) # 0.255137
train_label %>% filter(toilet_clean_urine==1) %>% summarise(x=mean(outcome=='T')) # 0.3497191
train_label %>% filter(toilet_clean_flies==1) %>% summarise(x=mean(outcome=='T')) # 0.3213675

#    instead of specifying a tolerance on stunt_geo, just use the avg values 
#    and let the rf code find a suitable cutoff

#    do I have anything about the interview date in there? not in MASS results
# Things I haven't included at all (as far as I remember)
#    floor/wall/roof materials
#    cooking fuels
#    mosquito nets
#    iodine test?
#    


###############################################################################
# Odds and ends below here
###############################################################################

########### ML models that didn't perform as well as my best choice so far
system.time(
  rf_model <- train(outcome~.,
                    data=train_label,
                    metric='ROC',
                    method='rf',
                    trControl=ctrl))
# best ROC = 0.753; took ~7 min
rfClasses <- predict(rf_model, newdata = test_label)
confusionMatrix(data = rfClasses, test_label$outcome)
# accuracy = 0.7323
# so the 95% confidence interval on accuracy still contains the no-information
# rate... I can't say definitively that this works (yet).

# add PCA
rf_model3 <- train(outcome~.,
                   data=train_label,
                   metric='ROC',
                   method='rf',
                   trControl=ctrl,
                   preProc = c("zv","center", "scale",'pca'))
rfClasses3 <- predict(rf_model3, newdata = test_label)
confusionMatrix(data = rfClasses3, test_label$outcome)
# accuracy = 0.7094; PCA makes things worse than the NIR.

# add knn imputation
rf_model4 <- train(outcome~.,
                   data=train_label,
                   metric='ROC',
                   method='rf',
                   trControl=ctrl,
                   preProc = c("zv","center", "scale",'knnImpute'))
rfClasses4 <- predict(rf_model4, newdata = test_label)
confusionMatrix(data = rfClasses4, test_label$outcome)
# accuracy = 0.7342; knn imputation also doesn't help

# I've also heard good things about Gradient-Boosted Machines...
system.time(
  gbm_model <- train(outcome~.,
                     data=train_label,
                     metric='ROC',
                     method='gbm',
                     trControl=ctrl,
                     preProc = c("zv","center", "scale"),
                     verbose=FALSE))
# finished in 45s
gbmClasses <- predict(gbm_model, newdata = test_label)
confusionMatrix(data = gbmClasses, test_label$outcome)
# accuracy = 0.7323; doesn't beat random forests

# also maybe try svm or nnet?
system.time(
  svm_model <- train(outcome~.,
                     data=train_label,
                     metric='Accuracy',
                     method='lssvmRadial',
                     preProc = c("zv","center", "scale")))
# took 87s
# FWIW, lssvmLinear and lssvmPoly crashed.
svmClasses <- predict(svm_model, newdata = test_label)
confusionMatrix(data = svmClasses, test_label$outcome)
# accuracy = 0.7075; worse than NIR

# try only the variables present in all 5 MASS results
form_5 <- var_list$.[var_list$. != 'stunted' & var_list$Freq == 5] %>% 
  as.character %>% paste(collapse=' + ') %>% paste('outcome ~ ',.) %>% 
  as.formula
rf_mass_vars5 <- train(form_5,
                       data=train_label,
                       metric='Accuracy',
                       method='rf',
                       trControl=ctrl,
                       preProc = c("zv","center", "scale"))
rf_mass_classes5 <- predict(rf_mass_vars5, newdata = test_label)
confusionMatrix(data = rf_mass_classes5, test_label$outcome)
# accuracy = 0.7323; not an improvement

# try adding some of the strong pairwise correlates back in
add_back <- c('electricity','water_treat_boil','urban','water_piped',
              'water_treat','toilet_bush','toilet_flush','toilet_clean_urine',
              'water_spring','mother_ed_level','age_calc_months','WDDS_total')
form_add <- var_list$.[var_list$. != 'stunted'] %>% as.character %>%
  c(add_back) %>% paste(collapse=' + ') %>% paste('outcome ~ ',.) %>% 
  as.formula
rf_mass_vars_add <- train(form_add,
                          data=train_label,
                          method='rf',
                          trControl=ctrl,
                          preProc = c("zv","center", "scale"))
rf_mass_classes_add <- predict(rf_mass_vars_add, newdata = test_label)
confusionMatrix(data = rf_mass_classes_add, test_label$outcome)
# No improvement in accuracy

nn_mass_vars1 <- train(form_1,
                       data=train_label,
                       metric='Accuracy',
                       method='nnet',
                       trControl=ctrl,
                       preProc = c("zv","center", "scale"),
                       tuneLength=20)
nn_mass_classes1 <- predict(nn_mass_vars1, newdata = test_label)
confusionMatrix(data = nn_mass_classes1, test_label$outcome)
# accuracy = 0.7189; doesn't beat RF. Took forever to run.

# if rf works well, maybe I should try similar methods:
# Boruta, cforest, extraTrees, ORFlog (and related), QRF, ranger, rrf, wsrf
# TODO: factor out a function to test all of these with the same options

test_model <- function(method,preProc=c('zv','center','scale')) {
  # Try out different ML models with default parameters to see what can
  # work the best with the data I've got
  my_model <- train(outcome~.,
                    data=train_label,
                    metric='Accuracy',
                    method=method,
                    preProc = preProc)
  my_classes <- predict(my_model, newdata = test_label)
  confusionMatrix(data = my_classes, test_label$outcome)
}

############################################

# First, use logistic regression to see what looks important
kids_reg <- my_kids %>%
  dplyr::select(sex,mother_ed_level,mother_ed_year,
         child_milk:child_other_food,birth_interval_preceding,birth_order,
         age_calc_months,wealth_index,stunted)
glm(stunted~.,family=binomial(link='logit'),data=kids_reg) %>% summary()
# sex, child_milk, child_tubers, child_veg_dark_green, child_fruit_other,
# child_meat_organ, child_legumes_nuts, age_calc_months, wealth_index

library(caret)

# simplest feature set -- only wealth quintile matters
feat1 <- my_kids %>% 
  dplyr::select(wealth_index,stunted) %>% 
  na.omit()

# first off, how well do we do if we assume no one is stunted?
sum(feat1$stunted=='F')/sum(!is.na(feat1$stunted))
# 68.25% accuracy!

train(stunted ~ .,
      data = feat1,
      trControl = trainControl(method='cv',number=10),
      method = "nb")
# That was useless

feat2 <- my_kids %>% 
  dplyr::select(sex, child_milk, child_tubers, child_veg_dark_green, 
                child_fruit_other, child_meat_organ, child_legumes_nuts, 
                age_calc_months, wealth_index, stunted) %>%
  na.omit()
sum(feat2$stunted=='F')/sum(!is.na(feat2$stunted)) # 69.1% null accuracy
train(stunted ~ .,
      data = feat2,
      trControl = trainControl(method='cv',number=10),
      tuneLength=15,
      method = "nb") 
# 69% without tuning. Maybe NB just isn't up to this?
train(stunted ~ .,
      data = feat2,
      trControl = trainControl(method='cv',number=10),
      method = "svmRadial") 
# pushes us up to 70% accuracy. Still pretty lame
train(stunted ~ .,
      data = feat2,
      trControl = trainControl(method='cv',number=10),
      method = "knn",
      tuneLength=20)
# still only a tiny improvement

# Let's use some geographic information

feat3 <- my_kids %>% 
  dplyr::select(sex, child_milk, child_tubers, child_veg_dark_green, 
                child_fruit_other, child_meat_organ, child_legumes_nuts, 
                age_calc_months, wealth_index, stunt_geo,
                stunted) %>%
  na.omit()

train(stunted ~ .,
      data = feat3,
      trControl = trainControl(method='cv',number=10),
      method = "svmRadial") 
# this is an improvement over feat2, but only a small one

# add in some household-level variables and see if these help

         
         
# TODO: may want to add livestock & ag assets
feat3 <- cbind(my_kids,kids_add_hh) %>%
  dplyr::select(stunted, sex, child_milk, child_tubers, child_veg_dark_green, 
         child_fruit_other, child_meat_organ, child_legumes_nuts, 
         age_calc_months, wealth_index, stunt_geo,num_hh,num_under5,urban,
         electricity,age_head,share_toilet,
         water_treat:water_treat_settle,has_soap,toilet_clean_dry,
         toilet_clean_urine,toilet_clean_flies,wealth_score,kitchen,
         ag_land,livestock,water_prot_spring:toilet_flush) 

nrow(na.omit(feat3)) / nrow(feat3) # looks like we'll need imputation
imp3 <- mice(feat3) # might be better to leave stunting out
form3 <-  dplyr::select(feat3,-stunted) %>% names() %>% 
  paste(collapse=' + ') %>% paste('stunted ~ ',.,collapse='')
fit3 <- with(data=imp3,exp=glm(as.formula(form3),family=binomial(link='logit')))
summary(fit3) # this actually doesn't summarize very well...

# When we add in more data, some things (like milk consumption) look significant
# that didn't before. This could also be a result of imputation.
# Geography seems to matter a lot. Treatment of water with
# filters seems to hurt. Some of the correlations have directionality
# that seems counterintuitive, but that can easily happen in a big multiple 
# regression. 

# What if we just throw everything (from one imputation) in a naive Bayes?
# (might be more robust to toss them all in)
feat3_1 <- complete(imp3,1)
train(stunted ~ .,
      data = feat3_1,
      trControl = trainControl(method='cv',number=10),
      tuneLength=15,
      method = "nb") 
# accuracy = 68.9% -- no real improvement

# I think I need to spend some more time exploring and visualizing
# this data to get a better idea of what's important before throwing
# everything in a ML model.



