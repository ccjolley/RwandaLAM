# Treat stunting as a binary variable and try out various ML
# models (and feature sets) using caret to see how well it 
# can be predicted.

#### Note 29 August 2016:
# I think I'm going to tie this one off. After spending a *lot* of time on
# feature engineering and tweaking model parameters, variable selection, and
# pre-processing steps, I've succeeded in getting one case of mediocre 
# statistical significance (which you'll find below). I think it's safe to 
# say that the available data (at least the cleaned variables from the
# household and kids modules) might be good enough for a correlative analysis,
# but not for a predictive one. 
#
# If we want to keep pushing for a predictive model, the best bet might be
# to boost the amount of data included by adding in DHS results from previous
# years and/or other Great Lakes countries. On their own, though, the Rwanda 
# data don't support this kind of model building.

source('R/01_RW_cleanDHS.R') # load and clean hh data
source('R/04_RW_cleanDHS_kids.R') # load Nada's work so far
source('R/simple_plots.R')

my_kids <- kids_clean %>% 
  mutate(stunted = as.numeric(height_age_zscore < -2))

###############################################################################
# Create a set of binary variables for each district and province.
###############################################################################
j <- plyr::join(my_kids,geo_clean,by='cluster_num')
for (p in j$province %>% as.character %>% unique) {
  n <- p %>% sub(' ','_',.) %>% paste("prov_",.,sep='')
  my_kids[,n] <- as.numeric(as.character(j$province) == p)
}
for (d in j$district  %>% as.character %>% unique) {
  n <- d %>% sub(' ','_',.) %>% paste("dist_",.,sep='')
  my_kids[,n] <- as.numeric(as.character(j$district) == d)
}

###############################################################################
# Add relevant household-level variables
###############################################################################
kids_add_hh <- kids_clean %>%
  dplyr::select(cluster_hh_num) %>%
  join(hh_clean,by='cluster_hh_num') %>%
  dplyr::select(-cluster_hh_num,-hhid,-cluster_num,-hh_num,-hh_weight,
                -interviewer_id,-sample_strata,-province_id,-district_id,
                -rural) %>%
  # add binary variables for categorical
  mutate(# water sources
         water_indoor = as.numeric(water_source==11),
         water_piped = as.numeric(water_source==12),
         water_standpipe = as.numeric(water_source==13),
         water_borehole = as.numeric(water_source==21),
         water_well_prot = as.numeric(water_source==31),
         water_well_unprot = as.numeric(water_source==32),
         water_spring_prot = as.numeric(water_source==41),
         water_spring_unprot = as.numeric(water_source==42),
         water_open = as.numeric(water_source==43),
         water_rain = as.numeric(water_source==51),
         water_cart = as.numeric(water_source==62),
         # toilet types
         toilet_flush = as.numeric(toilet_type < 16),
         toilet_vip = as.numeric(toilet_type==21),
         toilet_pit_slab = as.numeric(toilet_type==22),
         toilet_pit_open = as.numeric(toilet_type==23),
         toilet_bush = as.numeric(toilet_type==31),
         toilet_composting = as.numeric(toilet_type==41),
         # floor materials
         floor_earth = as.numeric(floor_material==11),
         floor_dung = as.numeric(floor_material==12),
         floor_tiles = as.numeric(floor_material==33),
         floor_cement = as.numeric(floor_material==34),
         floor_carpet = as.numeric(floor_material==35),
         # wall materials 
         wall_cane = as.numeric(wall_material == 12),
         wall_dirt = as.numeric(wall_material == 13),
         wall_bamboo_mud = as.numeric(wall_material == 21),
         wall_stone_mud = as.numeric(wall_material == 22),
         wall_adobe_uncovered = as.numeric(wall_material == 23),
         wall_wood_reused = as.numeric(wall_material == 26),
         wall_cement = as.numeric(wall_material == 31),
         wall_stone_lime = as.numeric(wall_material == 32),
         wall_bricks = as.numeric(wall_material == 33),
         wall_cement_blocks = as.numeric(wall_material == 34),
         wall_adobe_covered = as.numeric(wall_material == 35),
         wall_wood_planks = as.numeric(wall_material == 36),
         # roof materials
         roof_simple = as.numeric(roof_material %in% c(11,12,13,21)),
         roof_tile = as.numeric(roof_material==34),
         roof_bamboo = as.numeric(roof_material==22),
         roof_metal = as.numeric(roof_material==31),
         roof_nice = as.numeric(roof_material %in% c(23,35,36,32,33)),
         # cooking fuel
         fuel_advanced = as.numeric(cooking_fuel %in% c(1,3,4,5)),
         fuel_charcoal = as.numeric(cooking_fuel == 7),
         fuel_wood = as.numeric(cooking_fuel == 8),
         fuel_straw = as.numeric(cooking_fuel == 9),
         fuel_ag_waste = as.numeric(cooking_fuel %in% c(10,11)),
         # adult structure
         adult_one = as.numeric(adult_structure == 1),
         adult_hetero = as.numeric(adult_structure == 2),
         adult_samesex = as.numeric(adult_structure == 3),
         adult_threeplus = as.numeric(adult_structure == 4),
         adult_unrelated = as.numeric(adult_structure == 5),
         # use mosquito net
         mosquito_none = as.numeric(use_mosquito_net %in% c(0,3)),
         mosquito_all = as.numeric(use_mosquito_net == 1),
         mosquito_some = as.numeric(use_mosquito_net == 2),
         # handwashing site
         handwashing_obs = as.numeric(handwashing_site == 1),
         handwashing_nid = as.numeric(handwashing_site == 2),
         handwashing_nop = as.numeric(handwashing_site == 3),
         handwashing_nobs = as.numeric(handwashing_site == 4)) %>%
  dplyr::select(-water_source,-toilet_type,-floor_material,-wall_material,
                -wealth_index,-roof_material,-cooking_fuel,-adult_structure,
                -use_mosquito_net,-handwashing_site)

my_kids <- cbind(my_kids,kids_add_hh)

my_kids <- my_kids %>% 
  select(-height_age_percentile:-weight_height_zscore, # Drop other biometrics
         -caseid,-midx,-hh_num,-cluster_hh_num,-male_weight,# Drop cross-ref variables
         -wealth_index, # wealth_score is better
         -diet_meat_organ,-diet_eggs,-diet_veg_dark_green, # not linearly independent
         -diet_fruit_other,-diet_legumes_nuts,-diet_milk,-motorboat)  

# make factor variables numeric
for (n in names(my_kids)) {
  my_kids[,n] <- as.numeric(my_kids[,n])
}

###############################################################################
# Which variables have a lot of missing values? 
###############################################################################

nm <- is.na(my_kids) %>% colSums() %>% sort(decreasing=TRUE)
nm <- nm / nrow(my_kids)
head(nm,15)

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
my_kids <- my_kids %>% select(-handwashing_water,-has_soap,-kitchen)

###############################################################################
# Strongest 1-to-1 correlations? (Prior to any imputation)
###############################################################################

my_kids$diet_other_food <- na_if(my_kids$diet_other_food,8)
my_kids$diet_meat <- na_if(my_kids$diet_meat,8)
my_kids$wealth_score <- my_kids$wealth_score/1e5

# First, which variables take only two unique values?
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

nrow(na.omit(my_kids)) / nrow(my_kids) # only 19% have no missing values

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

train_imp1 <- complete(train_imp,1) 
test_imp1 <- complete(test_imp,1) 

###############################################################################
# Use stepwise regression to shorten my variable list
###############################################################################
library(MASS)
step1 <- glm(stunted ~ .,data=train_imp1) %>% stepAIC()
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

#### Look at common variable distributions with a box plot
df <- my_kids[,var_list$.[var_list$. != 'stunted' & var_list$Freq==5] %>% as.character]
ggplot(melt(df),aes(variable,value)) +
  geom_boxplot() + coord_flip()

###############################################################################
# Try visualizing with PCA
###############################################################################
scale1 <- rbind(test_imp1,train_imp1) %>% scale_vars
ggplot(melt(scale1),aes(variable,value)) +
  geom_boxplot() + coord_flip()

pca1 <- prcomp(scale1)
summary(pca1)
plot(pca1)
# Out of 45 PCs, it takes the first 32 to get to 90% variance. Doesn't augur
# well for the usefulness of PCA here.

pca1_lab <- pca1$x %>% as.data.frame %>%
  mutate(stunted=c(test_label$outcome,train_label$outcome) %>% 
           as.numeric)

glm(stunted ~ .,data=pca1_lab) %>% summary

# strong correlations with some PCs but not others
ggplot(pca1_lab,aes(x=PC1,y=PC2,color=stunted)) +
  geom_point()
ggplot(pca1_lab,aes(x=PC1,y=PC7,color=stunted)) +
  geom_point()
# PCA looks well-behaved, but separation isn't great, even with strongly-
# correlated dimensions.

###############################################################################
# Best ML model so far
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

# Fit a random forest model to subsets of the variable lists from MASS
# This is my best version; sometimes it's even statistically significant,
# depending on the random seed. They call that p-hacking where I come from.

form_5 <- var_list$.[var_list$. != 'stunted' & var_list$Freq==5] %>% 
  as.character %>% paste(collapse=' + ') %>% paste('outcome ~ ',.) %>% 
  as.formula
rf_mass_vars5 <- train(form_5,
                       data=train_label,
                       metric='ROC',
                       method='rf',
                       trControl=ctrl,
                       preProc = c("zv","center", "scale"))
rf_mass_classes5 <- predict(rf_mass_vars5, newdata = test_label)
confusionMatrix(data = rf_mass_classes5, test_label$outcome)

# Transformations for a few variables that look funny
# These actually hurt model performance when I tried them out.
scale_vars <- function(x,more_vars=NULL) {
  vars <- var_list$.[var_list$. != 'stunted' & var_list$Freq==5] %>% as.character
  xout <- x[,c(vars,more_vars)]
  xout$pig <- log10(1+x$pig)
  xout$sheep <- log10(1+x$sheep)
  xout$goat <- log10(1+x$goat)
  xout$birth_order <- log10(x$birth_order)
  xout$num_kids <- log10(1+x$num_kids)
  xout$num_under5 <- log10(1+x$num_under5)
  logscale <- function(y) {
    log10(1 + y - min(y,na.rm=TRUE)) %>% scale
  }
  xout$wealth_score <- logscale(x$wealth_score)
  xout$mother_height_age_percentile <- 
    logscale(x$mother_height_age_percentile) %>% logscale
  for (n in vars) { # don't apply scaling to additional vars
    xout[,n] <- scale(xout[,n]) %>% as.numeric
  }
  xout
}

###############################################################################
# My list of things that didn't help at all
###############################################################################

# Using all variables rather than output from stepwise regression in an RF
# model (takes forever).

# Using logistic regression to see which of the RHS variables in form_5 
# correlate most strongly with stunting, and only including those in an RF
# model.

# Transforming data with PCA before running an RF model (made things much 
# worse).

# Other model types: gbm, lssvmRadial, svmRadial, nb, knn, nnet -- none could
# beat rf