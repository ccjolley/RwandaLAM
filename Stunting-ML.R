# Treat stunting as a binary variable and try out various ML
# models (and feature sets) using caret to see how well it 
# can be predicted.

source('R/01_RW_cleanDHS.R') # load and clean hh data
source('R/04_RW_cleanDHS_kids.R') # load Nada's work so far
source('R/simple_plots.R')
# most importantly, this creates the data frames kids_clean 
# and kids_diet

my_kids <- kids_clean %>% 
  mutate(stunted = height_age_zscore < -2,
         stunted = as.factor(ifelse(stunted,'T','F')))

###############################################################################
# Add binary variable for high-stunting districts
###############################################################################
tmp <- my_kids[,c('cluster_num','stunted')] %>% 
  na.omit() %>%
  mutate(val=as.numeric(stunted=='T')) %>% 
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
# my_kids %>% 
#   mutate(val=as.numeric(stunted=='T')) %>%
#   adm2_map('val')
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
  dplyr::select(-wealth_index) %>%
  join(hh_clean,by=c('cluster_num','hh_num')) %>%
  dplyr::select(num_hh,num_under5,urban,water_source,toilet_type,
                electricity,age_head,share_toilet,
                water_treat:water_treat_settle,has_soap,toilet_clean_dry,
                toilet_clean_urine,toilet_clean_flies,wealth_score,kitchen,
                ag_land,livestock) %>%
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
  select(-weight_kg:-weight_height_std, # Drop other biometrics
         -caseid,-midx,-hh_num,-hwidx,  # Drop cross-ref variables
         -wealth_index)  # wealth_score is more informative


###############################################################################
# Which variables have a lot of missing values? 
###############################################################################

nm <- is.na(my_kids) %>% colSums() %>% sort(decreasing=TRUE)
nm <- nm / nrow(my_kids)
head(nm,15)


#### What's going on with the missing diet variables?
cor(is.na(my_kids$child_tubers),is.na(my_kids$child_milk))
# Missing for the same kids
cor(is.na(my_kids$child_tubers),my_kids$wealth_score)
# No wealth correlation
my_kids %>% mutate(food_na=is.na(child_tubers)) %>%
  adm2_map(.,'food_na')
# No obvious geographic pattern
cor(is.na(my_kids$child_tubers),is.na(my_kids$stunted))
# No correlation with whether measurements were recorded
cor(is.na(my_kids$child_tubers),as.numeric(my_kids$stunted=='T'),use='complete.obs')
# No correlation with stunting (when present)
# I think (tentatively) that it's safe to impute these

# After the dietary variables, the next-most-common missing
# variable is birth_interval_preceding
cor(is.na(my_kids$birth_interval_preceding),my_kids$birth_order==1)
# these are first children; replace NA values with zero
my_kids[is.na(my_kids$birth_interval_preceding),'birth_interval_preceding'] <- 0

# Remove variables that are missing more often than 'stunted'
my_kids <- my_kids %>%
  select(-age_death,-has_soap,-kitchen)

###############################################################################
# Use multiple imputation to fill in missing values
###############################################################################

kids_imp <- mice(my_kids)
imp1 <- complete(kids_imp,1)

###############################################################################
# Strongest 1-to-1 correlations (use un-imputed data at this step)
###############################################################################

# First, find all binary variables (use imp1 to avoid NA)
single_cor <- data.frame(name=names(my_kids)) %>%
  mutate(uniq = sapply(names(my_kids),function(x) length(unique(imp1[,x]))),
         pval=NA)

single_cor[single_cor$uniq==2,'pval'] <- 
  sapply(single_cor[single_cor$uniq==2,'name'],function(x) {
    print(x)
    prop.test(my_kids$stunted,my_kids[,x])$p.value
    })

# TODO: this doesn't work yet. Try fisher.test?
  



###############################################################################
# Odds and ends below here
###############################################################################


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



