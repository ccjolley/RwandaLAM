# Cleanup kids dataset for Rwanda DHS data --------------------------
# Nada Petrovic, USAID PPL & GeoCenter, npetrovic@usaid.gov
# 16 June 2016
# (c) 2016 via MIT License

## PULLING VARIABLES WE WANT

## Reads in excel spreadsheet that states which variables to keep
## from kids data set
kids_labels_tokeep<-read.csv('Excel/kids_labels_tokeep.csv')

## Relabels "NA" values (ie variables that have not been decided on yet) as 0
## so that they are not selected. From the Excel spreadsheet pulls the list 
## of variables to keep and what they should be renamed.
kids_labels_tokeep$Keep[is.na(kids_labels_tokeep$Keep)] <- 0
data_subset_vars <- as.character(kids_labels_tokeep$var[kids_labels_tokeep$Keep==1]) 
data_rename_vars <- as.character(kids_labels_tokeep$renamedVar[kids_labels_tokeep$Keep==1]) 

## Creates new clean data frame that is a subset of the overall data frame, 
## and renames the variables.
kids_clean <- kids_all[data_subset_vars]
names(kids_clean) <- data_rename_vars

## Creates functions that will be useful for pulling attributes/variable codes later
source('R/VarCodeDescrip_Functions.R')

## Creates smaller data frame of labels to make it easier to query relevant info
kids_clean_labels<-make_cleanLabelMat(kids_labels,data_subset_vars,data_rename_vars)

## Creates household id variable for merging with hh data set, note
## this is not the same as hhid due to weirdness in spacing of DHS
## paste job.  It can only be merged with a homemade household id ## like this one.
kids_clean$cluster_hh_num <- paste(kids_clean$cluster_num, kids_clean$hh_num)

##RECODING AND CLEANING VARIABLES

## Age in months ## 
## Calculate from interview date & dob 
## Note: no NA's in either interview date or dob, and values look normal
kids_clean$age_calc_months <- (kids_clean$interview_date_cmc-kids_clean$dob_cmc)
## table(kids_clean$age_calc_months, exclude=NULL) #output looks good, no NAs, min=0 max=59
## qplot(kids_clean$age_calc_months/12) #looks fairly flat, bump around 6 mo, fewer 4-5

## Wealth Index ##

#table(kids_clean$wealth_index,exclude=NULL) #output looks good, no NAs, values are: 1:5
#qplot(kids_clean$wealth_index, geom="bar") #1 has the most, somewhat flat otherwise

## Sex, 0=male, 1=female
#table(kids_clean$sex,exclude=NULL)/nrow(kids_clean) #looks good, no NAs, M:50.6% F:49.4%
kids_clean$sex<-kids_clean$sex-1 #recodes so that male is 0 and female is 1

## Stunting, Wasting, Body Mass Index, and Mother's Stunting ##
kids_clean <- kids_clean %>%
  mutate(height_age_percentile = na_if(height_age_percentile,9998),
         height_age_percentile = height_age_percentile/100,
         height_age_zscore     = na_if(height_age_zscore,9998),
         height_age_zscore     = height_age_zscore/100,
         weight_age_percentile = na_if(weight_age_percentile,9998),
         weight_age_percentile = weight_age_percentile/100,
         weight_age_zscore     = na_if(weight_age_zscore,9998),
         weight_age_zscore     = weight_age_zscore/100,
         weight_height_percentile = na_if(weight_height_percentile,9998),
         weight_height_percentile = weight_height_percentile/100,
         weight_height_zscore     = na_if(weight_height_zscore,9998),
         weight_height_zscore     = weight_height_zscore/100,
         mother_height_age_percentile = na_if(mother_height_age_percentile,9998),
         mother_height_age_percentile = mother_height_age_percentile/100,
         mother_height_age_zscore     = na_if(mother_height_age_zscore,9998),
         mother_height_age_zscore     = mother_height_age_zscore/100)


## Mother's Education ##

# Highest educational level with 0=none, 1=primary, 2=secondary, 3=higher
# table(kids_clean$mother_ed_level,exclude=NULL) #No NAs
# qplot(kids_clean$mother_ed_level,geom="bar") # Mostly primary

# Highest year of education 0-8
# table(kids_clean$mother_ed_year,exclude=NULL) #NAs=1147 -not sure why this is so many more # than levels
# qplot(kids_clean$mother_ed_year,geom="bar") #mostly 4-6 years

## Child's diet
# Note: Exact wording of the question is: "Now I would like to ask you about liquids or foods #that (NAME FROM 649) had yesterday during the day or at night. I am interested in whether your child had the item I mention even if it was combined with other foods. Did (NAME FROM 649) drink or eat:_____ [Where 649 refers to question about "Youngest child living with her born between 2013 and 2015]

## Checking if "gave child" variables vary by child within the household, by seeing if the sum total of all the food variables + caseid has more uniqueness than caseid alone
#diet_food_tot <- rowSums(select(kids_clean, contains("diet")), na.rm=TRUE)
#length(unique(kids_clean$caseid)) ## Output: [1] 5955
#length(unique(paste(kids_clean$caseid,diet_food_tot))) ## Output: [1] 5955
## It does not seem to vary per child because both outputs are the same

# make submatrix of diet div calculations 
kids_diet <- select(kids_clean,contains("diet"), -diet_other_food)

kids_diet<-na_if(kids_diet,8)

## Checking if "gave child" variables vary by child, by seeing if the sum total
## of all the food variables + caseid has more uniqueness than caseid alone
child_food_tot <- rowSums(select(kids_clean, contains("child")), na.rm=TRUE)

length(unique(kids_clean$caseid))
## Output: [1] 5955

length(unique(paste(kids_clean$caseid,child_food_tot)))
## Output: [1] 5955
## It does not seem to vary per child

# Wording of question: "Now I would like to ask you about liquids or foods #that (NAME FROM 649) had yesterday during the day or at night. 
#I am interested in whether your child had the #item I mention even if it was #combined with other foods.
#Did (NAME FROM 649) drink or eat:
# Where 649 refers to question about "Youngest child living with her born #between 2013 and #2015"

## Checking if there are rows with NAs in some but not all entries
table(rowSums(is.na(kids_diet)))
#Output:
#   0    1    3   13 
#4534   48    1 3273 

##Indices of the rows that are all NAs
#rows_allNAs <- rowSums(is.na(kids_diet)) == 13
#summary(kids_diet)

## Calculate WDDS
## The categories are: 1. Starchy staples (WDDS_starch) 
##                     2. Dark green leafy vegetables (WDDS_veg_green) 
##                     3. Other Vitamin A rich fruit and veg (WDDS_vitA)
##                     4. Other fruit and veg (WDDS_veg_other)
##                     5. Organ meat (WDDS_organ)
##                     6. Meat and fish (WDDS_meat_fish)
##                     7. Eggs (WDDS_eggs)
##                     8. Legumes, nuts, and seeds (WDDS_legumes)  
##                     9. Milk and milk products (WDDS_dairy)

na0 <- function(x,y=NULL) {
  # more flexible version would allow arbitrary number of inputs
  if (is.null(y)) ifelse(!is.na(x),x,0)
  else {
    nax <- ifelse(!is.na(x),x,0)
    nay <- ifelse(!is.na(y),y,0)
    ifelse(nax+nay>0,1,0)
  }
}

kids_diet <- kids_diet %>%
  mutate(WDDS_starch=na0(diet_tubers,diet_cereals),
         WDDS_veg_green=na0(diet_veg_dark_green),
         WDDS_vitA=na0(diet_veg_yellow_orange,diet_fruit_vit_a),
         WDDS_veg_other=na0(diet_fruit_other),
         WDDS_organ=na0(diet_meat_organ),
         WDDS_meat_fish=na0(diet_meat,diet_fish),
         WDDS_eggs=na0(diet_eggs),
         WDDS_legumes=na0(diet_legumes_nuts),
         WDDS_dairy=na0(diet_milk,diet_milk_products))

kids_diet$WDDS_total<-rowSums(select(kids_diet,contains("WDDS")))
# if all are NA, make the dietary diversity NA, not zero
all_na <- select(kids_diet,contains("diet_")) %>% is.na %>% rowSums == 13
kids_diet$WDDS_total[all_na] <- NA
#table(kids_diet$WDDS_total,exclude=NULL)
#    0    1    2    3    4    5    6    7    8 <NA> 
# 1446  318  613  885  679  401  182   44   15 3273 

kids_clean<-data.frame(kids_clean,select(kids_diet,contains("WDDS")))

##Variable summary:
## interview_date_cmc: units=cmc
## dob_cmc: date of birth, units=cmc
## age_calc_monts: age of child, units=months
## wealth_index: cumulative wealth, 1=poorest,2=poorer,3=middle,4=richer,5=richest
## sex: 0=male, 1=female
## height_age_percentile: continuous, min=0, max=100 
## height_age_zscore: continuous, min=-5.9 max=5.4
## weight_age_percentile: continuous, min=0, max=100 
## weight_age_zscore: continuous, min=-4.8 max=4.8
## weight_height_percentile: continuous, min=0, max=100 
## weight_height_zscore: continuous, min=-4.0 max=5.8
## mother_height_age_percentile: continuous, min=, max= 
## mother_height_age_zscore: continuous, min=, max=
## mother_ed_level: highest level of mother's education, 0=none, 1=primary, 
## 2=secondary, 3=higher
## mother_ed_year: highest year of education, 0-8, note: more NAs than level
## diet_xx: whether or not child was given ...
