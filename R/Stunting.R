source('R/00_RW_loadDHS.R')
source('R/02_RW_cleanDHS_hh.R')
source('R/04_RW_cleanDHS_kids.R')

#Adding relevant variables from hh_clean
kids_analysis<-left_join(kids_clean,select(hh_clean,cluster_hh_num,province_id,district_id,water_source,toilet_type),by="cluster_hh_num")

#Dependent Variable is "height_age_zscore"

#Independent Variables are:
# child birth intervals: birth_interval_preceding
# mothers' education: mother_highest_education_level
# child's sex: sex
# child's age calculated: age_calc_months
# birth order: birth_order
# wealth_index: cumulative wealth, 1=poorest,2=poorer,3=middle,4=richer,5=richest
# dietary diversity: WDDS_total
# seasonality: interview_date_cmc
# mother's stunting: mother_height_age_zscore

fit <- lm(height_age_zscore~birth_interval_preceding+mother_ed_level+sex+age_calc_months+birth_order+wealth_index+WDDS_total+interview_date_cmc+mother_height_age_zscore+province_id+district_id+as.factor(water_source)+as.factor(toilet_type),data=kids_analysis) 
summary(fit)

fit <- lm(height_age_zscore~birth_interval_preceding+mother_ed_level+sex+age_calc_months+birth_order+wealth_index+WDDS_total+interview_date_cmc+mother_height_age_zscore,data=kids_analysis) 
summary(fit)

fit <- lm(height_age_zscore~WDDS_starch+WDDS_veg_green+WDDS_vitA+WDDS_veg_other+WDDS_organ+WDDS_meat_fish+WDDS_eggs+WDDS_legumes+WDDS_dairy+birth_interval_preceding+mother_ed_level+sex+age_calc_months+birth_order+wealth_index+interview_date_cmc+mother_height_age_zscore+as.factor(water_source)+as.factor(toilet_type),data=kids_analysis) 
summary(fit)

hist(kids_analysis$height_age_zscore[kids_analysis$WDDS_starch==0], col=rgb(0,0,1,1/4),xlim=c(-6,6),ylim=c(0,400))  # first histogram
hist(kids_analysis$height_age_zscore[kids_analysis$WDDS_starch==1], col=rgb(1,0,0,1/4), add=T)  # second

hist(kids_analysis$height_age_zscore[kids_analysis$diet_cereals==0], col=rgb(0,0,1,1/4),xlim=c(-6,6),ylim=c(0,500))  # first histogram
hist(kids_analysis$height_age_zscore[kids_analysis$diet_cereals==1], col=rgb(1,0,0,1/4),xlim=c(-6,6),ylim=c(0,500), add=T)  # second

fit <- lm(weight_age_percentile~WDDS_total_good+WDDS_starch+WDDS_legumes+birth_interval_preceding+mother_ed_level+sex+age_calc_months+birth_order+wealth_index+interview_date_cmc+mother_height_age_zscore,data=kids_analysis) 
summary(fit)

kids_analysis$WDDS_total_good<-kids_analysis$WDDS_total-kids_analysis$WDDS_starch-kids_analysis$WDDS_legumes

fit <- lm((WDDS_total_good)~WDDS_starch+WDDS_legumes,data=kids_analysis) 
summary(fit)


source('R/simple_plots.R')




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
## mother_ed_level: highest level of mother's education, 0=none, 1=primary, 
## 2=secondary, 3=higher
## mother_ed_year: highest year of education, 0-8, note: more NAs than level
## diet_xx: whether or not child was given ...



##EXPLORATION

## Histogram of the age distribution of the NA values for stunting, doesn't
## seem to exhibit a pattern
hist(kids_clean$age_calc_months[is.na(kids_clean$height_age_zscore)],bins=20)

# Check z-score versus age
library(ggthemes) 
# No noticable difference between boys and girls stunting, does peak around 20 months
kids_clean %>% mutate(stunted = ifelse(height_age_zscore <= -2, 1, 0)) %>%
  ggplot(aes(x = age_calc_months, y = stunted, colour = factor(sex))) +
  #geom_jitter(width = 0.25, height = 0.25) +
  stat_smooth(method = "loess", se = TRUE, span = 0.75, size = 1.15, alpha = 0.1) + 
  theme_fivethirtyeight() + ggtitle("stunting appears to peak near 20 months")

# Steady downward trend for stunting and wealth, not surprising
kids_clean %>% mutate(stunted = ifelse(height_age_zscore <= -2, 1, 0)) %>%
  ggplot(aes(x = wealth_index, y = stunted, colour = factor(sex))) +
  #geom_jitter(width = 0.25, height = 0.25) +
  stat_smooth(method = "loess", se = TRUE, span = 0.75, size = 1.15, alpha = 0.1) + 
  theme_fivethirtyeight() +
  ggtitle("stunting declines steadily with wealth (asset accumulation)")

# Finally, break it down by wealth and age category
# TODO: figure out if 1 is boy or girl!
kids_clean %>% mutate(stunted = ifelse(height_age_zscore <= -2, 1, 0), 
                      agegroup = cut(age_calc_months, seq(0, 60, by = 6))) %>%
  group_by(agegroup, wealth_index, sex) %>% 
  summarise(stunting = mean(stunted, na.rm = TRUE)) %>% 
  filter(stunting != 0) %>% 
  ggplot(aes(x = agegroup, y = wealth_index, fill = stunting)) +
  geom_tile(colour = 'white',size = 0.25, stat = "identity") +
  scale_fill_viridis(option="D") +
  geom_text(aes(y = wealth_index, x = agegroup, label = sprintf("%1.0f%%", round(100*stunting, 2)), size = 1)) +
  theme_fivethirtyeight() + facet_wrap(~sex, nrow = 2)

# Notes about Variables:
# bidx: is the line number on which the information was recorded, starting with the youngest
#       child so bidx=1 is youngest child recorded, bidx=2 is the 2nd youngest child recorded
#       etc. Note: midx and bidx are the same thing
# bord (renamed birth_order): is the birth order counting from the first born, so if there 
#       are 6 children total, bord=6 refers to the youngest, bord=5 refers to the second 
#       youngest etc.   
#       Note: while bidx and bord are reverses of eachother in principle, in practice
#       children above 5 years of age are not recorded, so you may have only 1 entry from a
#       given household with bidx=1 and bord=6, which means there is only one child below 5 
#       and they were the 6th born
# Note: haven't dealt with twins yet

# According to DHS report pg 170 12,699 household completed the survey, which matches
# nrow(hh_all)
# Can't quite figure out why the kids matrix has the number of rows it has