# Cleanup household dataset for Rwanda DHS data --------------------------
# Laura Hughes & Nada Petrovic, USAID GeoCenter & PPL, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License

# basic info --------------------------------------------------------------

# TODO: Add some data quality checks for hh_vars_tokeep.xlsx
#   - Are all var and varDescrip entries valid and consistent?
#   - Are all renamedVar entries unique?
#   - Are all Keep entries either 1 or NA?

## Reads in excel spreadsheet that states which variables to keep
## from kids data set
hh_labels_tokeep<-read.csv('Excel/hh_vars_tokeep.csv')

## Relabels "NA" values (ie variables that have not been decided on yet) as 0
## so that they are not selected. From the Excel spreadsheet pulls the list 
## of variables to keep and what they should be renamed.
hh_labels_tokeep$Keep[is.na(hh_labels_tokeep$Keep)] <- 0
data_subset_vars <- as.character(hh_labels_tokeep$var[hh_labels_tokeep$Keep==1]) 
data_rename_vars <- as.character(hh_labels_tokeep$renamedVar[hh_labels_tokeep$Keep==1]) 

## Creates new clean data frame that is a subset of the overall data frame, 
## and renames the variables.
hh_clean <- hh_all[data_subset_vars]
names(hh_clean) <- data_rename_vars

## Creates functions that will be useful for pulling attributes/variable codes later
source('R/VarCodeDescrip_Functions.R')

## Creates smaller data frame of labels to make it easier to query relevant info
hh_clean_labels<-make_cleanLabelMat(hh_labels,data_subset_vars,data_rename_vars)

## Creates household id variable for merging with hh data set, note
## this is not the same as hhid due to weirdness in spacing of DHS
## paste job.  It can only be merged with a homemade household id ## like this one.
hh_clean$cluster_hh_num <- paste(hh_clean$cluster_num, hh_clean$hh_num)


  hh_clean <- mutate(hh_clean, 
    # -- Convert sampling rates to appropriate value --
    # -- Recode --
    urban = ifelse(urban == 1, TRUE, # recode rural category to be binary (1 if rural, 0 if urban)
                        ifelse(urban == 2, FALSE,
                               NA)),
    rural = !urban #do we want this variable to be created in df? 
    )

# WASH indicators ---------------------------------------------------------

binary10 <- function(x) {
  ifelse(x==1,TRUE,ifelse(x==0,FALSE,NA))
}

# report possible values for wash variables  
summary(hh_clean  %>% select(matches("water|_wash")))  
    
  hh_clean <- mutate(hh_clean,
    # -- Recode "don't know" answers as NA --
    water_treat = binary10(water_treat),
    water_treat_boil = binary10(water_treat_boil),
    water_treat_bleach = binary10(water_treat_bleach),
    water_treat_cloth = binary10(water_treat_cloth),
    water_treat_filter = binary10(water_treat_filter),
    water_treat_solar = binary10(water_treat_solar),
    water_treat_settle = binary10(water_treat_settle),
    container_wash = ifelse(container_wash < 8, container_wash,NA)
  )
# TODO: We're not using labels anymore. Get rid of these.
attr(hh_clean$water_treat,'label')        <- attr(hh$hv237,'label')
attr(hh_clean$water_treat_boil,'label')   <- attr(hh$hv237a,'label')
attr(hh_clean$water_treat_bleach,'label') <- attr(hh$hv237b,'label')
attr(hh_clean$water_treat_cloth,'label')  <- attr(hh$hv237c,'label')
attr(hh_clean$water_treat_filter,'label') <- attr(hh$hv237d,'label')
attr(hh_clean$water_treat_solar,'label')  <- attr(hh$hv237e,'label')
attr(hh_clean$water_treat_settle,'label') <- attr(hh$hv237f,'label')

# Exploratory descriptive stats -------------------------------------------

# -- distribution of sampling dates --
# hh_clean %>% 
#   group_by(interview_month) %>% 
#   summarise(n())

# Plot below suggests we need to control for interview month in the analysis
ggplot(hh_clean, aes(x = interview_month)) + 
  stat_bin(binwidth=1, fill = 'dodgerblue', colour = 'white') +   
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.2) + 
  scale_x_continuous(limits = c(0, 13),
                     breaks = seq(0,12, by = 2)) +
  theme_bw() + 
  ylab('') +
  ggtitle('Number of households interviewed by month')+
  annotate("text", x = 11, y = 500, label = "main lean season\n (Oct - Dec)", colour = "gray20")

# For reference below is a link to the Rwanda seasonal calendar
# Surprising about 40% of households surveyed were interviewed during lean season
#fewsCal <- c("http://www.fews.net/sites/default/files/styles/large/public/seasonal-calendar-rwanda.png")
#browseURL(fewsCal)


