uniqueVars = labels %>%
  filter(module != 'birth', module != 'couples') %>%
  group_by(var, varDescrip) %>% 
  summarise(num = n(), mod = first(module)) %>% 
  filter(num == 1)


uniqueVars %>% 
  group_by(mod) %>% 
  summarise(n())


# Remove variables that aren't included in the dataset.


# hh ----------------------------------------------------------------------

# -- hh level indicators --
hh_tocheck = hh_labels %>% 
  filter(!varDescrip %like% 'na - ') %>% 
  select(var, varDescrip, module)

uniqueVars = as.character(hh_tocheck$var)

write.csv(hh_tocheck, 'Excel/hh_labels_tokeep_original.csv')


# women’s module ----------------------------------------------------------

# -- woman indicators --
woman_tocheck = women_labels %>% 
  filter(!varDescrip %like% 'na - ',
         !var %in% uniqueVars) %>% # Make sure we haven't already examined 
  select(var, varDescrip, module)

# check there aren't dupes
intersect(hh_tocheck$var, woman_tocheck$var)

# update growing list of variables
uniqueVars = c(as.character(uniqueVars), as.character(woman_tocheck$var))

write.csv(woman_tocheck, 'Excel/women_labels_tokeep_original.csv')



# children's --------------------------------------------------------------

# -- children's indicators --
kids_tocheck = kids_labels %>% 
  filter(!varDescrip %like% 'na - ',
         !var %in% uniqueVars) %>% # Make sure we haven't already examined 
  select(var, varDescrip, module)

# check there aren't dupes
intersect(kids_tocheck$var, woman_tocheck$var)
intersect(kids_tocheck$var,hh_tocheck$var)

# update growing list of variables
uniqueVars = c(uniqueVars, kids_tocheck$var)

write.csv(kids_tocheck, 'Excel/kids_labels_tokeep_original.csv')


# men’s module ------------------------------------------------------------

# -- men's indicators --
mens_tocheck = men_labels %>% 
  filter(!varDescrip %like% 'na - ',
         !var %in% uniqueVars) %>% # Make sure we haven't already examined 
  select(var, varDescrip, module)

# update growing list of variables
uniqueVars = c(uniqueVars, mens_tocheck$var)

write.csv(mens_tocheck, 'Excel/mens_labels_tokeep_original.csv')

# -- roster indicators --
roster_tocheck = roster_labels %>% 
  filter(!varDescrip %like% 'na - ',
         !var %in% uniqueVars) %>% # Make sure we haven't already examined 
  select(var, varDescrip, module)

write.csv(roster_tocheck, 'Excel/roster_labels_tokeep_original.csv')

