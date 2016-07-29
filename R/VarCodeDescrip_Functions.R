## Function that creates smaller labels data frame with only the relevant info
make_cleanLabelMat<- function(frame_labels,data_subset_vars,data_rename_vars){
  frame_clean_labels_temp<-frame_labels[frame_labels$var %in% data_subset_vars,]
  data_subset<-data.frame(var=data_subset_vars,renamedVar=data_rename_vars)
  frame_clean_labels<-join(frame_clean_labels_temp,data_subset,by="var",type="inner")
  return(frame_clean_labels)}

## Function for easily pulling recode info for a given variable
pull_varCodes <- function(varName,mat_labels){
  varCodes<-mat_labels$varValues[mat_labels$renamedVar==varName]
  return(varCodes)
}

## Function for easily pulling descrption for a given variable
pull_varDescrip <- function(varName,mat_labels){
  varCodes<-mat_labels$varDescrip[mat_labels$renamedVar==varName]
  return(varCodes)
}