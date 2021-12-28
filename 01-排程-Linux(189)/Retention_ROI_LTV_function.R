


# RetentionRate & ROI & LTV  function ==================================================

#  
RetentionRate <- function(day,endday,DAU,install){
  if(Sys.Date() - as.Date(endday) > day){
    ifelse(install==0,"NO_Install", as.character((DAU / install))) %>% {suppressWarnings(as.numeric(.))} 
  }
  else {"-"} %>% {suppressWarnings(as.numeric(.))} 
}

ROI <- function(day,endday,Purchase,Spend){
  if(Sys.Date() - as.Date(endday) > day){
    ifelse(Spend==0,"NO_Spend", as.character((Purchase / Spend) - 1 )) %>% {suppressWarnings(as.numeric(.))} 
  }
  else {"-"} %>% {suppressWarnings(as.numeric(.))} 
}

LTV <- function(day,endday,Purchase,install){
  if(Sys.Date() - as.Date(endday) > day){
    ifelse(install==0,"NO_Install",as.character((Purchase / install ))) %>% {suppressWarnings(as.numeric(.))} 
  }
  else {"-"} %>% {suppressWarnings(as.numeric(.))} 
}

# long term
RetentionRate_long <- function(DAU,install){
    ifelse(install==0,"NO_Install", as.character((DAU / install))) %>% {suppressWarnings(as.numeric(.))} 
}

ROI_long <- function(Purchase,Spend){
    ifelse(Spend==0,"NO_Spend", as.character((Purchase / Spend) - 1 )) %>% {suppressWarnings(as.numeric(.))} 
}

LTV_long <- function(Purchase,install){
    ifelse(install==0,"NO_Install",as.character((Purchase / install ))) %>% {suppressWarnings(as.numeric(.))} 
}


