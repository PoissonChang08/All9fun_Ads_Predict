

#  MAPE MSE APE function  ============================= 

# APE function
APE <- function(predict_value,actual_value){
  abs(abs(predict_value - actual_value) / (actual_value))*100
}

# MAPE function
MAPE <- function(predict_value,actual_value){
  DT <- data.frame(predict_value,actual_value) %>% dplyr::filter(actual_value!=0) 
  predict_value <- DT$predict_value
  actual_value <- DT$actual_value
  mean(abs((predict_value - actual_value) / (actual_value)), na.rm = T)*100
  # mean(abs(abs(predict_value - actual_value) / (actual_value))*100, na.rm = T)
}

# MSE function
MSE <- function(predict_value,actual_value){
  mean((predict_value - actual_value)^2, na.rm = T)
} 
