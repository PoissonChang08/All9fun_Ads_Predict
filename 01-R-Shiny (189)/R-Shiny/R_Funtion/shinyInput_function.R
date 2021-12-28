

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id , label , ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i) , label = paste0("★") , icon("user-friends") , ...))}
  inputs
}

# label = "細節"
# A <- iris3 %>% as.data.frame()
# A$a <- shinyInput(actionButton , nrow(A) , 'button_' , label = 1:nrow(A) )
# 
#   cbind('*' = shinyInput(actionButton, nrow(fb_ad_prediction_ROI_60_break_even), 'button_' , label = "" , onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
#         , fb_ad_prediction_ROI_60_break_even)

