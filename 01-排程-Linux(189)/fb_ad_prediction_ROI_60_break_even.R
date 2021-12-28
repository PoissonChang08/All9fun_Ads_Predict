# === Start ===  
# 主程式 fb_ad_prediction_ROI_60_break_even
# 維護者: Poisson Chang
# 更新日期: 2019-07-31
# === End ===


# Create Model date-Dataframe
# 跑當天的資料(data_date) & 回補當天(data_date)前46天ROI_60資訊
# data <- as.data.frame(seq(as.Date("2019-06-01") , length = 95, by = "days") , stringsAsFactors = F)
data <- as.data.frame(seq(Sys.Date()-46 , length = 2, by = 46) , stringsAsFactors = F)
names(data) <- c("Date")

library(magrittr) # %>%
library(dplyr)
data %<>% dplyr::arrange(desc(Date))


for(i in 1:nrow(data)){
  execute_Date <- data[i,]
  print(execute_Date)


# Login & Connect  ==================================================
# Cube_DB
source("/home/poisson/login_DB.r")
source("/home/poisson/fn_Calculation/Retention_ROI_LTV_function.R") # loading function 
source("/home/poisson/fn_Calculation/MAPE_MSE_APE_function.R") # loading function 
  

# Insert Table：
# fb_ad_prediction_ROI_60_break_even

# execute_Date <- Sys.Date()
execute_time <- Sys.time()


# 載入套件 ==================================================
library(devtools)
library(dashboardthemes)
library(shinydashboard)
library(shinycssloaders) # display loading 
library(stringr)  # str_sub
library(shiny)
library(rsconnect)
library(DBI)
library(gWidgets)
library(RMySQL)
library(dbConnect)
library(tidyr)
library(dplyr)
library(magrittr) # %>%
library(DT)       # DT::renderDataTable, formatStyle
library(stringr)  # str_sub
library(xlsx)
library(plotly)   # plotly 




# 存log檔 ==================================================
library(futile.logger)   # log file
library(futile.options)  # log file
# 專案路徑
logpath <- "/home/poisson/log紀錄檔/mg_fb_tag_analysis/fb_ad_prediction_ROI_60_break_even.log"
# logpath <- "C:/Users/pc072/Desktop/fb_Tag_analysis/log紀錄檔/mobile_game_fb_tag_analysis.log"

# log檔路徑
if (file.exists(logpath) == FALSE){
  file.create(logpath)}
# 啟動log
flog.logger(appender = appender.file(logpath), name = "fb_ad_prediction_ROI_60_break_even_log")




# 設定日期 ==================================================
args <- commandArgs(trailingOnly = TRUE)

if(NROW(args) > 0) {
  begin_date <- args[1]
  end_date   <- args[2]
  
} else {
  one_month_begin_date <- execute_Date - 89
  begin_date <- execute_Date - 59
  end_date   <- execute_Date - 14
}

# # date
# begin_date <- "2016-12-01"
# end_date   <- "2019-06-28"


# 固定遊戲
Game = '將膽' # '一劍傾城','將膽','蜀門'
ad_begin_date = '2018-04-01'
ad_end_date =  Sys.Date()



# 函數設定 ==================================================
# R NA to SQL NULL函數(SQL value)
NAtoNULL <- function(R_data){
  outputs <- lapply(1:length(R_data), function(col_value){ 
    if (is.na(R_data[col_value])){
      trans_value <- "NULL"
    } else {
      trans_value <- paste0("'", R_data[col_value],"'")
    }
  }) %>%
    call("paste0", ., collapse = ", ") %>% 
    eval %>%
    paste0("(", .,")")
  return(outputs)
}



tryCatch(
  {  
      
      # fb 資料載入：  ==================================================
      FB_daily_SQL <- sprintf(" SELECT * , CONCAT(fileHost, filePath, fileSavedName) AS file_path
                              FROM analysis.mobile_game_fb_daily_tag
                              WHERE 1=1
                              AND  `date` BETWEEN '%s' AND '%s';"
                              , one_month_begin_date
                              , end_date)
      FB_daily <- dbGetQuery(connect_DB,FB_daily_SQL)
      # 為了shiny subtable 建立原始 & TA_Age
      FB_daily$TA_Age_org <- FB_daily$TA_Age
      # TA_Age Rename
      # right function
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))}
      FB_daily$TA_Age <- ifelse( substrRight(FB_daily$TA_Age,3)=="63_" , paste0(substr(FB_daily$TA_Age,1,2),"歲以上")
                                 , ifelse(substrRight(FB_daily$TA_Age,2) > substr(FB_daily$TA_Age,1,2)
                                        , paste(substr(FB_daily$TA_Age,1,2),substrRight(FB_daily$TA_Age,2),sep = "-")
                                        , paste(substrRight(FB_daily$TA_Age,2),substr(FB_daily$TA_Age,1,2),sep = "-"))
                                 )

      # TA_name 重組(由於age的字串縮減用R處理，因此新建TA_name寫在此。)
      FB_daily$Ta_Name <- paste0("「",FB_daily$Campaign_Charge,"_",FB_daily$Campaign_Channel,'_',FB_daily$TA_Location,"_",FB_daily$TA_Gender,"_",FB_daily$TA_Age,"」：",FB_daily$TA_name)
      # 篩選資料
      # 遊戲
      FB_daily %<>% dplyr::filter(Campaign_MobileGame %in% (Game))




      # AF 資料載入：  ==================================================
      AF_daily_SQL <- sprintf("SELECT * , click_date AS date , CONCAT(fileHost, filePath, fileSavedName) AS file_path
                              FROM analysis.fb_ta_cohort_tag
                              WHERE 1=1
                              AND  `click_date` BETWEEN '%s' AND '%s';"
                              , one_month_begin_date
                              , end_date)
      AF_daily <- dbGetQuery(connect_DB,AF_daily_SQL)
      # TA_Age Rename
      AF_daily$TA_Age <- ifelse( substrRight(AF_daily$TA_Age,3)=="63_" , paste0(substr(AF_daily$TA_Age,1,2),"歲以上")
                                 , ifelse(substrRight(AF_daily$TA_Age,2) > substr(AF_daily$TA_Age,1,2)
                                          , paste(substr(AF_daily$TA_Age,1,2),substrRight(AF_daily$TA_Age,2),sep = "-")
                                          , paste(substrRight(AF_daily$TA_Age,2),substr(AF_daily$TA_Age,1,2),sep = "-"))
                                 )
      # TA_name 重組
      AF_daily$Ta_Name <- paste0("「",AF_daily$Campaign_Charge,"_",AF_daily$Campaign_Channel,'_',AF_daily$TA_Location,"_",AF_daily$TA_Gender,"_",AF_daily$TA_Age,"」：",AF_daily$TA_name)
      # 篩選資料
      # 遊戲
      AF_daily %<>% dplyr::filter(Campaign_MobileGame %in% (Game))





      # fileName + Ta_Name  ==================================================

      # FB (廣告開關)
      FB_status <-  FB_daily %>% dplyr::distinct(status,fileName,file_path,Ta_Name,TA_name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age)
      # 新增開關條件 (ACTIVE = 0 、PAUSED = 1 、Other = 2)
      FB_status$status_01 <- ifelse(FB_status$status=="ACTIVE",0,ifelse(FB_status$status=="PAUSED",1,2))
      # 將數據轉為 data.table
      library(data.table)
      FB_status <- FB_status %>% as.data.table()
      FB_status %>% class()
      # TA+素材 依據status_01排序，並命名為「status_01」。
      FB_status[,valRank:=rank(status_01),by = paste0(fileName,file_path,Ta_Name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age)]
      FB_status <- FB_status %>% dplyr::arrange(paste0(fileName,file_path,Ta_Name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age),valRank)
      # 取valRank = 1 (若有相同的TA+素材且有不同的開關，則一律取「ACTIVE」)
      FB_status <- FB_status %>% dplyr::filter(valRank == 1)
      # # 同一個TA+素材有不同的開關結果名冊
      # FB_status_valRank <-  FB_status %>% dplyr::filter(valRank == 2)
      # FB_status_valRank2 <- FB_status %>% dplyr::filter(fileName %in% FB_status_valRank$fileName & file_path %in% FB_status_valRank$file_path &
      #                                                   Ta_Name %in% FB_status_valRank$Ta_Name & TA_Location %in% FB_status_valRank$TA_Location &
      #                                                   TA_Gender %in% FB_status_valRank$TA_Gender & TA_Age %in% FB_status_valRank$TA_Age)


      # FB (加TA_name是為了join後面的表格)
      Data_test_FB_Ta_Name <-  FB_daily %>% dplyr::group_by(fileName,file_path,Ta_Name,TA_name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age,TA_Age_org) %>%
                                            dplyr::summarise(
                                              # "COUNT" = n()
                                               "Sample_Size" = n_distinct(paste(ad_id,date,sep = ""))  # Now summarise with unique elements per group
                                              ,"DateDay" = n_distinct(date)
                                              ,"FB_impressions" = sum(impressions, na.rm=TRUE)
                                              ,"FB_reach" = sum(reach, na.rm=TRUE)
                                              ,"FB_clicks" = sum(clicks, na.rm=TRUE)
                                              ,"FB_install" = sum(app_install, na.rm=TRUE)
                                              ,"FB_spend" = sum(spend, na.rm=TRUE)
                                              # ,"purchase" = sum(purchase, na.rm=TRUE)
                                              # ,"leadgen" = sum(leadgen, na.rm=TRUE)
                                              # ,"likes" = sum(likes, na.rm=TRUE)
                                              ,"CPC" = sum(spend, na.rm=TRUE) / sum(clicks, na.rm=TRUE)
                                              ,"CTR" = sum(clicks, na.rm=TRUE) / sum(impressions, na.rm=TRUE)
                                              ,"CPI" = sum(spend, na.rm=TRUE) / sum(app_install, na.rm=TRUE)
                                              ,"CR"  = sum(app_install, na.rm=TRUE) / sum(clicks, na.rm=TRUE)
                                              # ,"CTR_CR" = sum(app_install, na.rm=TRUE) / sum(impressions, na.rm=TRUE)
                                              # ,"AVG_impressions" = round(sum(impressions, na.rm=TRUE) / n_distinct(paste(ad_id,date,sep = "")),2)
                                              # ,"AVG_clicks"  = round(sum(clicks, na.rm=TRUE) / n_distinct(paste(ad_id,date,sep = "")),2)
                                              # ,"AVG_app_install" = round(sum(app_install, na.rm=TRUE) / n_distinct(paste(ad_id,date,sep = "")),2)

                                            ) %>% data.frame(stringsAsFactors = F)
      # FB成效取得「status」
      Data_test_FB_Ta_Name <- dplyr::left_join(Data_test_FB_Ta_Name,FB_status, by = c("fileName" = 'fileName' ,"file_path" = "file_path" ,"Ta_Name" = "Ta_Name","TA_name" = "TA_name", "Campaign_Charge" = 'Campaign_Charge' ,"Campaign_Channel" = "Campaign_Channel" ,"TA_Location" = "TA_Location","TA_Gender" = "TA_Gender","TA_Age" = "TA_Age" ))
      Data_test_FB_Ta_Name %<>% dplyr::select(-c("status_01","valRank"))

      # 由於 綜合指標 由 FB_install、CTR、CR、CPI、ROI、次留組合而成，而以下數據NA顯示為infinite，故需轉換。
      Data_test_FB_Ta_Name$CPI[which(is.infinite(Data_test_FB_Ta_Name$CPI))] <- NaN
      Data_test_FB_Ta_Name$CTR[which(is.infinite(Data_test_FB_Ta_Name$CTR))] <- NaN
      Data_test_FB_Ta_Name$CR[which(is.infinite(Data_test_FB_Ta_Name$CR))] <- NaN


      # AF
      Data_test_AF_Ta_Name <-  AF_daily %>% dplyr::group_by(fileName,file_path,Ta_Name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age) %>%
                                            dplyr::summarise(
                                              # "COUNT" = n()
                                              # ,"DateDay" = n_distinct(date)
                                              "AF_install" = sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE)  # Now summarise with unique elements per group
                                              # ,"dau_1" = sum(ifelse(diff_day == 1 ,dau ,0), na.rm=TRUE)
                                              # ,"dau_2" = sum(ifelse(diff_day == 2 ,dau ,0), na.rm=TRUE)
                                              # ,"dau_7" = sum(ifelse(diff_day == 7 ,dau ,0), na.rm=TRUE)
                                              # ,"dau_14" = sum(ifelse(diff_day == 14 ,dau ,0), na.rm=TRUE)
                                              # ,"dau_30" = sum(ifelse(diff_day == 30 ,dau ,0), na.rm=TRUE)
                                              # ,"AF_spend" = sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE)
                                              # ,"revenue_7"   = sum(ifelse(diff_day >=1  & diff_day <= 7  ,revenue ,0), na.rm=TRUE)
                                              # ,"revenue_14"  = sum(ifelse(diff_day >=1  & diff_day <= 14 ,revenue ,0), na.rm=TRUE)
                                              # ,"revenue_30"  = sum(ifelse(diff_day >=1  & diff_day <= 30 ,revenue ,0), na.rm=TRUE)
                                              # ,"revenue_60"  = sum(ifelse(diff_day >=1  & diff_day <= 60 ,revenue ,0), na.rm=TRUE)
                                              # ,"revenue_90"  = sum(ifelse(diff_day >=1  & diff_day <= 90 ,revenue ,0), na.rm=TRUE)

                                              ,"RetentionRate_2" = RetentionRate(2,end_date,sum(ifelse(diff_day == 2 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_3" = RetentionRate(3,end_date,sum(ifelse(diff_day == 3 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_4" = RetentionRate(4,end_date,sum(ifelse(diff_day == 4 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_5" = RetentionRate(5,end_date,sum(ifelse(diff_day == 5 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_6" = RetentionRate(6,end_date,sum(ifelse(diff_day == 6 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_7" = RetentionRate(7,end_date,sum(ifelse(diff_day == 7 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_14" = RetentionRate(14,end_date,sum(ifelse(diff_day == 14 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"RetentionRate_20" = RetentionRate(20,end_date,sum(ifelse(diff_day == 20 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"RetentionRate_30" = RetentionRate(30,end_date,sum(ifelse(diff_day == 30 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"RetentionRate_40" = RetentionRate(40,end_date,sum(ifelse(diff_day == 40 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"RetentionRate_50" = RetentionRate(50,end_date,sum(ifelse(diff_day == 50 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"RetentionRate_60" = RetentionRate(60,end_date,sum(ifelse(diff_day == 60 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"RetentionRate_90" = RetentionRate(90,end_date,sum(ifelse(diff_day == 90 ,dau ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))

                                              ,"ROI_2"  = ROI(2, end_date ,sum(ifelse(diff_day >=1  & diff_day <= 2 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_3"  = ROI(3, end_date ,sum(ifelse(diff_day >=1  & diff_day <= 3 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_4"  = ROI(4, end_date ,sum(ifelse(diff_day >=1  & diff_day <= 4 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_5"  = ROI(5, end_date ,sum(ifelse(diff_day >=1  & diff_day <= 5 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_6"  = ROI(6, end_date ,sum(ifelse(diff_day >=1  & diff_day <= 6 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_7"  = ROI(7, end_date ,sum(ifelse(diff_day >=1  & diff_day <= 7 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_14" = ROI(14,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 14 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              # ,"ROI_20" = ROI(20,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 20 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              # ,"ROI_30" = ROI(30,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 30 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              # ,"ROI_40" = ROI(40,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 40 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              # ,"ROI_50" = ROI(50,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 50 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              ,"ROI_60" = ROI(60,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 60 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              # ,"ROI_90" = ROI(90,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 90 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))
                                              # # ROI
                                              # ,"ROI" = ROI_long(sum(ifelse(diff_day >=1 ,revenue ,0) , na.rm=TRUE),sum(ifelse(diff_day == 0 ,spend ,0), na.rm=TRUE))

                                              ,"LTV_1"  = LTV(1,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 1 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_2"  = LTV(2,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 2 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_3"  = LTV(3,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 3 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_4"  = LTV(4,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 4 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_5"  = LTV(5,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 5 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_6"  = LTV(6,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 6 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_7"  = LTV(7,end_date  ,sum(ifelse(diff_day >=1  & diff_day <= 7 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_14" = LTV(14,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 14 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"LTV_30" = LTV(30,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 30 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              ,"LTV_60" = LTV(60,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 60 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # ,"LTV_90" = LTV(90,end_date ,sum(ifelse(diff_day >=1  & diff_day <= 90 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))
                                              # # LTV
                                              # ,"LTV" = LTV_long(sum(ifelse(diff_day >=1 ,revenue ,0), na.rm=TRUE),sum(ifelse(diff_day == 0 ,dau ,0), na.rm=TRUE))

                                            ) %>% data.frame(stringsAsFactors = F)



      ##  merge data：==================================================

      # "fileName","file_path"  「加入Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age去join欄位才不會出現.x.y」
      Data_test_Ta_Name <- merge(x = Data_test_FB_Ta_Name, y = Data_test_AF_Ta_Name, by = c("fileName","file_path","Ta_Name"
                                                                                           ,"Campaign_Charge","Campaign_Channel","TA_Location","TA_Gender","TA_Age"), all = TRUE)



      ##  增加廣告投放時間：==================================================

      FB_ad_date_SQL <- sprintf("-- fb
                              SELECT fileName , CONCAT(fileHost, filePath, fileSavedName) AS file_path  ,TA_name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age
                                  , min(`date`) AS first_ad_date
                                  , max(`date`) AS last_ad_date
                              FROM analysis.mobile_game_fb_daily_tag
                              WHERE 1=1
                              AND  `date` BETWEEN '%s' AND '%s'
                              AND  Campaign_MobileGame = '%s'
                              GROUP BY fileName , CONCAT(fileHost, filePath, fileSavedName) ,TA_name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age;"
                              , ad_begin_date
                              , ad_end_date
                              , Game)
      FB_ad_date <- dbGetQuery(connect_DB,FB_ad_date_SQL)
      # TA_Age Rename
      FB_ad_date$TA_Age <- ifelse( substrRight(FB_ad_date$TA_Age,3)=="63_" , paste0(substr(FB_ad_date$TA_Age,1,2),"歲以上")
                                 , ifelse(substrRight(FB_ad_date$TA_Age,2) > substr(FB_ad_date$TA_Age,1,2)
                                          , paste(substr(FB_ad_date$TA_Age,1,2),substrRight(FB_ad_date$TA_Age,2),sep = "-")
                                          , paste(substrRight(FB_ad_date$TA_Age,2),substr(FB_ad_date$TA_Age,1,2),sep = "-"))
                                 )

      AF_ad_date_SQL <- sprintf("-- af
                              SELECT fileName , CONCAT(fileHost, filePath, fileSavedName) AS file_path ,TA_name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age
                              	   , min(click_date) AS first_ad_click_date
                                   , max(click_date) AS last_ad_click_date
                              FROM analysis.fb_ta_cohort_tag
                              WHERE `click_date` BETWEEN '%s' AND '%s'
                              AND  Campaign_MobileGame = '%s'
                              GROUP BY fileName , CONCAT(fileHost, filePath, fileSavedName) ,TA_name,Campaign_Charge,Campaign_Channel,TA_Location,TA_Gender,TA_Age;"
                              , ad_begin_date
                              , ad_end_date
                              , Game)
      AF_ad_date <- dbGetQuery(connect_DB,AF_ad_date_SQL)

      # TA_Age Rename
      AF_ad_date$TA_Age <- ifelse( substrRight(AF_ad_date$TA_Age,3)=="63_" , paste0(substr(AF_ad_date$TA_Age,1,2),"歲以上")
                                   , ifelse(substrRight(AF_ad_date$TA_Age,2) > substr(AF_ad_date$TA_Age,1,2)
                                            , paste(substr(AF_ad_date$TA_Age,1,2),substrRight(AF_ad_date$TA_Age,2),sep = "-")
                                            , paste(substrRight(AF_ad_date$TA_Age,2),substr(AF_ad_date$TA_Age,1,2),sep = "-"))
                                   )

      # 增加廣告最初、最終投放時間-欄位
      Data_test_Ta_Name <- dplyr::left_join(Data_test_Ta_Name , FB_ad_date , by = c("fileName" = 'fileName' ,"file_path" = "file_path" ,"TA_name" = "TA_name", "Campaign_Charge" = 'Campaign_Charge' ,"Campaign_Channel" = "Campaign_Channel" ,"TA_Location" = "TA_Location","TA_Gender" = "TA_Gender","TA_Age" = "TA_Age" ) ) %>%
                           dplyr::left_join(. , AF_ad_date , by = c("fileName" = 'fileName' ,"file_path" = "file_path" ,"TA_name" = "TA_name" , "Campaign_Charge" = 'Campaign_Charge' ,"Campaign_Channel" = "Campaign_Channel" ,"TA_Location" = "TA_Location","TA_Gender" = "TA_Gender","TA_Age" = "TA_Age" ) )


      # 增加寫入資料時間
      Data_test_Ta_Name$data_date <- as.Date(execute_Date)     # data_date (今天寫入的時間)
      Data_test_Ta_Name$one_month_data_begin_date <- as.Date(one_month_begin_date) # one_month_data_begin_date
      Data_test_Ta_Name$data_begin_date <- as.Date(begin_date) # data_begin_date
      Data_test_Ta_Name$data_end_date <- as.Date(end_date)     # data_end_date

      # 排除資料迄日大於59天的廣告
      Day59 <- as.Date(execute_Date - 59)
      Data_test_Ta_Name %<>% dplyr::filter(last_ad_date >= Day59)


      ##  file_path ：==================================================

      # 為了shiny subtable 建立原始 & file_path
      Data_test_Ta_Name$file_path_org <- Data_test_Ta_Name$file_path

      Data_test_Ta_Name$file_path <- ifelse(str_sub(Data_test_Ta_Name$file_path,-3,-1) == "jpg",paste0('<img src="', Data_test_Ta_Name$file_path, '" height="64"></img>'),
                                            ifelse(str_sub(Data_test_Ta_Name$file_path,-4,-1) == "jpeg",paste0('<img src="', Data_test_Ta_Name$file_path, '" height="64"></img>'),
                                                   paste0('<video src="', Data_test_Ta_Name$file_path, '" type="video/mp4" controls="controls" height="90"></video>')))



      ##  增加特徵值(Feature) ：==================================================

      # DF_LTV
      Data_test_Ta_Name %<>% dplyr::mutate(
                                            df2 = LTV_2 - LTV_1
                                          , df3 = LTV_3 - LTV_2
                                          , df4 = LTV_4 - LTV_3
                                          , df5 = LTV_5 - LTV_4
                                          , df6 = LTV_6 - LTV_5
                                          , df7 = LTV_7 - LTV_6
                                          , df7_1  = LTV_7 - LTV_1
                                          , df14_7  = LTV_14 - LTV_7
                                          # , df30_14 = LTV_30 - LTV_14
                                          # , df60_30 = LTV_60 - LTV_30
                                          # LTV成長比例
                                          , Ratio_7_1  = ifelse(LTV_1==0,0,LTV_7/LTV_1)
                                          , Ratio_14_7  = ifelse(LTV_7==0,0,LTV_14/LTV_7)
                                          # , Ratio_30_14 = ifelse(LTV_14==0,0,LTV_30/LTV_14)
                                          # , Ratio_60_30 = ifelse(LTV_30==0,0,LTV_60/LTV_30)
                                        )


      ##  選取有效樣本 ：==================================================
      # 排除 安裝數 = 0
      # 排除 曝光數小於1,000
      # 排除 次日留存率 = NA
      # 排除 次日ROI = NA
      # 排除 投放天數 = 1日
      Data_test_Ta_Name %<>% dplyr::filter(FB_install !=0
                                           , AF_install !=0
                                           , FB_impressions > 1000
                                           # , DateDay != 1
                                           , !is.na(RetentionRate_2)
                                           , !is.na(ROI_2)
                                           , !is.na(Campaign_Charge)
                                           , !is.na(Campaign_Channel)
                                           , !is.na(TA_Gender)
                                           ,  ROI_14 < 0 # 選擇14日未回本的廣告
                                           # , ROI_2 != -1
                                           # , (DateDay != 1)
                                           )
      # 排除 ROI_2 不等於-100% & ROI_14(Trainging是ROI_60) 不等於-100%
      Data_test_Ta_Name %<>% dplyr::filter(ROI_2 !=-1 & ROI_14 != -1)

      # 讀取模型資料
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/LR_20190430.rds")
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/NN_20190430.rds")
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/tree_20190430.rds")
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/RF_20190430.rds")
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/SVM_20190430.rds")
      # Trainging Test
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/TrainingData_20190430.rds")
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/TestData_20190430.rds")
      # Trainging Test Result
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/Trainging_Result_20190430.rds")
      load("/home/poisson/Program_fb_tag_analysis/fb_pred_ROI_60_day_break_even/Model_Engaged/Test_Result_20190430.rds")

      Data_test_Ta_Name %>% names()

      # pred
      # 要先把Data_test_Ta_Name的類別變數轉成因子，才能再轉乘TrainingData的level。
      Data_test_Ta_Name$Campaign_Charge <- Data_test_Ta_Name$Campaign_Charge  %>% as.factor()
      Data_test_Ta_Name$Campaign_Channel <- Data_test_Ta_Name$Campaign_Channel  %>% as.factor()
      Data_test_Ta_Name$TA_Location <- Data_test_Ta_Name$TA_Location  %>% as.factor()
      Data_test_Ta_Name$TA_Gender <- Data_test_Ta_Name$TA_Gender  %>% as.factor()
      Data_test_Ta_Name$TA_Age <- Data_test_Ta_Name$TA_Age  %>% as.factor()
      
      
      
      # levels(Data_test_Ta_Name$Campaign_Charge) <- rf3$forest$xlevels$Campaign_Charge
      # levels(Data_test_Ta_Name$Campaign_Channel) <- levels(TrainingData$Campaign_Channel)
      # levels(Data_test_Ta_Name$TA_Gender) <- levels(TrainingData$TA_Gender)
      
      # LR
      Data_test_Ta_Name$lr_p <- predict(LR, Data_test_Ta_Name, type="response") 
      Data_test_Ta_Name$lr <- ifelse(Data_test_Ta_Name$lr >=0.5,1,0)
      Data_test_Ta_Name$lr <- Data_test_Ta_Name$lr %>% as.numeric()
      Data_test_Ta_Name$lr %>% table()
      # NN
      library(nnet)
      Data_test_Ta_Name$nn <- predict(object = nn1 , newdata = Data_test_Ta_Name , type = "class")
      Data_test_Ta_Name$nn <- Data_test_Ta_Name$nn %>% as.numeric()
      Data_test_Ta_Name$nn %>% table()
      # tree
      library(rpart)
      Data_test_Ta_Name$tree <- predict(object = tree2, newdata = Data_test_Ta_Name, type = "class")
      Data_test_Ta_Name$tree <- Data_test_Ta_Name$tree %>% as.character()
      Data_test_Ta_Name$tree %>% table()
      
      
      
      # RF、SVM的虛擬變數level要與TrainingData一致才能跑預測值(predict function)。
      Org_Campaign_Charge <- Data_test_Ta_Name$Campaign_Charge
      Org_Campaign_Channel <- Data_test_Ta_Name$Campaign_Channel
      Org_TA_Gender <- Data_test_Ta_Name$TA_Gender
      
      Campaign_Charge_Level <- as.factor(rf3$forest$xlevels$Campaign_Charge)    # 把模型中的Charge變數裡的level取出
      Campaign_Channel_Level <- as.factor(rf3$forest$xlevels$Campaign_Channel)  # 把模型中的Charge變數裡的level取出
      TA_Gender_Level  <- as.factor(rf3$forest$xlevels$TA_Gender)               # 把模型中的Charge變數裡的level取出
      
      levels(Data_test_Ta_Name$Campaign_Charge) <- levels(Campaign_Charge_Level) # 強制設定給Testing的 Campaign_Charge
      levels(Data_test_Ta_Name$Campaign_Channel) <- levels(Campaign_Channel_Level) # 強制設定給Testing的 Campaign_Charge
      levels(Data_test_Ta_Name$TA_Gender) <- levels(TA_Gender_Level) # 強制設定給Testing的 Campaign_Charge
      
      Data_test_Ta_Name$Campaign_Charge %>% table() # 測試是否有類別被替換的問題，例如：VO變成OCPM。

      
      # rf
      library(randomForest)
      Data_test_Ta_Name$rf <- predict(object = rf3, newdata = Data_test_Ta_Name, type = "class")
      Data_test_Ta_Name$rf <- Data_test_Ta_Name$rf %>% as.character()
      Data_test_Ta_Name$rf %>% table()
      # svm
      library(kernlab)
      Data_test_Ta_Name$svm <- predict(object = svm4, newdata = Data_test_Ta_Name , type = "response")
      Data_test_Ta_Name$svm <- Data_test_Ta_Name$svm %>% as.character()
      Data_test_Ta_Name$svm %>% table()
      
      
      # 改回原本投放方式、性別、系統的欄位。
      Data_test_Ta_Name %<>% dplyr::mutate("Campaign_Charge" = Org_Campaign_Charge)
      Data_test_Ta_Name %<>% dplyr::mutate("Campaign_Channel" = Org_Campaign_Channel)
      Data_test_Ta_Name %<>% dplyr::mutate("TA_Gender" = Org_TA_Gender)
      

      
      # 可能虛擬變數無法帶入模型
      # nn_optCutOff
      # Data_test_Ta_Name$nn <- predict(object = nn1, newdata = Data_test_Ta_Name, type = "class")
      # Data_test_Ta_Name$nn_p <- predict(object = nn1, newdata = Data_test_Ta_Name, type = "raw")
      # Data_test_Ta_Name$nn_p01 <- ifelse(Data_test_Ta_Name$nn_p > nn_optCutOff , 1 , 0)
      # 
      # Data_test_Ta_Name$nn %>% table()
      # Data_test_Ta_Name$nn_p01 %>% table()   
      # 
      # 
      # tree_optCutOff
      # Data_test_Ta_Name$tree <- predict(object = tree2, newdata = Data_test_Ta_Name, type = "class")
      # Data_test_Ta_Name$tree_p <- predict(object = tree2, newdata = Data_test_Ta_Name, type = "prob")[,2]
      # Data_test_Ta_Name$tree_p01 <- ifelse(Data_test_Ta_Name$tree_p > tree_optCutOff , 1 , 0)
      # 
      # Data_test_Ta_Name$tree %>% table()
      # Data_test_Ta_Name$tree_p01 %>% table()      
      # 
      # rf_optCutOff
      # Data_test_Ta_Name$rf <- predict(object = rf3, newdata = Data_test_Ta_Name, type = "class")
      # Data_test_Ta_Name$rf_P <- predict(object = rf3, newdata = Data_test_Ta_Name, type = "prob")[,2]
      # Data_test_Ta_Name$rf_P01 <- ifelse(Data_test_Ta_Name$rf_P > rf_optCutOff , 1 , 0)
      # 
      # Data_test_Ta_Name$rf %>% table()
      # Data_test_Ta_Name$rf_P01 %>% table()
      # 
      # svm_optCutOff
      # Data_test_Ta_Name$svm <- predict(object = svm4, newdata = Data_test_Ta_Name , type = "response")
      # Data_test_Ta_Name$svm_P <- predict(object = svm4, newdata = Data_test_Ta_Name , type = "prob")[,2]
      # Data_test_Ta_Name$svm_P01 <- ifelse(Data_test_Ta_Name$svm_P > svm_optCutOff , 1 , 0)
      # 
      # Data_test_Ta_Name$svm %>% table()
      # Data_test_Ta_Name$svm_P01 %>% table()
      
      
      # Create Pred_ROI_60_break_even
      Data_test_Ta_Name %<>% dplyr::mutate("Pred_ROI_60_break_even" = as.numeric(lr)+as.numeric(nn)+as.numeric(tree)+as.numeric(rf)+as.numeric(svm) )
      Data_test_Ta_Name$Pred_ROI_60_break_even %>% table()

      # 增加 Model的時間
      Data_test_Ta_Name$model_end_date <- as.Date("2019-04-30")   # model_end_date (Model的時間)
      
      fb_ad_prediction_ROI_60_break_even <- Data_test_Ta_Name
      fb_ad_prediction_ROI_60_break_even %>% names()
      
      # 為了shiny subtable join故要把原本的TA_NAME加進去。
      fb_ad_prediction_ROI_60_break_even$TA_name_org <- fb_ad_prediction_ROI_60_break_even$TA_name 
      # select col ==================================================：
      fb_ad_prediction_ROI_60_break_even %<>% dplyr::select(c(fileName
                                                            , file_path , file_path_org
                                                            , Ta_Name , TA_name_org
                                                            , Campaign_Charge
                                                            , Campaign_Channel
                                                            , TA_Location
                                                            , TA_Gender
                                                            , TA_Age , TA_Age_org
                                                            , status
                                                            , model_end_date
                                                            , data_date
                                                            , one_month_data_begin_date
                                                            , data_begin_date
                                                            , data_end_date
                                                            , first_ad_date
                                                            , last_ad_date
                                                            , first_ad_click_date
                                                            , last_ad_click_date
                                                            , FB_impressions
                                                            , FB_reach
                                                            , FB_clicks
                                                            , FB_install
                                                            , FB_spend
                                                            , AF_install
                                                            , ROI_2
                                                            , ROI_7
                                                            , ROI_14
                                                            , ROI_60
                                                            , RetentionRate_2
                                                            , RetentionRate_7
                                                            , RetentionRate_14
                                                            , RetentionRate_60
                                                            , LTV_2
                                                            , LTV_7
                                                            , LTV_14
                                                            , LTV_60
                                                            , DateDay
                                                            , Ratio_7_1
                                                            , Ratio_14_7
                                                            , df7_1
                                                            , df14_7
                                                            , Pred_ROI_60_break_even
                                                            , lr
                                                            , nn
                                                            , tree
                                                            , rf
                                                            , svm))
      # 設定ad_pred_id編號
      fb_ad_prediction_ROI_60_break_even$ad_pred_id <- 1:nrow(fb_ad_prediction_ROI_60_break_even)
      # 雖然應該用不到，但暫時還是先加遊戲代碼
      fb_ad_prediction_ROI_60_break_even$group_id <- "jiangdan"
      
      # 設定每次匯入筆數
      parser_n <- 10000
      
      # ===== 5. 資料個別寫入 =====
      
      # 寫入DB fb_ad_prediction_ROI_60_break_even ==================================================
      print("fb_ad_prediction_ROI_60_break_even")
      if (nrow(fb_ad_prediction_ROI_60_break_even) > 0){
        
        for (i in seq(1, nrow(fb_ad_prediction_ROI_60_break_even), by = parser_n)){
          
          start_ind <- i
          end_ind <- i - 1 + parser_n
          
          if (end_ind > nrow(fb_ad_prediction_ROI_60_break_even)){ 
            end_ind <- nrow(fb_ad_prediction_ROI_60_break_even)
          }
          print(i)
          print(Sys.time())
          # 將資料寫入(SQL版本)
          insert_values <- fb_ad_prediction_ROI_60_break_even[start_ind:end_ind,] %>% apply(1, NAtoNULL) %>% paste0(., collapse = ",")
          insert_SQL <- sprintf("INSERT fb_ad_prediction_ROI_60_break_even (%s) VALUES ",
                                paste0(names(fb_ad_prediction_ROI_60_break_even), collapse = ", "))
          # ON DUPLICATE KEY UPDATE 組字串
          DUPLICATE_KEY_UPDATE_SQL <- names(fb_ad_prediction_ROI_60_break_even) %>% paste0(" = VALUES(",.,")") %>% 
            paste0(names(fb_ad_prediction_ROI_60_break_even),.) %>%
            paste0(collapse = " , ") %>% 
            paste0(" ON DUPLICATE KEY UPDATE ",.,";") 
          insert_fb_ad_prediction_ROI_60_break_even_SQL <- paste0(insert_SQL, insert_values, DUPLICATE_KEY_UPDATE_SQL)
          dbSendQuery(connect_DB, insert_fb_ad_prediction_ROI_60_break_even_SQL)
        }
      }
      
      # close DB
      if (exists("connect_DB")){
        dbDisconnect(connect_DB)
      }
      
      },
    error = function(e) {
      fb_ad_prediction_ROI_60_break_even_Error <- paste("fb_ad_prediction_ROI_60_break_even_Error.R執行失敗，錯誤訊息：", 
                                                  conditionMessage(e))
      }
)    

      
if(!exists("fb_ad_prediction_ROI_60_break_even_Error")){

  # 紀錄log
  futile.logger::flog.info(sprintf("fb_ad_prediction_ROI_60_break_even.R 執行成功 \n StartDate:%s，EndDate:%s，設定時間為：%s-%s", execute_time , Sys.time() , begin_date, end_date),
                           name = "fb_ad_prediction_ROI_60_break_even_log")
  
  # # 寄信通知
  # mailR::send.mail(from  = con_Mysql_gameDB$mail$Gmail_CM_Account,
  #                  to    = con_Mysql_gameDB$mail$Gmail_CM_Account,
  #                  subject = sprintf("fb_ad_prediction_ROI_60_break_even.R 執行成功 \n StartDate:%s，EndDate:%s，設定時間為：%s-%s", execute_time , Sys.time() , begin_date, end_date),
  #                  body    = sprintf("主程式已成功執行：\n 程式執行成功：\n %s\n",
  #                                    "fb_ad_prediction_ROI_60_break_even.R"),
  #                  encoding = "utf-8",
  #                  smtp = list(host.name = "aspmx.l.google.com", port = 25),
  #                  authenticate = FALSE,
  #                  send = TRUE)
  
} else {
  
  # 紀錄log
  futile.logger::flog.error(fb_ad_prediction_ROI_60_break_even_Error,
                            name = "fb_ad_prediction_ROI_60_break_even_log")
  # 寄信通知
  mailR::send.mail(from  = con_Mysql_gameDB$mail$Gmail_CM_Account,
                   to    = con_Mysql_gameDB$mail$Gmail_CM_Account,
                   subject = sprintf("fb_ad_prediction_ROI_60_break_even.R  主程式錯誤訊息，時間%s 設定時間為：%s-%s", execute_time , Sys.time() , begin_date, end_date),
                   body = sprintf("錯誤訊息：\n 程式執行失敗\n %s\n",
                                  fb_ad_tag_name_Error),
                   encoding = "utf-8",
                   smtp = list(host.name = "aspmx.l.google.com", port = 25),
                   authenticate = FALSE,
                   send = TRUE)
  }

      


}



      