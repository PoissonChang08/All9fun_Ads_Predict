# === Start ===  
# 主程式 fb_ad_prediction_ROI_60_break_even_accurancy
# 維護者: Poisson Chang
# 更新日期: 2019-07-31
# === End ===



  
# Login & Connect  ==================================================
# Cube_DB
source("/home/poisson/login_DB.r")


# Insert Table：
# fb_ad_prediction_ROI_60_break_even_accurancy

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
logpath <- "/home/poisson/log紀錄檔/mg_fb_tag_analysis/fb_ad_prediction_ROI_60_break_even_accurancy.log"
# logpath <- "C:/Users/pc072/Desktop/fb_Tag_analysis/log紀錄檔/mobile_game_fb_tag_analysis.log"

# log檔路徑
if (file.exists(logpath) == FALSE){
  file.create(logpath)}
# 啟動log
flog.logger(appender = appender.file(logpath), name = "fb_ad_prediction_ROI_60_break_even_accurancy_log")




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
    
    # accurancy-統計：  ==================================================
    fb_ad_prediction_ROI_60_break_even_accurancy <- dbGetQuery(connect_DB,
         "-- accurancy、sensitivity、specificity by data_date
         -- lr
         SELECT 'lr' AS Model , model_end_date , data_date 
         , SUM(CASE WHEN (A.Real = 0 AND A.Pred = 0) OR (A.Real = 1 AND A.Pred = 1) THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(A.COUNT) END AS accurancy
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 1 THEN A.COUNT ELSE 0 END) AS sensitivity
         , SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 0 THEN A.COUNT ELSE 0 END) AS specificity
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(CASE WHEN A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS `precision_01`
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_1_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_0_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_1_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_0_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(A.COUNT) END AS N
         FROM(
         SELECT model_end_date , data_date   
         , CASE WHEN lr = 1 THEN '1' ELSE '0' END AS `Pred`
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END AS `Real`
         , COUNT(1) AS COUNT
         FROM analysis.fb_ad_prediction_ROI_60_break_even 
         GROUP BY model_end_date , data_date 
         , CASE WHEN lr = 1 THEN '1' ELSE '0' END
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END) A
         GROUP BY  model_end_date , data_date 
         UNION ALL
         -- nn
         SELECT 'nn' AS Model , model_end_date , data_date 
         , SUM(CASE WHEN (A.Real = 0 AND A.Pred = 0) OR (A.Real = 1 AND A.Pred = 1) THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(A.COUNT) END AS accurancy
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 1 THEN A.COUNT ELSE 0 END) AS sensitivity
         , SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 0 THEN A.COUNT ELSE 0 END) AS specificity
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(CASE WHEN A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS `precision_01`
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_1_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_0_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_1_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_0_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(A.COUNT) END AS N
         FROM(
         SELECT model_end_date , data_date   
         , CASE WHEN nn = 1 THEN '1' ELSE '0' END AS `Pred`
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END AS `Real`
         , COUNT(1) AS COUNT
         FROM analysis.fb_ad_prediction_ROI_60_break_even 
         GROUP BY model_end_date , data_date 
         , CASE WHEN nn = 1 THEN '1' ELSE '0' END
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END) A
         GROUP BY  model_end_date , data_date 
         UNION ALL
         -- tree
         SELECT 'tree' AS Model ,  model_end_date , data_date  
         , SUM(CASE WHEN (A.Real = 0 AND A.Pred = 0) OR (A.Real = 1 AND A.Pred = 1) THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(A.COUNT) END AS accurancy
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 1 THEN A.COUNT ELSE 0 END) AS sensitivity
         , SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 0 THEN A.COUNT ELSE 0 END) AS specificity
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(CASE WHEN A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS `precision_01`
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_1_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_0_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_1_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_0_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(A.COUNT) END AS N
         FROM(
         SELECT model_end_date , data_date   
         , CASE WHEN tree = 1 THEN '1' ELSE '0' END AS `Pred`
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END AS `Real`
         , COUNT(1) AS COUNT
         FROM analysis.fb_ad_prediction_ROI_60_break_even 
         GROUP BY model_end_date , data_date 
         , CASE WHEN tree = 1 THEN '1' ELSE '0' END
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END) A
         GROUP BY  model_end_date , data_date  
         UNION ALL
         -- rf
         SELECT 'rf' AS Model ,  model_end_date , data_date  
         , SUM(CASE WHEN (A.Real = 0 AND A.Pred = 0) OR (A.Real = 1 AND A.Pred = 1) THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(A.COUNT) END AS accurancy
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 1 THEN A.COUNT ELSE 0 END) AS sensitivity
         , SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 0 THEN A.COUNT ELSE 0 END) AS specificity
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(CASE WHEN A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS `precision_01`
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_1_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_0_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_1_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_0_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(A.COUNT) END AS N
         FROM(
         SELECT model_end_date , data_date   
         , CASE WHEN rf = 1 THEN '1' ELSE '0' END AS `Pred`
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END AS `Real`
         , COUNT(1) AS COUNT
         FROM analysis.fb_ad_prediction_ROI_60_break_even 
         GROUP BY model_end_date , data_date 
         , CASE WHEN rf = 1 THEN '1' ELSE '0' END
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END) A
         GROUP BY  model_end_date , data_date  
         UNION ALL
         -- svm
         SELECT 'svm' AS Model ,  model_end_date , data_date  
         , SUM(CASE WHEN (A.Real = 0 AND A.Pred = 0) OR (A.Real = 1 AND A.Pred = 1) THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(A.COUNT) END AS accurancy
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 1 THEN A.COUNT ELSE 0 END) AS sensitivity
         , SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) / SUM(CASE WHEN A.Real = 0 THEN A.COUNT ELSE 0 END) AS specificity
         , SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) / CASE WHEN A.Real IS NULL THEN 0 ELSE SUM(CASE WHEN A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS `precision_01`
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_1_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_0_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 1 AND A.Pred = 0 THEN A.COUNT ELSE 0 END) END AS real_1_pred_0
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(CASE WHEN A.Real = 0 AND A.Pred = 1 THEN A.COUNT ELSE 0 END) END AS real_0_pred_1
         , CASE WHEN A.Real IS NULL THEN NULL ELSE SUM(A.COUNT) END AS N
         FROM(
         SELECT model_end_date , data_date    
         , CASE WHEN svm = 1 THEN '1' ELSE '0' END AS `Pred`
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END AS `Real`
         , COUNT(1) AS COUNT
         FROM analysis.fb_ad_prediction_ROI_60_break_even 
         GROUP BY model_end_date , data_date 
         , CASE WHEN svm = 1 THEN '1' ELSE '0' END
         , CASE WHEN ROI_60 IS NULL THEN NULL ELSE CASE WHEN ROI_60 >=0 THEN 1 ELSE 0 END END) A
         GROUP BY  model_end_date , data_date  ;
         ")
    
    
    # 設定每次匯入筆數
    parser_n <- 10000
    
    # ===== 5. 資料個別寫入 =====
    
    # 寫入DB fb_ad_prediction_ROI_60_break_even_accurancy ==================================================
    print("fb_ad_prediction_ROI_60_break_even_accurancy")
    if (nrow(fb_ad_prediction_ROI_60_break_even_accurancy) > 0){
      
      for (i in seq(1, nrow(fb_ad_prediction_ROI_60_break_even_accurancy), by = parser_n)){
        
        start_ind <- i
        end_ind <- i - 1 + parser_n
        
        if (end_ind > nrow(fb_ad_prediction_ROI_60_break_even_accurancy)){ 
          end_ind <- nrow(fb_ad_prediction_ROI_60_break_even_accurancy)
        }
        print(i)
        print(Sys.time())
        # 將資料寫入(SQL版本)
        insert_values <- fb_ad_prediction_ROI_60_break_even_accurancy[start_ind:end_ind,] %>% apply(1, NAtoNULL) %>% paste0(., collapse = ",")
        insert_SQL <- sprintf("INSERT fb_ad_prediction_ROI_60_break_even_accurancy (%s) VALUES ",
                              paste0(names(fb_ad_prediction_ROI_60_break_even_accurancy), collapse = ", "))
        
        # ON DUPLICATE KEY UPDATE 組字串
        DUPLICATE_KEY_UPDATE_SQL <- names(fb_ad_prediction_ROI_60_break_even_accurancy) %>% paste0(" = VALUES(",.,")") %>% 
          paste0(names(fb_ad_prediction_ROI_60_break_even_accurancy),.) %>%
          paste0(collapse = " , ") %>% 
          paste0(" ON DUPLICATE KEY UPDATE ",.,";") 
        fb_ad_prediction_ROI_60_break_even_accurancy_SQL <- paste0(insert_SQL, insert_values, DUPLICATE_KEY_UPDATE_SQL)
        dbSendQuery(connect_DB, fb_ad_prediction_ROI_60_break_even_accurancy_SQL)
      }
    }
    
    # close DB
    if (exists("connect_DB")){
      dbDisconnect(connect_DB)
    }
    
  },
  error = function(e) {
    fb_ad_prediction_ROI_60_break_even_accurancy_SQL_Error <- paste("fb_ad_prediction_ROI_60_break_even_accurancy_SQL_Error.R執行失敗，錯誤訊息：", 
                                                      conditionMessage(e))
  }
)    


if(!exists("fb_ad_prediction_ROI_60_break_even_accurancy_SQL_Error")){
  
  # 紀錄log
  futile.logger::flog.info(sprintf("fb_ad_prediction_ROI_60_break_even_accurancy.R 執行成功 \n StartDate:%s，EndDate:%s，設定時間為：All", execute_time , Sys.time()),
                           name = "fb_ad_prediction_ROI_60_break_even_accurancy_log")
  
  # # 寄信通知
  # mailR::send.mail(from  = con_Mysql_gameDB$mail$Gmail_CM_Account,
  #                  to    = con_Mysql_gameDB$mail$Gmail_CM_Account,
  #                  subject = sprintf("fb_ad_prediction_ROI_60_break_even_accurancy.R 執行成功 \n StartDate:%s，EndDate:%s，設定時間為：All", execute_time , Sys.time()),
  #                  body    = sprintf("主程式已成功執行：\n 程式執行成功：\n %s\n",
  #                                    "fb_ad_prediction_ROI_60_break_even_accurancy.R"),
  #                  encoding = "utf-8",
  #                  smtp = list(host.name = "aspmx.l.google.com", port = 25),
  #                  authenticate = FALSE,
  #                  send = TRUE)
  
} else {
  
  # 紀錄log
  futile.logger::flog.error(fb_ad_prediction_ROI_60_break_even_Error,
                            name = "fb_ad_prediction_ROI_60_break_even_accurancy_log")
  # 寄信通知
  mailR::send.mail(from  = con_Mysql_gameDB$mail$Gmail_CM_Account,
                   to    = con_Mysql_gameDB$mail$Gmail_CM_Account,
                   subject = sprintf("fb_ad_prediction_ROI_60_break_even_accurancy.R  主程式錯誤訊息，時間%s 設定時間為：All", execute_time , Sys.time()),
                   body = sprintf("錯誤訊息：\n 程式執行失敗\n %s\n",
                                  fb_ad_tag_name_Error),
                   encoding = "utf-8",
                   smtp = list(host.name = "aspmx.l.google.com", port = 25),
                   authenticate = FALSE,
                   send = TRUE)
}


    







