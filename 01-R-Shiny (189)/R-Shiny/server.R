

# server  ==================================================

server <- function(input, output , session) {
  
  
  # 載入套件 ==================================================
  library(xlsx)     # export xlsx report 
  # library(ggthemr)  # theme
  library(ggplot2)  # ggplot2 graph 
  library(plotly)   # plotly 
  library(plyr)  
  library(dplyr)    # data DML
  # install.packages("shinythemes")
  library(shinythemes)
  library(tidyr)
  library(DT)
  library(shinycssloaders) # Loading 
  
  
  # loading function 
  source("/home/poisson/fn_Calculation/Retention_ROI_LTV_function.R")
  source("/home/poisson/fn_shiny/shinyInput_function.R")    # DT-subtable(index 參數)
  source("/home/poisson/fn_shiny/DT_table_type_function.R") # DT-中文

  # 設定子表格數據計算期間
  execute_Date <- Sys.Date()
  one_month_begin_date <- execute_Date - 89
  begin_date <- execute_Date - 59
  end_date   <- execute_Date - 14
  
  # Loading Setting  ==================================================
  W <- weekdays(as.Date(Sys.Date()))
  
  if(W %in% c("Saturday","Sunday")){
    # Loading Setting
    LoadingType = "1"
    LoadingColor = "#FF9900"
    Loadingbackground = "background"
  } else if (W %in% c("Wednesday","Thursday","Friday")){
    # Loading Setting
    LoadingType = "7"
    LoadingColor = "#FFCCCC"
    Loadingbackground = "background"
  } else {
    # Loading Setting
    LoadingType = "7"
    LoadingColor = "#FFCCCC"
    Loadingbackground = "background"
  }
  
  

  # 建立模型相關資訊 
  observeEvent(input$Submit,{
    showModal( 
      modalDialog(  
                　  easyClose = T # 按esc直接關閉
                  , fade = T # animation
                  , footer = modalButton("關閉")
                　 
        　　     , div(p("建立模型相關資訊"),style="text-align: center;color: #000000; font-weight:bold ; font-size:30px")
                 , div(p("廣告樣本："),style="color: #000000; font-weight:bold ; font-size:24px")
                 , div(p("1、排除安裝數等於「0」。"),style="color: #669933; font-weight:bold ; font-size:14px")
                 , div(p("2、排除impression小於「1,000」的廣告。"),style="color: #669933; font-weight:bold ; font-size:14px")
        　　     , div(p("3、排除次日ROI = 「-100%」的廣告。"),style="color: #669933; font-weight:bold ; font-size:14px")
　　             , div(p("4、排除14日ROI = 「-100%」的廣告。"),style="color: #669933; font-weight:bold ; font-size:14px")
                 , div(p("5、排除14日ROI大於「0%」的廣告。"),style="color: #669933; font-weight:bold ; font-size:14px")
                 , div(p("註解：詳細廣告樣本篩選流程及統計方法，請參考第一次簡報"),style="color: #336699; font-weight:bold ; font-size:14px")
                 # 模型特徵值
                 , div(p("模型選取特徵值："),style="color: #000000; font-weight:bold ; font-size:24px")
                 , div(p("1、投放方式、系統、性別、年齡、地區。"),style="color: #990033; font-weight:bold ; font-size:14px")
                 , div(p("2、安裝、CR、CPI。"),style="color: #990033; font-weight:bold ; font-size:14px")
                 , div(p("3、14日留存、14日LTV。"),style="color: #990033; font-weight:bold ; font-size:14px")
                 , div(p("4、投放天數、Diff_1-7、Ratio_1-7、Ratio_7-14。"),style="color: #990033; font-weight:bold ; font-size:14px")
                 , div(p("預測變數(Y)：廣告60日ROI是否能回本 (是：1、否：0)。"),style="color: #996699; font-weight:bold ; font-size:14px")
                 , div(p("註解：模型特徵值挑選準則，請參考第一次簡報"),style="color: #336699; font-weight:bold ; font-size:14px")
                 # 訓練資料樣本、測試資料樣本
                 , div(p("建模樣本資訊："),style="color: #000000; font-weight:bold ; font-size:24px")
                 , div(p("1、Data：1002筆樣本《 回本：不回本 = 19.2% (192)：80.8% (810) 》。"),style="color: #CC6600; font-weight:bold ; font-size:14px")
                 , div(p("2、TrainingData：701筆樣本《 回本：不回本 = 19.2% (134)：80.8% (567) 》。"),style="color: #CC6600; font-weight:bold ; font-size:14px")
                 , div(p("3、TestData：301筆樣本《 回本：不回本 = 19.2% (58)：80.8% (243) 》。"),style="color: #CC6600; font-weight:bold ; font-size:14px")
                 , style="background-color: #FFFFCC")
      )　
  },ignoreInit = T) 
  
  
  # 機器學習應用表格(廣告預測名冊、預測回本廣告、預測不回本廣告)  ==================================================
  prediction_table <- eventReactive(input$prediction_Update,{

          source("/home/poisson/login_DB.r")
          # source("/opt/shiny-server/samples/sample-apps/fb_tag_analysis_daily/login_DB.r")

          # fb_ad_prediction_ROI_60_break_even ：  ==================================================
          fb_ad_prediction_ROI_60_break_even_SQL <- sprintf("SELECT status , fileName , file_path , file_path_org , TA_name , TA_name_org , TA_Age_org , Campaign_Charge , Campaign_Channel , TA_Location , TA_Gender ,  FB_impressions AS impressions, FB_install , AF_install , FB_spend , DateDay AS 投放天數 , RetentionRate_2 AS `次日留存`, RetentionRate_14  AS `十四日留存`, ROI_2 AS 次日ROI , ROI_7 AS 七日ROI , ROI_14 AS 十四日ROI , CASE WHEN ROI_60 IS NULL THEN '-' ELSE ROI_60 END AS 六十日ROI
                                                                  , CASE WHEN nn = '1' THEN 'V' ELSE 'X' END AS 廣告60日回本預測
                                                            FROM analysis.fb_ad_prediction_ROI_60_break_even
                                                            WHERE 1=1
                                                            AND data_date = '%s'
                                                            AND model_end_date = '2019-04-30'
                                                            ORDER BY file_path , TA_name;"
                                                            , Sys.Date())
                                                            # , input$daily_dates[1]
                                                            # , input$daily_dates[2])
          fb_ad_prediction_ROI_60_break_even <- dbGetQuery(connect_DB,fb_ad_prediction_ROI_60_break_even_SQL)
          fb_ad_prediction_ROI_60_break_even_1 <- fb_ad_prediction_ROI_60_break_even %>% dplyr::filter(廣告60日回本預測 == "V")
          fb_ad_prediction_ROI_60_break_even_0 <- fb_ad_prediction_ROI_60_break_even %>% dplyr::filter(廣告60日回本預測 == "X")

          # 新增子表格-欄位 (記得要先loading shinyInput函數)
          fb_ad_prediction_ROI_60_break_even <- as.data.frame(cbind('*' = shinyInput(actionButton, nrow(fb_ad_prediction_ROI_60_break_even), 'button_' , onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
                                                                    , fb_ad_prediction_ROI_60_break_even))
          # 新增子表格-欄位 (記得要先loading shinyInput函數)
          fb_ad_prediction_ROI_60_break_even_1 <- as.data.frame(cbind('*' = shinyInput(actionButton, nrow(fb_ad_prediction_ROI_60_break_even_1),'button_' , onclick = 'Shiny.onInputChange(\"select_button_1\",  this.id)' )
                                                                    , fb_ad_prediction_ROI_60_break_even_1))
          # 新增子表格-欄位 (記得要先loading shinyInput函數)
          fb_ad_prediction_ROI_60_break_even_0 <- as.data.frame(cbind('*' = shinyInput(actionButton, nrow(fb_ad_prediction_ROI_60_break_even_0),'button_',  onclick = 'Shiny.onInputChange(\"select_button_0\",  this.id)' )
                                                                      , fb_ad_prediction_ROI_60_break_even_0))

          suppressWarnings(fb_ad_prediction_ROI_60_break_even)
          suppressWarnings(fb_ad_prediction_ROI_60_break_even_1)
          suppressWarnings(fb_ad_prediction_ROI_60_break_even_0)
          # Colse DB
          dbDisconnect(connect_DB)


          prediction_list <- list(fb_ad_prediction_ROI_60_break_even
                                , fb_ad_prediction_ROI_60_break_even_1
                                , fb_ad_prediction_ROI_60_break_even_0
                                  )

          return(prediction_list)
  })
  
  
  
  # 子表格可任意移動
  # jqui_draggable(selector = '.modal-content')
  # session$sendCustomMessage(type = 'resetInputValue', message =  "prediction_Update")
  
  # # This is needed so that the button is clicked once for modal to show, a bug reported here
  # # https://github.com/ebailey78/shinyBS/issues/57
  
  # # toggleModal select_button
  # observeEvent(input$select_button, {
  #   print(input$select_button)
  #   # print(strsplit(input$select_button, "_"))
  #   # print(strsplit(input$select_button, "_")[[1]])
  #   # print(strsplit(input$select_button, "_")[[1]][2])
  #   # print(as.numeric(strsplit(input$select_button, "_")[[1]][2]))
  #   toggleModal(session, "modalExample", "open")
  #   session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
  #   # jqui_draggable(selector = '.modal-content')
  # })
  # # toggleModal select_button_1
  # observeEvent(input$select_button_1, {
  #   print(strsplit(input$select_button_1, "_"))
  #   toggleModal(session, "modalExample_1", "open")
  # })
  # # toggleModal select_button_0
  # observeEvent(input$select_button_0, {
  #   print(strsplit(input$select_button_0, "_"))
  #   toggleModal(session, "modalExample_0", "open")
  # })

  # # 子表格-廣告預測名冊
  # FB_ad_id_all <- eventReactive(input$select_button,{
  #                   # 帶入子表格函數(輸出是一個datatable的格式)
  #                   FB_ad_id_fn(prediction_table()[[1]] , input$select_button)
  # })
  # output$FB_ad_id_all <- DT::renderDataTable({FB_ad_id_all()})
  # # 子表格-預測回本廣告
  # FB_ad_id_all_1 <- eventReactive(input$select_button_1,{
  #                   FB_ad_id_fn(prediction_table()[[2]],input$select_button_1)
  # 
  # })
  # output$FB_ad_id_all_1 <- DT::renderDataTable({FB_ad_id_all_1()})
  # 
  # # 子表格-預測不回本廣告
  # FB_ad_id_all_0 <- eventReactive(input$select_button_0,{
  #                   FB_ad_id_fn(prediction_table()[[3]],input$select_button_0)
  # })
  # output$FB_ad_id_all_0 <- DT::renderDataTable({FB_ad_id_all_0()})
  
  
  FB_ad_id_fn <- function(data,select_button){
    
        # condition
          condition <- data[strsplit(select_button, "_")[[1]][2],]
          condition %<>% dplyr::select("fileName" , "file_path" , "file_path_org" , "TA_name" , "TA_name_org" , "TA_Age_org" , "Campaign_Charge" , "Campaign_Channel" , "TA_Location" ,"TA_Gender")
        
        source("/home/poisson/login_DB.r")
        # source("/opt/shiny-server/samples/sample-apps/fb_tag_analysis_daily/login_DB.r")
        
        FB_ad_id_SQL <-  sprintf("	SELECT ad_id, campaign_id , CONCAT(fileHost, filePath, fileSavedName) AS file_path , fileName ,TA_name , Campaign_Charge , Campaign_Channel , TA_Location , TA_Gender , TA_Age 
                                 , SUM(impressions) AS impressions
                                 , SUM(reach) AS reach
                                 , SUM(clicks) AS clicks
                                 , SUM(app_install) AS FB_install
                                 , CASE WHEN SUM(app_install) = 0 THEN '無法計算' ELSE SUM(spend) / NULLIF(SUM(app_install),0) END AS `FB_CPI`
                                 , SUM(spend) AS FB_spend
                                 , SUM(purchase) AS FB_purchase
                                 
                                 FROM analysis.mobile_game_fb_daily_tag
                                 WHERE 1=1
                                 AND Campaign_MobileGame = '將膽'
                                 AND fileName = '%s'
                                 AND CONCAT(fileHost, filePath, fileSavedName) = '%s'
                                 AND TA_name = '%s'
                                 AND Campaign_Charge = '%s'
                                 AND Campaign_Channel = '%s'
                                 AND TA_Location = '%s'
                                 AND TA_Gender = '%s'
                                 AND TA_Age = '%s'
                                 AND `date` BETWEEN '%s' AND '%s'
                                 GROUP BY ad_id, campaign_id , CONCAT(fileHost, filePath, fileSavedName) , fileName ,TA_name , Campaign_Charge , Campaign_Channel , TA_Location , TA_Gender , TA_Age ;"
                                 , condition[,"fileName"][1]
                                 , condition[,"file_path_org"][1]
                                 , condition[,"TA_name_org"][1]
                                 , condition[,"Campaign_Charge"][1]
                                 , condition[,"Campaign_Channel"][1]
                                 , condition[,"TA_Location"][1]
                                 , condition[,"TA_Gender"][1]
                                 , condition[,"TA_Age_org"][1]
                                 , one_month_begin_date , end_date
        )
        FB_ad_id <- dbGetQuery(connect_DB,FB_ad_id_SQL)
        
        
        AF_ad_id_SQL <-  sprintf("		SELECT  fbid AS ad_id, campaign_id , CONCAT(fileHost, filePath, fileSavedName) AS file_path , fileName ,TA_name , Campaign_Charge , Campaign_Channel , TA_Location , TA_Gender , TA_Age
                                 , SUM(CASE WHEN diff_day = 0 THEN dau ELSE 0 END)  AS `AF_install`
                                 -- 留存
                                 , CASE WHEN datediff(CURDATE() , '%s') >= 2 THEN
                                 CASE WHEN SUM(CASE WHEN diff_day = 0 THEN dau ELSE 0 END) = 0 THEN '無法計算' ELSE
                                 SUM(CASE WHEN diff_day = 2 THEN dau ELSE 0 END) /
                                 NULLIF(SUM(CASE WHEN diff_day = 0 THEN dau ELSE 0 END),0) END
                                 ELSE '-' END  AS 次日留存

                                 , CASE WHEN datediff(CURDATE() , '%s') >= 14 THEN
                                 CASE WHEN SUM(CASE WHEN diff_day = 0 THEN dau ELSE 0 END) = 0 THEN '無法計算' ELSE
                                 SUM(CASE WHEN diff_day = 14 THEN dau ELSE 0 END) /
                                 NULLIF(SUM(CASE WHEN diff_day = 0 THEN dau ELSE 0 END),0) END
                                 ELSE '-' END  AS 十四日留存
                                 -- ROI
                                 , CASE WHEN datediff(CURDATE() , '%s') >= 2 THEN
                                 SUM(CASE WHEN diff_day BETWEEN 1 AND 2 THEN revenue ELSE 0 END)/
                                 NULLIF(SUM(CASE WHEN diff_day = 0 THEN spend ELSE 0 END),0) - 1 ELSE '-' END  AS 次日ROI
                                 , CASE WHEN datediff(CURDATE() , '%s') >= 14 THEN
                                 SUM(CASE WHEN diff_day BETWEEN 1 AND 14 THEN revenue ELSE 0 END)/
                                 NULLIF(SUM(CASE WHEN diff_day = 0 THEN spend ELSE 0 END),0) - 1 ELSE '-' END  AS 十四日ROI
                                 FROM analysis.fb_ta_cohort_tag
                                 WHERE 1=1
                                 AND Campaign_MobileGame = '將膽'
                                 AND fileName = '%s'
                                 AND CONCAT(fileHost, filePath, fileSavedName) = '%s'
                                 AND TA_name = '%s'
                                 AND Campaign_Charge = '%s'
                                 AND Campaign_Channel = '%s'
                                 AND TA_Location = '%s'
                                 AND TA_Gender = '%s'
                                 AND TA_Age = '%s'
                                 AND `click_date` BETWEEN '%s' AND '%s'
                                 GROUP BY  fbid , campaign_id , CONCAT(fileHost, filePath, fileSavedName) , fileName ,TA_name , Campaign_Charge , Campaign_Channel , TA_Location , TA_Gender , TA_Age ;"
                                       , end_date , end_date , end_date , end_date
                                       , condition[,"fileName"][1]
                                       , condition[,"file_path_org"][1]
                                       , condition[,"TA_name_org"][1]
                                       , condition[,"Campaign_Charge"][1]
                                       , condition[,"Campaign_Channel"][1]
                                       , condition[,"TA_Location"][1]
                                       , condition[,"TA_Gender"][1]
                                       , condition[,"TA_Age_org"][1]
                                       , one_month_begin_date , end_date
                                       )
              AF_ad_id <- dbGetQuery(connect_DB,AF_ad_id_SQL)


              FB_ad_id <- FB_ad_id %>% dplyr::full_join(AF_ad_id , by = c('ad_id' = 'ad_id' , 'campaign_id' = 'campaign_id' , 'file_path' = 'file_path' , 'fileName' = 'fileName' , 'TA_name' = 'TA_name' , 'Campaign_Charge' = 'Campaign_Charge' , 'Campaign_Channel' = 'Campaign_Channel' , 'TA_Location' = 'TA_Location' , 'TA_Gender' = 'TA_Gender' , 'TA_Age' = 'TA_Age'))
              FB_ad_id %<>% dplyr::mutate("AF_CPI" = ifelse(FB_ad_id$AF_install == 0 , '無法計算', FB_ad_id$FB_spend / FB_ad_id$AF_install ))


              FB_account_id <- dbGetQuery(connect_DB,"SELECT A.ad_id , A.account_id , B.account_name
                                                      FROM analysis.mobile_game_fb_campaign A
                                                      LEFT JOIN analysis.fb_ad_account B
                                                      ON A.account_id = B.account_id
                                                      GROUP BY A.ad_id , A.account_id , B.account_name ;")
              FB_ad_id %<>% dplyr::left_join(FB_account_id , by = c('ad_id' = 'ad_id'))
              
              
              # TA_Age Rename
              # right function
              substrRight <- function(x, n){
                substr(x, nchar(x)-n+1, nchar(x))}
              FB_ad_id %<>% dplyr::mutate("TA_Age" = ifelse( substrRight(FB_ad_id$TA_Age,3)=="63_" , paste0(substr(FB_ad_id$TA_Age,1,2),"歲以上")
                                                             , ifelse(substrRight(FB_ad_id$TA_Age,2) > substr(FB_ad_id$TA_Age,1,2)
                                                                      , paste(substr(FB_ad_id$TA_Age,1,2),substrRight(FB_ad_id$TA_Age,2),sep = "-")
                                                                      , paste(substrRight(FB_ad_id$TA_Age,2),substr(FB_ad_id$TA_Age,1,2),sep = "-")))
                                          )

              # TA_name 重組(由於age的字串縮減用R處理，因此新建TA_name寫在此。)
              FB_ad_id %<>% dplyr::mutate("Ta_Name" = paste0("「",FB_ad_id$Campaign_Charge,"_",FB_ad_id$Campaign_Channel,'_',FB_ad_id$TA_Location,"_",FB_ad_id$TA_Gender,"_",FB_ad_id$TA_Age,"」：",FB_ad_id$TA_name))

              library(stringr)  # str_sub
              FB_ad_id$file_path <- ifelse(str_sub(FB_ad_id$file_path,-3,-1) == "jpg",paste0('<img src="', FB_ad_id$file_path, '" height="64"></img>'),
                                           ifelse(str_sub(FB_ad_id$file_path,-4,-1) == "jpeg",paste0('<img src="', FB_ad_id$file_path, '" height="64"></img>'), 
                                                  paste0('<video src="', FB_ad_id$file_path, '" type="video/mp4" controls="controls" height="90"></video>')))
              FB_ad_id %<>% dplyr::select("account_id","account_name",　"ad_id","file_path","fileName","Ta_Name","impressions","FB_install",'AF_install',"FB_spend",'FB_CPI','AF_CPI','十四日留存','十四日ROI')
              
              # Colse DB
              dbDisconnect(connect_DB)
              # prediction_list <- list(FB_ad_id
              #                         )
              # return(prediction_list)
              # filter = 'top'
                
              DataRow <- FB_ad_id %>%  DT::datatable(
                                                    # filter = 'top'
                                                    rownames = FALSE
                                                    , extensions = c('Select','FixedColumns','Scroller','Buttons') # FixedColumns：可以使用「fixedColumns」功能，Scroller：可以使用「scrollX = TRUE」功能。
                                                    , selection=list(mode="single"
                                                                   , target="cell")
                                                    , options = list(
                                                                      　pageLength = 5 
                                                                      , lengthMenu = c(5,50,100,250)
                                                                      , fixedColumns = list(leftColumns = 2)  # fix column
                                                                      , scrollX = T
                                                                      , language = table_type()
                                                                      , buttons = list(list(extend = 'excel',charset = 'Big5',text = '檔案下載'),
                                                                                       list(extend ='copy',text = '複製')) # datatable 自動下載excel參數。
                                                                      , dom ='fltBip'
                                                                      , searchHighlight = TRUE
                                                                      , autoWidth = F
                                                                      # column_color
                                                                      , initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#FFCCCC', 'color': '#000000'});","}")
                                                                      # , columnDefs=list(list(targets=c(0),width="1%"),
                                                                      #                   list(targets=c(1),width="1%"),
                                                                      #                   list(targets=c(2),width="10%"),
                                                                      #                   list(targets=c(3),width="30%"),
                                                                      #                   list(targets=c(4),width="5%"),
                                                                      #                   list(targets=c(5),width="5%"),
                                                                      #                   list(targets=c(6),width="5%"),
                                                                      #                   list(targets=c(7),width="5%"),
                                                                      #                   list(targets=c(8),width="5%"),
                                                                      #                   list(targets=c(9),width="5%")
                                                                      #                   )
                                                                    )
                                                    , escape = F
                                                  ) %>% 
                
                # 要先把格式改成 datatable 才能改儲存格樣式，例如：百分比%，但若需要篩選功能，必須先決定 filter = 'top'。
                formatCurrency(c("impressions",  "FB_install" , "AF_install" , "FB_spend" )
                               , currency = ""
                               , interval = 3
                               , mark = ","
                               , digits = 0) %>% 
                formatPercentage(c('十四日留存','十四日ROI'), 2) %>%
                formatRound(c('十四日留存','十四日ROI','FB_CPI','AF_CPI'),digits = 2) %>%
                formatStyle(c(0:2,5:100),'text-align' = 'center') %>% 
                formatStyle(c('FB_CPI','AF_CPI','十四日留存'),  # column.name == 'xxx'
                            color = styleEqual("無法計算", c('red')), # value = 1.0 , fontColor = 'red'
                            backgroundColor = '', #背景颜色
                            fontWeight = styleEqual("無法計算", c('bold')))  # 字体粗细
              
              
              # bsModal(name, h2(paste0("廣告細項資料","："), style = "text-align: center;color:#993333 ; font-family: '標楷體' ; font-size:32px ; font-weight:bold" ), "", size = "large",
              #         column(12,DT::renderDataTable(DataRow))
  }
  

  # toggleModal select_button
  observeEvent(input$select_button,{
    tmp <- FB_ad_id_fn(prediction_table()[[1]] , input$select_button) # 要分開寫才可以
    output$FB_ad_id_all_UI <- renderUI({
          # 寫成bsmodel 
          output$tmp_output <- DT::renderDataTable(tmp)
          bsModal(id = "modalExample"
                  , title = h2(paste0(" ","廣告細項資料(",execute_Date,")："), style = "text-align: center;color:#993333 ; font-family: '標楷體' ; font-size:32px ; font-weight:bold" )
                  , trigger = "select_button"
                  , size = "large" # large or small
                  , column(12, withSpinner(DT::dataTableOutput('tmp_output') , type = LoadingType , color = LoadingColor )))
    })
    # 開啟觸及按鈕『select_button』
    toggleModal(session, "modalExample", "open")
    # 可以連續點及兩次
    session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
    # 可以移動視窗
    # jqui_draggable(selector = '.modal-content')
    
  })
  
  # toggleModal select_button_1
  observeEvent(input$select_button_1, {

    tmp_1 <- FB_ad_id_fn(prediction_table()[[2]] , input$select_button_1) # 要分開寫才可以
    output$FB_ad_id_1_UI <- renderUI({
          # 寫成bsmodel 
          output$tmp_output_1 <- DT::renderDataTable(tmp_1)
          bsModal(id = "modalExample_1"
                  , title = h2(paste0(" ","廣告細項資料(",execute_Date,")："), style = "text-align: center;color:#993333 ; font-family: '標楷體' ; font-size:32px ; font-weight:bold" )
                  , trigger = "select_button_1"
                  , size = "large" # large or small
                  , column(12, withSpinner(DT::dataTableOutput('tmp_output_1') , type = LoadingType , color = LoadingColor )))
    })
    
    toggleModal(session, "modalExample_1", "open")
    session$sendCustomMessage(type = 'resetInputValue', message = "select_button_1")
    # jqui_draggable(selector = '.modal-content')
    
  })


  # toggleModal select_button_0
  observeEvent(input$select_button_0, {
    
    tmp_0 <- FB_ad_id_fn(prediction_table()[[3]] , input$select_button_0) # 要分開寫才可以
    output$FB_ad_id_0_UI <- renderUI({
          # 寫成bsmodel 
          output$tmp_output_0 <- DT::renderDataTable(tmp_0)
          bsModal(id = "modalExample_0"
                  , title = h2(paste0(" ","廣告細項資料(",execute_Date,")："), style = "text-align: center;color:#993333 ; font-family: '標楷體' ; font-size:32px ; font-weight:bold" )
                  , trigger = "select_button_0"
                  , size = "large" # large or small
                  , column(12, withSpinner(DT::dataTableOutput('tmp_output_0') , type = LoadingType , color = LoadingColor)))
    })
    
    toggleModal(session, "modalExample_0", "open")
    session$sendCustomMessage(type = 'resetInputValue', message = "select_button_0")
    # jqui_draggable(selector = '.modal-content')
    
  })
  
  
  
  # 準確度驗證-完整表格(verification) ==================================================
  prediction_table_verification <- eventReactive(input$prediction_verification_Update,{
    
          source("/home/poisson/login_DB.r")
          # source("/opt/shiny-server/samples/sample-apps/fb_tag_analysis_daily/login_DB.r")
          
          # fb_ad_prediction_ROI_60_break_even (verification) ：  ==================================================
          fb_ad_prediction_ROI_60_break_even_verification_SQL <- sprintf("SELECT fileName , file_path , TA_name , data_date , DateDay AS 投放天數 , FB_impressions AS impressions , AF_install , FB_spend , ROI_2 AS 次日ROI , ROI_7 AS 七日ROI , ROI_14 AS 十四日ROI , CASE WHEN ROI_60 IS NULL THEN '-' ELSE ROI_60 END AS 六十日ROI 
                                                            	                 , CASE WHEN nn = '1' THEN 'V' ELSE 'X' END AS 廣告60日回本預測
                                                                        FROM analysis.fb_ad_prediction_ROI_60_break_even
                                                                        WHERE 1=1
                                                                        AND data_date = '%s' 
                                                                        AND model_end_date = '%s' 
                                                                        ORDER BY file_path , TA_name;"
                                                                         , input$daily_dates_table
                                                                         # , input$daily_dates_table[2]
                                                                         , input$select_model_verification)
          
          fb_ad_prediction_ROI_60_break_even_verification <- dbGetQuery(connect_DB,fb_ad_prediction_ROI_60_break_even_verification_SQL)
          suppressWarnings(fb_ad_prediction_ROI_60_break_even_verification)
          
          
          # Colse DB
          dbDisconnect(connect_DB)
          
          prediction_list <- list(fb_ad_prediction_ROI_60_break_even_verification
          )
          return(prediction_list)
  })
  
  
  

  # 準確度驗證-彙整表格  ==================================================
  prediction_boxplot_table <- eventReactive(input$prediction_boxplot_Update,{
    
          source("/home/poisson/login_DB.r")
          # source("/opt/shiny-server/samples/sample-apps/fb_tag_analysis_daily/login_DB.r")
          
          # fb_ad_prediction_ROI_60_break_even_accurancy ：  ==================================================
          fb_ad_prediction_ROI_60_break_even_accurancy_SQL <- sprintf("SELECT  Model , model_end_date AS 模型迄日, data_date AS 資料時間, accurancy AS 整體準確率 , sensitivity AS 回本準確率 , specificity AS 不回本準確率  , real_0_pred_0 AS 實際不回本且預測不回本, real_0_pred_1 AS 實際不回本且預測回本, real_1_pred_0 AS 實際回本且預測不回本, real_1_pred_1 AS 實際回本且預測回本, N AS 廣告數
                                                                      FROM analysis.fb_ad_prediction_ROI_60_break_even_accurancy
                                                                      WHERE 1=1 
                                                                      AND data_date BETWEEN '%s' AND '%s' 
                                                                      -- AND model_end_date = '2019-04-30'
                                                                      ORDER BY data_date , Model;"
                                                                      , input$daily_dates[1]
                                                                      , input$daily_dates[2])
          fb_ad_prediction_ROI_60_break_even_accurancy <- dbGetQuery(connect_DB,fb_ad_prediction_ROI_60_break_even_accurancy_SQL)
          suppressWarnings(fb_ad_prediction_ROI_60_break_even_accurancy)
          # 篩選模型時段
          fb_ad_prediction_ROI_60_break_even_accurancy  %<>%  dplyr::filter(模型迄日 == input$select_model)
      
          # Colse DB
          dbDisconnect(connect_DB)
          # 製作圖形title名稱所需參數
          data_date_start <- input$daily_dates[1]
          data_date_end   <- input$daily_dates[2]
          Indicator   <- input$select_Indicator
          
          prediction_list <- list(fb_ad_prediction_ROI_60_break_even_accurancy
                                  , data_date_start
                                  , data_date_end
                                  , Indicator
          )
          return(prediction_list)
  })
  

  ML_table_fn <- function(data){
    
            # 篩選條件
              prediction_table  <- data
              # 廣告是否正在投放
              if(input$select_ad_condition != "All"){ 
                prediction_table  %<>%  dplyr::filter(status ==  input$select_ad_condition)
                
                      if(input$select_ad_condition == "PAUSED"){
                        # 預測回本 目前：PAUSED → 建議開啟，故找安裝數、留存高一點的廣告進行。(從大排到小)
                        prediction_table %<>% dplyr::arrange(desc(十四日留存),desc(AF_install))
                      }
                      if(input$select_ad_condition == "ACTIVE"){
                        # 預測不回本 目前：ACTIVE → 建議關閉，故找安裝數、留存低的廣告進行。(從小排到大)
                        prediction_table %<>% dplyr::arrange(十四日留存,AF_install)
                      }
              }
              # FB安裝數篩選
              if(input$select_fb_install != "All"){
                # input$select_fb_install 為字串必須更改為數值才能篩選資料(比較大小)。
                prediction_table %<>% dplyr::filter(FB_install >= as.numeric(input$select_fb_install))
              }
              # AF安裝數篩選
              if(input$select_af_install != "All"){
                # input$select_af_install 為字串必須更改為數值才能篩選資料(比較大小)。
                prediction_table %<>% dplyr::filter(AF_install >= as.numeric(input$select_af_install))
              }
              # Charge 篩選條件
              if(input$select_fb_charge != "不篩選"){
                prediction_table %<>% dplyr::filter(Campaign_Charge == input$select_fb_charge)
              }
              # Channel 篩選條件
              if(input$select_fb_Channel != "不篩選"){
                prediction_table %<>% dplyr::filter(Campaign_Channel == input$select_fb_Channel)
              }
              # Location 篩選條件
              if(input$select_fb_Location != "不篩選"){
                prediction_table %<>% dplyr::filter(TA_Location == input$select_fb_Location)
              }
            
            # 移除status欄位   
            prediction_table %<>% dplyr::select(-2,-c("fileName","Campaign_Charge" , "Campaign_Channel" , "TA_Location" ,"TA_Gender" ,"file_path_org" , "TA_name_org" , "TA_Age_org" ))
            
            prediction_table %>% DT::datatable(
                                              # filter = 'top'
                                              rownames = FALSE
                                              , extensions = c('FixedColumns','Scroller') # FixedColumns：可以使用「fixedColumns」功能，Scroller：可以使用「scrollX = TRUE」功能。
                                              , options = list(
                                                                pageLength = 50　, lengthMenu = c(50,100,250,1000)
                                                              , language = table_type()
                                                              , fixedColumns = list(leftColumns = 2)  # fix column
                                                              , scrollX = T　, scrollY = "500px"
                                                              , searchHighlight = TRUE
                                                              , autoWidth = T
                                                              # column_color
                                                              , initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#FFFFCC', 'color': '#000000'});","}")
                                                              , columnDefs=list(list(targets=c(1),width="1%"),
                                                                                list(targets=c(2),width="30%"),
                                                                                list(targets=c(10),width="1%"),
                                                                                list(targets=c(11),width="1%"),
                                                                                list(targets=c(12),width="1%"),
                                                                                list(targets=c(13),width="7%"),
                                                                                list(targets=c(14),width="10%")
                                                                                # list(targets=c(2:7),width="35%"),
                                                                                # list(targets=c(8:15),width="80%")
                                                              )
                                                              # , dom ='ftp'
                                              )
                                              , escape = F
                                            )  %>%
              # 要先把格式改成 datatable 才能改儲存格樣式，例如：百分比%，但若需要篩選功能，必須先決定 filter = 'top'。
              formatCurrency(c("impressions", "FB_install" , "AF_install" , "FB_spend" , "投放天數")
                             , currency = ""
                             , interval = 3
                             , mark = ","
                             , digits = 0) %>%
              formatPercentage(c("次日ROI","七日ROI","十四日ROI","六十日ROI",'次日留存','十四日留存'), 2) %>%
              formatRound(c("次日ROI","七日ROI","十四日ROI","六十日ROI",'次日留存','十四日留存'),digits = 2) %>%
              formatStyle(c(1,4:100),'text-align' = 'center') %>%
              formatStyle(c("廣告60日回本預測"),  # column.name == 'xxx'
                          # color = styleEqual("V", c('red')), # value = 1.0 , fontColor = 'red'
                          color = styleEqual(c("V","X"), c('red',"green")),
                          backgroundColor = '', #背景颜色
                          fontWeight = 'bold')  # 字体粗细
    
  }
  
  

    # 機器學習應用表格(廣告預測名冊) - renderDataTable  ==================================================

        prediction_Table <- reactive({
                            # Use ML_table_fn function (函數輸出是DataTable格式)
                            ML_table_fn(prediction_table()[[1]])
          })
        output$value_prediction_Table <- DT::renderDataTable({prediction_Table()})

    
    # 機器學習應用表格(預測回本廣告) - renderDataTable   ==================================================
    
        prediction_Table_1 <- reactive({
                              # Use ML_table_fn function (函數輸出是DataTable格式)
                              ML_table_fn(prediction_table()[[2]])
          })
        output$value_prediction_Table_1 <- DT::renderDataTable({prediction_Table_1()})
    
    # 機器學習應用表格(預測不回本廣告)  - renderDataTable    ==================================================
    
        prediction_Table_0 <- reactive({
                              # Use ML_table_fn function (函數輸出是DataTable格式)
                              ML_table_fn(prediction_table()[[3]])
          })
    output$value_prediction_Table_0 <- DT::renderDataTable({prediction_Table_0()})   
    
    
    
    # 準確度驗證-完整表格 輸出(verification) ==================================================
    
    accurancy_verification_Table <- reactive({
      # 移除fileName欄位
      prediction_table <- prediction_table_verification()[[1]] %>% dplyr::select(-"fileName") 
      prediction_table %>% DT::datatable(
                                        # filter = 'top'
                                        rownames = FALSE 
                                        , extensions = c('FixedColumns','Scroller') # FixedColumns：可以使用「fixedColumns」功能，Scroller：可以使用「scrollX = TRUE」功能。
                                        , options = list(
                                                          pageLength = 50 , lengthMenu = c(50,100,250,1000)
                                                        , language = table_type()
                                                        , fixedColumns = list(leftColumns = 2)  # fix column
                                                        , scrollX = T　, scrollY = "500px"
                                                        , searchHighlight = TRUE
                                                        , autoWidth = T
                                                        # column_color
                                                        , initComplete = JS("
                                                                        function(settings, json) {
                                                                          $(this.api().table().header()).css({'background-color': '#99CCFF', 'color': '#000000'});
                                                                        // 改指定顏色
                                                                        var obj = $(this.api().table().header());
                                                                        var tenth = $(obj).find('tr th').eq(10);
                                                                        var eleventh = $(obj).find('tr th').eq(11);
                                                                        $(tenth).css({'background-color': '#FFFFCC', 'color': '#000000'});
                                                                        $(eleventh).css({'background-color': '#FFFFCC', 'color': '#000000'});}
                                                                            ")
                                                        , columnDefs=list(list(targets=c(0),width="2%"),
                                                                          list(targets=c(1),width="30%")
                                                                          # list(targets=c(2:7),width="35%"),
                                                                          # list(targets=c(8:15),width="80%")
                                                        )
                                                        # , dom ='ftp'
                                                      ) 
                                        , escape = F
                                      )  %>% 
                                        # 要先把格式改成 datatable 才能改儲存格樣式，例如：百分比%，但若需要篩選功能，必須先決定 filter = 'top'。
                                        formatCurrency(c("impressions" , "AF_install" , "FB_spend" )
                                                       , currency = ""
                                                       , interval = 3
                                                       , mark = ","
                                                       , digits = 0) %>% 
                                        formatPercentage(c("次日ROI","七日ROI","十四日ROI","六十日ROI"), 2) %>% 
                                        formatRound(c("次日ROI","七日ROI","十四日ROI","六十日ROI"),digits = 2) %>%
                                        formatStyle(c(0,2:100),'text-align' = 'center') %>%
                                        formatStyle(c(1),'text-align' = 'left') %>%
                                        formatStyle(c("廣告60日回本預測"),  # column.name == 'xxx'
                                                    # color = styleEqual("V", c('red')), # value = 1.0 , fontColor = 'red'
                                                    color = styleEqual(c("V","X"), c('red',"green")),
                                                    backgroundColor = '', #背景颜色
                                                    fontWeight = styleInterval("V", c('normal', 'bold')))  # 字体粗细
    })
    
    output$accurancy_verification_Table <- DT::renderDataTable({accurancy_verification_Table()})
    

    # 準確度驗證-彙整資料(Boxplot graph)  ==================================================
    
    # Boxplot
     prediction_Boxplot_plot <- reactive({
       
               if (nrow(prediction_boxplot_table()[[1]])>0 ) {
                 
                         prediction_boxplot_table()[[1]] %>% 
                          plotly::plot_ly(. 
                                          , x =~ Model
                                          , y =~round(as.numeric(.[,input$select_Indicator])*100,2)
                                          , split =~ Model
                                          , type = "box"
                                          , alpha = 0.5) %>% 
                          plotly::layout(title = ""
                                         , xaxis = list(title = "model", titlefont = list(size = 14) , tickangle = 45 ,tickfont = list(size = 14))
                                         , yaxis = list(title = paste0(input$select_Indicator," (%)") , titlefont = list(size = 14) , tickangle = 0, tickfont = list(size = 14))
                                         # , boxmode = "group"
                          ) 
                 
               }else{plotly_empty()}
       
    })
    output$value_prediction_Boxplot_plot <- renderPlotly({prediction_Boxplot_plot()})

    
    # 準確度驗證-彙整資料(表格) ==================================================
    accurancy_Table <- reactive({

                    prediction_boxplot_table()[[1]] %>% dplyr::arrange(desc(資料時間)) %>% 
                                                            DT::datatable(
                                                                          # filter = 'top'
                                                                          rownames = FALSE 
                                                                        , extensions = c('FixedColumns','Scroller')
                                                                        , options = list(
                                                                                        pageLength = 5
                                                                                      , language = table_type()
                                                                                        # , autoWidth = T
                                                                                      # # column_color
                                                                                      # , initComplete = JS(
                                                                                      #   "function(settings, json) {","$(this.api().table().header()).css({'background-color': '#99CC66', 'color': '#000000'});","}")
                                                                                      , escape = T
                                                                                      # column_color fix
                                                                                      , fixedColumns = list(leftColumns = 1)
                                                                                      , scrollX=TRUE
                                                                                      , searchHighlight = TRUE
                                                                                      )
                                                                        )  %>% 
                      # 要先把格式改成 datatable 才能改儲存格樣式，例如：百分比%，但若需要篩選功能，必須先決定 filter = 'top'。
                      formatPercentage(c("整體準確率","回本準確率","不回本準確率"), 2) %>% 
                      formatRound(c("整體準確率","回本準確率","不回本準確率"),digits = 2) %>%
                      formatStyle(c(1:100),'text-align' = 'center') %>% 
                      # 不同模型繪製不同顏色
                      formatStyle(
                        'Model', target = 'row', 
                        backgroundColor = styleEqual(
                          unique(prediction_boxplot_table()[[1]]$Model),
                          c('#99CCFF', '#FFCC33', '#99CC33','#FFCCCC','#CCCCFF'))
                      )
        
    })
    output$value_accurancy_Table <- DT::renderDataTable({accurancy_Table()})
    
    

    #  Excel 匯出表格(verification) ==================================================
    download_verification_output <- reactive({
      
      # 移除file_path欄位
      prediction_table_verification <- prediction_table_verification()[[1]] %>% dplyr::select(-"file_path")
      
      prediction_list <- list(prediction_table_verification
                              )
      return(prediction_list)
    })
    
    output$download_Excel_verification <- downloadHandler(
      
      filename = function() { paste0(Sys.Date(),"：廣告驗證名冊",".xlsx")},
      content = function(file) {
        
        write.xlsx(as.data.frame(download_verification_output()[[1]]), file, sheetName= "廣告驗證名冊" , append=TRUE, row.names = F)
      }
    )
    
    
    # Excel 匯出表格 ==================================================
    download_output <- reactive({
      
      # format_fn <- function(data){
      # data_K <- data
      #   # 更改欄位儲存格格式
      #   for (i in 1:dim(data_K)[2]) {
      #     if (names(data_K)[i] %in% c("次日ROI","七日ROI","十四日ROI",'次日留存','十四日留存')) {
      #       data_K[,i] <- round(as.numeric(data_K[,i])*100,2) %>% paste0(.,"%")
      #     }
      #   }
      # }
      # # 移除status欄位
      # prediction_table_1 <- prediction_table()[[1]] %>% dplyr::select(-1,-c("file_path", "Campaign_Charge" , "Campaign_Channel" , "TA_Location")) 
      # prediction_table_1 <-  format_fn(prediction_table_1)
      # 
      # prediction_table_2 <-  format_fn(prediction_table_2)
      # 
      # prediction_table_3 <- prediction_table()[[3]] %>% dplyr::select(-1,-c("file_path", "Campaign_Charge" , "Campaign_Channel" , "TA_Location")) 
      # prediction_table_3 <-  format_fn(prediction_table_3)
      
      # 篩選條件 Condition_fn
      condition_excel_fn <- function(data){
                # 篩選條件
                # 廣告是否正在投放
                if(input$select_ad_condition != "All"){ 
                  data  %<>%  dplyr::filter(status ==  input$select_ad_condition)
                              if(input$select_ad_condition == "PAUSED"){
                                # 預測回本 目前：PAUSED → 建議開啟，故找安裝數、留存高一點的廣告進行。(從大排到小)
                                data %<>% dplyr::arrange(desc(十四日留存),desc(AF_install))
                              }
                              if(input$select_ad_condition == "ACTIVE"){
                                # 預測不回本 目前：ACTIVE → 建議關閉，故找安裝數、留存低的廣告進行。(從小排到大)
                                data %<>% dplyr::arrange(十四日留存,AF_install)
                              }
                }
                # FB安裝數篩選
                if(input$select_fb_install != "All"){
                  # input$select_fb_install 為字串必須更改為數值才能篩選資料(比較大小)。
                  data %<>% dplyr::filter(FB_install >= as.numeric(input$select_fb_install))
                }
                # AF安裝數篩選
                if(input$select_af_install != "All"){
                  # input$select_af_install 為字串必須更改為數值才能篩選資料(比較大小)。
                  data %<>% dplyr::filter(AF_install >= as.numeric(input$select_af_install))
                }
                # Charge 篩選條件
                if(input$select_fb_charge != "不篩選"){
                  data %<>% dplyr::filter(Campaign_Charge == input$select_fb_charge)
                }
                # Channel 篩選條件
                if(input$select_fb_Channel != "不篩選"){
                  data %<>% dplyr::filter(Campaign_Channel == input$select_fb_Channel)
                }
                # Location 篩選條件
                if(input$select_fb_Location != "不篩選"){
                  data %<>% dplyr::filter(TA_Location == input$select_fb_Location)
                }
        return(data)
      }
      # 移除status欄位
      prediction_table_1 <- prediction_table()[[1]] %>% condition_excel_fn() %>% dplyr::select(-c(1:2),-c("file_path", "Campaign_Charge" , "Campaign_Channel" , "TA_Location" ,"TA_Gender" ,"file_path_org" , "TA_name_org" , "TA_Age_org"))
      prediction_table_2 <- prediction_table()[[2]] %>% condition_excel_fn() %>% dplyr::select(-c(1:2),-c("file_path", "Campaign_Charge" , "Campaign_Channel" , "TA_Location" ,"TA_Gender" ,"file_path_org" , "TA_name_org" , "TA_Age_org"))
      prediction_table_3 <- prediction_table()[[3]] %>% condition_excel_fn() %>% dplyr::select(-c(1:2),-c("file_path", "Campaign_Charge" , "Campaign_Channel" , "TA_Location" ,"TA_Gender" ,"file_path_org" , "TA_name_org" , "TA_Age_org"))


      
      # # 更改欄位儲存格格式
      # for (i in 1:dim(prediction_table_1)[2]) {
      #   if (names(prediction_table_1)[i] %in% c("次日ROI","七日ROI","十四日ROI",'次日留存','十四日留存')) {
      #     prediction_table_1[,i] <- round(as.numeric(prediction_table_1[,i])*100,2) %>% paste0(.,"%")
      #   }
      # }

      # if( rnow(prediction_table_2) >0 ){
      #         # 更改欄位儲存格格式
      #         for (i in 1:dim(prediction_table_2)[2]) {
      #           if (names(prediction_table_2)[i] %in% c("次日ROI","七日ROI","十四日ROI",'次日留存','十四日留存')) {
      #             prediction_table_2[,i] <- round(as.numeric(prediction_table_2[,i])*100,2) %>% paste0(.,"%")
      #           }
      #         }
      # } else {prediction_table_2 <- prediction_table_2}

      # # 更改欄位儲存格格式
      # for (i in 1:dim(prediction_table_3)[2]) {
      #   if (names(prediction_table_3)[i] %in% c("次日ROI","七日ROI","十四日ROI",'次日留存','十四日留存')) {
      #     prediction_table_3[,i] <- round(as.numeric(prediction_table_3[,i])*100,2) %>% paste0(.,"%")
      #   }
      # }
      prediction_list <- list(prediction_table_1
                            , prediction_table_2
                            , prediction_table_3
                            )
      
      return(prediction_list)
    })
    
    # # 測試資料格式 
    # output$selected_var <- renderText({ })
    #   download_output()[[2]][,"次日ROI"]

    output$download_Excel <- downloadHandler(

      filename = function() { paste0(Sys.Date(),"：廣告預測名冊",".xlsx")},
      content = function(file) {
        
        if(nrow(download_output()[[1]]) == 0 & nrow(download_output()[[2]]) == 0 & nrow(download_output()[[3]]) == 0 ){
          DT <- as.data.frame("無資料")
          write.xlsx(DT, file, sheetName= "廣告名冊" , append=TRUE, row.names = F )
          write.xlsx(DT, file, sheetName= "廣告回本-名冊" , append=TRUE, row.names = F)
          write.xlsx(DT, file, sheetName= "廣告不回本-名冊" , append=TRUE, row.names = F)
        } else if(nrow(download_output()[[2]])==0) {
          DT <- as.data.frame("無資料")
          write.xlsx(as.data.frame(download_output()[[1]]), file, sheetName= "廣告名冊" , append=TRUE, row.names = F )
          write.xlsx(DT, file, sheetName= "廣告回本-名冊" , append=TRUE, row.names = F)
          write.xlsx(as.data.frame(download_output()[[3]]), file, sheetName= "廣告不回本-名冊" , append=TRUE, row.names = F)
        } else if (nrow(download_output()[[3]])==0) {
          DT <- as.data.frame("無資料")
          write.xlsx(as.data.frame(download_output()[[1]]), file, sheetName= "廣告名冊" , append=TRUE, row.names = F )
          write.xlsx(as.data.frame(download_output()[[2]]), file, sheetName= "廣告回本-名冊" , append=TRUE, row.names = F)
          write.xlsx(DT, file, sheetName= "廣告不回本-名冊" , append=TRUE, row.names = F)
        } else {
          write.xlsx(as.data.frame(download_output()[[1]]), file, sheetName= "廣告名冊" , append=TRUE, row.names = F )
          write.xlsx(as.data.frame(download_output()[[2]]), file, sheetName= "廣告回本-名冊" , append=TRUE, row.names = F)
          write.xlsx(as.data.frame(download_output()[[3]]), file, sheetName= "廣告不回本-名冊" , append=TRUE, row.names = F)
        }

      }
    )
 
}




#  ==================================================






