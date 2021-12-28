

# Login & Connect DB ==================================================

source("/home/poisson/login_DB.r")
# source("/opt/shiny-server/samples/sample-apps/fb_tag_analysis_daily/login_DB.r")
# source("C:/Users/pc072/Desktop/fb_Tag_analysis/login_DB.r")



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


library(DT)
library(shiny)
library(shinyBS)   # 點擊後視窗呈現
library(shinyjs)   # 連續點擊視窗
library(shinyjqui) # 移動視窗
library(shinydashboard)


# 設定時間
Satrt_Date <- as.Date("2019-06-01")
End_Date   <- Sys.Date()-46

execute_Date <- Sys.Date()
one_month_begin_date <- execute_Date - 89
begin_date <- execute_Date - 59
end_date   <- execute_Date - 14


####  設定參數  ==================================================
condition_parameter <- dbGetQuery(connect_DB,
                                             " SELECT DISTINCT
                                                     Campaign_Channel
                                                   , TA_Location
                                                   , Campaign_Charge
                                                   , TA_Age
                                                   , TA_Gender   
                                             FROM analysis.fb_ad_prediction_ROI_60_break_even;") 
condition_Campaign_Charge   <- condition_parameter %>% dplyr::select(Campaign_Charge) %>% dplyr::distinct(Campaign_Charge) %>% dplyr::arrange(Campaign_Charge) %>% dplyr::filter(Campaign_Charge!="NA") %>% t() %>% cbind("不篩選",.)
condition_Campaign_Channel  <- condition_parameter %>% dplyr::select(Campaign_Channel) %>% dplyr::distinct(Campaign_Channel) %>% dplyr::arrange(Campaign_Channel) %>% dplyr::filter(Campaign_Channel!="NA") %>% t() %>% cbind("不篩選",.)
condition_TA_Location       <- condition_parameter %>% dplyr::select(TA_Location) %>% dplyr::distinct(TA_Location) %>% dplyr::arrange(TA_Location) %>% dplyr::filter(TA_Location!="NA") %>% t() %>% cbind("不篩選",.)



# Colse DB
# dbDisconnect(connect_DBUbuntu28)
dbDisconnect(connect_DB)

# Loading Setting
W <- weekdays(as.Date(Sys.Date()))

if(W %in% c("Saturday","Sunday")){
  # Loading Setting
  LoadingType = "1"
  LoadingColor = "#0066CC"
  Loadingbackground = "background"
} else if (W %in% c("Wednesday","Thursday","Friday")){
  # Loading Setting
  LoadingType = "7"
  LoadingColor = "#99CC33"
  Loadingbackground = "background"
} else {
  # Loading Setting
  LoadingType = "7"
  LoadingColor = "#FF9900"
  Loadingbackground = "background"
}


# ui  ==================================================
## ui.R ##

library(devtools)
library(dashboardthemes)
library(shinydashboard)
library(shinyBS) # bsCollapse、bsCollapsePanel

ui <- 
  shinyUI(
  navbarPage( inverse = F , collapsible = T 
            , title = "Sheet Options"
            , id='tabs'
            ,
            # 一、Sheet(1)-廣告60日回本預測-機器學習模型應用  ==================================================
            # tabPanel(p(id="big-heading_1", "廣告60日回本預測-五種機器學習模型應用",tags$style( HTML("#big-heading_1{color: #000000;}"))),
            tabPanel(HTML("<i class='glyphicon glyphicon-th-list' style='font-size:16px'></i> 廣告60日回本預測-機器學習模型應用"),value="ad_tab",
                     tags$style(HTML(".nav > li > a[data-value='ad_tab'] {background-color:#ffdbdb;color:#c42f2f;font-size:16px; font-weight:bold;}
                                      .navbar-default .navbar-nav > .active > a[data-value='ad_tab'] {background:#c42f2f;color:#ffdbdb;font-size:16px; font-weight:bold;}
                                      .nav a:hover[data-value='ad_tab'] {background-color: #f9fce8 !important;color:black;}"
                                     )),
                     tags$style(HTML(".navbar.navbar-default.navbar-static-top{font-size:16px;background-color:#FFCC99;}")),
                     tags$style(".modal-lg {width: 95%;}"),
                     mainPanel(actionButton("Submit","建立模型相關資訊", icon = icon("book",lib = "glyphicon"), style="color:#CC0033 ; background-color: #FFCCCC ; font-size:14px ; font-weight:bold ")),br(),   
                  # title - 廣告60日回本預測-機器學習模型應用  (置中)
                  div(p("廣告60日回本預測-機器學習模型應用"),style="position: absolute; text-align: left ;font-size:32px ; padding: 0 0 0 35% ; color: #000000; font-weight:bold"),br(), br(), br(), 
                  fluidRow(
                    sidebarPanel(width = 12,
                                 h6(paste0(execute_Date,"-預測資料"), style = "text-align: center;color:#336633 ; font-family: '標楷體' ; font-size:24px ; font-weight:bold "),
                                 h6(paste0("","(09:25AM 更新資料)"), style = "text-align: center;color:#336633 ; font-family: '標楷體' ; font-size:24px ; font-weight:bold "),
                                 
                                 h6(paste0("遊戲：","將膽"), style = "text-align: center;color:#339933 ; font-family: '標楷體' ; font-size:16px ; font-weight:bold"),
                                 h6(paste0("廣告投放期間：",begin_date,"~",end_date), style = "text-align: center;color:#339933 ; font-family: '標楷體' ; font-size:16px ; font-weight:bold"),
                                 h6(paste0("數據計算範圍：",one_month_begin_date,"~",end_date), style = "text-align: center;color:#339933 ; font-family: '標楷體' ; font-size:16px ; font-weight:bold"),
                                 h6(paste0("模型訓練範圍：","2018-04-01","~","2019-04-30"), style = "text-align: center;color:#339933 ; font-family: '標楷體' ; font-size:16px ; font-weight:bold"),
                                 h6(paste0("目前採用的演算法：","Neural Network"), style = "text-align: center;color:#339933 ; font-family: '標楷體' ; font-size:16px ; font-weight:bold")

                    )),
                  
                  # fluidRow(column(12,actionButton("prediction_Update", label = p("查詢",span(style="text-align: center ; color:#99CC00 ; font-size:10px ; text-align: center")) , icon = icon("search",lib = "glyphicon"), class = "btn-primary") , style="text-align: center;")),
                  fluidRow(column(12,actionButton("prediction_Update", label = "查詢" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFFF ; background-color: #6699CC ; font-size:16px ; font-weight:bold ") , style="text-align: center ")),
                  br(),
                  # filter condition
                  fluidRow(column(12,
                           bsCollapse(id = "collapseExample", open = "篩選條件",
                                      bsCollapsePanel(
                                                      title = "篩選條件",
                                                      # title = h4('篩選條件',style="text-align: center;color:#336699; font-weight:bold") ,
                                                      column(4,
                                                             selectInput("select_fb_charge", label = h4(paste0("","投放方式 篩選條件"),style="color:#2f51c4; font-weight:bold"),
                                                                         choices = condition_Campaign_Charge , selected = "不篩選" ,multiple = F)),
                                                      column(4,
                                                             selectInput("select_fb_Channel", label = h4(paste0("","系統 篩選條件"),style="color:#2f51c4; font-weight:bold"),
                                                                         choices = condition_Campaign_Channel , selected = "不篩選" ,multiple = F)),
                                                      column(4,
                                                             selectInput("select_fb_Location", label = h4(paste0("","國別 篩選條件"),style="color:#2f51c4; font-weight:bold"),
                                                                         choices = condition_TA_Location , selected = "不篩選" ,multiple = F)),
                                                      column(4,
                                                              selectInput("select_fb_install", label = h4(paste0("","FB 篩選條件"),style="color:#2f51c4; font-weight:bold"),
                                                                               choices = list("不篩選" = "All",
                                                                                              "FB安裝數大於等於10" = 10,
                                                                                              "FB安裝數大於等於50" = 50), selected = "All" ,multiple = F)),
                                                      column(4,
                                                             selectInput("select_af_install", label = h4(paste0("","AF 篩選條件"),style="color:#2f51c4; font-weight:bold"),
                                                                               choices = list("不篩選" = "All",
                                                                                              "AF安裝數大於等於10" = 10,
                                                                                              "AF安裝數大於等於50" = 50), selected = "All" ,multiple = F)),
                                                      column(4,
                                                             selectInput("select_ad_condition", label = h4(paste0("","廣告開關 篩選條件"),style="color:#2f51c4; font-weight:bold"),
                                                                         choices = list("不篩選" = "All",
                                                                                        "目前正在投放的廣告" = "ACTIVE",
                                                                                        "目前未在投放的廣告" = "PAUSED"), selected = "All" ,multiple = F))
                                                      , style = "info")
                                      )
                           )),
                  # mutiple sheets
                  mainPanel(width = 12,
                    tabsetPanel(
                                tabPanel(p(id="big-heading_ad", paste0(execute_Date,"-廣告預測樣本"),tags$style(HTML("#big-heading_ad{color: #336699;font-weight:bold;}")))
                                         , br()
                                         , shinyjs::useShinyjs()
                                         , tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                                                        Shiny.onInputChange(variableName, null);}); ")
                                         , withSpinner(DT::dataTableOutput("value_prediction_Table"), type = LoadingType , color = LoadingColor)
                                         , uiOutput("FB_ad_id_all_UI")
                                         # , bsModal(id = "modalExample"
                                         #         , title = h2(paste0(" ","廣告細項資料(",execute_Date,")："), style = "text-align: center;color:#993333 ; font-family: '標楷體' ; font-size:32px ; font-weight:bold" )
                                         #         , trigger = "select_button"
                                         #         , size = "large" # large or small 
                                         #         , column(12, withSpinner(DT::dataTableOutput(('FB_ad_id_all')), type = LoadingType , color = LoadingColor ))
                                         #            ) 
                                         , position = "left"),
                                tabPanel(p(id="big-heading_ad_0", paste0("","預測回本廣告"),tags$style(HTML("#big-heading_ad_0{color: #336699;font-weight:bold;}")))
                                         , br()
                                         , withSpinner(DT::dataTableOutput("value_prediction_Table_1"), type = LoadingType , color = LoadingColor ) 
                                         , uiOutput("FB_ad_id_1_UI")
                                         , position = "left"), 
                                
                                tabPanel(p(id="big-heading_ad_1", paste0("","預測不回本廣告"),tags$style(HTML("#big-heading_ad_1{color: #336699;font-weight:bold;}")))
                                         , br() 
                                         , withSpinner(DT::dataTableOutput("value_prediction_Table_0"), type = LoadingType , color = LoadingColor )
                                         , uiOutput("FB_ad_id_0_UI")
                                         , position = "left") 
                                ),
                    fluidRow(column(12,downloadButton("download_Excel", label = "DownloadExcel" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFFF ; background-color: #6699CC ; font-size:16px ; font-weight:bold "), style="text-align: left ")),
                    br(),br()
                  ),
                
          position = "right"),
            # ==== End ====
          
            # 二、Sheet(2)-廣告60日預測-資料精確度驗證  ==================================================
            # tabPanel(p(id="big-heading_1", "廣告60日預測-資料精確度驗證",tags$style(HTML("#big-heading_1{color: #000000;}"))),
            tabPanel(HTML("<i class='glyphicon glyphicon-eye-open' style='font-size:16px'></i> 廣告60日預測-資料準確度驗證"),value="ad_tab_accurancy",
                     tags$style(HTML(".nav > li > a[data-value='ad_tab_accurancy'] {background-color:#ffdbdb;color:#c42f2f;font-size:16px; font-weight:bold;}
                                     .navbar-default .navbar-nav > .active > a[data-value='ad_tab_accurancy'] {background:#c42f2f;color:#ffdbdb;font-size:16px; font-weight:bold;}
                                     .nav a:hover[data-value='ad_tab_accurancy'] {background-color: #f9fce8 !important;color:black;}"
                     )),
                     tags$style(HTML(".navbar.navbar-default.navbar-static-top{font-size:16px;background-color:#FFCC99;}")),
                     # 標題
                     div(p("廣告60日預測-準確度評估"),style="position: absolute; text-align: left ;font-size:32px ; padding: 0 0 0 40% ; color: #000000; font-weight:bold"),br(), br(), br(),
                     # mutiple sheets
                     mainPanel(width = 12,
                               # 實際資料驗證
                               tabsetPanel(
                                 tabPanel(HTML("<i class='glyphicon glyphicon-eye-close' style='font-size:16px'></i> 實際資料驗證"),value="trend",
                                          tags$style(HTML(".tabbable > .nav > li > a[data-value='trend'] {background-color:#CCFFFF; color:#006699; font-weight:bold}
                                                           .tabbable > .nav > li[class=active] > a[data-value='trend'] {background-color:#006699; color:#CCFFFF; font-weight:bold}"
                                          )),
                                          # title - 廣告60日預測-資料精確度驗證  (置中)
                                          div(h3('廣告60日預測-實際資料(準確度評估)'),style="position: absolute;left: 0;right: 0;text-align: center;color: #003366; font-weight:bold"),br(),
                                          br(),br(),
                                          div(h4(paste0("目前能夠驗證的數據日期：", End_Date," ","以前")),style="position: absolute;left: 0;right: 0;text-align: center;color: #3399CC; font-weight:bold"),br(),
                                          br(),
                                          div(h4(paste0("目前採用的演算法：","Neural Network")),style="position: absolute;left: 0;right: 0;text-align: center;color: #3399CC; font-weight:bold"),br(),
                                          fluidRow(column(2,dateInput("daily_dates_table", h4("請選擇 資料時間",style="color:#2f51c4; font-weight:bold"), value = End_Date ))),
                                          fluidRow(column(12,selectInput("select_model_verification", h4("挑選 模型區間",style="color:#2f51c4; font-weight:bold"),choices = list("建模範圍：2018/04/01 ~ 2019/03/31" = "2019-03-31",
                                                                                                                                                                           "建模範圍：2018/04/01 ~ 2019/04/30" = "2019-04-30"), selected = "2019-04-30"))),

                                          br(),
                                          fluidRow(column(12,actionButton("prediction_verification_Update", label = "查詢" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFFF ; background-color: #6699CC ; font-size:16px ; font-weight:bold "))),
                                          br(),
                                          fluidRow(column(12,withSpinner(DT::dataTableOutput("accurancy_verification_Table"), type = LoadingType , color = LoadingColor ))),
                                          fluidRow(column(12,downloadButton("download_Excel_verification", label = "DownloadExcel" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFFF ; background-color: #6699CC ; font-size:16px ; font-weight:bold "), style="text-align: left ")),
                                          br()
                               , position = "left"),
                               # 彙整資料驗證
                               tabPanel(HTML("<i class='glyphicon glyphicon-adjust' style='font-size:16px'></i> 彙整資料驗證"),value="ad_feature",
                                        tags$style(HTML(".tabbable > .nav > li > a[data-value='ad_feature'] {background-color:#CCFFFF; color:#006699; font-weight:bold}
                                                        .tabbable > .nav > li[class=active] > a[data-value='ad_feature'] {background-color:#006699; color:#CCFFFF; font-weight:bold}"
                                        )),
                                          # title - 廣告60日預測-資料精確度驗證  (置中)
                                          div(h3('廣告60日預測-彙整資料(準確度評估)'),style="position: absolute;left: 0;right: 0;text-align: center;color: #003300; font-weight:bold"),br(),br(),br(),
                                          fluidRow(column(12,dateRangeInput("daily_dates", h4("請選擇 資料時間",style="color:#336633; font-weight:bold"), start = Satrt_Date , end = End_Date))),
                                          fluidRow(column(12,selectInput("select_model", h4("挑選 模型區間",style="color:#336633; font-weight:bold"),choices = list("建模範圍：2018/04/01 ~ 2019/03/31" = "2019-03-31",
                                                                                                                                                                    "建模範圍：2018/04/01 ~ 2019/04/30" = "2019-04-30"), selected = "2019-04-30"))),
                                          br(),
                                          fluidRow(column(12,actionButton("prediction_boxplot_Update", label = "查詢" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFCC ; background-color: #99CC66 ; font-size:16px ; font-weight:bold "))),
                                          # Accurancy Indicator
                                          br(),
                                          fluidRow(column(12,selectInput("select_Indicator", h4("請選擇 穩定度指標",style="color:#336633"),choices = list("整體-準確率" = "整體準確率",
                                                                                                                                                          "回本-準確率" = "回本準確率",
                                                                                                                                                          "不回本-準確率" = "不回本準確率"), selected = "整體準確率"))),
                                          fluidRow(column(12,withSpinner(plotlyOutput("value_prediction_Boxplot_plot"), type = LoadingType , color = LoadingColor ))),
                                          br(),
                                          fluidRow(column(12,withSpinner(DT::dataTableOutput("value_accurancy_Table"), type = LoadingType , color = LoadingColor )))
                               , position = "left")
                               )
                               ),
             position = "right")

            # ==== End ====


            # # 二、Sheet(2)-廣告60日預測-資料精確度驗證  ==================================================
            # # tabPanel(p(id="big-heading_1", "廣告60日預測-資料精確度驗證",tags$style(HTML("#big-heading_1{color: #000000;}"))),
            # tabPanel(HTML("<i class='glyphicon glyphicon-eye-open' style='font-size:16px'></i> 廣告60日預測-資料準確度驗證"),value="ad_tab_accurancy",
            #          tags$style(HTML(".nav > li > a[data-value='ad_tab_accurancy'] {background-color:#ffdbdb;color:#c42f2f;font-size:16px; font-weight:bold;}
            #                          .navbar-default .navbar-nav > .active > a[data-value='ad_tab_accurancy'] {background:#c42f2f;color:#ffdbdb;font-size:16px; font-weight:bold;}
            #                          .nav a:hover[data-value='ad_tab_accurancy'] {background-color: #f9fce8 !important;color:black;}"
            #          )),
            #          tags$style(HTML(".navbar.navbar-default.navbar-static-top{font-size:16px;background-color:#FFCC99;}")),
            # 
            #      # 標題
            #      # fluidRow(column(12,h2('廣告60日預測-準確度評估'),style="text-align:center ; color:#000000 ; font-weight:bold")),br(),
            #      div(p("廣告60日預測-準確度評估"),style="position: absolute; text-align: left ;font-size:32px ; padding: 0 0 0 40% ; color: #000000; font-weight:bold"),br(), br(), br(),
            # 
            #      # 實際資料
            #      fluidRow(column(1),column(10,
            #          bsCollapse(id = "collapseExamle_1", open = "Panel_1", multiple = T ,
            #                     bsCollapsePanel(title = h4('實際資料',style="color:#336699; font-weight:bold") ,
            # 
            #                           # title - 廣告60日預測-資料精確度驗證  (置中)
            #                           div(h3('廣告60日預測-實際資料(準確度評估)'),style="position: absolute;left: 0;right: 0;text-align: center;color: #003366; font-weight:bold"),br(),
            #                           br(),br(),
            #                           div(h4(paste0("目前能夠驗證的數據日期：", End_Date," ","以前")),style="position: absolute;left: 0;right: 0;text-align: center;color: #3399CC; font-weight:bold"),br(),
            #                           br(),
            #                           div(h4(paste0("目前採用的演算法：","Neural Network")),style="position: absolute;left: 0;right: 0;text-align: center;color: #3399CC; font-weight:bold"),br(),
            #                           fluidRow(column(12,dateRangeInput("daily_dates_table", h4("請選擇 資料時間",style="color:#2f51c4; font-weight:bold"), start = End_Date , end = End_Date , separator = " - "))),
            #                           fluidRow(column(12,selectInput("select_model_verification", h4("挑選 模型區間",style="color:#2f51c4; font-weight:bold"),choices = list("建模範圍：2018/04/01 ~ 2019/03/31" = "2019-03-31",
            #                                                                                                                                                                  "建模範圍：2018/04/01 ~ 2019/04/30" = "2019-04-30"), selected = "2019-04-30"))),
            # 
            #                           br(),
            #                           fluidRow(column(12,actionButton("prediction_verification_Update", label = "查詢" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFFF ; background-color: #6699CC ; font-size:16px ; font-weight:bold "))),
            #                           br(),
            #                           fluidRow(column(12,withSpinner(DT::dataTableOutput("accurancy_verification_Table"), type = LoadingType , color = LoadingColor ))),
            #                           fluidRow(column(12,downloadButton("download_Excel_verification", label = "DownloadExcel" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFFF ; background-color: #6699CC ; font-size:16px ; font-weight:bold "), style="text-align: left ")),
            #                           br(),
            #                           style = "info")
            #                     )
            #      ), column(1)),
            #      # 彙整資料
            #      fluidRow(column(1),column(10,
            #         bsCollapse(id = "collapseExamle", open = "Panel", multiple = T ,
            #                    bsCollapsePanel(title = h4('彙整資料',style="color:#336600 ; font-weight:bold") ,
            # 
            #                           # title - 廣告60日預測-資料精確度驗證  (置中)
            #                           div(h3('廣告60日預測-彙整資料(準確度評估)'),style="position: absolute;left: 0;right: 0;text-align: center;color: #003300; font-weight:bold"),br(),br(),br(),
            #                           fluidRow(column(12,dateRangeInput("daily_dates", h4("請選擇 資料時間",style="color:#336633; font-weight:bold"), start = Satrt_Date , end = End_Date))),
            #                           fluidRow(column(12,selectInput("select_model", h4("挑選 模型區間",style="color:#336633; font-weight:bold"),choices = list("建模範圍：2018/04/01 ~ 2019/03/31" = "2019-03-31",
            #                                                                                                                                                     "建模範圍：2018/04/01 ~ 2019/04/30" = "2019-04-30"), selected = "2019-04-30"))),
            #                           br(),
            #                           fluidRow(column(12,actionButton("prediction_boxplot_Update", label = "查詢" , icon = icon("search",lib = "glyphicon"), style="color:#FFFFCC ; background-color: #99CC66 ; font-size:16px ; font-weight:bold "))),
            #                           # Accurancy Indicator
            #                           # fluidRow(column(12,h3(id="big-heading", "準確率指標(穩定度) ",tags$style(HTML("#big-heading{color: red;}"))))),
            #                           br(),
            #                           fluidRow(column(12,selectInput("select_Indicator", h4("請選擇 穩定度指標",style="color:#336633"),choices = list("整體-準確率" = "整體準確率",
            #                                                                                                                                           "回本-準確率" = "回本準確率",
            #                                                                                                                                           "不回本-準確率" = "不回本準確率"), selected = "整體準確率"))),
            #                           fluidRow(column(12,withSpinner(plotlyOutput("value_prediction_Boxplot_plot"), type = LoadingType , color = LoadingColor ))),
            #                           br(),
            #                           fluidRow(column(12,withSpinner(DT::dataTableOutput("value_accurancy_Table"), type = LoadingType , color = LoadingColor ))),
            #                    style = "success")
            #                    )
            #       ), column(1)),
            # 
            # position = "left")
            # 測試資料格式 
            # , mainPanel(
            #   textOutput("selected_var")
            # )
          
            # ==== End ====
  ))





