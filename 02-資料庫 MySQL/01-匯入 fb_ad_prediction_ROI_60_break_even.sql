USE analysis;
CREATE TABLE `fb_ad_prediction_ROI_60_break_even` (
  -- `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '流水號',
  `ad_pred_id` int(11) NOT NULL COMMENT '資料流水號',
  `status` varchar(50) NOT NULL,
  `group_id` varchar(50) NOT NULL,
  `model_end_date` date NOT NULL COMMENT '模型的時間',  
  `data_date` date NOT NULL COMMENT '資料寫入時間',
  `one_month_data_begin_date` date NOT NULL COMMENT '資料計算起始日(追朔至資料起日前一個月計算數據)',
  `data_begin_date` date NOT NULL COMMENT '原始-資料起始日',
  `data_end_date`  date NOT NULL COMMENT '資料迄日',
  
  `first_ad_date` date NOT NULL COMMENT '最早廣告投放時間',
  `last_ad_date` date NOT NULL COMMENT '最後廣告投放時間',
--  `ad_using` date NOT NULL COMMENT '廣告是否還投放中',

  `first_ad_click_date` date NOT NULL COMMENT 'AF最早點擊廣告投放時間',
  `last_ad_click_date`  date NOT NULL COMMENT  'AF最後點擊廣告投放時間',
  
  `fileName` varchar(255) DEFAULT NULL COMMENT '影片、圖片，檔案名稱',
  `file_path` varchar(255) DEFAULT NULL COMMENT '影片、圖片，檔案路徑', `file_path_org` varchar(255) DEFAULT NULL COMMENT '影片、圖片，檔案路徑(原始)',
  `Ta_name` varchar(225) DEFAULT NULL COMMENT 'TA組合，含計費方式、系統', `TA_name_org` varchar(225) DEFAULT NULL COMMENT 'TA組合，含計費方式、系統',
  `Campaign_Charge`   VARCHAR(64)  NULL COMMENT '計費方式',
  `Campaign_Channel`   VARCHAR(64)  NULL COMMENT '系統',
  `TA_Location`   VARCHAR(500) NULL COMMENT '國別',
  `TA_Gender`   VARCHAR(64) NULL COMMENT '性別',
  `TA_Age`   VARCHAR(500)   NULL COMMENT '年齡', `TA_Age_org`   VARCHAR(500)   NULL COMMENT '年齡(原始)',

  `FB_impressions` int(10) DEFAULT NULL,
  `FB_reach` int(10) DEFAULT NULL,
  `FB_clicks` int(10) DEFAULT NULL,
  `FB_install` int(10) DEFAULT NULL,
  `FB_spend` float(10,2) DEFAULT NULL COMMENT '廣告費用(美金)',
  
  `AF_install` int(10) DEFAULT NULL,
  `ROI_2` float(10,4) DEFAULT NULL COMMENT '廣告2日ROI',
  `ROI_7` float(10,4) DEFAULT NULL COMMENT '廣告7日ROI',
  `ROI_14` float(10,4) DEFAULT NULL COMMENT '廣告14日ROI',
  `ROI_60` float(10,4) DEFAULT NULL COMMENT '廣告60日ROI',
  
  `RetentionRate_2` float(10,4) DEFAULT NULL COMMENT '廣告2日留存',
  `RetentionRate_7` float(10,4) DEFAULT NULL COMMENT '廣告7日留存',
  `RetentionRate_14` float(10,4) DEFAULT NULL COMMENT '廣告14日留存',
  `RetentionRate_60` float(10,4) DEFAULT NULL COMMENT '廣告30日留存',
  
  `LTV_2` float(10,4) DEFAULT NULL COMMENT '廣告2日LTV',
  `LTV_7` float(10,4) DEFAULT NULL COMMENT '廣告7日LTV',
  `LTV_14` float(10,4) DEFAULT NULL COMMENT '廣告14日LTV',
  `LTV_60` float(10,4) DEFAULT NULL COMMENT '廣告60日LTV',

  `DateDay` int(10) DEFAULT NULL,
  `Ratio_7_1` float(10,2) DEFAULT NULL COMMENT '廣告1-7天LTV成長幅度',
  `Ratio_14_7` float(10,2) DEFAULT NULL COMMENT '廣告7-14天LTV成長幅度',
  `df7_1` float(10,2) DEFAULT NULL COMMENT '廣告1-7天LTV差距',
  `df14_7` float(10,2) DEFAULT NULL COMMENT '廣告7-14天LTV差距',

  `Pred_ROI_60_break_even` int(1) DEFAULT NULL,
  `lr` int(1) DEFAULT NULL,
  `nn` int(1) DEFAULT NULL,
  `tree` int(1) DEFAULT NULL,
  `rf` int(1) DEFAULT NULL,
  `svm` int(1) DEFAULT NULL,

  PRIMARY KEY (`ad_pred_id`,`group_id`,`model_end_date`,`data_date`)
  -- UNIQUE KEY `uni_key` (`fileName`,`TA_name`,`data_date`) 

) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='FB廣告預測數據';



-- 新增欄位  
USE analysis;
ALTER TABLE `fb_ad_prediction_ROI_60_break_even`
ADD  `fb_campaign_id` varchar(50) DEFAULT NULL COMMENT '行銷活動id(函非廣告工具的ads)';


-- fb
SELECT fileName , CONCAT(fileHost, filePath, fileSavedName) AS file_path , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name) AS Ta_Name
	 , min(`date`) AS first_ad_fb_date
     , max(`date`) AS last_ad_fb_date
FROM analysis.mobile_game_fb_daily_tag  
WHERE 1=1
AND  `date` BETWEEN '2018-04-01' AND '2019-07-29' 
AND  Campaign_MobileGame = '將膽'
GROUP BY fileName , CONCAT(fileHost, filePath, fileSavedName) , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name);


-- af
SELECT fileName , CONCAT(fileHost, filePath, fileSavedName) AS file_path , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name) AS Ta_Name
	 , min(click_date) AS first_ad_click_date
     , max(click_date) AS last_ad_click_date
FROM analysis.fb_ta_cohort_tag
WHERE `click_date` BETWEEN '2018-04-01' AND '2019-07-29' 
AND  Campaign_MobileGame = '將膽'
GROUP BY fileName , CONCAT(fileHost, filePath, fileSavedName) , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name);


SELECT fileName , CONCAT(fileHost, filePath, fileSavedName) AS file_path , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name) AS Ta_Name
                              	   , min(click_date) AS first_ad_click_date
                                   , max(click_date) AS last_ad_click_date
                              FROM analysis.fb_ta_cohort_tag
                              WHERE `click_date` BETWEEN '2018-04-01' AND '2019-07-29' 
                              AND  Campaign_MobileGame = '將膽'
                              GROUP BY fileName , CONCAT(fileHost, filePath, fileSavedName) , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name);






SELECT fileName , CONCAT(fileHost, filePath, fileSavedName) AS file_path , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name) AS Ta_Name
	 , click_date
     , COUNT(1)
FROM analysis.fb_ta_cohort_tag
WHERE `click_date` BETWEEN '2018-04-01' AND '2019-07-29' 
AND  Campaign_MobileGame = '將膽'
GROUP BY fileName , CONCAT(fileHost, filePath, fileSavedName) , CONCAT(Campaign_Charge,'_', Campaign_Channel ,'_' , TA_name) , click_date
HAVING fileName = 'SLG_無雙測試素材_美芳.mp4' AND file_path = 'http://210.242.121.185:81/fb_ad/40d199892073ca2a996fe4e6ed11aa19.mp4' AND Ta_Name = 'CPA_Android_將膽_台_開啟5%_18-54_男@';










