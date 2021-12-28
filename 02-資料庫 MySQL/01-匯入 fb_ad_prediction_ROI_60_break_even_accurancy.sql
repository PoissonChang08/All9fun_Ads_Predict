


CREATE TABLE `fb_ad_prediction_ROI_60_break_even_accurancy` (
  `Model` varchar(10) NOT NULL COMMENT '資料流水號',
  `model_end_date` date NOT NULL COMMENT '模型的時間',  
  `data_date` date NOT NULL COMMENT '資料寫入時間',
  `accurancy` float(10,4) DEFAULT NULL COMMENT 'accurancy',
  `sensitivity` float(10,4) DEFAULT NULL COMMENT 'sensitivity',
  `specificity` float(10,4) DEFAULT NULL COMMENT 'specificity',
  `precision_01` float(10,4) DEFAULT NULL COMMENT 'precision',
  `real_1_pred_1` int(10) DEFAULT NULL,
  `real_0_pred_0` int(10) DEFAULT NULL,
  `real_1_pred_0` int(10) DEFAULT NULL,
  `real_0_pred_1` int(10) DEFAULT NULL,
  `N` int(10) DEFAULT NULL,
  PRIMARY KEY (`Model`,`model_end_date`,`data_date`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='FB廣告預測數據';


-- 新增欄位  
USE analysis;
ALTER TABLE `fb_ad_prediction_ROI_60_break_even_accurancy`
ADD  `fb_campaign_id` varchar(50) DEFAULT NULL COMMENT '行銷活動id(函非廣告工具的ads)';



SELECT * FROM analysis.fb_ad_prediction_ROI_60_break_even_accurancy;





