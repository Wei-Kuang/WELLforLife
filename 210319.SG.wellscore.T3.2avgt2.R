#### library ####

library('tidyverse')
library('scales')

dataset_name = 'WELLSingaporeData_DATA_2021-02-19_1308.csv'  #9338

setwd("/Users/foxeshen/Job/CFA-Singapore/CFA-Singapore-data")
DF = read.csv(dataset_name, stringsAsFactors = FALSE)




#### Singapore Well Score  ####

f_score_SG_T3.2avgt2 = function(data.arg){
  
  #### Prepare the data ####
  DF = data.arg # note: DF is thje input data
  
  # note: 11 factors 19 items
  item = c(
    'core_hopeless',
    'core_sad',
    
    'core_happy',
    'core_joyful',
    
    'core_engage_oppo',
    
    'core_money_needs',
    
    'core_fitness_level',
    'core_health_selfreported',
    
    'core_contribute_alive',
    'core_contribute_doing',
    
    'core_capable',
    'core_satisfied_yourself',
    
    'core_people_close',
    'core_people_talk',
    
    'core_religious_beliefs',
    
    'core_deal_whatever',
    'core_overcome_obstacles',
    
    'core_important_energy',
    'core_overwhelm_difficult'
  )
  
  # note: core_money_needs and core_fitness_level has 6 responses
  k_response = c(5,5, 5,5, 5, 6, 6,5, 5,5, 5,5, 5,5, 5, 5,5, 5,5)
  
  fct = c(
    'CFA.SG.F1.negemo',
    'CFA.SG.F1.negemo',
    
    'CFA.SG.F2.posemo',
    'CFA.SG.F2.posemo',
    
    'CFA.SG.F3.expl',
    
    'CFA.SG.F4.fina',
    
    'CFA.SG.F5.phyhlt',
    'CFA.SG.F5.phyhlt',
    
    'CFA.SG.F6.purp',
    'CFA.SG.F6.purp',
    
    'CFA.SG.F7.self',
    'CFA.SG.F7.self',
    
    'CFA.SG.F8.social',
    'CFA.SG.F8.social',
    
    'CFA.SG.F9.relig',
    
    'CFA.SG.F10.resili',
    'CFA.SG.F10.resili',
    
    'CFA.SG.F11.stress',
    'CFA.SG.F11.stress'
  )
  
  # DF_setting is like a dictionary providing the item-factor relationship.  
  DF_setting = data.frame(Factor = fct,
                          Item=item,
                          K_response= k_response,
                          stringsAsFactors = F)
  
  #### Select id and items from input data ####
  # note: df is the internal dataframe for score calculation
  df = DF[, c('record_id',DF_setting$Item)]
  names(df)[names(df) == 'record_id'] <- 'id'
  
  ####  Reverse coding includes core_hopeless, core_sad, core_important_energy, core_overwhelm_difficult  ####
  df$core_hopeless            = 6 - DF$core_hopeless 
  df$core_sad                 = 6 - DF$core_sad
  df$core_important_energy    = 6 - DF$core_important_energy 
  df$core_overwhelm_difficult = 6 - DF$core_overwhelm_difficult 
  
  ####  Recode item - Assign a proportional score to the participant's response for each item ####
  # Each response will proportionally get the score 0-100
  df_recoded_item = data.frame(id = df$id) # Place holder
  
  for (i in 1:nrow(DF_setting) ){
    item_i = DF_setting$Item[i] # item_i is one of the 19 items
    item_i_raw_resp = df[[item_i]] # df provides the raw responses for each item 
    k = DF_setting$K_response[i]
    df_recoded_item[[item_i]]  = (item_i_raw_resp-1) * (  100/ (k-1)  ) # 100 is the max score to be assigned to the highest responses
  }
  
  #### Factor score ####
  # Factor score = the average of the recoded items in that factor
  df_fct_score = data.frame(id = df$id) # Place holder
  Factor_vect = unique(DF_setting$Factor)
  
  for (f in 1:length(Factor_vect) ){
    # fct_f: the f-th factor
    fct_f = Factor_vect[f]
    # item_in_fct: items in the f-th factor
    item_in_fct = DF_setting %>% 
      filter(Factor == fct_f) %>% 
      dplyr::select(Item) %>% 
      unlist()
    # factor scores
    df_fct_score[[fct_f]] = df_recoded_item %>%  # The key is to use the recoded item to calculate the factor score
      dplyr::select( all_of (item_in_fct) ) %>%
      rowMeans(na.rm = FALSE)
  }
  
  #### WELL score ####
  df_fct_score[['CFA.SG.well_score']] = df_fct_score %>% 
    dplyr::select(-id) %>%
    rowMeans(na.rm = FALSE)
  #### Output ####
  print('This is Singapore T3.2avgt2 for well score calculation')
  print(DF_setting)
  return(df_fct_score)
  
}

#### check ####
f_score_SG_T3.2avgt2(DF)

setwd("/Users/foxeshen/Job/WELL Score Analysis/WELL Score - Singapore/WELL Score.SG.Data")
saveRDS(f_score_SG_T3.2avgt2(DF), "SG.wellscore.T3.2avgt2.rds")
