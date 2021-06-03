
#### library ####
library('tidyverse')
library('scales')


#########################################################
#### Function version ( EFA.CFA 7 fcts and 42 items) ####
#########################################################

f_score_US_efacfa_7fct_42items = function(data.arg){
  
  #### Note for debugging ####
  # Assign your dataset with id and item to "DF" 
  
  #### Step 0.1 Prepare the data ####
  DF = data.arg # note: DF is thje input data
  
  item = c(
    'core_overwhelm_difficult'
    ,'core_hopeless'
    ,'core_sad'
    ,'core_worried'
    ,'core_frustrated'
    ,'core_drained'
    ,'core_important_energy'
    ,'core_meet_expectations'
    ,'core_people_upset'
    ,'core_important_time'
    ,'core_drained_helping'
    ,'core_deal_whatever'
    ,'core_overcome_obstacles'
    ,'core_bounce_back'
    ,'core_strong_person'
    ,'core_adapt_change'
    ,'core_disheartened_setbacks'
    ,'core_unpleasant_feelings'
    ,'core_focused_pressure'
    ,'core_humorous_side'
    ,'core_people_talk'
    ,'core_people_rely'
    ,'core_people_close'
    ,'core_group_friends'
    ,'core_isolated_others'
    ,'core_true_person'
    ,'core_left_out'
    ,'core_tune_people'
    ,'core_lack_companionship'
    ,'core_help'
    ,'core_contribute_alive'
    ,'core_contribute_doing'
    ,'core_engage_oppo'
    ,'core_health_selfreported'
    ,'core_fitness_level'
    ,'core_physical_illness'
    ,'core_happy'
    ,'core_joyful'
    ,'core_excited'
    ,'core_satisfied_yourself'
    ,'core_accepting_yourself'
    ,'core_capable'
  )
  
  # note: core_money_needs and core_fitness_level has 6 responses
  k_response = c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,5,5,5,5,5,5,5)
  
  
  fct = c(
    'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F1'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F4'
    ,'F4'
    ,'F4'
    ,'F5'
    ,'F5'
    ,'F5'
    ,'F6'
    ,'F6'
    ,'F6'
    ,'F7'
    ,'F7'
    ,'F7'
    
  )
  

  
  # DF_setting is like a dictionary providing the item-factor relationship.  
  DF_setting = data.frame(Item=item,
                          K_response= k_response,
                          Factor = fct,
                          stringsAsFactors = F)
  
  #### Step 0.2 Select id and items from input data ####
  # df is the internal dataframe for score calculation
  df = DF[, c('id', DF_setting$Item)]
  
  #### Step 0.3 Reverse coding ####
  df$core_overwhelm_difficult    = 6 - DF$core_overwhelm_difficult
  df$core_hopeless               = 6 - DF$core_hopeless
  df$core_sad                    = 6 - DF$core_sad
  df$core_worried                = 6 - DF$core_worried
  df$core_frustrated             = 6 - DF$core_frustrated
  df$core_drained                = 6 - DF$core_drained
  df$core_important_energy       = 6 - DF$core_important_energy
  df$core_meet_expectations      = 6 - DF$core_meet_expectations
  df$core_people_upset           = 6 - DF$core_people_upset
  df$core_important_time         = 6 - DF$core_important_time
  df$core_drained_helping        = 6 - DF$core_drained_helping
  df$core_isolated_others        = 6 - DF$core_isolated_others
  df$core_left_out               = 6 - DF$core_left_out
  df$core_lack_companionship     = 6 - DF$core_lack_companionship
  
  
  
  #############################
  #### Step 1 Re-code item ####
  #############################
  # Assign a proportional score to the participant's response for each item ####
  # Each response will proportionally get the score 0-100
  # For example, a item with 1-5 responses, its '1' gets 0 score, '2' gets 25 score, ... , and '5' gets 100 score.
  df_recoded_item = data.frame(id = df$id) # Place holder to store recoded items
  
  for (i in 1:nrow(DF_setting) ){
    item_i = DF_setting$Item[i]    # item_i is one of the 19 items
    item_i_raw_resp = df[[item_i]] # df provides the raw responses for each item 
    k = DF_setting$K_response[i]   # k is the scale of raw responses (e.g., 1-5 or 1-6)
    df_recoded_item[[item_i]]  = (item_i_raw_resp-1) * (  100/ (k-1)  ) # 100 is the max score to be assigned to the highest responses
  }
  
  #############################
  #### Step 2 Factor score ####
  #############################
  # Factor score = the average of the recoded items in that factor
  df_fct_score = data.frame(id = df$id) # Place holder
  Factor_vect = unique(DF_setting$Factor)
  
  for (f in 1:length(Factor_vect) ){
    # Select the factor. fct_f = the f-th factor
    fct_f = Factor_vect[f]
    # Select the items in the f-th factor
    item_in_fct = DF_setting %>% 
      filter(Factor == fct_f) %>% 
      dplyr::select(Item) %>% 
      unlist()
    # The f-th factor score is the rowwise average among the items in the f-factor.
    df_fct_score[[fct_f]] = df_recoded_item %>%  # The key is to use the recoded item to calculate the factor score
      dplyr::select( all_of (item_in_fct) ) %>%
      rowMeans(na.rm = FALSE)
  }
  
  ###########################
  #### Step 3 WELL score ####
  ###########################
  df_fct_score[['well_score']] = df_fct_score %>% 
    dplyr::select(-id) %>%
    rowMeans(na.rm = FALSE)
  
  ################
  #### Output ####
  ################
  print('This is the US-EFA-CFA model with 7 factors and 42 itmes')
  print(DF_setting)
  return(df_fct_score)
}


#
#
#


########################################################
#### Function version (EFA.CFA 7 fcts and 21 items) ####
########################################################


f_score_US_efacfa_7fct_21items = function(data.arg){
  
  #### Note for debugging ####
  # Assign your dataset with id and item to "DF" 
  
  #### Step 0.1 Prepare the data ####
  DF = data.arg # note: DF is thje input data
  
  item = c(
    'core_overwhelm_difficult'
    ,'core_hopeless'
    ,'core_sad'
    ,'core_deal_whatever'
    ,'core_overcome_obstacles'
    ,'core_bounce_back'
    ,'core_people_talk'
    ,'core_people_rely'
    ,'core_people_close'
    ,'core_contribute_alive'
    ,'core_contribute_doing'
    ,'core_engage_oppo'
    ,'core_health_selfreported'
    ,'core_fitness_level'
    ,'core_physical_illness'
    ,'core_happy'
    ,'core_joyful'
    ,'core_excited'
    ,'core_satisfied_yourself'
    ,'core_accepting_yourself'
    ,'core_capable'
    
  )
  
  # note: core_money_needs and core_fitness_level has 6 responses
  k_response = c(5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,6
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5)
  
  fct = c(
    'F1'
    ,'F1'
    ,'F1'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F4'
    ,'F4'
    ,'F4'
    ,'F5'
    ,'F5'
    ,'F5'
    ,'F6'
    ,'F6'
    ,'F6'
    ,'F7'
    ,'F7'
    ,'F7')
  
  
  
  # DF_setting is like a dictionary providing the item-factor relationship.  
  DF_setting = data.frame(Item=item,
                          K_response= k_response,
                          Factor = fct,
                          stringsAsFactors = F)
  
  #### Step 0.2 Select id and items from input data ####
  # df is the internal dataframe for score calculation
  df = DF[, c('id', DF_setting$Item)]
  
  #### Step 0.3 Reverse coding ####
  df$core_overwhelm_difficult    = 6 - DF$core_overwhelm_difficult
  df$core_hopeless               = 6 - DF$core_hopeless
  df$core_sad                    = 6 - DF$core_sad
  
  
  #############################
  #### Step 1 Re-code item ####
  #############################
  # Assign a proportional score to the participant's response for each item ####
  # Each response will proportionally get the score 0-100
  # For example, a item with 1-5 responses, its '1' gets 0 score, '2' gets 25 score, ... , and '5' gets 100 score.
  df_recoded_item = data.frame(id = df$id) # Place holder to store recoded items
  
  for (i in 1:nrow(DF_setting) ){
    item_i = DF_setting$Item[i]    # item_i is one of the 19 items
    item_i_raw_resp = df[[item_i]] # df provides the raw responses for each item 
    k = DF_setting$K_response[i]   # k is the scale of raw responses (e.g., 1-5 or 1-6)
    df_recoded_item[[item_i]]  = (item_i_raw_resp-1) * (  100/ (k-1)  ) # 100 is the max score to be assigned to the highest responses
  }
  
  #############################
  #### Step 2 Factor score ####
  #############################
  # Factor score = the average of the recoded items in that factor
  df_fct_score = data.frame(id = df$id) # Place holder
  Factor_vect = unique(DF_setting$Factor)
  
  for (f in 1:length(Factor_vect) ){
    # Select the factor. fct_f = the f-th factor
    fct_f = Factor_vect[f]
    # Select the items in the f-th factor
    item_in_fct = DF_setting %>% 
      filter(Factor == fct_f) %>% 
      dplyr::select(Item) %>% 
      unlist()
    # The f-th factor score is the rowwise average among the items in the f-factor.
    df_fct_score[[fct_f]] = df_recoded_item %>%  # The key is to use the recoded item to calculate the factor score
      dplyr::select( all_of (item_in_fct) ) %>%
      rowMeans(na.rm = FALSE)
  }
  
  ###########################
  #### Step 3 WELL score ####
  ###########################
  df_fct_score[['well_score']] = df_fct_score %>% 
    dplyr::select(-id) %>%
    rowMeans(na.rm = FALSE)
  
  ################
  #### Output ####
  ################
  print('This is the US-EFA-CFA model with 7 factors and 21 itmes')
  print(DF_setting)
  return(df_fct_score)
}



#
#
#

#######################################################
#### Function version (EFA.CFA 7 fcts and 7 items) ####
#######################################################

f_score_US_efacfa_7fct_7items = function(data.arg){
  
  #### Note for debugging ####
  # Assign your dataset with id and item to "DF" 
  
  #### Step 0.1 Prepare the data ####
  DF = data.arg # note: DF is thje input data
  
  item = c(
    'core_overwhelm_difficult'
    ,'core_hopeless'
    ,'core_sad'
    ,'core_deal_whatever'
    ,'core_overcome_obstacles'
    ,'core_bounce_back'
    ,'core_people_talk'
    ,'core_people_rely'
    ,'core_people_close'
    ,'core_contribute_alive'
    ,'core_contribute_doing'
    ,'core_engage_oppo'
    ,'core_health_selfreported'
    ,'core_fitness_level'
    ,'core_physical_illness'
    ,'core_happy'
    ,'core_joyful'
    ,'core_excited'
    ,'core_satisfied_yourself'
    ,'core_accepting_yourself'
    ,'core_capable'
    
  )
  
  # note: core_money_needs and core_fitness_level has 6 responses
  k_response = c(5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,6
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5
                 ,5)
  
  fct = c(
    'F1'
    ,'F1'
    ,'F1'
    ,'F2'
    ,'F2'
    ,'F2'
    ,'F3'
    ,'F3'
    ,'F3'
    ,'F4'
    ,'F4'
    ,'F4'
    ,'F5'
    ,'F5'
    ,'F5'
    ,'F6'
    ,'F6'
    ,'F6'
    ,'F7'
    ,'F7'
    ,'F7')
  
  
  
  # DF_setting is like a dictionary providing the item-factor relationship.  
  DF_setting = data.frame(Item=item,
                          K_response= k_response,
                          Factor = fct,
                          stringsAsFactors = F)
  
  #### Step 0.2 Select id and items from input data ####
  # df is the internal dataframe for score calculation
  df = DF[, c('id', DF_setting$Item)]
  
  #### Step 0.3 Reverse coding ####
  df$core_overwhelm_difficult    = 6 - DF$core_overwhelm_difficult

  
  #############################
  #### Step 1 Re-code item ####
  #############################
  # Assign a proportional score to the participant's response for each item ####
  # Each response will proportionally get the score 0-100
  # For example, a item with 1-5 responses, its '1' gets 0 score, '2' gets 25 score, ... , and '5' gets 100 score.
  df_recoded_item = data.frame(id = df$id) # Place holder to store recoded items
  
  for (i in 1:nrow(DF_setting) ){
    item_i = DF_setting$Item[i]    # item_i is one of the 19 items
    item_i_raw_resp = df[[item_i]] # df provides the raw responses for each item 
    k = DF_setting$K_response[i]   # k is the scale of raw responses (e.g., 1-5 or 1-6)
    df_recoded_item[[item_i]]  = (item_i_raw_resp-1) * (  100/ (k-1)  ) # 100 is the max score to be assigned to the highest responses
  }
  
  #############################
  #### Step 2 Factor score ####
  #############################
  # Factor score = the average of the recoded items in that factor
  df_fct_score = data.frame(id = df$id) # Place holder
  Factor_vect = unique(DF_setting$Factor)
  
  for (f in 1:length(Factor_vect) ){
    # Select the factor. fct_f = the f-th factor
    fct_f = Factor_vect[f]
    # Select the items in the f-th factor
    item_in_fct = DF_setting %>% 
      filter(Factor == fct_f) %>% 
      dplyr::select(Item) %>% 
      unlist()
    # The f-th factor score is the rowwise average among the items in the f-factor.
    df_fct_score[[fct_f]] = df_recoded_item %>%  # The key is to use the recoded item to calculate the factor score
      dplyr::select( all_of (item_in_fct) ) %>%
      rowMeans(na.rm = FALSE)
  }
  
  ###########################
  #### Step 3 WELL score ####
  ###########################
  df_fct_score[['well_score']] = df_fct_score %>% 
    dplyr::select(-id) %>%
    rowMeans(na.rm = FALSE)
  
  ################
  #### Output ####
  ################
  print('This is the US-EFA-CFA model with 7 factors and 7 itmes')
  print(DF_setting)
  return(df_fct_score)
}

