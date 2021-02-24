
#### library ####
library('tidyverse')
library('scales')
library('hash')

#### Prepare dictionary for K responses ####
item_name = c(
  'core_drained',
  'core_frustrated',
  'core_hopeless',
  'core_sad',
  'core_worried',
  'core_calm',
  'core_content',
  'core_excited',
  'core_happy',
  'core_joyful',
  'core_secure',
  'core_engage_oppo',
  'core_money_needs',
  'core_energy_level',
  'core_fitness_level',
  'core_health_selfreported',
  'core_interfere_life',
  'core_physical_illness',
  'core_contribute_alive',
  'core_contribute_doing',
  'core_accepting_yourself',
  'core_capable',
  'core_daily_activities',
  'core_satisfied_yourself',
  'core_true_person',
  'core_drained_helping',
  'core_energized_help',
  'core_group_friends',
  'core_help',
  'core_isolated_others',
  'core_lack_companionship',
  'core_left_out',
  'core_meet_expectations',
  'core_people_close',
  'core_people_rely',
  'core_people_talk',
  'core_people_upset',
  'core_tune_people',
  'core_religious_beliefs',
  'core_adapt_change',
  'core_bounce_back',
  'core_deal_whatever',
  'core_disheartened_setbacks',
  'core_focused_pressure',
  'core_humorous_side',
  'core_overcome_obstacles',
  'core_strong_person',
  'core_unpleasant_feelings',
  'core_confident_psnlproblem',
  'core_going_way',
  'core_important_energy',
  'core_important_time',
  'core_overwhelm_difficult'
)


# note: core_money_needs and core_fitness_level has 6 responses
k_response_value = c(5,5,5,5,5,5,5,5,5,5,5,5,6,5,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
h = hash( keys= item_name, values=  k_response_value)
values(h, item_name) %>% as.integer()


##############################
#### Function Development ####
##############################

f_score_well_general = function(data.arg, item.arg, fct.arg, hash.arg = h){
  
  #### Note for debugging ####
  # Assign your dataset with id and item to "DF" 
  
  #### Step 0.1 Prepare the data ####
  DF = data.arg # note: DF is thje input data
  
  #### Reverse coding
  DF$core_drained             = 6 - DF$core_drained 
  DF$core_frustrated          = 6 - DF$core_frustrated 
  DF$core_hopeless            = 6 - DF$core_hopeless 
  DF$core_sad                 = 6 - DF$core_sad 
  DF$core_worried             = 6 - DF$core_worried 
  DF$core_interfere_life      = 6 - DF$core_interfere_life 
  DF$core_drained_helping     = 6 - DF$core_drained_helping 
  DF$core_isolated_others     = 6 - DF$core_isolated_others 
  DF$core_lack_companionship  = 6 - DF$core_lack_companionship 
  DF$core_left_out            = 6 - DF$core_left_out 
  DF$core_meet_expectations   = 6 - DF$core_meet_expectations 
  DF$core_people_upset        = 6 - DF$core_people_upset 
  DF$core_important_energy    = 6 - DF$core_important_energy 
  DF$core_important_time      = 6 - DF$core_important_time 
  DF$core_overwhelm_difficult = 6 - DF$core_overwhelm_difficult 
  
  
  #### Model setting
  item = item.arg
  fct = fct.arg
  if( typeof(fct) == 'character'){
    print("good: factor is character")
  }else{
    stop ("error: factor needs to be character")
  }
  
  h = hash.arg
  k_response = values(h, keys = item) # Use [item name -> k resp] 
  
  
  # DF_setting is like a dictionary providing the item-factor relationship.  
  DF_setting = data.frame(Item=item,
                          K_response= k_response,
                          Factor = fct,
                          stringsAsFactors = F)
  
  #### Step 0.2 Select id and items from input data ####
  # df is the internal dataframe for score calculation
  df = DF[, c('id', DF_setting$Item)]
  
  #### Step 1 Recode item - Assign a proportional score to the participant's response for each item ####
  # Each response will proportionally get the score 0-100
  # For example, a item with 1-5 responses, its '1' gets 0 score, '2' gets 20 score, ... , and '5' gets 100 score.
  df_recoded_item = data.frame(id = df$id) # Place holder
  
  for (i in 1:nrow(DF_setting) ){  # i is the index for item
    item_i = DF_setting$Item[i]    # item_i is one of the 19 items
    item_i_raw_resp = df[[item_i]] # df provides the raw responses for each item 
    k = DF_setting$K_response[i]
    df_recoded_item[[item_i]]  = (item_i_raw_resp-1) * (  100/ (k-1)  ) # 100 is the max score to be assigned to the highest responses
  }
  
  
  #### Step 2 Factor score ####
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
  #### Step 3 WELL score ####
  df_fct_score[['well_score']] = df_fct_score %>% 
    dplyr::select(-id) %>%
    rowMeans(na.rm = FALSE)
  #### Output ####
  print('This is the model for well score calculation')
  print(DF_setting)
  return(df_fct_score)
}


#################
#### Example ####
#################

# #### Default ####
# h_item_IDtoName = hash( keys= c(1:53) , values=item_name  )
# 
# #### Set up the used_item_id, used_fct, and  used_item_name according to the model
# used_item_id   = c(3,4,9,10,12,13,15,16,20,19,24,21,35,36,39,42,46,51,52)
# used_fct       = c(1,1,2,2 ,3 ,4 ,5 ,5 ,6 ,6 ,7 ,7 ,8 ,8 ,9 ,10,10,11,11)
# used_item_name = values(h_item_IDtoName, keys = used_item_id )
# 
# f_score_well_us_general(data.arg = DF, 
#                         item.arg = used_item_name , 
#                         fct.arg  = used_fct
#                         )
