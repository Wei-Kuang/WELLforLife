
#### library ####
library('tidyverse')
library('scales')



#### Function version -2  ####
  
f_score_China_model_7.3_v2 = function(data.arg){
  
  #### Note for debugging ####
  # Assign your dataset with id and item to "DF" 
  
  #### Step 0.1 Prepare the data ####
  DF = data.arg # note: DF is thje input data
  
  item = c(
    'core_hopeless',
    'core_worried',
    'core_sad',
    'core_left_out',
    'core_overwhelm_difficult',
    'core_important_energy',
    
    'core_content',
    'core_happy',
    'core_secure',
    
    'core_engage_oppo',
    'core_contribute_alive',
    'core_contribute_doing',
    
    'core_money_needs',
    
    'core_fitness_level',
    'core_physical_illness',
    'core_deal_whatever',
    'core_overcome_obstacles',
    
    'core_accepting_yourself',
    'core_satisfied_yourself',
    
    'core_people_close',
    'core_people_rely'
  )
  
  # note: core_money_needs and core_fitness_level has 6 responses
  k_response = c(5,5,5,5,5,5, 5,5,5, 5,5,5, 6, 6,5,5,5, 5,5, 5,5)
  
  
  fct = c(
    'CFA21items_dom_score_negemo_stress',
    'CFA21items_dom_score_negemo_stress',
    'CFA21items_dom_score_negemo_stress',
    'CFA21items_dom_score_negemo_stress',
    'CFA21items_dom_score_negemo_stress',
    'CFA21items_dom_score_negemo_stress',
    
    'CFA21items_dom_score_posemo',
    'CFA21items_dom_score_posemo',
    'CFA21items_dom_score_posemo',
    
    'CFA21items_dom_score_expl_purp', 
    'CFA21items_dom_score_expl_purp', 
    'CFA21items_dom_score_expl_purp', 
    
    'CFA21items_dom_score_fina',
    
    'CFA21items_dom_score_phyhlt_resili',
    'CFA21items_dom_score_phyhlt_resili',
    'CFA21items_dom_score_phyhlt_resili',
    'CFA21items_dom_score_phyhlt_resili',
    
    'CFA21items_dom_score_self',
    'CFA21items_dom_score_self',
    
    'CFA21items_dom_score_social',
    'CFA21items_dom_score_social'
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
  df$core_hopeless             = 6 - DF$core_hopeless
  df$core_worried              = 6 - DF$core_worried
  df$core_sad                  = 6 - DF$core_sad
  df$core_left_out             = 6 - DF$core_left_out
  df$core_overwhelm_difficult  = 6 - DF$core_overwhelm_difficult
  df$core_important_energy     = 6 - DF$core_important_energy
  
  #### Step 1 Recode item - Assign a proportional score to the participant's response for each item ####
  # Each response will proportionally get the score 0-100
  # For example, a item with 1-5 responses, its '1' gets 0 score, '2' gets 20 score, ... , and '5' gets 100 score.
  df_recoded_item = data.frame(id = df$id) # Place holder
  
  for (i in 1:nrow(DF_setting) ){
    item_i = DF_setting$Item[i] # item_i is one of the 19 items
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
      select(Item) %>% 
      unlist()
    # factor scores
    df_fct_score[[fct_f]] = df_recoded_item %>%  # The key is to use the recoded item to calculate the factor score
      select( all_of (item_in_fct) ) %>%
      rowMeans(na.rm = FALSE)
  }
  #### Step 3 WELL score ####
  df_fct_score[['CFA21items_well_score']] = df_fct_score %>% 
    select(-id) %>%
    rowMeans(na.rm = FALSE)
  #### Output ####
  print('This is the China model 7.3 for well score calculation')
  print(DF_setting)
  return(df_fct_score)
}




# Example of using the function
#```{r}
#df = f_score_China_model_7.3_v2(data.arg = Dataset_China)
#df will have id and the columns (shown below)
# Could you merge this df back to China Master Dataset for analysis?
#```
#"CFA21items_well_score" is the new well score for China