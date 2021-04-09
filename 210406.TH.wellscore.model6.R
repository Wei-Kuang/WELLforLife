
#### library ####
library('tidyverse')
library('scales')

#### Function TH.model6  ####

f_score_Thailand_model_6 = function(data.arg){
  
  #### Note for debugging ####
  # Assign your dataset with id and item to "DF" 
  
  #### Step 0.1 Prepare the data ####
  DF = data.arg # note: DF is the input data
  
  item = c(
    'overwhelm_difficult',
    'meet_expectations',
    
    'hopeless',
    'sad',
    
    'content',
    'happy',
    
    'family_listen_good_th',
    'family_listen_bad_th',
    
    'money_needs',
    'financial_spending_th', 
    
    'fitness_level',
    'health_selfreported',
    
    'people_rely',
    'people_talk',
    
    'experiences_fulfill_th',
    'contribute_doing',
    
    'accepting_yourself',
    'satisfied_yourself',
    
    'sacred_beings_th',
    'spiritual_beliefs_th',
    
    'future_possibilities_th',
    'future_smiling_th'
  )
  
  # note: core_money_needs and core_fitness_level has 6 responses
  k_response = c(5,5, 5,5, 5,5, 5,5, 5,3, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5 )
  
  
  fct = c(
    'TH.Demands and Responsibilities',
    'TH.Demands and Responsibilities',
    
    'Experience of Emotions-Negative',
    'Experience of Emotions-Negative',
    
    'Experience of Emotions-Positive',
    'Experience of Emotions-Positive',
    
    'Family',
    'Family',
    
    'Finance',
    'Finance',
    
    'Personal Health',
    'Personal Health',
    
    'Relationship', 
    'Relationship', 
    
    'Self-Care', 
    'Self-Care', 
    
    'Sense of Self',
    'Sense of Self',
    
    'Spirituality and Religiosity',
    'Spirituality and Religiosity',
    
    'Thoughts and Feelings About the Future',
    'Thoughts and Feelings About the Future')
  
  
  
  # DF_setting is like a dictionary providing the item-factor relationship.  
  DF_setting = data.frame(Item=item,
                          K_response= k_response,
                          Factor = fct,
                          stringsAsFactors = F)
  
  #### Step 0.2 Select id and items from input data ####
  # df is the internal dataframe for score calculation
  df = DF[, c('id', DF_setting$Item)]
  
  #### Step 0.3 Reverse coding ####
  ## Thailand data has been reversed.
  # df$core_hopeless             = 6 - DF$core_hopeless
  # df$core_worried              = 6 - DF$core_worried
  # df$core_sad                  = 6 - DF$core_sad
  # df$core_left_out             = 6 - DF$core_left_out
  # df$core_overwhelm_difficult  = 6 - DF$core_overwhelm_difficult
  # df$core_important_energy     = 6 - DF$core_important_energy
  
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
  df_fct_score[['TH.model6.WellScore']] = df_fct_score %>% 
    select(-id) %>%
    rowMeans(na.rm = FALSE)
  #### Output ####
  print('This is the Thailand model 6 for well score calculation')
  print(DF_setting)
  return(df_fct_score)
}



#### WELL Score Thailand
dataset_name = 'WELL_Thailand_cleaned_data_19_01_2021_forJerry&EShen.csv'

setwd("/Users/foxeshen/Job/CFA-Thailand/CFA-Thailand-Data/")
DF = read.csv(dataset_name, stringsAsFactors = FALSE)

f_score_Thailand_model_6(data.arg= DF)

setwd("/Users/foxeshen/Job/WELL Score Analysis/WELL Score - China/WELL Score.CN.Data")
#saveRDS(f_score_Thailand_model_6(data.arg= DF), "TH.wellscore.model6.rds")
