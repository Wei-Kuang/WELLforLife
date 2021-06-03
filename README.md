# WELLforLife
This is  the repo for WELL for Life.




### WELL score



#### WELL US score

```R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/0923.function_well_score_US_model7_version2.R")

f_score_US_model_7_v2(data.arg= DF)

# Please check the dictionary (tab-dicty) at https://github.com/Wei-Kuang/WELLforLife/blob/main/CFA_US_Model%207.%2009-23-20.JL.xlsx
```


#### WELL US score - short Form
Please check the dictionary at https://github.com/Wei-Kuang/WELLforLife/blob/main/US.EFACFA_ShortForm_Development_2021_0601.xlsx

Model: This is a model based on EFA and CFA. The mother model has 7 factors and 42 items.

Short form (21 items): We select the **top 3 items** in each factor based item-factor correlation (factor loading)

Super Short form (7 items): We select the **top 1** items in each factor based item-factor correlation (factor loading)

```R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/US.EFA.CFA_ShortForm_well_score_function_2021_0601.R.R")

# Full form score (7 factors and 42 items)
f_score_US_efacfa_7fct_42items(data.arg= DF)


# Short form score (7 factors and 21 items)
f_score_US_efacfa_7fct_21items(data.arg= DF)


# Super short form score (7 factors and 7 items)
f_score_US_efacfa_7fct_7items(data.arg= DF)
```


#### WELL China score

```R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/2020.1012.function_well_score_China_model7.3_version2.R")

f_score_China_model_7.3_v2(data.arg= DF)

# please check the dictionary(tab-dicty) at https://github.com/Wei-Kuang/WELLforLife/blob/main/CFA_China_Model%207.3_10-12-20.JL.xlsx
```

#### WELL China Old Well score (9dom) - Recalculate the old well score (9 dom)
Assume that **DF_Old_Score is the dataframe** which has original 9 domians scores, the **well_9dom_v2** is the re-calcuated old well score with range 0-100. This method is more similar to new CFA, so these two score can be compared.
```R
#### Derive old well score (9dom) by taking sum and then average ####
  # Step.1 scale factor score to 0-100
  # Step.2 take the average of factor scores
  # Note: this score is more like a NEW algorithm

domain_9 = c('dom_ee_score_revised', 'dom_en_score',          'dom_fs_score', 
             'dom_ph_score',         'dom_pm_score',          'dom_ss_score', 
             'dom_sc_score',         'dom_re_score',          'dom_sr_score_revised')

scale_0to100 = function(x){ rescale(x,to=c(0,100))}
DF_Scaled_Old = DF_Old_Score[, domain_9] %>% mutate_at(.vars =domain_9, .funs = scale_0to100)
well_9dom_v2 = DF_Scaled_Old %>% rowMeans(na.rm = T)
```


#### WELL Singapore score
```R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/210319.SG.wellscore.T3.2avgt2.R")

f_score_SG_T3.2avgt2(data.arg= DF)
```

#### WELL Thailand score
```R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/210406.TH.wellscore.model6.R")

f_score_Thailand_model_6(data.arg= DF)
```

### Recoding

#### WELL US
*US_W0_GlobalRecoding_2020_0104.Rmd*

#### WELL China
*China_GlobalReode_2021_0318.Rmd*

#### WELL Taiwan
*TW_GlobalRecoding_2020_0113.Rmd*

#### WELL Singapore
*210319.SG.Recode.R*

