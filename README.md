# WELLforLife
This is  the repo for WELL for Life.




## WELL score
#### This is a general well score calculation

#### Example ####
```
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/2021_0115.General_Function_well_score.R")
```

```
h_item_IDtoName = hash( keys= c(1:53) , values=item_name  )
# Set up the used_item_id, used_fct, and  used_item_name according to the model
used_item_id   = c(3,4,9,10,12,13,15,16,20,19,24,21,35,36,39,42,46,51,52)
used_fct       = c(1,1,2,2 ,3 ,4 ,5 ,5 ,6 ,6 ,7 ,7 ,8 ,8 ,9 ,10,10,11,11)
used_item_name = values(h_item_IDtoName, keys = used_item_id )
 
f_score_well_general(data.arg = DF, 
                        item.arg = used_item_name , 
                        fct.arg  = used_fct
                        )
```


#### WELL US score ####
```
library('devtools')
source_url(https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/0923.function_well_score_US_model7_version2.R?token=AIMUTNPE2NY3XDYL2RICI6LAKPQZI")
```

```
f_score_US_model_7_v2(data.arg= DF)

# Please check the dictionary (tab-dicty) at https://github.com/Wei-Kuang/WELLforLife/blob/main/CFA_US_Model%207.%2009-23-20.JL.xlsx
```


#### WELL China score ####
```
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/2020.1012.function_well_score_China_model7.3_version2.R?token=AIMUTNPP5GIMXWQKCJK7GALAKPQWE")
```

```
f_score_China_model_7.3_v2(data.arg= DF)

# please check the dictionary(tab-dicty) at https://github.com/Wei-Kuang/WELLforLife/blob/main/CFA_China_Model%207.3_10-12-20.JL.xlsx
```


## Recoding

#### WELL US
*US_W0_GlobalRecoding_2020_0104.Rmd*

#### WELL China
*China_GlobalReode_2021_0318.Rmd*

#### WELL Taiwan
*TW_GlobalRecoding_2020_0113.Rmd*
