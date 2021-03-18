# WELLforLife
This is  the repo for WELL for Life.


#### This is a general well score calculation
```
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/WELLforLife/main/2021_0115.General_Function_well_score.R")
```
#### Example ####
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
f_score_US_model_7_v2(data.arg= DF)

Variable_name	Note
CFA21items_dom_score_negemo_stress	Domain = Negative Emotion and Stress
CFA21items_dom_score_posemo	Domain = Positive Emotion
CFA21items_dom_score_expl_purp	Domain = Exploration and Purpose
CFA21items_dom_score_fina	Domain = Finance
CFA21items_dom_score_phyhlt_resili	Domain = Physical Health and Resilience
CFA21items_dom_score_self	Domain = Sense of Self
CFA21items_dom_score_social	Domain = Social Connectedness
CFA21items_well_score	CFA WELL score
![image](https://user-images.githubusercontent.com/35211701/111592769-75ffd500-8786-11eb-99fc-334b197ebc51.png)

```


#### WELL China score ####
```
f_score_China_model_7.3_v2(data.arg= DF)
```
