#### Package ####
library('xlsx')
library("readxl")
library('tidyverse')
library('scales')
library('lazyeval')


#### Load Data ####
filename <- 'SG.wellscore.T3.2avgt2.rds'
setwd('/Users/foxeshen/Job/WELL Score Analysis/WELL Score - Singapore/WELL Score.SG.Data/')
df_SG <- readRDS(filename)

dataset_name <- 'WELLSingaporeData_DATA_2021-02-19_1308.csv'  #9338
setwd("/Users/foxeshen/Job/CFA-Singapore/CFA-Singapore-data")
DF_SG <- read.csv(dataset_name, stringsAsFactors = FALSE)
names(DF_SG)[names(DF_SG) == 'record_id'] <- 'id'


#### Recode ####
# Age
DF_SG$intage_mainqn
X = DF_SG$intage_mainqn %>% cut( breaks= c(18, 30, 50, 60, 70, 120),right = F) %>% unlist() %>% as.vector()
X %>% table(useNA='always')
X = tidyr::replace_na(X, 'Missing')
X_fct = factor(X, labels = c('18-29','30-49','50-59','60-69','70+','Missing'))
X_fct %>% table(useNA = 'always')
DF_SG$age_new = X_fct

# Gender
DF_SG$gender
my_var = 'gender'
table(DF_SG[[my_var]], useNA = 'always')
Y = DF_SG[[my_var]]
X = DF_SG[[my_var]]
X[Y==1] <- '1.Male'
X[Y==2] <- '2.Female'
X_fct = factor(X) 
X_fct %>% table(useNA = 'always')
DF_SG$gender_new = X_fct

# Ethnicity
DF_SG$race_derived
table(DF_SG$race_derived)
my_var = 'race_derived'
table(DF_SG[[my_var]], useNA = 'always')
Y = DF_SG[[my_var]]
X = DF_SG[[my_var]]
X[Y==1] <- '1.Chinese'
X[Y==2] <- '2.Malay'
X[Y==3] <- '3.Indian'
X[Y==4] <- 'Missing'
X_fct = factor(X) 
X_fct %>% table(useNA = 'always')
DF_SG$ethnicity_new = X_fct

# Education
DF_SG$c8
table(DF_SG$c8)
my_var = 'c8'
table(DF_SG[[my_var]], useNA = 'always')
Y = DF_SG[[my_var]]
X = DF_SG[[my_var]]
X[Y==1] <- '1.No formal qualifications/lower primary'
X[Y==2] <- '2.Primary (PSLE)'
X[Y==3] <- '3.Secondary (O/N Level)'
X[Y==4] <- '4.ITE/NTC'
X[Y==5] <- '5.A level/Polytechnic/Diploma'
X[Y==6] <- '6.University'
X[Y==888] <- 'Missing'
X = tidyr::replace_na(X, 'Missing')
X_fct = factor(X) 
X_fct %>% table(useNA = 'always')
DF_SG$education_new = X_fct

# Work Status
DF_SG$c5
table(DF_SG$c5)
my_var = 'c5'
table(DF_SG[[my_var]], useNA = 'always')
Y = DF_SG[[my_var]]
X = DF_SG[[my_var]]
X[Y==1] <- '1.Working'
X[Y==2] <- '2.Student (full-time)'
X[Y==3] <- '3.Homemaker/Housewife'
X[Y==4] <- '4.Retired'
X[Y==5] <- '5.Unemployed (able to work)'
X[Y==6] <- '6.Unemployed (unable to work)'
X[Y==7] <- '7.Others'
X[Y==888] <- 'Missing'
X = tidyr::replace_na(X, 'Missing')
X_fct = factor(X) 
X_fct %>% table(useNA = 'always')
DF_SG$workstatus_new = X_fct

# Income
DF_SG$c6
table(DF_SG$c6)
my_var = 'c6'
table(DF_SG[[my_var]], useNA = 'always')
Y = DF_SG[[my_var]]
X = DF_SG[[my_var]]
X[Y==1] <- '1.<$2,000'
X[Y==2] <- '2.$2,000-$3,999'
X[Y==3] <- '3.$4,000-$5,999'
X[Y==4] <- '4.$6,000-$9,999'
X[Y==5] <- '5.>$10,000'
X[Y==888] <- 'Missing'
X[Y %in% c(888,999)] <- 'Missing'
X = tidyr::replace_na(X, 'Missing')
X_fct = factor(X) 
X_fct %>% table(useNA = 'always')
DF_SG$income_new = X_fct

DATA_SG <- merge(DF_SG,df_SG,by="id")
setwd("/Users/foxeshen/Job/WELL Score Analysis/WELL Score - Singapore/WELL Score.SG.Data")
saveRDS(DATA_SG, "SG.wellscore.Recode.rds")

