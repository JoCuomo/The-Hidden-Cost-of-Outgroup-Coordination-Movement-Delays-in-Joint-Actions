######### data processing Joint Action Task - Experiment 2 #####

library(tidyverse)
library(psych)
library(Hmisc)
library(reshape2)
library(here)
library(readr)
library(utils)
library(openxlsx)

# set the directory and create the dataset
setwd("C:/...")
Df_BW = read.xlsx("Dati_TMS_Studio2.xlsx")

#add the bw column 
Df_BW <- Df_BW %>% 
  mutate(bw = ifelse(ExperimentName == "TMS_Luca","White","Black"))

#declare the factors
names <- c("Subject","Session", "bw","sito")
Df_BW [,names] <- lapply(Df_BW [,names] , factor)

### clean the dataframe removing catch trials and trials in which participants started the movement before having received the instructions
BW_Clean = subset(Df_BW, Df_BW$CatchTrial_CORR != 1 &
                    Df_BW$StartSoggetto1 != 1 & Df_BW$StopSoggetto1 != 1)


### prepare the new dataframe, marking as NA trials in which participants failed to touch the right section of the object 
BW_Clean$Asynchrony_ok = ifelse(BW_Clean$AccSogg1 == 1, BW_Clean$RTNetto,NA)
BW_Clean$Start_ok = ifelse(BW_Clean$AccSogg1 == 1, BW_Clean$StartSoggetto1,NA)   
BW_Clean$Stop_ok = ifelse(BW_Clean$AccSogg1 == 1, BW_Clean$StopSoggetto1,NA)
BW_Clean$TM_ok = ifelse(BW_Clean$AccSogg1 == 1, BW_Clean$TMSoggetto1,NA)


### add grasp and movement as indipendent variables and rename the levels                                    
BW_Clean$Grasp = ifelse(BW_Clean$CorrectResponse == 3, "Precision","Power")                                                         
BW_Clean$Movement = ifelse(BW_Clean$OP_UG == 0, "Complementary","Imitative")  

BW_Clean$Grasp = as.factor(BW_Clean$Grasp)                                                       
BW_Clean$Movement = as.factor(BW_Clean$Movement) 



######## outliers removal: remove trials 2.5 sd over and 2.5 sd below the mean of each condition of each participant
## create a dataframe with means and sd divided by subject and by condition
ordinaper <- BW_Clean %>% group_by(Subject,bw, sito, Movement, Grasp)
head(ordinaper)
tabella1 = ordinaper %>% summarise(
  av_Asy = mean(Asynchrony_ok,na.rm = TRUE),
  sd_Asy = sd(Asynchrony_ok,na.rm = TRUE),
  av_TM = mean(TM_ok,na.rm = TRUE),
  sd_TM = sd(TM_ok,na.rm = TRUE))

# merge the old dataframe with the one that includes the means and sd for each condition
df2 = merge(BW_Clean, tabella1, all=T)

## another dataframe where start is mediated without the grasp level
ordinaper2 <- BW_Clean %>% group_by(Subject,bw, sito, Movement)
head(ordinaper2)
tabella2 = ordinaper2 %>% summarise(
  av_Start = mean(Start_ok,na.rm = TRUE),
  sd_Start = sd(Start_ok,na.rm = TRUE))

# merge the two dataframes
df3 = merge(df2, tabella2, all=T)
str(df3)

################# actual outliers removal #############################
df3$Asynchrony = ifelse(df3$Asynchrony_ok > df3$av_Asy + 2.5 * df3$sd_Asy | 
                          df3$Asynchrony_ok < df3$av_Asy - 2.5 * df3$sd_Asy, NA,df3$Asynchrony_ok )

df3$MovTime = ifelse(df3$TM_ok > df3$av_TM + 2.5 * df3$sd_TM | 
                       df3$TM_ok < df3$av_TM - 2.5 * df3$sd_TM, NA, df3$TM_ok )

df3$Start = ifelse(df3$Start_ok > df3$av_Start + 2.5 * df3$sd_Start | 
                     df3$Start_ok < df3$av_Start - 2.5 * df3$sd_Start, NA, df3$Start_ok)

############ create the final dataset keeping only the variables of interest  
str(df3) 
# keep important columns
keeps <- c("Subject","Session", "bw","sito","Movement","Grasp","Session","Asynchrony",
           "MovTime","Start")
df = df3[keeps]

str(df)

## export in excel

write.table(df, file = "DatiTMSBW_Studio2_Clean.csv", sep = ",", row.names = F)

