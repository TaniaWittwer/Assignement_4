#--------------#
#QUESTIONS 1 & 2
#--------------#
install.packages("pacman")
library(pacman)
p_load(tidyverse, psych, magrittr, readr, DHARMa, broom, stats, caret, vcd, fmsb, lmtest, dplyr)

#load the .csv file (converted from the excel file) - this one will be the test dataset
testdata_2.dat <- read_csv2("Merged_15032016.csv")

#load the Rfile and rename it for easier use - this one will be the train dataset
load("SAFRANCEEWdata-1.Rdata")
decision_strat.dat <-  decisionstrategyrespwide2

#create a subdataset which contain only participants who completed Trail 1 and ExpProc
#select only useful vectors (same as the given cleaned files) + decisions ones
testdata_2_1.dat <- 
  testdata_2.dat %>% 
  select(Subject, Confidence.RESP, DecisionStrategyQues, DecisionStrategy.RESP, Lineup.ACC, LineupTPTA, Trial, `Procedure[Block]`, Lineup.RT, VideoCondition)

testdata_2_1.dat$`Procedure[Block]` <- as.character(testdata_2_1.dat$`Procedure[Block]`)
testdata_2_1.dat$Trial <- as.numeric(testdata_2_1.dat$Trial)

testdata_2_2.dat <- 
  testdata_2_1.dat %>% 
  dplyr::filter(Trial == 1, `Procedure[Block]` == "ExpProc")

#Keep only all relevant variables of strategies in the second file
#After having explored the structure of the file on excel, it appears that :
#if we keep only ProcedureBlock = ExpProc, we do not have strategies's rating
#Every participant ID are in double in ExpProc, expect for "trial" (probably reason why it is aked to keep Trail = 1)
#Every participant ID are also present in ProcedureBlock = DecisionstrategyProc, 17 times for each participant ID
#The file then also contain decision strategy rating for each participant, one row for each question, 17 rows/part
#Column DecisionStrategyQuest seems to be the number given to each question (need to be kept then)
#Column DecisionStrategy.RESP seems to be the rating in the likert scale 1-5
#According to the assignement instructions, we should keep only numbers 1 (automatic) and 8 (facecomparison)

#Thus we need to create a new file containing which is the raw dataset - extracted dataset

to_be_cleaned.dat <- 
  testdata_2_1.dat %>% 
  dplyr::filter(`Procedure[Block]` == "DecisionStrategyProc")

#and keep only participant ID and strategy (because other vectors are in the first file)
#Keep only the items 1 and 8 of Decision Strat
to_be_cleaned2.dat <-
  to_be_cleaned.dat %>% 
  select(Subject, DecisionStrategyQues, DecisionStrategy.RESP) %>% 
  dplyr::filter(DecisionStrategyQues == 1 | DecisionStrategyQues == 8)

#transform the file into wide format
strat_file.dat <- spread(to_be_cleaned2.dat, key = DecisionStrategyQues, value = DecisionStrategy.RESP)
colnames(strat_file.dat) <-  c("Subject", "automatic", "facecomparison")

#join to datasets
final_testdata.dat <- 
  left_join(testdata_2_2.dat, strat_file.dat, key = Subject) %>% 
  select(Subject, Confidence.RESP, Lineup.ACC, LineupTPTA, Lineup.RT, VideoCondition, automatic, facecomparison)

#convert tolower and rename some variables for easier usage
names(final_testdata.dat) %<>% tolower
colnames(final_testdata.dat)[colnames(final_testdata.dat) == "subject"] <- "particip"
colnames(final_testdata.dat)[colnames(final_testdata.dat) == "confidence.resp"] <- "confidence"
colnames(final_testdata.dat)[colnames(final_testdata.dat) == "lineup.acc"] <- "lineupacc"
colnames(final_testdata.dat)[colnames(final_testdata.dat) == "lineup.rt"] <- "lineuprt"
colnames(final_testdata.dat)[colnames(final_testdata.dat) == "videocondition"] <- "exposure"

#according to the already cleaned data (train and test): ta = 1 / tp = 2; long = 1 / short = 2
final_testdata.dat[final_testdata.dat == "TP"] <- 2
final_testdata.dat[final_testdata.dat == "TA"] <- 1
final_testdata.dat[final_testdata.dat == "long"] <- 1
final_testdata.dat[final_testdata.dat == "short"] <- 2



#now we can clean the second file (SAFRANCEEWdata) which is actually already cleaned. 
#We then only have to select the right variables and rename them
final_traindata.dat <- 
  decision_strat.dat %>% 
  select(subject, confidenceresp, decisionstrategyresp.1, decisionstrategyresp.8, lineupacc, lineuptpta, lineuprt, videocondition)

#rename variables
colnames(final_traindata.dat)[colnames(final_traindata.dat) == "subject"] <- "particip"
colnames(final_traindata.dat)[colnames(final_traindata.dat) == "confidenceresp"] <- "confidence"
colnames(final_traindata.dat)[colnames(final_traindata.dat) == "decisionstrategyresp.1"] <- "automatic"
colnames(final_traindata.dat)[colnames(final_traindata.dat) == "decisionstrategyresp.8"] <- "facecomparison"
colnames(final_traindata.dat)[colnames(final_traindata.dat) == "videocondition"] <- "exposure"

#correct variable statute
final_traindata.dat$lineuptpta <- as.numeric(final_traindata.dat$lineuptpta)
final_traindata.dat$exposure <- as.numeric(final_traindata.dat$exposure)

#rename data
final_traindata.dat[final_traindata.dat == "tp"] <- 2
final_traindata.dat[final_traindata.dat == "ta"] <- 1
final_traindata.dat[final_traindata.dat == "long"] <- 1
final_traindata.dat[final_traindata.dat == "short"] <- 2




#Here both cleaned dataset (identical as the one gave in the assignment)
final_testdata.dat
final_traindata.dat
