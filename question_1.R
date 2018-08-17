#----------#
#QUESTION 1
#----------#
#Note the following correspondence between variables in the 2014 and 2015 files [lineuptpta = LineupTPTA, lineupacc = Lineup.ACC, videocondition = VideoCondition, confidenceresp = Confidence.RESP, lineuprt = Lineup.RT]; 
#Note that to select the relevant data for the variables above you need to filter cases so that only trial = 1, and `Procedure[Block]` = ExpProc (note the backticks). 
#Then you need to think about how to get the decision strategy data into the data file. You will likely need to read them into a separate dataframe, filter to select just the ones you want, create a wide data frame, and join that. Tricky!



#replace all ExpProc by a numerical value (1)
full_data.dat$ProcedureBlock <- if_else(full_data.dat$`Procedure[Block]` == "ExpProc", 1, 0)

#create a subdataset which containt only participants who completed Trail 1 and ExpProc
full_data_clean1.dat <- 
  full_data.dat %>% 
  filter(Trial == 1, ProcedureBlock == 1)

#--------> strategy columns are empty, is it normal?



#Keep only all relevant variables of strategies
