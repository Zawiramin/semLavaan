
#' WHAT ARE THE STEP 2
#' Basically step 2 SUPPOSEDLY are focusing on the data management section
#' 1st we extract only Malaysia from the original database
#' 2nd i rename the school stratum into much simpler one
#' 3rd i subset only the variables needed for the statistic and analysis
#' 4th i performed IMPUTATION using MICE
#' 5th i split or partitioned the data into desired % (70-30 or 80-20)
#' #--------------------------------------------------------------------------------------------------#
#'                          LINK AND NOTE USING DATA.TABLE
#' https://okanbulut.github.io/bigdata/wrangling-big-data.html#readingwriting-data-with-data.table
#' #--------------------------------------------------------------------------------------------------#



#' Need to remove unused collumn first before doing IMPUTATION
#' USING subset 
#' > table(pisaQ$IC001Q01TA)
# < table of extent 0 >
#   > table(pisaQ$PA001Q01TA)
# < table of extent 0 >
#   > table(pisaQ$EC001Q01NA)
# < table of extent 0 >
#'----------------------------#
#' Variables with ICT + 25 Latent Variables 
#' The only variables used in the analysis
#'----------------------------#
#' dataNew <- subset(pisaQ,
#'                          select = c(
#'                                     ST004D01T, #sex
#'                                     ESCS, #' Other derived indices from the questionnaire
#'                                     TMINS, # learning time in mins(total min)
#'                                     DISCLISCI:SCIEACT,# 12
#'                                     BELONG:ADINST,    # 8
#'                                     CULTPOSS:WEALTH,  # 5
#'                                     PV1SCIE:PV10SCIE, # to be converted into scie_perf (mean)
#'                                     ST076Q01NA:ST078Q11NA, # Activities before/ after school
#'                                     # 1
#'                                     #' [BELONG] 6 items
#'                                     ST034Q01TA:ST034Q06TA,
#'                                     
#'                                     # 2 & 3
#'                                     #' [COOPERATE] & [CPSVALUE] 8 items
#'                                     ST082Q01NA:ST082Q14NA,
#'                                     
#'                                     # 4
#'                                     #' Environmental awareness [(ENVAWARE)] 7 items
#'                                     ST092Q01TA:ST092Q09NA,
#'                                     
#'                                     # 5
#'                                     #' Environmental optimism [(ENVOPT)] 7 items
#'                                     ST093Q01TA:ST093Q08NA,
#'                                     
#'                                     # 6
#'                                     #' [JOYSCIE] 5 items
#'                                     ST094Q01NA:ST094Q05NA,
#'                                     
#'                                     # 7
#'                                     #' [(INTBRSCI)] 5 items
#'                                     ST095Q04NA:ST095Q15NA,
#'                                     
#'                                     # 8
#'                                     #' Instrumental motivation [(INSTSCIE)] 4 items
#'                                     ST113Q01TA:ST113Q04TA,
#'                                     
#'                                     # 9
#'                                     #' [ANXTEST] 5 items
#'                                     ST118Q01NA:ST118Q05NA,
#'                                     
#'                                     # 10
#'                                     #' [MOTIVAT] 5 items
#'                                     ST119Q01NA:ST119Q05NA,
#'                                     
#'                                     #' 11
#'                                     #' Science self-efficacy ([SCIEEFF]) 8 items
#'                                     ST129Q01TA:ST129Q08TA,
#'                                     
#'                                     # 12
#'                                     #' [EPIST] 6 items
#'                                     ST131Q01NA:ST131Q11NA
#'                          ))
#------------------------------------------#
# NEW DATA
# using read.spss data at step1, call the data with instruction, label name = false
# thus no need to recode from categorical to numerical
# in other words recodeData.R script might be using to recode reverse-coded variables only, if needed.
scieData <- studData[,-c(1:7,9:25,250:725,808:829,840:921)]
# put weighed and PVs into diff dataframe
# since no NAs
# will merge after imputations
wPV <- scieData[,c(227:317)]

# drop the wPV
scieDataNew <- scieData[,-c(227:317)]
# further investigate to drop unused column/variables
# drop ST001D01T:ST013Q01TA
# 45             ST019AQ01T
# 56             ST111Q01TA
#'55 ST127Q01TA, ST127Q02TA, and ST127Q03TA [REPEAT] NAs
# 106 ST063 School science courses attended, 
# 120            ST064Q03NA Freedom of curricular choice
# ST003D02T
scieDataNew <- scieDataNew[,-c(3:40,45:56,106:120)]
data.frame(colnames(scieDataNew))



#' Check for missing values and its pattern
md.pattern(scieDataNew)
names(dataNew)
summary(scieDataNew)

#------------------------------------------#
# Should I Perform imputation first before recoding the items
# or should I recode first??
# First run of the MICE package before recode, the algo skipped 
# the item variables since it is characters => PERFORM RECODE to change the datatype to numerical
# After run the code in recodeData.R, run the IMPUTATION
# 
# NEW:
# recode will only be performed to the desired columns once I've determined which variables to 
# be used for the analysis to save computational time that IMPUTATION algo needed
# so that I'm not wasting 6-10 hours of working worth just to wait for the computation done 
# if doing whole dataset IMPUTATIONS!
#------------------------------------------#
#' Set the seed before splitting the data
set.seed(12345)
library("Rcpp")
library("mice") #' handling missing values, md.pattern()

#--------------------------------#
# IMPUTATION
#' Performed Imputation using MICE
#' scieD.ind = index
# scieD.ind <- mice(scieDataNew,m=5,maxit = 50,method = 'pmm', seed = 1000)
# scieData.imp <- complete(scieD.ind) #commenting this as to note that i don't need to run this code again
# summary(dataNewImp)

#' convert data.frame to data.table 
#' read here for more details on the disadvantages of data.frame (R base)
#' https://www.quora.com/What-is-the-difference-between-data-frame-and-data-table-in-R-programming-language
# scieData.imp <- data.table(scieData.imp)

#------------------------------------------#
# IMPUTATION DONE
# merge back data wPV cbind with scieData.imp for data analysis
scieData.imp <- cbind(scieData.imp,wPV)

#------------#
# add weighted back to the imputed data
df1 = data.table(pisaQ[,W_FSTUWT:W_FSTURWT80])
dataNewImp = cbind(dataNewImp,df1)

#'-------------------------------#
#' Get the MEAN for the PV values
#'-------------------------------#
# dataNewImp[,':='
#           (avg.scie = rowMeans(dataNewImp[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE))]
# dataNewImp[,mean(avg.scie)]

#--------------#
#' TARGET CLASS
#--------------#
#' Using a set of predictors in the pisa dataset, we will predict whether students are above 
#' or below the mean scale score for science. The average science score in PISA 2015 was 493 
#' across all participating countries (see PISA 2015 Results in Focus for more details). 
#' Using this score as a cut-off value, we will first create a binary variable called 
#' science_perf where science_perf= High if a student’s science score is equal or larger than
#'  493; otherwise science_perf= Low
#'  USING Malaysia' OECD AVERAGE AS CUT OFF

#'  USING MALAYSIA AVERAGE AS CUT OFF
# sciePerf_HL = science performance high low 
# dataNewImp[, sciePerf_HL := 
#              as.factor(ifelse(science >= mean(science), "High", "Low"))]
# 
# # sciePerf_01 = science performance high low 
# dataNewImp[, sciePerf_01 := 
#              as.factor(ifelse(science >= mean(science), "1", "0"))]

