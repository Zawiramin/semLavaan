#-----------------------------------------------#
#'             Required 
#' #--------------------------------------------#
#' 
library("lavaan") #penting
library(lavaan.survey)

# FASTER THAN tidyverse and RECOMMENDED
# fread(), fwrite()
library("data.table") 

# get the data
# overall data
pisaQ = fread("pisaQQQ.csv",na.strings = "")
pisaQ = pisaQ[CNTRYID == "Malaysia"]
# science related data
ScRltd = fread("ScRltd.csv")
str(ScRltd)
table(pisaQ$ST129Q01TA)
