library(readr)
library(plyr)
library(reshape2)
mdcsv <- read_csv("~/Desktop/schoolwork/dataSci250/week8/CollegeScorecard_Raw_Data/Most-Recent-Cohorts-All-Data-Elements.csv")

ucHD <- mdcsv[,c("REGION", "HIGHDEG")]
hdCounts <- count(ucHD, REGION ~ HIGHDEG)
hdMCounts <- acast(hdCounts, REGION ~ HIGHDEG, value.var = "freq")
hdMCounts <- na.omit(hdMCounts)

ucPD <- mdcsv[,c("REGION", "PREDDEG")]
pdCounts <- count(ucPD, REGION ~ PREDDEG)
pdMCounts <- acast(pdCounts, REGION ~ PREDDEG, value.var = "freq")
pdMCounts <- na.omit(pdMCounts)


degreeInfo <- mdcsv[,c(4, 62:99)]
da <-  mdcsv[, c("D150_4",	"D150_L4",	"D150_4_POOLED",	"D150_L4_POOLED")]
is.na(da) <- da == "NULL"
da[is.na(da)] <- 0
awardedList <- pmax(da$D150_4, da$D150_L4, da$D150_4_POOLED, da$D150_L4_POOLED)
awardedList[awardedList == 0] <- NA
degreeInfo['awarded'] <- awardedList
degreeInfo <- as.data.frame(na.omit(degreeInfo))

headerList <- c("OPEID","OPEID6", "INSTNM", "CITY","COMPL_RPY_1YR_RT","NONCOM_RPY_1YR_RT","LO_INC_RPY_1YR_RT","MD_INC_RPY_1YR_RT","HI_INC_RPY_1YR_RT","DEP_RPY_1YR_RT","IND_RPY_1YR_RT","PELL_RPY_1YR_RT","NOPELL_RPY_1YR_RT","FEMALE_RPY_1YR_RT","MALE_RPY_1YR_RT","FIRSTGEN_RPY_1YR_RT","NOTFIRSTGEN_RPY_1YR_RT","RPY_3YR_RT","COMPL_RPY_3YR_RT","NONCOM_RPY_3YR_RT","LO_INC_RPY_3YR_RT","MD_INC_RPY_3YR_RT","HI_INC_RPY_3YR_RT","DEP_RPY_3YR_RT","IND_RPY_3YR_RT","PELL_RPY_3YR_RT","NOPELL_RPY_3YR_RT","FEMALE_RPY_3YR_RT","MALE_RPY_3YR_RT","FIRSTGEN_RPY_3YR_RT","NOTFIRSTGEN_RPY_3YR_RT","RPY_5YR_RT","COMPL_RPY_5YR_RT","NONCOM_RPY_5YR_RT","LO_INC_RPY_5YR_RT","MD_INC_RPY_5YR_RT","HI_INC_RPY_5YR_RT","DEP_RPY_5YR_RT","IND_RPY_5YR_RT","PELL_RPY_5YR_RT","NOPELL_RPY_5YR_RT","FEMALE_RPY_5YR_RT","MALE_RPY_5YR_RT","FIRSTGEN_RPY_5YR_RT","NOTFIRSTGEN_RPY_5YR_RT","RPY_7YR_RT","COMPL_RPY_7YR_RT","NONCOM_RPY_7YR_RT","LO_INC_RPY_7YR_RT","MD_INC_RPY_7YR_RT","HI_INC_RPY_7YR_RT","DEP_RPY_7YR_RT","IND_RPY_7YR_RT","PELL_RPY_7YR_RT","NOPELL_RPY_7YR_RT","FEMALE_RPY_7YR_RT","MALE_RPY_7YR_RT","FIRSTGEN_RPY_7YR_RT","NOTFIRSTGEN_RPY_7YR_RT","INC_PCT_LO","DEP_STAT_PCT_IND","DEP_INC_PCT_LO","IND_INC_PCT_LO")

library(dplyr)
library(fun)
femaleData <- select(mdcsv, starts_with("FEMALE"))
femaleData$REGION <- mdcsv$REGION
femaleData$OPEID <- mdcsv$OPEID
is.na(femaleData) <- femaleData == "PrivacySuppressed"
is.na(femaleData) <- femaleData == "NULL"
femaleData[is.na(femaleData)] <- 0

maleData <- select(mdcsv, starts_with("MALE"))
maleData$REGION <- mdcsv$REGION 
maleData$OPEID <- mdcsv$OPEID
is.na(maleData) <- maleData == "PrivacySuppressed"
is.na(maleData) <- maleData == "NULL"
maleData[is.na(maleData)] <- 0

allData <- merge(femaleData, maleData, by=c("OPEID", "REGION"))
allData <- as.data.frame(lapply(allData,as.numeric))

yearSplits <- c('YR3_RT', 'YR4_RT', 'YR6_RT', 'YR8_RT')
finishers <- 'COMP'
genders <- c('MALE', 'FEMALE')
allData[,] <- sapply(allData[,], function(x) as.numeric(as.character(x)))
#normally I would write a function to do something like this but in the interest of times I'm trying to speed this up.
allData$year3CompF <- rowSums(allData[,c('FEMALE_COMP_ORIG_YR3_RT', 'FEMALE_COMP_4YR_TRANS_YR3_RT','FEMALE_COMP_2YR_TRANS_YR3_RT')])
allData$year4CompF <- rowSums(allData[,c('FEMALE_COMP_ORIG_YR4_RT', 'FEMALE_COMP_4YR_TRANS_YR4_RT','FEMALE_COMP_2YR_TRANS_YR4_RT')])
allData$year6CompF <- rowSums(allData[,c('FEMALE_COMP_ORIG_YR6_RT', 'FEMALE_COMP_4YR_TRANS_YR6_RT','FEMALE_COMP_2YR_TRANS_YR6_RT')])
allData$year8CompF <- rowSums(allData[,c('FEMALE_COMP_ORIG_YR8_RT', 'FEMALE_COMP_4YR_TRANS_YR6_RT','FEMALE_COMP_2YR_TRANS_YR8_RT')])
allData$year3CompM <- rowSums(allData[,c('MALE_COMP_ORIG_YR3_RT', 'MALE_COMP_4YR_TRANS_YR3_RT','MALE_COMP_2YR_TRANS_YR3_RT')])
allData$year4CompM <- rowSums(allData[,c('MALE_COMP_ORIG_YR4_RT', 'MALE_COMP_4YR_TRANS_YR4_RT','MALE_COMP_2YR_TRANS_YR4_RT')])
allData$year6CompM <- rowSums(allData[,c('MALE_COMP_ORIG_YR6_RT', 'MALE_COMP_4YR_TRANS_YR6_RT','MALE_COMP_2YR_TRANS_YR6_RT')])
allData$year8CompM <- rowSums(allData[,c('MALE_COMP_ORIG_YR8_RT', 'MALE_COMP_4YR_TRANS_YR6_RT','MALE_COMP_2YR_TRANS_YR8_RT')])

subView <- allData[,c('year3CompF','year4CompF','year6CompF','year8CompF', 
                      'year3CompM', 'year4CompM', 'year6CompM', 'year8CompM',
                      'FEMALE_RPY_1YR_RT', 'MALE_RPY_1YR_RT', 'FEMALE_RPY_3YR_RT', 'MALE_RPY_3YR_RT',
                      'FEMALE_RPY_5YR_RT', 'MALE_RPY_5YR_RT', 'FEMALE_RPY_7YR_RT', 'MALE_RPY_7YR_RT')]

library(corrplot)
subCor <- cor(subView)
corrplot(subCor)
