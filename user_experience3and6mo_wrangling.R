#descriptive statistics on perceptions of MVCN at 3-month and 6-month
#install gmodels
install.packages("gmodels")
install.packages("rmarkdown")
install.packages("plotly")
#load relevant packages
library(dplyr)
library(gmodels)
library(sas7bdat)
library(rmarkdown)
library(plotly)

#set working directory
setwd("~/TAPS online caregiver support/Data/final")

#read in data
wide <- read.sas7bdat("widefile_7dec17.sas7bdat")

#create subset of wide data that includes only MVCN members (at baseline, 3 month, or 6 month) to do attrition analysis
#mvcnany: 1 = joined MVCN at baseline, 3 month, or 6 month; 0 = was never in MVCN
wide$mvcnany <- 0
wide$mvcnany[wide$ver==2 | wide$ver3==2 | wide$ver6==2] <- 1
table(wide$mvcnany)

#create subset of data that includes only baseline survey completers in MVCN group
mvcnonly <- subset(wide, mvcnany==1)
nrow(mvcnonly)

#COMPLETED SIX-MONTH SURVEY OR NOT (0=no, 1=yes)
mvcnonly$sixmocomp <- 0
mvcnonly$sixmocomp[mvcnonly$ver6 == 2] <- 1
table(mvcnonly$sixmocomp)

#create subset of data that includes only MVCN 6-month survey non-completers for ease of 
#computing %s
sixmonon <- subset(mvcnonly, sixmocomp==0)
nrow(sixmonon)

##TYPES OF MEDICAL CONDITIONS WITH WHICH CARE RECIPIENTS HAVE BEEN DIAGNOSED

#PHYSICAL CONDITIONS (vdxphytw1):
#back pain (q8_1)
#limited motion or other impairment of knee (q8_4)
#hearing loss (q8_5)
#tinnitus (q8_6)
#hypertensive vascular disease (q8_7)
#diabetes (q8_8)
#traumatic arthritis (q8_9)
#paralysis or spinal cord injury (q8_12)
#injury-related reproductive disorder (q8_13)
#non-injury-related reproductive disorder (q8_14)
#blindness (q8_15)
#any amputated arms, hands, feet, or legs (q8_16)
#cancer (q8_17)
#multiple sclerosis (q8_18)

#create variable to represent total number of physical conditions

wide$vdxphytw1 <- rowSums(wide[, c("q8_1", "q8_4", "q8_5", "q8_6", "q8_7", "q8_8", "q8_9", 
                                   "q8_12", "q8_13", "q8_14", "q8_15", "q8_16", "q8_17", 
                                   "q8_18")], na.rm=TRUE)

#create binary variable to represent whether had at least one physical condition

wide$vdxphybw1 <- NA
wide$vdxphybw1[wide$vdxphytw1 >= 1] <- 1
wide$vdxphybw1[wide$vdxphytw1 == 0] <- 0

#PSYCHOLOGICAL CONDITIONS: 
#PTSD (q8_2)
#Depression (q8_3)
#Substance use disorder (q8_10)

#create variable to represent total number of psychological conditions

wide$vdxpsytw1 <- rowSums(wide[, c("q8_2", "q8_3", "q8_10")], na.rm=TRUE)

#create binary variable to represent whether had at least one psych condition

wide$vdxpsybw1 <- NA
wide$vdxpsybw1[wide$vdxpsytw1 >= 1] <- 1
wide$vdxpsybw1[wide$vdxpsytw1 == 0] <- 0

#NEUROLOGICAL CONDITIONS:
#TBI (q8_11)
#Parkinson's disease (q8_19)
#dementia (q8_20)

#create variable to represent total number of neurological conditions

wide$vdxneurotw1 <- rowSums(wide[, c("q8_11", "q8_19", "q8_20")], na.rm=TRUE)

#create binary variable to represent whether had neurological condition

wide$vdxneurobw1 <- NA
wide$vdxneurobw1[wide$vdxneurotw1 >= 1] <- 1
wide$vdxneurobw1[wide$vdxneurotw1 == 0] <- 0

#create variable to represent total number of medical conditions
wide$vdxsumw1 <- rowSums(wide[, c("vdxphytw1", "vdxpsytw1", "vdxneurotw1")], na.rm=TRUE)

#create binary variable to indicate six or more medical conditions (1=yes, 0=no)
wide$vdxsum6w1[wide$vdxsumw1 >= 6] <- 1
wide$vdxsum6w1[wide$vdxsumw1 < 6] <- 0

#ENGAGEMENT--CONTROL GROUP

#create score to indicate total time spent using other group among control participants
#recode frequency of visits to site used most by control group participants to represent # visits to site 
#in the past three months
#1 (every day) -> 90 times in past 3 months
#2 (almost every day) -> 60 times in past 3 months
#3 (2-3x/week) -> 30 times in past 3 months
#4 (once/week) -> 12 times in past 3 months
#5 (2-3x/month) -> 7.5 times in past 3 months
#6 (once/month or less) -> 3 times in past 3 months
#7 (not visited in past 3 months) -> 0
wide$freqcontrol6[wide$m6w36_q76==1] <- 90
wide$freqcontrol6[wide$m6w36_q76==2] <- 60
wide$freqcontrol6[wide$m6w36_q76==3] <- 30
wide$freqcontrol6[wide$m6w36_q76==4] <- 12
wide$freqcontrol6[wide$m6w36_q76==5] <- 7.5
wide$freqcontrol6[wide$m6w36_q76==6] <- 3
wide$freqcontrol6[wide$m6w36_q76==7] <- 0
wide$freqcontrol6[wide$m6w36_q76==0 | wide$m6w36_q76=='Nan'] <- NA

#recode typical duration of visits to site used most by control group participants in minutes
#1 (<10 min) -> 5 min
#2 (10-20 min) -> 15 min
#3 (20-30 min) -> 25 min
#4 (30-60 min) -> 45 min
#5 (>60 min) -> 75 min
wide$durcontrol6[wide$m6w36_q76==7] <- 0
wide$durcontrol6[wide$m6w36_q77==1] <- 5
wide$durcontrol6[wide$m6w36_q77==2] <- 15
wide$durcontrol6[wide$m6w36_q77==3] <- 25
wide$durcontrol6[wide$m6w36_q77==4] <- 45
wide$durcontrol6[wide$m6w36_q77==5] <- 75
wide$durcontrol6[wide$m6w36_q77==0 | wide$m6w36_q77=='Nan'] <- NA

#CREATE SUBSET OF DATA WITH JUST 6-MONTH SURVEY COMPLETERS IN CONTROL GROUP
control6 <- subset(wide, ver6==1)
nrow(control6)

CrossTable(control6$freqcontrol6, prop.c = TRUE)
CrossTable(control6$m6w36_q76, prop.c = TRUE)
CrossTable(control6$durcontrol6, prop.c = TRUE)
CrossTable(control6$m6w36_q77, prop.c = TRUE)

CrossTable(wide$freqcontrol6, prop.c = TRUE)
CrossTable(wide$durcontrol6, prop.c = TRUE)

table(wide$freqcontrol6, wide$durcontrol6, exclude=NULL)

#create total time spent on control group site by multiplying frequency x duration
wide$timecontrol6 <- wide$freqcontrol6*wide$durcontrol6

CrossTable(wide$timecontrol6, prop.c = TRUE)

attach(wide) 

#3-month
varsm3q73 <- paste0("m3w36_q73_", 1:11)

#6-month
varsm6q73 <- paste0("m6w36_q73_", 1:11)

#3-month
varsm3q74 <- paste0("m3w36_q74_", 1:9)

#6-month
varsm6q74 <- paste0("m6w36_q74_", 1:9)

#6-month
varsm6w6q56 <- paste0("m6w6_q56_", 1:6)

detach(wide)

#recode perceptions of MVCN so that higher scores indicate more positive perceptions

#3-month
wide[paste0(varsm3q73, "r")] <- apply(wide[varsm3q73], 2, function(x) {
  rev <- NA
  rev[x==1] <- 5
  rev[x==2] <- 4
  rev[x==3] <- 3
  rev[x==4] <- 2
  rev[x==5] <- 1
  return(rev)
})

#6-month
wide[paste0(varsm6q73, "r")] <- apply(wide[varsm6q73], 2, function(x) {
  rev <- NA
  rev[x==1] <- 5
  rev[x==2] <- 4
  rev[x==3] <- 3
  rev[x==4] <- 2
  rev[x==5] <- 1
  return(rev)
})

#PERCEIVED BARRIERS TO USING MVCN (W36Q74 series)

#create dichotomized versions of perceived barriers to using MVCN 

#3-month
wide[paste0(varsm3q74, "b")] <- apply(wide[varsm3q74], 2, function(x) {
  res <- NA
  res[x==1 | x==2] <- 1
  res[x==3 | x==4 | x==5] <- 0
  return(res)
})

#6-month
wide[paste0(varsm6q74, "b")] <- apply(wide[varsm6q74], 2, function(x) {
  res <- NA
  res[x==1 | x==2] <- 1
  res[x==3 | x==4 | x==5] <- 0
  return(res)
})

#recode perceived barriers to using MVCN so that higher scores indicate
#greater agreement with statement

#3-month
wide[paste0(varsm3q74, "r")] <- apply(wide[varsm3q74], 2, function(x) {
  rev <- NA
  rev[x==1] <- 5
  rev[x==2] <- 4
  rev[x==3] <- 3
  rev[x==4] <- 2
  rev[x==5] <- 1
  return(rev)
})

#6-month
wide[paste0(varsm6q74, "r")] <- apply(wide[varsm6q74], 2, function(x) {
  rev <- NA
  rev[x==1] <- 5
  rev[x==2] <- 4
  rev[x==3] <- 3
  rev[x==4] <- 2
  rev[x==5] <- 1
  return(rev)
})

##PERCEPTIONS OF MVCN (W36Q73 series)

#create dichotomized versions of perceptions of MVCN

#3-month
wide[paste0(varsm3q73, "b")] <- apply(wide[varsm3q73], 2, function(x) {
  res <- NA
  res[x==1 | x==2] <- 1
  res[x==3 | x==4 | x==5] <- 0
  return(res)
})

#6-month
wide[paste0(varsm6q73, "b")] <- apply(wide[varsm6q73], 2, function(x) {
  res <- NA
  res[x==1 | x==2] <- 1
  res[x==3 | x==4 | x==5] <- 0
  return(res)
})

#PERCEPTIONS OF RESOURCES FOR CAREGIVERS

#6-month
#1=agree or strongly agree with statement
#0=strongly disagree, disagree, or neither agree nor disagree
wide[paste0(varsm6w6q56, "b")] <- apply(wide[varsm6w6q56], 2, function(x) {
  res <- NA
  res[x==4 | x==5] <- 1
  res[x==1 | x==2 | x==3] <- 0
  return(res)
})

#check new vars against original vars
#table(wide$m3w36_q73_1b, wide$m3w36_q73_1)
#table(wide$m3w36_q73_11b, wide$m3w36_q73_11)
#table(wide$m3w36_q73_1r, wide$m3w36_q73_1)
#table(wide$m3w36_q73_11r, wide$m3w36_q73_11)

#table(wide$m6w36_q73_1b, wide$m6w36_q73_1)
#table(wide$m6w36_q73_11b, wide$m6w36_q73_11)
#table(wide$m6w36_q73_1r, wide$m6w36_q73_1)
#table(wide$m6w36_q73_11r, wide$m6w36_q73_11)

#table(wide$m3w36_q74_1b, wide$m3w36_q74_1)
#table(wide$m3w36_q74_9b, wide$m3w36_q74_9)
#table(wide$m3w36_q74_1r, wide$m3w36_q74_1)
#table(wide$m3w36_q74_9r, wide$m3w36_q74_9)

#table(wide$m6w36_q74_1b, wide$m6w36_q74_1)
#table(wide$m6w36_q74_9b, wide$m6w36_q74_9)
#table(wide$m6w36_q74_1r, wide$m6w36_q74_1)
#table(wide$m6w36_q74_9r, wide$m6w36_q74_9)

#table(wide$m6w6_q56_6b, wide$m6w6_q56_6)


#CHARACTERISTICS OF CAREGIVERS AND CARE RECIPIENTS--RECODES
#years as a caregiver
#recode continuous variable to cgdurcat1 using the following categories (in years)
#1 = 1-4 years
#2 = 5-8 years
#3 = 9+ years
wide$cgdurcat1[wide$cgdur1 >= 1 & wide$cgdur1 < 5] <- 1
wide$cgdurcat1[wide$cgdur1 >= 5 & wide$cgdur1 < 9] <- 2
wide$cgdurcat1[wide$cgdur1 >= 9] <- 3

#CARE RECIPIENT'S ERA OF MILITARY SERVICE WITH MISSING SET TO MISSING
wide$v911stat[wide$v911stat=='NaN'] <- NA

#CARE RECIPIENT'S AGE RECODED FROM CONTINUOUS TO CATEGORICAL
#vagecat: 
#1 = 18-29
#2 = 30-39
#3 = 40-49
#4 = 50+

wide$vagecat[wide$vage >= 18 & wide$vage < 30] <- 1
wide$vagecat[wide$vage >= 30 & wide$vage < 40] <- 2
wide$vagecat[wide$vage >= 40 & wide$vage < 50] <- 3
wide$vagecat[wide$vage >= 50] <- 4

#FREQUENCY OF MVCN WEBSITE VISITATION
#Set "0" to missing (NA) and collapse categories with too few obs
#mvcn$m3w36_q57_c and mvcn$m6w36_q57_c:
#1 = have not visited MVCN since joining
#2 = once a month or less
#3 = 2-3 times a month
#4 = once a week or more

#create subset of data that includes only cases that completed the 3 mo and the 6 mo survey and were in 
#the MVCN group at both time points (167 cases)
mvcn <- subset(wide, ver3==2 & ver6==2)
nrow(mvcn)

#CREATE SUBSET OF DATA WITH JUST 3-MONTH SURVEY COMPLETERS IN MVCN
mvcn3 <- subset(wide, ver3==2)
nrow(mvcn3)

#CREATE SUBSET OF DATA WITH JUST 6-MONTH SURVEY COMPLETERS IN MVCN
mvcn6 <- subset(wide, ver6==2)
nrow(mvcn6)

#3-MONTH
mvcn3$m3w36_q57_c[mvcn3$m3w36_q57==0] <- NA
mvcn3$m3w36_q57_c[mvcn3$m3w36_q57==7] <- 1
mvcn3$m3w36_q57_c[mvcn3$m3w36_q57==6] <- 2
mvcn3$m3w36_q57_c[mvcn3$m3w36_q57==5] <- 3
mvcn3$m3w36_q57_c[mvcn3$m3w36_q57==1 | mvcn3$m3w36_q57==2 | mvcn3$m3w36_q57==3 |
                   mvcn3$m3w36_q57==4] <- 4

#6-MONTH
mvcn6$m6w36_q57_c[mvcn6$m6w36_q57==0] <- NA
mvcn6$m6w36_q57_c[mvcn6$m6w36_q57==7] <- 1
mvcn6$m6w36_q57_c[mvcn6$m6w36_q57==6] <- 2
mvcn6$m6w36_q57_c[mvcn6$m6w36_q57==5] <- 3
mvcn6$m6w36_q57_c[mvcn6$m6w36_q57==1 | mvcn6$m6w36_q57==2 | mvcn6$m6w36_q57==3 |
                   mvcn6$m6w36_q57==4] <- 4

#create binary variables for frequency of visitation categories to compute SEs

#binary variable for "1" category
mvcn6$m6w36_q57_c1 <- NA
mvcn6$m6w36_q57_c1[mvcn6$m6w36_q57_c == 1] <- 1
mvcn6$m6w36_q57_c1[mvcn6$m6w36_q57_c >= 2] <- 0
#table(mvcn6$m6w36_q57_c1, mvcn6$m6w36_q57_c)

#binary variable for "2" category
mvcn6$m6w36_q57_c2 <- NA
mvcn6$m6w36_q57_c2[mvcn6$m6w36_q57_c == 2] <- 1
mvcn6$m6w36_q57_c2[mvcn6$m6w36_q57_c == 1 | mvcn6$m6w36_q57_c >= 3] <- 0
#table(mvcn6$m6w36_q57_c2, mvcn6$m6w36_q57_c)

#binary variable for "3" category
mvcn6$m6w36_q57_c3 <- NA
mvcn6$m6w36_q57_c3[mvcn6$m6w36_q57_c == 3] <- 1
mvcn6$m6w36_q57_c3[mvcn6$m6w36_q57_c <= 2 | mvcn6$m6w36_q57_c == 4] <- 0
#table(mvcn6$m6w36_q57_c3, mvcn6$m6w36_q57_c)

#binary variable for "4" category
mvcn6$m6w36_q57_c4 <- NA
mvcn6$m6w36_q57_c4[mvcn6$m6w36_q57_c == 4] <- 1
mvcn6$m6w36_q57_c4[mvcn6$m6w36_q57_c <= 3] <- 0
#table(mvcn6$m6w36_q57_c4, mvcn6$m6w36_q57_c)

#DURATION OF MVCN WEBSITE VISITATION
#Set 0 to missing for those who did not answer the question
#If said hasn't visited MVCN website over the past 3 months, set to 0
#Collapse those who said 30 minutes to an hour or an hour or more into a single category
#mvcn$m3w36_q58_c and mvcn$m6w36_q58_c:
#0 = has not visited MVCN website in past 3 mos
#1 = usually spent < 10 min on MVCN website
#2 = usually spent 10-20 min on MVCN website
#3 = usually spent 20-30 min on MVCN website
#4 = usually spent >= 30 min on MVCN website

#3-MONTH
mvcn3$m3w36_q58_c <- NA
mvcn3$m3w36_q58_c[mvcn3$m3w36_q57_c==1] <- 0
mvcn3$m3w36_q58_c[mvcn3$m3w36_q58==1] <- 1
mvcn3$m3w36_q58_c[mvcn3$m3w36_q58==2] <- 2
mvcn3$m3w36_q58_c[mvcn3$m3w36_q58==3] <- 3
mvcn3$m3w36_q58_c[mvcn3$m3w36_q58==4 | mvcn3$m3w36_q58==5] <- 4

#6-MONTH
mvcn6$m6w36_q58_c <- NA
mvcn6$m6w36_q58_c[mvcn6$m6w36_q57_c==1] <- 0
mvcn6$m6w36_q58_c[mvcn6$m6w36_q58==1] <- 1
mvcn6$m6w36_q58_c[mvcn6$m6w36_q58==2] <- 2
mvcn6$m6w36_q58_c[mvcn6$m6w36_q58==3] <- 3
mvcn6$m6w36_q58_c[mvcn6$m6w36_q58==4 | mvcn6$m6w36_q58==5] <- 4

#create binary variables for duration of MVCN site visitation to compute SEs

#binary var for category "0"
mvcn6$m6w36_q58_c0 <- NA
mvcn6$m6w36_q58_c0[mvcn6$m6w36_q58_c == 0] <- 1
mvcn6$m6w36_q58_c0[mvcn6$m6w36_q58_c >= 1] <- 0
#table(mvcn6$m6w36_q58_c0, mvcn6$m6w36_q58_c)

#binary var for category "1"
mvcn6$m6w36_q58_c1 <- NA
mvcn6$m6w36_q58_c1[mvcn6$m6w36_q58_c == 1] <- 1
mvcn6$m6w36_q58_c1[mvcn6$m6w36_q58_c == 0 | mvcn6$m6w36_q58_c >= 2] <- 0
#table(mvcn6$m6w36_q58_c1, mvcn6$m6w36_q58_c)

#binary var for category "2"
mvcn6$m6w36_q58_c2 <- NA
mvcn6$m6w36_q58_c2[mvcn6$m6w36_q58_c == 2] <- 1
mvcn6$m6w36_q58_c2[mvcn6$m6w36_q58_c <= 1 | mvcn6$m6w36_q58_c >= 3] <- 0
#table(mvcn6$m6w36_q58_c2, mvcn6$m6w36_q58_c)

#binary var for category "3"
mvcn6$m6w36_q58_c3 <- NA
mvcn6$m6w36_q58_c3[mvcn6$m6w36_q58_c == 3] <- 1
mvcn6$m6w36_q58_c3[mvcn6$m6w36_q58_c <= 2 | mvcn6$m6w36_q58_c == 4] <- 0
#table(mvcn6$m6w36_q58_c3, mvcn6$m6w36_q58_c)

#binary var for category "4"
mvcn6$m6w36_q58_c4 <- NA
mvcn6$m6w36_q58_c4[mvcn6$m6w36_q58_c == 4] <- 1
mvcn6$m6w36_q58_c4[mvcn6$m6w36_q58_c <= 3] <- 0
#table(mvcn6$m6w36_q58_c4, mvcn6$m6w36_q58_c)


#create subset of cases that have visited MVCN site at least once in last 3 mos (3 mo survey)
visitmvcn3 <- filter(mvcn3, m3w36_q57_c > 1)
nrow(visitmvcn3)

#have visited MVCN site at least once in last 3 mos (6 mo survey)
visitmvcn6 <- filter(mvcn6, m6w36_q57_c > 1)
nrow(visitmvcn6)

#ACTIVITIES IN MVCN NETWORK

#Interacting with other members outside of the MVCN online network
#recode to be 0=no, 1 = yes

visitmvcn6$m6w36_q71_b <- NA
visitmvcn6$m6w36_q71_b[visitmvcn6$m6w36_q71==1] <- 1
visitmvcn6$m6w36_q71_b[visitmvcn6$m6w36_q71==2] <- 0
table(visitmvcn6$m6w36_q71_b, visitmvcn6$m6w36_q71)

#CAREGIVING TASKS

#recode var about posting, reading, and responding to requests for caregiving
visitmvcn6$m6w36_q59_b <- NA
visitmvcn6$m6w36_q59_b[visitmvcn6$m6w36_q59==1] <- 1
visitmvcn6$m6w36_q59_b[visitmvcn6$m6w36_q59==2] <- 0

#combine posting, reading, and responding to requests for caregiving into the
#same variables at 3- and 6-months
#1 = posted, read, or responded to info
#0 = did none of those

#3-months
visitmvcn3$m3w36_q61_1_123 <- 0
visitmvcn3$m3w36_q61_1_123[visitmvcn3$m3w36_q61_1_1==1 | visitmvcn3$m3w36_q61_1_2==1 | 
                           visitmvcn3$m3w36_q61_1_3==1] <- 1

#6-months
visitmvcn6$m6w36_q61_1_123 <- 0
visitmvcn6$m6w36_q61_1_123[visitmvcn6$m6w36_q61_1_1==1 | visitmvcn6$m6w36_q61_1_2==1 | 
                           visitmvcn6$m6w36_q61_1_3==1] <- 1

#combine posting and responding to requests for caregiving into the
#same variables at 3- and 6-months
#1 = posted or responded to info
#0 = didn't do either of those

#6-months
visitmvcn6$m6w36_q61_1_13 <- 0
visitmvcn6$m6w36_q61_1_13[visitmvcn6$m6w36_q61_1_1==1 | visitmvcn6$m6w36_q61_1_3==1] <- 1

#RESOURCES FOR CAREGIVING

#combine posting, reading, and responding to requests for info about resources for 
#caregiving into the same variables at 3- and 6-months
#1 = posted, read, or responded to info
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_2_123 <- 0
visitmvcn3$m3w36_q61_2_123[visitmvcn3$m3w36_q61_2_1==1 | visitmvcn3$m3w36_q61_2_2==1 |
                             visitmvcn3$m3w36_q61_2_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_2_123 <- 0
visitmvcn6$m6w36_q61_2_123[visitmvcn6$m6w36_q61_2_1==1 | visitmvcn6$m6w36_q61_2_2==1 |
                             visitmvcn6$m6w36_q61_2_3==1] <- 1

#combine posting and responding to requests for info about resources for caregiving into the
#same variables at 3- and 6-months
#1 = posted or responded to info
#0 = didn't do either of those

#6-month
visitmvcn6$m6w36_q61_2_13 <- 0
visitmvcn6$m6w36_q61_2_13[visitmvcn6$m6w36_q61_2_1==1 | visitmvcn6$m6w36_q61_2_3==1] <- 1

#EMOTIONAL SUPPORT AND/OR PRAYER

#combine posting, reading, and responding to requests for emotional support and/or 
#prayer for into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_3_123 <- 0
visitmvcn3$m3w36_q61_3_123[visitmvcn3$m3w36_q61_3_1==1 | visitmvcn3$m3w36_q61_3_2==1 |
                             visitmvcn3$m3w36_q61_3_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_3_123 <- 0
visitmvcn6$m6w36_q61_3_123[visitmvcn6$m6w36_q61_3_1==1 | visitmvcn6$m6w36_q61_3_2==1 |
                             visitmvcn6$m6w36_q61_3_3==1] <- 1

#combine posting and responding to requests for emotional support and/or prayer into the
#same variables at 3- and 6-months
#1 = posted or responded to info
#0 = didn't do either of those

#6-month
visitmvcn6$m6w36_q61_3_13 <- 0
visitmvcn6$m6w36_q61_3_13[visitmvcn6$m6w36_q61_3_1==1 | visitmvcn6$m6w36_q61_3_3==1] <- 1

#INFO ABOUT VA OR DOD BENEFITS FOR CARE RECIPIENT

#combine posting, reading, and responding to requests for info about VA or DoD 
#benefits for care recipient into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_4_123 <- 0
visitmvcn3$m3w36_q61_4_123[visitmvcn3$m3w36_q61_4_1==1 | visitmvcn3$m3w36_q61_4_2==1 |
                             visitmvcn3$m3w36_q61_4_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_4_123 <- 0
visitmvcn6$m6w36_q61_4_123[visitmvcn6$m6w36_q61_4_1==1 | visitmvcn6$m6w36_q61_4_2==1 |
                             visitmvcn6$m6w36_q61_4_3==1] <- 1

#combine posting and responding to requests for info about VA or DoD 
#benefits for care recipient into the
#same variables at 3- and 6-months
#1 = posted or responded to info
#0 = didn't do either of those

#6-month
visitmvcn6$m6w36_q61_4_13 <- 0
visitmvcn6$m6w36_q61_4_13[visitmvcn6$m6w36_q61_4_1==1 | visitmvcn6$m6w36_q61_4_3==1] <- 1

#INFO OR REQUESTS ABOUT NON-CAREGIVING TOPICS

#combine posting, reading, and responding to requests for info about 
#non-caregiving topics into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_5_123 <- 0
visitmvcn3$m3w36_q61_5_123[visitmvcn3$m3w36_q61_5_1==1 | visitmvcn3$m3w36_q61_5_2==1 |
                             visitmvcn3$m3w36_q61_5_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_5_123 <- 0
visitmvcn6$m6w36_q61_5_123[visitmvcn6$m6w36_q61_5_1==1 | visitmvcn6$m6w36_q61_5_2==1 |
                             visitmvcn6$m6w36_q61_5_3==1] <- 1

#combine posting and responding to requests for info about non-caregiving topics
#into the same variables at 3- and 6-months
#1 = posted or responded to info
#0 = didn't do either of those

#6-month
visitmvcn6$m6w36_q61_5_13 <- 0
visitmvcn6$m6w36_q61_5_13[visitmvcn6$m6w36_q61_5_1==1 | visitmvcn6$m6w36_q61_5_3==1] <- 1

#ADVICE OR INFO IN RESPONSE TO ANOTHER MEMBER'S POST

#combine posting, reading, and responding to advice or info in response to 
#another member's post into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_6_123 <- 0
visitmvcn3$m3w36_q61_6_123[visitmvcn3$m3w36_q61_6_1==1 | visitmvcn3$m3w36_q61_6_2==1 |
                             visitmvcn3$m3w36_q61_6_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_6_123 <- 0
visitmvcn6$m6w36_q61_6_123[visitmvcn6$m6w36_q61_6_1==1 | visitmvcn6$m6w36_q61_6_2==1 |
                             visitmvcn6$m6w36_q61_6_3==1] <- 1

#combine posting and responding to requests for info in response to 
#another member's post 
#into the same variables at 3- and 6-months
#1 = posted or responded to info
#0 = didn't do either of those

#6-month
visitmvcn6$m6w36_q61_6_13 <- 0
visitmvcn6$m6w36_q61_6_13[visitmvcn6$m6w36_q61_6_1==1 | visitmvcn6$m6w36_q61_6_3==1] <- 1

#EMOTIONAL SUPPORT OR SOLIDARITY WITH ANOTHER MEMBER'S POST

#combine posting, reading, and responding to emotional support or solidarity 
#with another member's post into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_7_123 <- 0
visitmvcn3$m3w36_q61_7_123[visitmvcn3$m3w36_q61_7_1==1 | visitmvcn3$m3w36_q61_7_2==1 |
                             visitmvcn3$m3w36_q61_7_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_7_123 <- 0
visitmvcn6$m6w36_q61_7_123[visitmvcn6$m6w36_q61_7_1==1 | visitmvcn6$m6w36_q61_7_2==1 |
                             visitmvcn6$m6w36_q61_7_3==1] <- 1

#combine posting and responding to emotional support or solidarity 
#with another member's post into the same variables at 3- and 6-months
#1 = posted or responded to request
#0 = did neither of those

#6-month
visitmvcn6$m6w36_q61_7_13 <- 0
visitmvcn6$m6w36_q61_7_13[visitmvcn6$m6w36_q61_7_1==1 | visitmvcn6$m6w36_q61_7_3==1] <- 1

#LINKS TO INFO OR RESOURCES FOR CAREGIVERS

#combine posting, reading, and responding to links to info or resources for CGs
#into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_8_123 <- 0
visitmvcn3$m3w36_q61_8_123[visitmvcn3$m3w36_q61_8_1==1 | visitmvcn3$m3w36_q61_8_2==1 |
                             visitmvcn3$m3w36_q61_8_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_8_123 <- 0
visitmvcn6$m6w36_q61_8_123[visitmvcn6$m6w36_q61_8_1==1 | visitmvcn6$m6w36_q61_8_2==1 |
                             visitmvcn6$m6w36_q61_8_3==1] <- 1

#combine posting and responding to links to info or resources for CGs 
#into the same variables at 3- and 6-months
#1 = posted or responded to links to info or resources
#0 = did neither of those

#6-month
visitmvcn6$m6w36_q61_8_13 <- 0
visitmvcn6$m6w36_q61_8_13[visitmvcn6$m6w36_q61_8_1==1 | visitmvcn6$m6w36_q61_8_3==1] <- 1


#INFO OR LINKS TO INFO ABOUT VA OR DOD BENEFITS FOR CARE RECIPIENTS

#combine posting, reading, and responding to info or links to info 
#about VA or DoD benefits for care recipients into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
visitmvcn3$m3w36_q61_9_123 <- 0
visitmvcn3$m3w36_q61_9_123[visitmvcn3$m3w36_q61_9_1==1 | visitmvcn3$m3w36_q61_9_2==1 |
                             visitmvcn3$m3w36_q61_9_3==1] <- 1

#6-month
visitmvcn6$m6w36_q61_9_123 <- 0
visitmvcn6$m6w36_q61_9_123[visitmvcn6$m6w36_q61_9_1==1 | visitmvcn6$m6w36_q61_9_2==1 |
                             visitmvcn6$m6w36_q61_9_3==1] <- 1

#combine posting and responding to info or links to info 
#about VA or DoD benefits for care recipients 
#into the same variables at 3- and 6-months
#1 = posted or responded to links to info or resources
#0 = did neither of those

#6-month
visitmvcn6$m6w36_q61_9_13 <- 0
visitmvcn6$m6w36_q61_9_13[visitmvcn6$m6w36_q61_9_1==1 | visitmvcn6$m6w36_q61_9_3==1] <- 1

#create variable to represent number of types of posts and responses at 6-month survey
visitmvcn6$numposttype6 <- visitmvcn6$m6w36_q61_1_13 + visitmvcn6$m6w36_q61_2_13 + visitmvcn6$m6w36_q61_3_13 + 
                    visitmvcn6$m6w36_q61_4_13 + visitmvcn6$m6w36_q61_5_13 + visitmvcn6$m6w36_q61_6_13 + 
                    visitmvcn6$m6w36_q61_7_13 + visitmvcn6$m6w36_q61_8_13 + visitmvcn6$m6w36_q61_9_13

#PARTICIPATION IN OTHER CAREGIVING GROUPS

#A military or veteran caregiver group on Facebook (other than specifically named programs)

#recode "NaN" to "NA"
#3-month
mvcn3$m3q48_3[mvcn3$m3q48_3=='NaN'] <- NA

#6-month
mvcn6$m6q48_3[mvcn6$m6q48_3=='NaN'] <- NA

#Number of in-person caregiving support groups other than MVCN

#clean up variables
#recoded variable is:
#0 = none
#1 = 1
#2 = 2
#3 = 3 or more

#3-month
mvcn3$m3w36_q54_1_c[mvcn3$m3w36_q54_1==0] <- NA
mvcn3$m3w36_q54_1_c[mvcn3$m3w36_q54_1==1] <- 0
mvcn3$m3w36_q54_1_c[mvcn3$m3w36_q54_1==2] <- 1
mvcn3$m3w36_q54_1_c[mvcn3$m3w36_q54_1==3] <- 2
mvcn3$m3w36_q54_1_c[mvcn3$m3w36_q54_1==4 | mvcn3$m3w36_q54_1==5] <- 3

#6-month
mvcn6$m6w36_q54_1_c[mvcn6$m6w36_q54_1==0] <- NA
mvcn6$m6w36_q54_1_c[mvcn6$m6w36_q54_1==1] <- 0
mvcn6$m6w36_q54_1_c[mvcn6$m6w36_q54_1==2] <- 1
mvcn6$m6w36_q54_1_c[mvcn6$m6w36_q54_1==3] <- 2
mvcn6$m6w36_q54_1_c[mvcn6$m6w36_q54_1==4 | mvcn6$m6w36_q54_1==5] <- 3

#create binary variables for all four categories to compute SEs

#binary var for "0" category (1=yes, 0=no)
mvcn6$m6w36_q54_1_c0 <- NA
mvcn6$m6w36_q54_1_c0[mvcn6$m6w36_q54_1_c==0] <- 1
mvcn6$m6w36_q54_1_c0[mvcn6$m6w36_q54_1_c >= 1] <- 0
#table(mvcn6$m6w36_q54_1_c, mvcn6$m6w36_q54_1_c0)

#binary var for "1" category (1=yes, 0=no)
mvcn6$m6w36_q54_1_c1 <- NA
mvcn6$m6w36_q54_1_c1[mvcn6$m6w36_q54_1_c==1] <- 1
mvcn6$m6w36_q54_1_c1[mvcn6$m6w36_q54_1_c==0 | mvcn6$m6w36_q54_1_c >= 2] <- 0
#table(mvcn6$m6w36_q54_1_c, mvcn6$m6w36_q54_1_c1)

#binary var for "2" category (1=yes, 0=no)
mvcn6$m6w36_q54_1_c2 <- NA
mvcn6$m6w36_q54_1_c2[mvcn6$m6w36_q54_1_c==2] <- 1
mvcn6$m6w36_q54_1_c2[mvcn6$m6w36_q54_1_c <= 1 | mvcn6$m6w36_q54_1_c == 3] <- 0
#table(mvcn6$m6w36_q54_1_c, mvcn6$m6w36_q54_1_c2)

#binary var for "3" category (1=yes, 0=no)
mvcn6$m6w36_q54_1_c3 <- NA
mvcn6$m6w36_q54_1_c3[mvcn6$m6w36_q54_1_c == 3] <- 1
mvcn6$m6w36_q54_1_c3[mvcn6$m6w36_q54_1_c <= 2] <- 0
#table(mvcn6$m6w36_q54_1_c, mvcn6$m6w36_q54_1_c3)

#Number of online only caregiving support groups other than MVCN

#clean up variables
#recoded variable is:
#0 = none
#1 = 1
#2 = 2
#3 = 3 or more

#3-month
mvcn3$m3w36_q54_2_c[mvcn3$m3w36_q54_2==0] <- NA
mvcn3$m3w36_q54_2_c[mvcn3$m3w36_q54_2==1] <- 0
mvcn3$m3w36_q54_2_c[mvcn3$m3w36_q54_2==2] <- 1
mvcn3$m3w36_q54_2_c[mvcn3$m3w36_q54_2==3] <- 2
mvcn3$m3w36_q54_2_c[mvcn3$m3w36_q54_2==4 | mvcn3$m3w36_q54_2==5] <- 3

#6-month
mvcn6$m6w36_q54_2_c[mvcn6$m6w36_q54_2==0] <- NA
mvcn6$m6w36_q54_2_c[mvcn6$m6w36_q54_2==1] <- 0
mvcn6$m6w36_q54_2_c[mvcn6$m6w36_q54_2==2] <- 1
mvcn6$m6w36_q54_2_c[mvcn6$m6w36_q54_2==3] <- 2
mvcn6$m6w36_q54_2_c[mvcn6$m6w36_q54_2==4 | mvcn6$m6w36_q54_2==5] <- 3

#create binary vars for 4-category variable to compute SEs

#binary var for "0" category (1=yes, 0=no)
mvcn6$m6w36_q54_2_c0 <- NA
mvcn6$m6w36_q54_2_c0[mvcn6$m6w36_q54_2_c==0] <- 1
mvcn6$m6w36_q54_2_c0[mvcn6$m6w36_q54_2_c >= 1] <- 0
#table(mvcn6$m6w36_q54_2_c, mvcn6$m6w36_q54_2_c0)

#binary var for "1" category (1=yes, 0=no)
mvcn6$m6w36_q54_2_c1 <- NA
mvcn6$m6w36_q54_2_c1[mvcn6$m6w36_q54_2_c==1] <- 1
mvcn6$m6w36_q54_2_c1[mvcn6$m6w36_q54_2_c==0 | mvcn6$m6w36_q54_2_c >= 2] <- 0
#table(mvcn6$m6w36_q54_2_c, mvcn6$m6w36_q54_2_c1)

#binary var for "2" category (1=yes, 0=no)
mvcn6$m6w36_q54_2_c2 <- NA
mvcn6$m6w36_q54_2_c2[mvcn6$m6w36_q54_2_c==2] <- 1
mvcn6$m6w36_q54_2_c2[mvcn6$m6w36_q54_2_c <= 1 | mvcn6$m6w36_q54_2_c == 3] <- 0
#table(mvcn6$m6w36_q54_2_c, mvcn6$m6w36_q54_2_c2)

#binary var for "3" category (1=yes, 0=no)
mvcn6$m6w36_q54_2_c3 <- NA
mvcn6$m6w36_q54_2_c3[mvcn6$m6w36_q54_2_c == 3] <- 1
mvcn6$m6w36_q54_2_c3[mvcn6$m6w36_q54_2_c <= 2] <- 0
#table(mvcn6$m6w36_q54_2_c, mvcn6$m6w36_q54_2_c3)

#FREQUENCY OF PARTICIPATION IN CAREGIVER SUPPORT GROUPS OTHER THAN MVCN
#recode NaN to missing values (NA) and collapse categories to form a smaller set of categories
#1 = once a week or more (1, 2)
#2 = one to three times a month (3, 4)
#3 = every few months or less (5, 6, 7)

#3-month
mvcn3$m3q49_c[mvcn3$m3q49=='NaN'] <- NA
mvcn3$m3q49_c[mvcn3$m3q49==1 | mvcn3$m3q49==2] <- 1
mvcn3$m3q49_c[mvcn3$m3q49==3 | mvcn3$m3q49==4] <- 2
mvcn3$m3q49_c[mvcn3$m3q49==5 | mvcn3$m3q49==6 | mvcn3$m3q49==7] <- 3


#6-month
mvcn6$m6q49_c[mvcn6$m6q49=='NaN'] <- NA
mvcn6$m6q49_c[mvcn6$m6q49==1 | mvcn6$m6q49==2] <- 1
mvcn6$m6q49_c[mvcn6$m6q49==3 | mvcn6$m6q49==4] <- 2
mvcn6$m6q49_c[mvcn6$m6q49==5 | mvcn6$m6q49==6 | mvcn6$m6q49==7] <- 3

#create binary vars for each category of response

#binary var for "1" category
mvcn6$m6q49_c1 <- NA
mvcn6$m6q49_c1[mvcn6$m6q49_c==1] <- 1
mvcn6$m6q49_c1[mvcn6$m6q49_c==2 | mvcn6$m6q49_c==3] <- 0
#table(mvcn6$m6q49_c1, mvcn6$m6q49_c)

#binary var for "2" category
mvcn6$m6q49_c2 <- NA
mvcn6$m6q49_c2[mvcn6$m6q49_c==2] <- 1
mvcn6$m6q49_c2[mvcn6$m6q49_c==1 | mvcn6$m6q49_c==3] <- 0
#table(mvcn6$m6q49_c2, mvcn6$m6q49_c)

#binary var for "3" category
mvcn6$m6q49_c3 <- NA
mvcn6$m6q49_c3[mvcn6$m6q49_c==3] <- 1
mvcn6$m6q49_c3[mvcn6$m6q49_c==1 | mvcn6$m6q49_c==2] <- 0
#table(mvcn6$m6q49_c3, mvcn6$m6q49_c)

#SATISFACTION WITH MVCN--DICHOTOMIZED VARIABLES
#not at all or slightly satisfied (visitmvcn6$m6w36_q72_ns)
visitmvcn6$m6w36_q72_ns <- NA
visitmvcn6$m6w36_q72_ns[visitmvcn6$m6w36_q72==1 | visitmvcn6$m6w36_q72==2] <- 1
visitmvcn6$m6w36_q72_ns[visitmvcn6$m6w36_q72 >= 3] <- 0

#moderately satisfied (visitmvcn6$m6w36_q72_ms)
visitmvcn6$m6w36_q72_ms <- NA
visitmvcn6$m6w36_q72_ms[visitmvcn6$m6w36_q72 <= 2 | visitmvcn6$m6w36_q72 >=4] <- 0
visitmvcn6$m6w36_q72_ms[visitmvcn6$m6w36_q72==3] <- 1

#very or extremely satisfied (visitmvcn6$m6w36_q72_vs)
visitmvcn6$m6w36_q72_vs <- NA
visitmvcn6$m6w36_q72_vs[visitmvcn6$m6w36_q72 <= 3] <- 0
visitmvcn6$m6w36_q72_vs[visitmvcn6$m6w36_q72 >= 4] <- 1

#CHECK NEW VARS
#table(visitmvcn6$m6w36_q72, visitmvcn6$m6w36_q72_ns)
#table(visitmvcn6$m6w36_q72, visitmvcn6$m6w36_q72_ms)
#table(visitmvcn6$m6w36_q72, visitmvcn6$m6w36_q72_vs)









