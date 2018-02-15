#descriptive statistics on perceptions of MVCN at 3-month and 6-month
#install gmodels
install.packages("gmodels")
install.packages("rmarkdown")
install.packages("plotly")
install.packages("knitr")
#load relevant packages
library(dplyr)
library(gmodels)
library(sas7bdat)
library(rmarkdown)
library(plotly)
library(DescTools)
library(psych)
library(knitr)

#set working directory
setwd("~/TAPS online caregiver support/Data/final")

#read in data
wide <- read.sas7bdat("widefile_7dec17.sas7bdat")

#compute 95% CI around proportion
#compute SE of sample proportion: SE = SQRT((p(1 - p))/n)
#multiply SE by 2 and add and subtract from sample proportion
#CILL = p - 2SE
#CIUL = p + 2se

#FREQUENCY OF PARTICIPATION IN ONLINE OR IN-PERSON STRUCTURED SOC SUPP GRPS FOR CAREGIVERS
CrossTable(mvcn6$q49, prop.c = TRUE)
table(mvcn6$m6w36_q54_1, mvcn6$m6w36_q54_2)
CrossTable(mvcn6$m6w36_q54_1, prop.c = TRUE)

#FREQUENCY OF MVCN WEBSITE VISITATION
#1 = have not visited MVCN since joining (or in past 3 months)
#2 = once a month or less
#3 = 2-3 times a month
#4 = once a week or more

#compute proportion of people who visit the MVCN website with different frequencies
CrossTable(mvcn3$m3w36_q57_c, prop.c = TRUE)
CrossTable(mvcn6$m6w36_q57_c, prop.c = TRUE)
describe(mvcn6$m6w36_q57_c1)
describe(mvcn6$m6w36_q57_c2)
describe(mvcn6$m6w36_q57_c3)
describe(mvcn6$m6w36_q57_c4)


#DURATION OF MVCN WEBSITE VISITATION
#0 = has not visited MVCN website in past 3 mos
#1 = usually spent < 10 min on MVCN website
#2 = usually spent 10-20 min on MVCN website
#3 = usually spent 20-30 min on MVCN website
#4 = usually spent >= 30 min on MVCN website

#compute proportions of people who spend different amounts of time on MVCN's website
#out of those who have visited MVCN site at least once in last 3 mos
CrossTable(visitmvcn3$m3w36_q58_c, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q58_c, prop.c = TRUE)

#compute SEs for duration of site visitation 6 mos
describe(visitmvcn6$m6w36_q58_c1)
describe(visitmvcn6$m6w36_q58_c2)
describe(visitmvcn6$m6w36_q58_c3)
describe(visitmvcn6$m6w36_q58_c4)


#TYPES OF ACTIVITIES CONDUCTED IN MVCN
#compute proportion of people who post comments, questions, or links to MVCN network
CrossTable(visitmvcn3$m3w36_q59, prop.C = TRUE)
CrossTable(visitmvcn6$m6w36_q59_b, prop.C = TRUE)
describe(visitmvcn6$m6w36_q59_b)

#frequencies of posting comments, questions, or links to MVCN network
CrossTable(visitmvcn3$m3w36_q60, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q60, prop.c = TRUE)

#ACTIVITIES IN MVCN NETWORK

#CAREGIVING TASKS
#posted to requests for info about caregiving tasks?
CrossTable(visitmvcn3$m3w36_q61_1_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_1_1, prop.c = TRUE)
#read requests for info about caregiving tasks?
CrossTable(visitmvcn3$m3w36_q61_1_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_1_2, prop.c = TRUE)
#responded to requests for info about caregiving tasks?
CrossTable(visitmvcn3$m3w36_q61_1_3, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_1_3, prop.c = TRUE)

#percentages of people who posted, read, or responded to requests for info about CG tasks
#1 = posted, read, or responded to info
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_1_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_1_123, prop.c = TRUE)

#RESOURCES FOR CAREGIVING
#3-month
CrossTable(visitmvcn3$m3w36_q61_2_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_2_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_2_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_2_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_2_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_2_3, prop.c = TRUE)


#percentages of people who posted, read, or responded to requests for info about CG RESOURCES
#1 = posted, read, or responded to info
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_2_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_2_123, prop.c = TRUE)

#EMOTIONAL SUPPORT AND/OR PRAYER
#3-month
CrossTable(visitmvcn3$m3w36_q61_3_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_3_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_3_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_3_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_3_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_3_3, prop.c = TRUE)

#percentages of people who posted, read, or responded to requests for EMOTIONAL
#SUPPORT AND/OR PRAYER
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_3_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_3_123, prop.c = TRUE)

#INFO ABOUT VA OR DOD BENEFITS FOR CARE RECIPIENT
#3-month
CrossTable(visitmvcn3$m3w36_q61_4_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_4_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_4_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_4_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_4_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_4_3, prop.c = TRUE)

#combine posting, reading, and responding to requests for info about VA or DoD 
#benefits for care recipient into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_4_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_4_123, prop.c = TRUE)

#INFO OR REQUESTS ABOUT NON-CAREGIVING TOPICS

#3-month
CrossTable(visitmvcn3$m3w36_q61_5_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_5_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_5_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_5_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_5_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_5_3, prop.c = TRUE)

#combine posting, reading, and responding to requests for info about 
#non-caregiving topics into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_5_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_5_123, prop.c = TRUE)

#ADVICE OR INFO IN RESPONSE TO ANOTHER MEMBER'S POST
#3-month
CrossTable(visitmvcn3$m3w36_q61_6_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_6_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_6_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_6_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_6_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_6_3, prop.c = TRUE)

#combine posting, reading, and responding to advice or info in response to 
#another member's post into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_6_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_6_123, prop.c = TRUE)

#EMOTIONAL SUPPORT OR SOLIDARITY WITH ANOTHER MEMBER'S POST
#3-month
CrossTable(visitmvcn3$m3w36_q61_7_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_7_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_7_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_7_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_7_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_7_3, prop.c = TRUE)

#combine posting, reading, and responding to emotional support or solidarity 
#with another member's post into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_7_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_7_123, prop.c = TRUE)

#LINKS TO INFO OR RESOURCES FOR CAREGIVERS
#3-month
CrossTable(visitmvcn3$m3w36_q61_8_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_8_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_8_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_8_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_8_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_8_3, prop.c = TRUE)

#combine posting, reading, and responding to links to info or resources for CGs
#into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_8_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_8_123, prop.c = TRUE)

#INFO OR LINKS TO INFO ABOUT VA OR DOD BENEFITS FOR CARE RECIPIENTS
#3-month
CrossTable(visitmvcn3$m3w36_q61_9_1, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_9_2, prop.c = TRUE)
CrossTable(visitmvcn3$m3w36_q61_9_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_9_1, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_9_2, prop.c = TRUE)
CrossTable(visitmvcn6$m6w36_q61_9_3, prop.c = TRUE)

#combine posting, reading, and responding to info or links to info 
#about VA or DoD benefits for care recipients into the same variables at 3- and 6-months
#1 = posted, read, or responded to request
#0 = did none of those

#3-month
CrossTable(visitmvcn3$m3w36_q61_9_123, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q61_9_123, prop.c = TRUE)

#compute frequency distribution for number of types of posts and responses at 6-mo
CrossTable(visitmvcn6$numposttype6, prop.c = TRUE)

#ACTIVITIES: USE OF MVCN CONTENT AND FEATURES

#Accessing info and resources from the website's resource library

#3-month
CrossTable(visitmvcn3$m3w36_q62_1, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q62_1, prop.c = TRUE)
describe(visitmvcn6$m6w36_q62_1)

#Joining an interest group (e.g., PTSD CGs)

#3-month
CrossTable(visitmvcn3$m3w36_q62_2, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q62_2, prop.c = TRUE)
describe(visitmvcn6$m6w36_q62_2)

#Attending a webinar hosted by MVCN

#3-month
CrossTable(visitmvcn3$m3w36_q62_3, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q62_3, prop.c = TRUE)
describe(visitmvcn6$m6w36_q62_3)

#Attending a webchat online

#3-month
CrossTable(visitmvcn3$m3w36_q62_4, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q62_4, prop.c = TRUE)
describe(visitmvcn6$m6w36_q62_4)

#Interacting with other members outside of the MVCN online network

#3-month
CrossTable(visitmvcn3$m3w36_q71, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q71_b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q71_b)

#COMPUTE UNIVARIATE DESCRIPTIVE STATS FOR PERCEPTIONS OF MVCN SITE

#SITE INSPIRES ME IN MY OWN LIFE (1)

#3-month
CrossTable(visitmvcn3$m3w36_q73_1b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_1r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_1r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_1b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_1b)
mean(visitmvcn6$m6w36_q73_1r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_1r, na.rm=TRUE)

#SITE MAKES ME THINK OF THINGS IN NEW, MORE POSITIVE WAYS (2)

#3-month
CrossTable(visitmvcn3$m3w36_q73_2b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_2r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_2r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_2b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_2b)
mean(visitmvcn6$m6w36_q73_2r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_2r, na.rm=TRUE)

#USING THIS SITE MAKES A DIFFERENCE IN MY LIFE (3)

#3-month
CrossTable(visitmvcn3$m3w36_q73_3b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_3r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_3r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_3b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_3b)
mean(visitmvcn6$m6w36_q73_3r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_3r, na.rm=TRUE)

#I AM A BETTER PERSON FOR USING THIS SITE (4)

#3-month
CrossTable(visitmvcn3$m3w36_q73_4b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_4r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_4r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_4b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_4b)
mean(visitmvcn6$m6w36_q73_4r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_4r, na.rm=TRUE)

#SITE HELPS ME MAKE GOOD CAREGIVING DECISIONS (5)

#3-month
CrossTable(visitmvcn3$m3w36_q73_5b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_5r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_5r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_5b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_5b)
mean(visitmvcn6$m6w36_q73_5r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_5r, na.rm=TRUE)

#HAVE LEARNED HOW TO IMPROVE MYSELF FROM THIS SITE (6)

#3-month
CrossTable(visitmvcn3$m3w36_q73_6b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_6r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_6r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_6b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_6b)
mean(visitmvcn6$m6w36_q73_6r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_6r, na.rm=TRUE)

#SITE PROVIDES INFO THAT HELPS ME MAKE IMPORTANT DECISIONS (7)

#3-month
CrossTable(visitmvcn3$m3w36_q73_7b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_7r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_7r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_7b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_7b)
mean(visitmvcn6$m6w36_q73_7r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_7r, na.rm=TRUE)

#SITE HELPS ME BETTER MANAGE MY TIME AND RESOURCES (8)

#3-month
CrossTable(visitmvcn3$m3w36_q73_8b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_8r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_8r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_8b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_8b)
mean(visitmvcn6$m6w36_q73_8r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_8r, na.rm=TRUE)

#I'M AS INTERESTED IN INPUT FROM OTHER USERS AS I AM IN RESOURCES PROVIDED BY SITE (9)

#3-month
CrossTable(visitmvcn3$m3w36_q73_9b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_9r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_9r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_9b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_9b)
mean(visitmvcn6$m6w36_q73_9r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_9r, na.rm=TRUE)

#SITE DOES A GOOD JOB OF GETTING ITS VISITORS TO CONTRIBUTE OR PROVIDE FEEDBACK (10)

#3-month
CrossTable(visitmvcn3$m3w36_q73_10b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_10r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_10r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_10b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_10b)
mean(visitmvcn6$m6w36_q73_10r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_10r, na.rm=TRUE)

#I HAVE LEARNED A LOT FROM POSTS OF OTHER CGS WHO VISIT THIS SITE (11)

#3-month
CrossTable(visitmvcn3$m3w36_q73_11b, prop.c = TRUE)
mean(visitmvcn3$m3w36_q73_11r, na.rm=TRUE)
sd(visitmvcn3$m3w36_q73_11r, na.rm=TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q73_11b, prop.c = TRUE)
describe(visitmvcn6$m6w36_q73_11b)
mean(visitmvcn6$m6w36_q73_11r, na.rm=TRUE)
sd(visitmvcn6$m6w36_q73_11r, na.rm=TRUE)

#Overall satisfaction with MVCN

#3-month
CrossTable(visitmvcn3$m3w36_q72, prop.c = TRUE)
mean(visitmvcn3$m3w36_q72)
sd(visitmvcn3$m3w36_q72)

#6-month
CrossTable(visitmvcn6$m6w36_q72, prop.c = TRUE)
mean(visitmvcn6$m6w36_q72)
sd(visitmvcn6$m6w36_q72)

#compute SEs
#not at all or slightly satisfied (1) vs. all other (0)
describe(visitmvcn6$m6w36_q72_ns)
#moderately satisfied (1) vs. all other (0)
describe(visitmvcn6$m6w36_q72_ms)
#very or extremely satisfied (1) vs. all other (0)
describe(visitmvcn6$m6w36_q72_vs)



#REASONS WHY PEOPLE DON'T USE MVCN SITE

#COMPUTE UNIVARIATE DESCRIPTIVE STATS 


#INFO GIVEN BY OTHER USERS IS NOT ACCURATE (1)

#3-month
CrossTable(mvcn3$m3w36_q74_1b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_1r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_1r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_1b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_1b)
mean(mvcn6$m6w36_q74_1r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_1r, na.rm=TRUE)

#INFO GIVEN BY OTHER USERS IS NOT USEFUL (2)

#3-month
CrossTable(mvcn3$m3w36_q74_2b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_2r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_2r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_2b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_2b)
mean(mvcn6$m6w36_q74_2r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_2r, na.rm=TRUE)

#DIFFICULT TO FIND WHAT YOU NEED ON THE WEBSITE (3)

#3-month
CrossTable(mvcn3$m3w36_q74_3b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_3_r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_3_r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_3b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_3b)
mean(mvcn6$m6w36_q74_3r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_3r, na.rm=TRUE)

#OTHER USERS ARE NOT WELCOMING/FRIENDLY (4)

#3-month
CrossTable(mvcn3$m3w36_q74_4b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_4r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_4r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_4b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_4b)
mean(mvcn6$m6w36_q74_4r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_4r, na.rm=TRUE)

#DON'T HAVE A LOT IN COMMON WITH OTHER USERS (5)

#3-month
CrossTable(mvcn3$m3w36_q74_5b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_5r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_5r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_5b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_5b)
mean(mvcn6$m6w36_q74_5r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_5r, na.rm=TRUE)

#A LOT OF GOSSIP POSTED BY OTHER USERS (6)

#3-month
CrossTable(mvcn3$m3w36_q74_6b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_6r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_6r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_6b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_6b)
mean(mvcn6$m6w36_q74_6r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_6r, na.rm=TRUE)

#A LOT OF SNIPING/ATTACKING OF PEOPLE WHO POST (7)

#3-month
CrossTable(mvcn3$m3w36_q74_7b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_7r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_7r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_7b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_7b)
mean(mvcn6$m6w36_q74_7r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_7r, na.rm=TRUE)

#NOT ENOUGH ACTIVITY ON THE SITE (8)

#3-month
CrossTable(mvcn3$m3w36_q74_8b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_8r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_8r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_8b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_8b)
mean(mvcn6$m6w36_q74_8r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_8r, na.rm=TRUE)

#SITE IS DIFFICULT TO USE (9)

#3-month
CrossTable(mvcn3$m3w36_q74_9b, prop.c = TRUE)
mean(mvcn3$m3w36_q74_9r, na.rm=TRUE)
sd(mvcn3$m3w36_q74_9r, na.rm=TRUE)

#6-month
CrossTable(mvcn6$m6w36_q74_9b, prop.c = TRUE)
describe(mvcn6$m6w36_q74_9b)
mean(mvcn6$m6w36_q74_9r, na.rm=TRUE)
sd(mvcn6$m6w36_q74_9r, na.rm=TRUE)

#Participation in caregiver support groups other than MVCN

#A military or veteran caregiver group on Facebook (other than specifically named programs)

#3-month
CrossTable(mvcn3$m3q48_3, prop.c = TRUE)

#6-month
CrossTable(mvcn6$m6q48_3, prop.c = TRUE)
describe(mvcn6$m6q48_3)

#Number of in-person caregiving support groups other than MVCN

#clean up variables
#recoded variable is:
#0 = none
#1 = 1
#2 = 2
#3 = 3 or more

#3-month
CrossTable(mvcn3$m3w36_q54_1_c, prop.c = TRUE)

#6-month
CrossTable(mvcn6$m6w36_q54_1_c, prop.c = TRUE)
describe(mvcn6$m6w36_q54_1_c0)
describe(mvcn6$m6w36_q54_1_c1)
describe(mvcn6$m6w36_q54_1_c2)
describe(mvcn6$m6w36_q54_1_c3)


#Number of online only caregiving support groups other than MVCN

#clean up variables
#recoded variable is:
#0 = none
#1 = 1
#2 = 2
#3 = 3 or more

#3-month
CrossTable(mvcn3$m3w36_q54_2_c, prop.c = TRUE)

#6-month
CrossTable(mvcn6$m6w36_q54_2_c, prop.c = TRUE)
describe(mvcn6$m6w36_q54_2_c0)
describe(mvcn6$m6w36_q54_2_c1)
describe(mvcn6$m6w36_q54_2_c2)
describe(mvcn6$m6w36_q54_2_c3)

#FREQUENCY OF PARTICIPATION IN CAREGIVER SUPPORT GROUPS OTHER THAN MVCN
#1 = once a week or more (1, 2)
#2 = one to three times a month (3, 4)
#3 = every few months or less (5, 6, 7)

#3-month
CrossTable(mvcn3$m3q49_c, prop.c = TRUE)

#6-month
CrossTable(mvcn6$m6q49, prop.c = TRUE)
CrossTable(mvcn6$m6q49_c, prop.c = TRUE)
describe(mvcn6$m6q49_c1)
describe(mvcn6$m6q49_c2)
describe(mvcn6$m6q49_c3)

#monitor several different military cg websites to get info, resources, and support I need
CrossTable(mvcn6$m6w6_q56_2b, prop.c = TRUE)
describe(mvcn6$m6w6_q56_2b)

#PERCEPTIONS OF RESOURCES FOR MILITARY CAREGIVERS
#6-month only

##too many online groups for military caregivers
CrossTable(mvcn6$m6w6_q56_1b, prop.c = TRUE)
describe(mvcn6$m6w6_q56_1b)

#more online peer support groups for caregivers are needed
CrossTable(mvcn6$m6w6_q56_3b, prop.c = TRUE)
describe(mvcn6$m6w6_q56_3b)

#more in-person peer support groups for caregivers are needed
CrossTable(mvcn6$m6w6_q56_4b, prop.c = TRUE)
describe(mvcn6$m6w6_q56_4b)

#online resources are needed that provide specific types of help for military caregivers
CrossTable(mvcn6$m6w6_q56_5b, prop.c = TRUE)
describe(mvcn6$m6w6_q56_5b)

#resources providing info and support for military cgs need to be located in one central place online
CrossTable(mvcn6$m6w6_q56_6b, prop.c = TRUE)
describe(mvcn6$m6w6_q56_6b)

#PEER MENTOR AND MODERATOR PROGRAM


#Serving as a trained MVCN Peer Mentor 

#3-month
CrossTable(visitmvcn3$m3w36_q62_5, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q62_5, prop.c = TRUE)

#Serving as a trained MVCN Peer Moderator
#3-month
CrossTable(visitmvcn3$m3w36_q62_6, prop.c = TRUE)

#6-month
CrossTable(visitmvcn6$m6w36_q62_6, prop.c = TRUE)


