#descriptive statistics on perceptions of MVCN at 3-month and 6-month
#install gmodels
install.packages("gmodels")
install.packages("rmarkdown")
install.packages("plotly")
install.packages("DescTools")

#load relevant packages
library(dplyr)
library(gmodels)
library(sas7bdat)
library(rmarkdown)
library(plotly)
library(DescTools)


#set working directory
setwd("~/TAPS online caregiver support/Data/final")

#read in data
wide <- read.sas7bdat("widefile_7dec17.sas7bdat")

#examine number of MVCN participants who completed 6 mo survey
table(wide$ver6)
nrow(wide)

#compute 95% CI around proportion
#compute SE of sample proportion: SE = SQRT((p(1 - p))/n)
#multiply SE by 2 and add and subtract from sample proportion
#CILL = p - 2SE
#CIUL = p + 2se

#compute proportion of people who visit the MVCN website with different frequencies
CrossTable(mvcn6$m6w36_q57_c, prop.c = TRUE)
MultinomCI(mvcn6$m6w36_q57_c, conf.level=0.95)

#CHARACTERISTICS OF MVCN MEMBERS WHO COMPLETED 6-MO SURVEY 
#gender
CrossTable(mvcn6$cmale, prop.c = TRUE)

#race/ethnicity
CrossTable(mvcn6$craceth5, prop.c = TRUE)

#age
CrossTable(mvcn6$cagecat, prop.c = TRUE)

#marital status
CrossTable(mvcn6$cinrel1, prop.c = TRUE)

#highest level of education
CrossTable(mvcn6$cedu, prop.c = TRUE)

#children under 18 in household
CrossTable(mvcn6$m6w6_q92, prop.c = TRUE)

#years as a caregiver
#recode continuous variable to cgdurcat1 using the following categories (in years)
#1 = 1-4 years
#2 = 5-8 years
#3 = 9+ years
CrossTable(mvcn6$cgdurcat1, prop.c = TRUE)

#CHARACTERISTICS OF CARE RECIPIENTS

#care recipient gender
CrossTable(mvcn6$q2, prop.c = TRUE)

#relationship of care recipient to caregiver (q3 = 1 is spouse, 2 = partner or sig. other)
CrossTable(mvcn6$q3, prop.c = TRUE)

#age of care recipient (1 and 2 together make up the under 40 category)
CrossTable(mvcn6$vagecat, prop.c = TRUE)

#care recipient lives in same household with caregiver
CrossTable(mvcn6$q5, prop.c = TRUE)

#era of military service (1 = post-9/11 only, 2 = pre-9/11 only, 3 = both post- and pre-9/11)
CrossTable(mvcn6$v911stat, prop.c = TRUE)

#types of medical conditions diagnosed
#physical conditions (yes/no)
CrossTable(mvcn6$vdxphybw1, prop.c = TRUE)
#psychological conditions (yes/no)
CrossTable(mvcn6$vdxpsybw1, prop.c = TRUE)
#neurological conditions (yes/no)
CrossTable(mvcn6$vdxneurobw1, prop.c = TRUE)
#six or more medical conditions total
CrossTable(mvcn6$vdxsum6w1, prop.c = TRUE)

#ATTRITION ANALYSIS: compare 6-month survey completers to non-completers (sixmocomp) on sample chars
#gender
CrossTable(mvcnonly$cmale, mvcnonly$sixmocomp, fisher=TRUE)
CrossTable(sixmonon$cmale, prop.c = TRUE)

#race/ethnicity
CrossTable(mvcnonly$craceth3, mvcnonly$sixmocomp, chisq=TRUE)
CrossTable(sixmonon$craceth3, prop.c = TRUE)

#age
CrossTable(mvcnonly$cagecat, mvcnonly$sixmocomp, chisq=TRUE)
CrossTable(sixmonon$cagecat, prop.c = TRUE)

#marital status
CrossTable(mvcnonly$cinrel1, mvcnonly$sixmocomp, fisher=TRUE)
CrossTable(sixmonon$cinrel1, prop.c = TRUE)

#highest level of education
CrossTable(mvcnonly$cedu, mvcnonly$sixmocomp, chisq=TRUE)

#children under 18 in household
CrossTable(mvcnonly$m6w6_q92, mvcnonly$sixmocomp, chisq=TRUE)
CrossTable(sixmonon$m6w6_q92, prop.c = TRUE)

