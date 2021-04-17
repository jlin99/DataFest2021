#Data Exploration
library(tidyverse)

#Load in data
df_us18 <- read.csv("/Users/egghead/Desktop/datafest/data/US/us18.csv")

ggplot(df_us18, aes(x=DEM_AGE)) + geom_histogram(binwidth=1)
#Age distribution is almost normal? Definitely drops off around 65 ish
ggplot(df_us18, aes(x=DEM_GENDER)) + geom_histogram()
#Gender is exactly even

sum(df_us18$DEM_STDNT)/nrow(df_us18)
#About 9% are students
sum(df_us18$DEM_VET)/nrow(df_us18)
#About 10% are vets
sum(df_us18$DEM_HEALTH)/nrow(df_us18)
#About 5% are healthcare workers 

sum(df_us18$DEM_HISPANIC)/nrow(df_us18)
#About 15% are hispanic
ggplot(df_us18, aes(x=DEM_RACE)) + geom_histogram()
#Large majority of people are caucasian

ggplot(df_us18, aes(x=DEM_INCOME)) + geom_histogram()
#Large chunk of people make over 100k 
#Inverted normal distribution ish 

ggplot(df_us18, aes(x=DEM_MARITAL)) + geom_histogram()
#About half are married, next largest chunk is never marreid

ggplot(df_us18, aes(x=DEM_EDU)) + geom_histogram()
#About what we would expect. Biggest chunk is undergrad, next biggest are college dropouts and high school graduates

#Let's make tables for drug usage
df_drugs <- df_us18 %>%
  select(29:62)
numYes <- colSums(df_drugs, na.rm=TRUE)
numNonNAs <- sapply(df_drugs, function(y) nrow(df_drugs) - sum(length(which(is.na(y)))))
drugPct <- numYes / numNonNAs
drugPct <- as.data.frame(drugPct)
drugPct["Question"] <- rownames(drugPct)
#Note that NMUs are percent of people who said yes to the USE question

ggplot(drugPct, aes(x=Question, y = drugPct)) + 
  geom_bar(stat="identity") + 
  coord_flip()
#A little hard to read, so I'm going to split it into two graphs 

#First USE
drugUSE <- drugPct[seq(1,34, 2),]
ggplot(drugUSE, aes(x=Question, y = drugPct)) + 
  geom_bar(stat="identity") + 
  coord_flip()
#Codeine, Hydrocodone, and Oxycodone are used most often

#Next, let's look at NMU (Non medical drug usage)
drugNMU <- drugPct[seq(2,34, 2),]
ggplot(drugNMU, aes(x=Question, y = drugPct)) + 
  geom_bar(stat="identity") + 
  coord_flip()
#As expected, THC has some of the highest non medical drug usage out of people who have done said drug
#Interestingly, the highest ones in the first chart are all really low here 

#Lastly, I want to look at distribution of drug abusers 
ggplot(df_us18, aes(x=DAST_SUM)) + geom_histogram()
#The higher the number, the more severe the drug abuse 
#As expected, majority of people fall under little to no drug abuse

#Curious what the most common checked off question out of people with low level risk 
df_lowDAST <- select(df_us18, )

