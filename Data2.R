library(dplyr)
library(ggplot2)
library(quantmod)

sensex = getSymbols("^BSESN",auto.assign = FALSE)
head(sensex,2)
# How do we handle missing data?
sum(is.na(sensex))
# 144/6 = 24 rows. 

sensex[is.na(sensex$BSESN.Open),]

# What to do with missing data?
# 1. Delete, 2. Extrapolate, 3. Retain 

sensex_missing_delete = na.exclude(sensex)

sensex_missing_extrapolate = na.approx(sensex)

sum(is.na(sensex_missing_extrapolate))

dim(sensex_missing_delete)
dim(sensex_missing_extrapolate)


#Obtaining foreign currency data

inr=getSymbols("USD/INR",src='oanda',auto.assign = F)
head(inr)
tail(inr)
chartSeries(inr)

# Get google trends data

library(gtrendsR)

data("categories")

categories

# A data frame with 1426 rows and 2 variables
# id for "all categories" is 0. 

data("countries")
head(countries[countries$name=="INDIA",],1)

df = gtrends(keyword=c("Covid"),geo="IN",time="now 7-d")

str(df)

plot(df)

unemp = gtrends("jobs",geo="IN",time="today+5-y")

plot(unemp)

plot(unemp)

unemp$interest_by_region


