#### Reading Files
library(readxl)
library(tidyverse)

## Parsing Data in R

df = read_csv("Toy Data.csv")

head(df,2)

gsub("Rs.","","Rs.100 crores")

gsub(" crores","",gsub("Rs.","","Rs.100 crores"))

as.numeric(gsub(" crores","",gsub("Rs.","","Rs.100 crores")))

df$Revenue = as.numeric(gsub(" crores","",gsub("Rs.","",df$Revenue)))

head(df,2)

df$Profit = as.numeric(gsub(" crores","",df$Profit))
df$ROE = as.numeric(gsub("%","",df$ROE))/100

### An Exercise###
## Open the file Future-500.csv. Clean the file. 


### Another way of cleaning the data
library(lubridate)
df$Date = dmy(df$Date)
head(df,2)

parse_number("$ 100 million")

parse_number("$ 100 crores")

library(janitor)
df = read_csv("janitor.csv")
df

df = clean_names(df)
df

## Managing Missing Data

df = read_csv("missing toy data.csv")
df

df = clean_names(df)

df$credit_rating = na_if(df$credit_rating,"T")
df

df = read_csv("Toy Data.csv")
anyNA(df)

head(df,2)

df %>% complete(CoName,Date) %>% print(n=nrow(df)+1)


# lengthy column names
colNames = c("CompanyName","FinancialYear","YearlyRevenue","YearlyProfitAfterTax")
abbreviate(colNames)

library(snakecase)
to_snake_case(colNames)
to_lower_camel_case(colNames)
to_upper_camel_case(colNames)

library(naniar)
data("riskfactors")

miss_var_summary(riskfactors)
miss_var_table(riskfactors)

x = c(NA,rlnorm(99))
mean(x,na.rm=T)

y = rlnorm(100)

cor(x,y,use='pairwise.complete.obs')

cor(x,y,use='pairwise')

na.exclude(df)

####Missing data Imputation
library(Hmisc)
x = c(10,11,12,NA,13,17,9,20)
impute(x,fun=mean)

###Managing Invalid Data

companies = paste(LETTERS[1:4]," Limited")
assets = c(1000,2000,3000,4000)
liabilities = c(1000,2010,3000,4001)
df = tibble(companies=companies, assets=assets, liabilities = liabilities)
df

library(validate)

check = check_that(df,assets==liabilities)
summary(check)

values(check)
barplot(check)

check = check_that(df,assets==liabilities,assets>0,liabilities>0)
barplot(check)

## Managing Outliers

x = c(1,1,1,2,2,2,3,3,3,1000,10000)

x[abs(scale(x))>3]
mean(x)
sd(x)

boxplot(x,main = "Boxplot")

madn = mad(x)
med = median(x)

abs(x-med)/madn

library(DescTools)
y = c(-1000, -200, -100, 50, 0, 200, 300, 430, 500, 1000)

Winsorize(y, minval=tail(Small(y, k=3), 1), maxval=head(Large(y, k=3), 1))

Winsorize(y)

quantile(y,0.05)
quantile(y,0.95)

mean(y,trim=0.1)
mean(y)
mean(y[c(-1,-10)])

#Single Value Column
company_names = paste("A",1:5,sep=" ")
years = 2016:2020
long = tibble(companies = rep(company_names,times=5),fin_year = rep(years,each=5))
set.seed(13)
long$revenue = rlnorm(25,6.9,5.3)
options(scipen = 10)
long[c(1,2,24,25),]

##Convert to wide form
wide1 = pivot_wider(long,id_cols = 'companies',names_from = 'fin_year',
                    values_from = 'revenue')
wide1

### Multiple Value Columns

company_names = paste("A",1:5,sep=" ")
years = 2016:2020
long1 = tibble(companies = rep(company_names,times=5),fin_year = rep(years,each=5))
set.seed(13)
long1$revenue = rlnorm(25,6.9,5.3)
long1$profit = long$revenue * sample(c(0.2,0.3,0.4),size=25,replace=TRUE)
long1$assets = rlnorm(25,7.09,5.7)
options(scipen = 10)
long1[c(1,2,24,25),]

wide2 = pivot_wider(long1,id_cols = 'companies',names_from = 'fin_year',
                    values_from = c('revenue','profit','assets'))

wide2

names(wide2)
##Convert back to long form
long1 = pivot_longer(data = wide1,cols = '2016':'2020',
                     names_to = 'fin_year',values_to = 'revenue')

head(long1)

long2 = pivot_longer(data=wide2,cols=-1,names_to = c('.value',"fin_year"),
                     names_sep = "_")

head(long2)

### Another Solved Exercise

##Reading Excel Files

df = read_excel("ProwessIQ.xlsx")
df[1:6,1:3]

years = as.numeric(df[4,2:dim(df)[2]])
headers = as.character(df[5,2:dim(df)[2]])

headers = paste(headers,years,sep="_")
headers

df = data.frame(df[6:dim(df)[1],])
names(df)=c("coName",headers)
df_long = pivot_longer(df,cols=!coName,names_to = c("Variable","year"), names_sep="_")
df_long$year = as.Date(as.numeric(df_long$year),origin="1899-12-30")
df_long$value = as.numeric(df_long$value)
head(df_long)

# years = as.Date(years,origin="1899-12-30")
df_wide = pivot_wider(df_long,names_from =Variable )
head(df_wide)

library(tidyverse)

## Method 1 (Filtering the data)
setwd("C:\\Users\\amite\\Desktop\\XLRI - Data Science")
df_500 = read.csv("Future-500.csv")
df_500$Revenue = parse_number(df_500$Revenue)
mean(df_500[df_500$Industry=="Software" & !is.na(df_500$Industry),]$Revenue,na.rm=T)

#Method 2
mean(subset(df_500,Industry=="Software")$Revenue)

#Method 3

df_500 %>% group_by(Industry) %>% summarize(nCompanies=n(),avgRevenue = mean(Revenue,na.rm=T))
#%>% filter(!is.na(Industry))




df_500 %>% group_by(Industry) %>% summarise(nCompanies=n(),avgRevenue = mean(Revenue,na.rm=T)) %>% 
  filter(!is.na(Industry)) %>% ggplot(aes(x=Industry,y=avgRevenue))+geom_col()


library(naniar)
data("riskfactors")
miss_var_summary(riskfactors)
miss_var_table(riskfactors)
x=c(NA,rlnorm(99))
mean(x,na.rm=T)
y=rlnorm(100)

x
mean(x,na.rm=T)
y

cor(x,y)
cor(x,y,use='pairwise.complete.obs')
cor(x,y,use='pairwise')

na.exclude(df)

library(Hmisc)






