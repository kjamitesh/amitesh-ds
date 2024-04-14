# 1.Theory Behind Chart Creation

# 2. High level and low level plotting functions

# 3. R has a graphics package that automatically gets imported when we open R. 

# First call a high-level function to create a plot and then call low-level functions to add more features to the plot. 

set.seed(13)
x = rnorm(1000000)

plot(x,type='l') #type can be p (point), l(line), b(both), h(height), s(step)
# type o for o-sized plints and line

plot(x,type='l',col = 'red')
plot(x,type='l',col = 'red',xlab='X-axis',ylab='Y-axis',main='Heading')

#additional arguments: lwd (line width), lty (line type),xlim(x limit), ylim

### Plot function is very generic and depending on the type of data, it produces different outputs. 

data("pressure")

head(pressure,2)

plot(pressure$temperature,pressure$pressure,type='l',lwd=2,col='blue')

df = data.frame(x = rnorm(100),y=sample(c("Male","Female"),100,replace=TRUE))

plot(x=as.factor(df$y),y=df$x)

### Adding another plot
y = rlnorm(1000000)
plot(y,type='l',col='green') #It removed the earlier plot

plot(x,type='l',col = 'red',xlab='X-axis',ylab='Yaxis',main='Heading')

lines(y,col='green')

plot(x,type='l',col = 'red',xlab='X-axis',ylab='Yaxis',main='Heading',
     ylim = c(-2.5,18))

lines(y,col='green')

#Easy to create functional graphics
curve(x^3 - 5*x^2 + 6,from = -10, to = +10,col='red')

##Create histogram

hist(x)
hist(x,col='blue')

##Add a density plot
hist(x)
plot(density(x))
polygon(density(x),col='red')

hist(x)
lines(density(x)) #Not visible

hist(x,freq=F)
lines(density(x))

##
boxplot(x)

## If x is a factor, plot() produces a barplot. 
x = c(rep('Male',10),rep('Female',15))
x = factor(x,levels=c('Male',"Female"))
plot(x)

##Barplot for numeric values
x = c(Male=10,Female=15)
barplot(x)
pie(x)

###Two dimensional Plots
x = seq(0,6*pi,length=100)
sinx = sin(x)
cosx = cos(x)
plot(x,sinx,type='l',col='red')

# Exercise: Add the cosx plot on this plot
lines(x,cosx,col='blue')

###Plotting with two factors
df = data.frame(Gender = sample(c("Male","Female"),100,replace = TRUE),
                Age=sample(c("Young","Old"),100,replace=TRUE))
head(df)
plot(as.factor(df$Gender),
     as.factor(df$Age),col=c('red','blue'))

##Getting multiple graphics
par(mfrow=c(2,2))
hist(x,freq=F)
plot(density(x))
polygon(density(x),col='red')
plot(x,type='l')
hist(y)

par(mfrow=c(1,1)) #Reset the default
par(col='red',lty='dashed') #Changing default in basic plots. Rarely used
plot(x,type='l')

#Another way of plotting multiple plots
layout(matrix(c(1,2),byrow=T))
x = rnorm(100)
y = 0.6 + 2.3 * x * rnorm(100)
plot(y~x)
hist(y)

par(mfrow=c(1,1)) 

#####Creating plots using ggplot
library(ggplot2)
library(readxl)
ad = read_excel("chart-intro.xlsx",sheet=1)
head(ad)

ggplot(ad,aes(x=Sales,y=Advertisement))

ggplot(ad,aes(x=Sales,y=Advertisement))+geom_point(color='red')
ggplot(ad,aes(x=Sales,y=Advertisement))+geom_point(aes(color=Year))

ggplot(ad,aes(x=Sales))+geom_boxplot()

# Suppose, we have a vector. How to draw a plot?
x = rnorm(100)
ggplot(data.frame(x = x),aes(x))+geom_histogram()

#Find relationship between two variables

ggplot(ad,aes(x=Advertisement,y=Sales))+geom_point()+geom_smooth()
ggplot(ad,aes(x=Advertisement,y=Sales))+geom_point()+geom_smooth(method='lm')
ggplot(ad,aes(x=Advertisement,y=Sales))+geom_point()+geom_smooth(method='lm',se=F)

#Bar Graph
df = data.frame(Time = c(1:5,7),Demand = c(10,12,15,20,25,27))
ggplot(df,aes(x=Time,y=Demand))+geom_bar(stat='identity',fill='lightblue')

ggplot(df,aes(x=factor(Time),y=Demand))+geom_bar(stat='identity',fill='indianred')

###Analyzing GDP Data

gdp = read_excel("tableau-intro.xlsx",
                 sheet=5,col_types = c("text","numeric","numeric"))
head(gdp,1)

gdp1 = subset(gdp,Country %in% c("India","United States","China"))

head(gdp1)

ggplot(gdp1,aes(x=Year,y=GDP,color=Country))+geom_line()

ggplot(gdp1,aes(x=Year,y=GDP))+geom_line()+facet_wrap(~Country)

###Small Case Study
library(ggthemes) #for color blind palette
library(GDAdata)
data(SpeedSki)

head(SpeedSki)

ggplot(SpeedSki,aes(x=Speed,fill=Sex))+xlim(160,220)+
  geom_histogram(binwidth=2.5)+xlab("Speed (Km/hr)")+facet_wrap(~Sex,ncol=1)+ylab("")+
  theme(legend.position="none")

ggplot(SpeedSki,aes(x=Speed,fill=Sex))+xlim(160,220)+
  geom_histogram(binwidth=2.5)+xlab("Speed (Km/hr)")+facet_grid(Sex~Event)+ylab("")+
  theme(legend.position="none")

## Another case study
library(gridExtra) #for arranging graphics drawn using ggplot

ucba = as.data.frame(UCBAdmissions)
head(ucba)

a = ggplot(ucba,aes(Dept))+geom_bar(aes(weight=Freq))
b = ggplot(ucba,aes(Gender))+geom_bar(aes(weight=Freq))
c = ggplot(ucba,aes(Admit))+geom_bar(aes(weight=Freq))

grid.arrange(a,b,c,nrow=1,widths=c(7,3,3))


library(vcd) #graphics for categorical data

ucb = within(ucba, Accept <- factor(Admit,levels=c("Rejected","Admitted")))

doubledecker(xtabs(Freq ~ Dept + Gender + Accept, data=ucb),
             gp = gpar(fill = c("grey90","steelblue")))



