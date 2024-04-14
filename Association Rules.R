###################
#Association Rules#
###################

#install the following libraries first. 
library(arules)
library(arulesViz)

#Learning about transaction matrixes

set.seed(13)
tran = matrix(runif(1000)>0.9,ncol=20)
head(tran)
head(tran*1)
inspect(head(as(tran,"itemMatrix")))

item_matrix=as(tran,"itemMatrix")

inspect(item_matrix)
head(as(item_matrix,"ngCMatrix"))
head(as(item_matrix,"matrix"))

#ngCMatrix is transposed

data("Groceries")

summary(Groceries)
inspect(head(Groceries,5))

itemFrequencyPlot(Groceries,topN=10,col=rainbow(10))
itemFrequencyPlot(Groceries,topN=10,col=rainbow(10),horiz=TRUE)


#Use apriori algorithm
#It searches through the item sets that occur frequently in a list of
#transactions. It then retains rules that show confidence above some 
#threshold value. 

grocery_rules=apriori(Groceries,parameter=list(support=0.01,
                                               confidence=0.3,
                                               target='rules'))

grocery_rules=apriori(Groceries,parameter=list(support=0.01,
                                               confidence=0.1,target='rules'))


#values of support and confidence will vary from industry
#to industry. 

#If this is too many or too few, change the support and conf values. 

#Let's fix the minimum lift at 3.

inspect(subset(grocery_rules,lift>3))

### Plotting in Market Basket Analysis

inspectDT(head(grocery_rules))

plot(grocery_rules)

plot(grocery_rules,engine='plotly')

plot(head(grocery_rules),method='graph',engine='htmlwidget')

library(readr)

groceries = as(Groceries,"matrix")
groceries



