# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
options(digits=2)
inspect(rules[1:5])
summary(rules)
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])


### removing redundant rules

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#### Finding rules with given l.h.s


rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


##### Plotting the graph

library(arulesViz)
plot(rules,method="graph",engine='interactive')
