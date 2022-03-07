## By all respondants then by all network nodes
## by matriliny and patriliny
## by women and men
## N, Age, Education; Degree, Betweenness, Closeness, Transitivity (for two networks)
library(igraph)
library(ynlb)

yn <- list(CM = make_meals_network("yn"), EL = make_equipment_network("yn"))
lb <- list(CM = make_meals_network("lb"), EL = make_equipment_network("lb"))

### Respondants first

### N
## YN Women
sapply(yn, function(x) length(V(x)[which(V(x)$Respondent == TRUE & V(x)$Gender == "F")]))

## YN Men
sapply(yn, function(x) length(V(x)[which(V(x)$Respondent == TRUE & V(x)$Gender == "M")]))

## LB Women
sapply(lb, function(x) length(V(x)[which(V(x)$Respondent == TRUE & V(x)$Gender == "F")]))

## LB Men
sapply(lb, function(x) length(V(x)[which(V(x)$Respondent == TRUE & V(x)$Gender == "M")]))

### Age
## YN
###sapply(yn, function(x) summary(V(x)$Age[which(V(x)$Respondent == TRUE)]))
sapply(yn, function(x) quantile(V(x)$Age[which(V(x)$Respondent == TRUE)], probs = c(.25, .75)))
## LB
##sapply(lb, function(x) summary(V(x)$Age[which(V(x)$Respondent == TRUE)]))
sapply(lb, function(x) quantile(V(x)$Age[which(V(x)$Respondent == TRUE)], probs = c(.25, .75)))

## YN Women
sapply(yn, function(x) summary(V(x)$Age[which(V(x)$Respondent == TRUE & V(x)$Gender == "F")]))

## YN Men
sapply(yn, function(x) summary(V(x)$Age[which(V(x)$Respondent == TRUE & V(x)$Gender == "M")]))

## LB Women
sapply(lb, function(x) summary(V(x)$Age[which(V(x)$Respondent == TRUE & V(x)$Gender == "F")]))

## LB Men
sapply(lb, function(x) summary(V(x)$Age[which(V(x)$Respondent == TRUE & V(x)$Gender == "M")]))

### Network stats?
## Degree
sapply(yn, function(x) summary(degree(x, v = V(x)$Respondent == TRUE, mode = "in")))
sapply(lb, function(x) summary(degree(x, v = V(x)$Respondent == TRUE, mode = "in")))

### All Nodes
### N
## YN Women
sapply(yn, function(x) length(V(x)[which(V(x)$Gender == "F")]))

## YN Men
sapply(yn, function(x) length(V(x)[which(V(x)$Gender == "M")]))

## LB Women
sapply(lb, function(x) length(V(x)[which(V(x)$Gender == "F")]))

## LB Men
sapply(lb, function(x) length(V(x)[which(V(x)$Gender == "M")]))

### Age
lapply(yn, function(x) summary(V(x)$Age))#quantile(V(x)$Age, probs = c(.25, .75))))
lapply(lb, function(x) summary(V(x)$Age))#quantile(V(x)$Age, probs = c(.25, .75))))

## YN Women
sapply(yn, function(x) summary(V(x)$Age[which(V(x)$Gender == "F")]))

## YN Men
sapply(yn, function(x) summary(V(x)$Age[which(V(x)$Gender == "M")]))

## LB Women
sapply(lb, function(x) summary(V(x)$Age[which(V(x)$Gender == "F")]))

## LB Men
sapply(lb, function(x) summary(V(x)$Age[which(V(x)$Gender == "M")]))

## Degree
lapply(yn, function(x) summary(degree(x, mode = "in")))
lapply(lb, function(x) summary(degree(x, mode = "in")))

## Density
lapply(yn, edge_density)
lapply(lb, edge_density)

## Transitivity
lapply(yn, transitivity)
lapply(lb, transitivity)
