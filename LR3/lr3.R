library("arules")
library("arulesViz")

data <- read.delim(file = "lab3_5.csv", sep = ",", header = TRUE, row.names = 1)
dataN <- data[, -11]
isCredit <- data$Кредит
isCredit[isCredit == 1] <- "С кредитом"
isCredit[isCredit == 0] <- "Без кредита"
itemsList <- sapply(1:nrow(dataN), function(i) paste(c(isCredit[i], colnames(dataN[i, dataN[i, ] == 1])), collapse = ",", sep = "\n"))
write(itemsList, file = "basket.csv")

trans <- read.transactions("basket.csv", format = "basket", sep = ",")
print(summary(trans))

windows()
itemFrequencyPlot(trans, cex.names = 0.8) 

rules <- apriori(trans, parameter = list(support = 0.1, confidence = 0.5))
print(summary(rules))
rulesWithCredit <- subset(rules, subset = rhs %in% "С кредитом")
windows()
plot(head(sort(rulesWithCredit, by = "support"), 10), method = "paracoord")
windows()
plot(head(sort(rulesWithCredit, by = "support"), 10), method = "graph",engine = "igraph", control = list(nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1))

rulesWithCredit <- subset(rules, subset = rhs %in% "Без кредита")
windows()
plot(head(sort(rulesWithCredit, by = "support"), 10), method = "paracoord")
windows()
plot(head(sort(rulesWithCredit, by = "support"), 10), method = "graph",engine = "igraph", control = list(nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1))

options(width = 150)
inspect(head(sort(rules, by = "support"), 10))
inspect(head(sort(rules, by = "confidence"), 10))