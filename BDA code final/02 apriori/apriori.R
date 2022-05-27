install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

library(arules)
library(arulesViz)
library(RColorBrewer)

data(Groceries)
Groceries

summary(Groceries)
class(Groceries)

rules = apriori(Groceries, parameter = list(supp = 0.02, conf = 0.2))
summary (rules)


inspect(rules[1:10])

arules::itemFrequencyPlot(Groceries, topN = 20,
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")
itemsets = apriori(Groceries, parameter = list(minlen=2, maxlen=2,support=0.02, target="frequent itemsets"))
summary(itemsets)

inspect(itemsets[1:10])
itemsets_3 = apriori(Groceries, parameter = list(minlen=3, maxlen=3,support=0.02, target="frequent itemsets"))
summary(itemsets_3)
rules <- apriori(Groceries, parameter = list(support=0.02, confidence= 0.4, target ="rules"))
summary(rules)
plot(rules)
inspect(rules)
