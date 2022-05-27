install.packages("ISLR")
library(ISLR)

data <- ISLR::Default
print (head(ISLR::Default))

summary(data)

nrow(data)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
print (sample)
train <- data[sample, ]
test <- data[!sample, ]
nrow(train)
nrow(test)

model <- glm(default~student+balance+income, family="binomial", data=train)

summary(model)

install.packages("InformationValue")
library(InformationValue)
predicted <- predict(model, test, type="response")
test$default <- ifelse(test$default=="Yes", 1, 0)
optimal <- optimalCutoff(test$default, predicted)[1]
confusionMatrix(test$default, predicted)

sensitivity(test$default, predicted)

specificity(test$default, predicted)


misClassError(test$default, predicted, threshold = optimal)
