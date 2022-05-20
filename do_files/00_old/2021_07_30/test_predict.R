lm3 <- lm(Sepal.Length ~ Species, data=iris)
summary(lm3)

newdata <- data.frame(Species=factor("setosa", levels=c("setosa", "versicolor", "virginica")))
predict(lm3, newdata)

newdata <- data.frame(Species=factor("versicolor", levels=c("setosa", "versicolor", "virginica")))
predict(lm3, newdata)
