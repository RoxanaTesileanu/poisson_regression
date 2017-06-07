mydata <- read.csv("/home/roxana/poisson_data.csv", header=TRUE)
attach(mydata)
names(mydata)

plot(panta, vizuini)
model1 <- glm(vizuini ~ panta , data=mydata, family=poisson())
summary(model1)
deviance(model1)/df.residual(model1)
model2 <- glm(vizuini ~panta, data = mydata, family= quasipoisson())
summary(model2)

library(robust)
model3 <- glmRob(vizuini~panta, family = poisson(), data= mydata)
summary(model3)
