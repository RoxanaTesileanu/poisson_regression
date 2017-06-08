# POISSON REGRESSION - INVESTIGATING THE RELATIONSHIP BETWEEN THE TERRAIN SLOPE AND THE OBSERVED COUNT OF BEAVER DAMS ALONG A SLOPE RANGE  


# 1) Regression analysis

mydata <- read.csv("/home/roxana/poisson_data.csv", header=TRUE)
attach(mydata)
names(mydata)

plot(panta, vizuini)
model1 <- glm(vizuini ~ panta , data=mydata, family=poisson()) # Poisson as a pmf with one parameter: the mean (dispersion parameter set to 1)
summary(model1)
deviance(model1)/df.residual(model1) # indicates overdispersion in count data: variance greater than the mean 
model2 <- glm(vizuini ~panta, data = mydata, family= quasipoisson()) 
# using the quasipoisson distribution as underlying distribution for the response variable
# the dispersion parameter is estimated from the data and not set to 1

summary(model2)

library(robust)
model3 <- glmRob(vizuini~panta, family = poisson(), data= mydata)
summary(model3)
# the robust method doesn't deal with the overdispersion (dispersion parameter set to 1)


# 2) Plotting the results



# 3) Conclusions