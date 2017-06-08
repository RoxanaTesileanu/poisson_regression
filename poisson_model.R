# POISSON REGRESSION - INVESTIGATING THE RELATIONSHIP BETWEEN THE TERRAIN SLOPE AND THE OBSERVED COUNT OF BEAVER DAMS ALONG A SLOPE RANGE  


# 1) Regression analysis

mydata <- read.csv("/home/roxana/poisson_data.csv", header=TRUE)
attach(mydata)
names(mydata)
summary(mydata)
plot(panta, vizuini)
model1 <- glm(vizuini ~ panta , data=mydata, family=poisson()) # Poisson as a pmf with one parameter: the mean (dispersion parameter set to 1)
summary(model1)
deviance(model1)/df.residual(model1) # indicates overdispersion in count data: variance greater than the mean 
model2 <- glm(vizuini ~panta, data = mydata, family= quasipoisson()) 
# using the quasipoisson distribution as underlying distribution for the response variable
# the dispersion parameter is estimated from the data and not set to 1

summary(model2)
plot(model2)

plot(predict(model2, type="response"), residuals(model2, type="deviance"))
plot(hatvalues(model2))
plot(rstudent(model2))
plot(cooks.distance(model2))



library(robust)
model3 <- glmRob(vizuini~panta, family = poisson(), data= mydata)
summary(model3)
# the robust method doesn't deal with the overdispersion (dispersion parameter set to 1)

library("MASS")
model4 <- glm.nb(vizuini~panta)
summary(model4)
plot(model4)


# 2) Plotting the results

#plot 1
pathToPlot <- "/home/roxana/poisson_regression/plot1.bmp"
bmp(pathToPlot, width = 500, height = 500)
plot(panta, vizuini, main= "Distribution of beaver dam counts along the slope range", ylab= "nr. vizuini", xlab= "panta")
dev.off()

#plot 2 
newXs <- seq(0,22,0.1)
newXs
coef(model2)
newLogMu <- 4.6587012 -0.2314341*newXs
newLogMu
newdf <- data.frame(newXs, newLogMu)
plot(newdf)

newdf2 <- data.frame(newXs, exp(newLogMu))
plot(newdf2)
points(mydata)

#plot 3

plot(newdf)
points(log(vizuini))

# 3) Conclusions



# 4) References: 
# R I Kabacoff, 2015: R in action - Data analysis and graphics with R,  second edition, Manning
# M J Crawley, 2013: The R book, second edition, Wiley 
# G P Quinn, M J Keough, 2002: Experimental design and data analysis for biologists, CUP

