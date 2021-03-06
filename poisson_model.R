# POISSON REGRESSION - INVESTIGATING THE RELATIONSHIP BETWEEN THE TERRAIN SLOPE AND THE OBSERVED COUNT OF BEAVER DAMS ALONG A SLOPE RANGE  
# Roxana Tesileanu (INCDS-Romania)

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
# method = "cubif"for the conditionally unbiased bounded influence estimator
plot(model3) # has very good diagnosis plots! 
coefficients(model3) # Intercept -4907.1189  panta  306.7826 
# ok, the plot 5 shows that the robust model is not plausible for this kind of data.
# in our case the overdispersion is caused mainly due to the inaccuracies produced by a low GIS resolution in the available DEM (pixel width 30m) 
# (Popa M) and also by the phenomenon known as "state dependence" (Kabakoff R p. 315), i.e. we assume that the spot where a dam is present is 
# independent of the other spots of observed presence for the same terrain slope, but in reality there might be a preference of beavers for certain
# terrain slopes, and in the case of beavers it is the 0 to 1 slope (Pasca C).    

 

library("MASS")
model4 <- glm.nb(vizuini~panta)
summary(model4)
plot(model4)
plot(predict(model4, type="response"), residuals(model4, type="deviance"))

# the glm.nb includes a second parameter theta - the clumping parameter (estimated from data to be 1.65269), and uses the log link 
# the glm.nb is another way to deal with overdispersion

# both models, the quasipoisson and negative binomial, behave more or less the same in the diagnosis plots. 
# the plots showing the predicted values and the observed values along the slope range for both models (nb and quasipoisson) indicate 
# a better fit for the nb model

coef(model4) # Intercept  4.7412066  panta -0.2620124 
exp(4.7412066) # 114.5724
exp( 4.7412066  -0.2620124*seq(0,22,1))
exp(0.2620124)
114.5723613 - 88.1636018
88.1636018 - 67.8420222 
# Interpretation coeff. model4: for 0 slope we have about 114 beaver dams (the intercept in this case brings an important amount of information)
# The linear model indicates that for each unit increase in terrain slope we have a decrease in the log no. of dams of about 0.2620124.
# Let's test that to be sure:
log(114.5724)-0.2620124 # 4.479195 which is log(88.1636018), the no. of dams at 1 degree of terrain slope
log(88.1636018) - 0.2620124 # 4.217182 which is log(67.8420222), the no. of dams at 2 degrees of terrain slope
log(67.8420222) 

coef(model2) # 4.6587012  -0.2314341 
exp(4.6587012) # 105.499
# Interpretation coeff. model2: for 0 slope we have 105.499 beaver dams.
# For each unit increase in terrain slope we have a decrease in the log no. of dams of about 0.2314341.


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

#plot 4 (the negative binomial)

newLogMuBn <-  4.7412066  -0.2620124*newXs
newdfBn <- data.frame(newXs, exp(newLogMuBn))
plot(newdfBn)
points(mydata)

# plot 5 (the robust poisson regression)
coefficients(model3) # Intercept -4907.1189  panta  306.7826
newLogMuRob <- -4907.1189 + 306.7826*newXs
newdfRob <- data.frame(newXs, exp(newLogMuRob))
plot(newdfRob)
points(mydata)


# 3) Risk analysis 

# using the negative binomial model and then the quasipoisson model to turn the results into probabilities of occurence along the regression curve

sumDams = sum(vizuini) 
sumDams

vizuini

probDamAlongTerSlope = vizuini/sumDams
probDamAlongTerSlope 
plot(probDamAlongTerSlope)
coef(model4)

# now, similar to model4 (the negative binomial model), log(probDamAlongTerSlope) = Intercept - beta1(vizuini)
# let's rerun the model with probDamAlongTerSlope to see the coefficients:

mydataProb = read.table("/home/roxana/poisson_regression/Poisson_data_prob.txt", header=TRUE)
names(mydataProb)

model5 = glm.nb(mydataProb$probDamAlongTerSlope ~ mydataProb$panta)
warnings(model5) # doesn't like lists as arguments...let's build a data frame
# ok I guess the problem is I want a probability response which is continuous and continuous responses for probabilities are impossible
# because if they have to sum up to 1 it will tear the individual probabilities towards 0. or what? (ask Baruch on that)

summary(model5) 
coef(model5) # Intercept -1.5478736      mydataProb$panta -0.2314343 
# ok now let's plot the model curve on top of the observed probabilities:

plot(newXs, exp(-1.5478736 -0.2314343*newXs))
points(probDamAlongTerSlope)

# maybe a Bayesian analysis would be good with a conjugate Poisson-Gamma model given the inputs of the technical stuff
# on the beaver preference for 0 to 1 terrain slope and the results of the logistic regression indocating a high probability of occurrence of
# beaver dams at water velocities close to 0 (Tesileanu 2016).


# 4) Bayesian analysis for Poisson count data and conjugate Gamma prior (with negative binomial predictive mass function)

# a) estimate the intensity parameter lambda: conjugate analysis of Poisson data (Poisson-Gamma model)   
# b) use the Poisson-Gamma model for predictive purposes: predictive mass function for Poisson count data and conjugate Gamma prior

# a) the Poisson-Gamma model
# the conjugate analysis of Poisson data involves a Poisson likelohood (Poisson distributed observed variable) and a Gamma prior density (with 
# prior beliefs over the intensity parameter lambda) in order to return a posterior Gamma density over lambda.

# defining the likelihood: vizuini ~ Poisson(lambdahatMLE)

lambdahatMLE <- sum(vizuini)/19 # S/n
lambdahatMLE # 26.10526

# so, the likelihood density is vizuini~Poisson(26.10526)

# defining the prior density: p(lambda)~Gamma(a,b)
# from the expertise of our technical stuff (Pasca C), it is assumed that the prior average count a/b=0.5, 
# i.e. the beaver dams occur on average at slope 0.5. this returns a Gamma prior of Gamma(9.5, 19)

# so, the prior density is p(lambda) ~ Gamma(9.5, 19)

# defining the posterior density: p(lambda|vizuini)~Gamma(a*, b*)
# where a* = S+a and b* = b+n

sumVizuini= sum(vizuini)
sumVizuini
aAsterisk = sumVizuini + 9.5
aAsterisk # 505.5
bAsterisk = 19 + 19 
bAsterisk # 38

# so, our posterior density is p(lambda|vizuini)~Gamma(505.5, 38)
# now, I'll use this posterior density to find out the posterior predictive mass function (which is negative binomial) to be able to 
# make predictions on future dam counts along the terrain slope range

# b) posterior predictive mass function for the Poisson-Gamma model
# If y is Poisson distributed count variable with prior/posterior beliefs over lambda represented with a Gamma density, 
# then the predictive mass function for a future observation y_future, p(y_future, y), is a negative binomial density.
# In other words, the success probability theta of y_future given y is given by a negative binomial distribution with the parameters theta and a 
# (the success probability theta and the success count a)

nbTheta = 1 - 1/(bAsterisk +1)
nbTheta # 0.974359


# now, using the prior over lambda as being p(lambda)~Gamma(9.5, 19) and the posterior p(lambda|vizuini)~Gamma(505.5, 38), the 
# p(y_future, y) ~ NBinomial(a, theta) is p(future_vizuini, vizuini)~NBinimial(505.5, 0.974359) -using the posterior Gamma density

# examine the results with some plots:

bayesXs = seq(0, 22, 1)

plot(dpois(bayesXs, lambdahatMLE)) # Poisson likelihood
plot(bayesXs, dgamma(bayesXs, 9.5, 19)) # prior Gamma
plot(bayesXs, dgamma(bayesXs, 505.5, 38)) # posterior Gamma
plot(bayesXs, dnbinom(bayesXs, 505.5, 0.974359)) # posterior negative binomial predictive mass function
plot(bayesXs,dnbinom(bayesXs, 505.5, 0.974359))




# it seams the prior beliefs were updated by the observed data but the difference between the two distributions is still very high and 
# needs another update in the form of new observed data in order to make predictive inferences possible. 

# at this time I've used the conjugacy property of the models. 
# because the data are not ideally distributed (the fitted empirical probability distrib. obtained using the glm.nb function is actually
# as its name suggests a negative binomial one), the overdispersion is present. Overdispersion is a phenomenon with many sources: 
# errors in the observed variables (low resolution of the DEM) or unexplained variation due to additional variables not included in the model 
# (i.e. vegetation cover). Future research should aim at generating a predictive mass function for the counts along the terrain slope range. 
# So, it is important to reduce these two distortion sources in the future, to be able to exploit the Poisson-Gamma model for predictive purposes.


# 5) Conclusions





# 5) References: 
# 1. Kabacoff R I, 2015: R in action - Data analysis and graphics with R,  Second edition, Manning
# 2. Crawley M J, 2013: The R book, Second edition, Wiley 
# 3. Jackman S, 2009: Bayesian analysis for the social sciences, Wiley 
# 4. Quinn G P, Keough M J, 2002: Experimental design and data analysis for biologists, CUP
# 5. R Core Team, 2017, R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna,
#    Austria. URL https://www.R-project.org/
# 6. https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/glm.nb.html (08.06.2017)
# 7. Tesileanu R, 2016: The occurrence of beaver dams in relation to features of the water stream and vegetation characterizing the area,
#    Technical Report INCDS 
# 8. Venables, W N, Ripley B D, 2002: Modern Applied Statistics with S, Fourth edition, Springer
# 9. Wang J, Zamar R, Marazzi A, Yohai V, Salibian-Barrera M, Maronna R, Zivot E, Rocke D, Martin D, Maechler M, Konis K, (2017): 
#    robust: Port of the S+ "Robust Library". R package version 0.4-18, https://CRAN.R-project.org/package=robust
# 10. http://www.statmethods.net/advgraphs/probability.html (10.06.2017)

