Appendix A
################################ # BAMS 507
# Final project
# Hossein Piri ################################
#############
# IMPORT DATA
#############
mydata <- read.csv(file.choose(), header=TRUE)
str(mydata) summary(mydata)
#############
# (1.2) Creating scatterplots of the response variable against each explanatory variable #############
# sale price vs. lot size
#plot(sale.price/1000 ~ lot.size, data=mydata,ylab= "Sale price in thousands of dollars", xlab="Lot size in acres", pch = 1)
# Lowess line
plot(sale.price ~ lot.size, data=mydata,ylab= "Sale price in dollars", xlab="Lot size in acres", pch = 1)
x1 <- mydata$lot.size[order(mydata$lot.size)] #put the x-values in order
y1 <- mydata$sale.price[order(mydata$lot.size)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=0.1), col="red")
abline(v=0.5, col='red', lty=2) # This helps us figure out the location of pick
abline(h=235000, col='red', lty=2)
# sale price vs. living area
#plot(sale.price/1000 ~ living.area, data=mydata,ylab= "Sale price in thousands of dollars", xlab="living area in square feets", pch = 1)
# Lowess line
plot(sale.price ~ living.area, data=mydata,ylab= "Sale price in dollars", xlab="living area in square feets", pch = 1)
x2 <- mydata$living.area[order(mydata$living.area)] #put the x-values in order
y2 <- mydata$sale.price[order(mydata$living.area)] # put the y-values in order based on the order of the x-values
lines(lowess(x2, y2, delta=0.1), col="red")
# sale price vs. number of bedrooms
#plot(sale.price/1000 ~ bedrooms, data=mydata,ylab= "Sale price in thousands of dollars", xlab="Number of bedrooms in the house", pch = 1)
66
# Lowess line
plot(sale.price ~ bedrooms, data=mydata,ylab= "Sale price in dollars", xlab="Number of bedrooms in the house", pch = 1)
x3 <- mydata$bedrooms[order(mydata$bedrooms)] #put the x-values in order
y3 <- mydata$sale.price[order(mydata$bedrooms)] # put the y-values in order based on the order of the x-values
lines(lowess(x3, y3, delta=0.1), col="red")
# Plotting sales price vs. lot size for lot sizes < 0.05
dt2=mydata[mydata$lot.size<=0.05,]
plot(dt2$sale.price ~ dt2$lot.size,ylab= "Sale price in dollars", xlab="Lot size in acres", pch = 1) x4 <- dt2$lot.size[order(dt2$lot.size)] #put the x-values in order
y4 <- dt2$sale.price[order(dt2$lot.size)] # put the y-values in order based on the order of the x- values
lines(lowess(x4, y4, delta=0.1), col="red")
# Plotting sales price vs. lot size for lot sizes < 1
dt2=mydata[mydata$lot.size<=1,]
plot(dt2$sale.price ~ dt2$lot.size,ylab= "Sale price in dollars", xlab="Lot size in acres", pch = 1) x4 <- dt2$lot.size[order(dt2$lot.size)] #put the x-values in order
y4 <- dt2$sale.price[order(dt2$lot.size)] # put the y-values in order based on the order of the x- values
lines(lowess(x4, y4, delta=0.1), col="red")
abline(h=235000, col='red', lty=2) #######################################################
# (1.3) Creating square root transformation of response variable and creating corresponding plots #######################################################
mydata$sqrt.sale.price <- sqrt(mydata$sale.price)
# Sqrt of sales price vs. lot size
plot(sqrt.sale.price ~ lot.size, data=mydata,ylab= "Square root of sales price", xlab="Lot size in acres", pch = 1)
x1 <- mydata$lot.size[order(mydata$lot.size)]
y1 <- mydata$sqrt.sale.price[order(mydata$lot.size)]
lines(lowess(x1, y1, delta=0.1), col="red")
# Sqrt of sales price vs. living area
plot(sqrt.sale.price ~ living.area, data=mydata,ylab= "Square root of sales price", xlab="living area in square feet", pch = 1)
x2 <- mydata$living.area[order(mydata$living.area)]
y2 <- mydata$sqrt.sale.price[order(mydata$living.area)]
67

lines(lowess(x2, y2, delta=0.1), col="red")
# Sqrt of sales price vs. number of bedrooms
plot(sqrt.sale.price ~ bedrooms, data=mydata,ylab= "Square root of sales price", xlab="Number of bedrooms in the house", pch = 1)
x3 <- mydata$bedrooms[order(mydata$bedrooms)]
y3 <- mydata$sqrt.sale.price[order(mydata$bedrooms)]
lines(lowess(x3, y3, delta=0.1), col="red")
#######################################################
# (1.4) Creating log transformation and creating corresponding plots #######################################################
# Creating log transformation of requsted variables and adding them to mydata
mydata$log.sale.price <- log(mydata$sale.price) mydata$log.lot.size <- log(mydata$lot.size) mydata$log.living.area <- log(mydata$living.area)
# Plot of log(sales.price) vs. log(lot.size)
plot(log.sale.price ~ log.lot.size, data=mydata,ylab= "log of sales price", xlab="log of lot size", pch = 1)
x5 <- mydata$log.lot.size[order(mydata$log.lot.size)]
y5 <- mydata$log.sale.price[order(mydata$log.lot.size)]
lines(lowess(x5, y5, delta=0.1), col="red")
# Plot of log(sales.price) vs. log(living.area)
plot(log.sale.price ~ log.living.area, data=mydata,ylab= "log of sales price", xlab="log of living area", pch = 1)
x6 <- mydata$log.living.area[order(mydata$log.living.area)]
y6 <- mydata$log.sale.price[order(mydata$log.living.area)]
lines(lowess(x6, y6, delta=0.1), col="red")
##############################################################################
################################
##############################################################################
################################
# PART2 ############################################################################## ################################ ############################################################################## ################################
# Model A #######################################################
68

# (2.1) #######################################################
# Scatter plot of sqrt(sales price) vs living area
plot(sqrt.sale.price ~ living.area, data=mydata,ylab= "Square root of sales price", xlab="living area in square feet", pch = 1)
x2 <- mydata$living.area[order(mydata$living.area)]
y2 <- mydata$sqrt.sale.price[order(mydata$living.area)]
lines(lowess(x2, y2, delta=0.1), col="red")
# creating SLR
z1 <- lm(sqrt.sale.price ~ living.area, data=mydata) # Residual plots
resid1 <- resid(z1) residA=resid1 predict1 <- predict(z1) predictA=predict1
plot(residA ~ predictA, pch=16) abline(h=0, lty=2) abline(v=300, col='red', lty=2) abline(v=400, col='red', lty=2) abline(v=500, col='red', lty=2) abline(v=600, col='red', lty=2) abline(v=700, col='red', lty=2) abline(v=800, col='red', lty=2) abline(v=900, col='red', lty=2)
# For the assumption of independence plot(residA ~ mydata$living.area, pch=16) abline(h=0, lty=2)
abline(v=1000, col='red', lty=2) abline(v=2000, col='red', lty=2) abline(v=3000, col='red', lty=2) abline(v=4000, col='red', lty=2) abline(v=5000, col='red', lty=2)
# Q-Q normalitry plot
qqnorm(resid1, ylab= "residuals", xlab = "Normal scores", pch=16) qqline(resid1)
69

# Histogram of the residuals hist(resid1)
# Normality test
shapiro.test(resid1) #Shapiro-Wilk test
# Significance of the resgression with F-test
N=nrow(mydata)
df1.A=1
df2.A=N-df1.A-1 SSreg.A=sum((predict1-mean(mydata$sqrt.sale.price))^2) SSreg.A
SSE.A=sum((mydata$sqrt.sale.price-predict1)^2) SSE.A
SSy.A=sum((mydata$sqrt.sale.price-mean(mydata$sqrt.sale.price))^2) SSy.A
MSreg.A=SSreg.A/df1.A MSreg.A
MSE.A=SSE.A/(df2.A) MSE.A
Fstat.A=MSreg.A/MSE.A Fstat.A
p_value=pf(Fstat.A, df1.A, df2.A, lower.tail = FALSE) p_value
Fcrit=qf(0.95, 1, 1198)
Fcrit
# Anova table
anova(z1)
# Summary command to check the results summary(z1)
# Calculating slope and intercept manually
70

SPxy=sum((mydata$sqrt.sale.price-mean(mydata$sqrt.sale.price))*(mydata$living.area- mean(mydata$living.area)))
SPxy
SSX=sum((mydata$living.area-mean(mydata$living.area))^2) SSX
b1=SPxy/SSX
b1 b0=mean(mydata$sqrt.sale.price)-b1*mean(mydata$living.area) b0
# The t critical value
t_crit=qt(0.975, 1198, lower.tail=TRUE ) t_crit
# CI from R confint(z1)
# Get the predicted values in the original units, using (predict1)^2 mydata$predict.orig=(predict1)^2 ################################################
# Calcuating R-squared and root MSE for model A ################################################ SSy=sum((mean(mydata$sale.price)-mydata$sale.price)^2)
SSy
SSE=sum((mydata$sale.price-mydata$predict.orig)^2) SSE
pseudo.R2=1-SSE/SSy pseudo.R2
N=nrow(mydata) rootMSE=sqrt(SSE/N-2) rootMSE
################################################################### # (A.4) Creating a plot with prediction intervals in the original units ################################################################### # creating new values in sqrt units, creating prediction intervals in sqrt units
xnew <- seq(min(mydata$living.area), max(mydata$living.area), length.out = 100)
71

ynew.ci <- data.frame(predict(z1, newdata = data.frame(living.area = xnew), interval = "prediction", level = 0.95))
new.values <- cbind(xnew,ynew.ci)
# backtransform everything
new.values$fit.back <- (new.values$fit)^2
new.values$lwr.back <- (new.values$lwr)^2
new.values$upr.back <- (new.values$upr)^2
# create the plot in the original units, with the prediction intervals backtransformed into the original units
plot(sale.price ~ living.area, data=mydata,ylab= "Sale price in dollars", xlab="living area in square feets", pch = 1)
lines(new.values$fit.back ~ new.values$xnew, lty=1)
lines(new.values$lwr.back ~ new.values$xnew, lty = 2)
lines(new.values$upr.back ~ new.values$xnew, lty = 2)
##############################################################################
########################################################
##############################################################################
########################################################
#### MODEL B MODEL B MODEL B MODEL B MODEL B MODEL B MODEL B MODEL B ############################################################################## ######################################################## ############################################################################## ########################################################
# Creating scatter plots
# log(sales.price) vs. living area with lowess line
plot(log.sale.price ~ living.area, data=mydata,ylab= "log of sale price", xlab="living area in feet", pch = 1)
x6 <- mydata$living.area[order(mydata$living.area)] #put the x-values in order
y6 <- mydata$log.sale.price[order(mydata$living.area)] # put the y-values in order based on the order of the x-values
lines(lowess(x6, y6, delta=0.1), col="red")
# log(sales.price) vs. bedrooms with lowess line
plot(log.sale.price ~ bedrooms, data=mydata,ylab= "log of sale price", xlab="bedrooms", pch = 1) x7 <- mydata$bedrooms[order(mydata$bedrooms)] #put the x-values in order
y7 <- mydata$log.sale.price[order(mydata$bedrooms)] # put the y-values in order based on the order of the x-values
lines(lowess(x7, y7, delta=0.1), col="red")
# creating the MLR
zB <- lm(log.sale.price ~ log.living.area+living.area+bedrooms, data=mydata)
72

# Residual plots
residB <- resid(zB) predictB <- predict(zB)
plot(residB ~ predictB, pch=16) abline(h=0, lty=2) abline(v=11.5, col='red', lty=2) abline(v=12, col='red', lty=2) abline(v=12.5, col='red', lty=2) abline(v=13, col='red', lty=2) abline(v=13.5, col='red', lty=2) abline(v=14, col='red', lty=2)
# Q-Q normalitry plot
qqnorm(residB, ylab= "residuals", xlab = "Normal scores", pch=16) qqline(residB)
# Histogram of the residuals hist(residB)
# Normality test
shapiro.test(residB) #Shapiro-Wilk test
# Significance of the resgression with F-test
df1.B=3
df2.B=N-df1.B-1 SSreg.B=sum((predictB-mean(mydata$log.sale.price))^2) SSreg.B
SSE.B=sum((mydata$log.sale.price-predictB)^2) SSE.B
SSy.B=sum((mydata$log.sale.price-mean(mydata$log.sale.price))^2) SSy.B
MSreg.B=SSreg.B/df1.B MSreg.B
MSE.B=SSE.B/(df2.B) MSE.B
73

Fstat.B=MSreg.B/MSE.B Fstat.B
p_value=pf(Fstat.B, df1.B, df2.B, lower.tail = FALSE) p_value
Fcrit=qf(0.95, 3, 1196)
Fcrit
# Anova table
anova(zB)
# Summary command to check the results summary(zB)
#####################################################
# B.2 partial F-tests
#####################################################
# Testing the variable log(living.area) and living.area at the same time using a partial F- test.
# Full model
SSreg.full.B <- sum((predictB - mean(mydata$log.sale.price))^2) SSreg.full.B
SSE.full.B=sum((mydata$log.sale.price-predictB)^2) SSE.full.B
#fit the reduced model
# Reduced Model
zB.red <- lm(log.sale.price ~ bedrooms, data=mydata) predictB.red <- predict(zB.red)
SSreg.red.B <- sum((predictB.red - mean( mydata$log.sale.price))^2) SSreg.red.B
SSreg.full.B-SSreg.red.B
Fcrit=qf(0.95, 2, 1196) Fcrit
anova(zB , zB.red) ##################################################### # B.3 partial F-tests to see teh effect of variable bedrooms
74

#####################################################
#fit the reduced model
# Reduced Model with dropping variable bedrooms to test its effect zB.red2 <- lm(log.sale.price ~ log.living.area+living.area, data=mydata) predictB.red2 <- predict(zB.red2)
SSreg.red2.B <- sum((predictB.red2 - mean( mydata$log.sale.price))^2) SSreg.red2.B
SSreg.full.B-SSreg.red2.B
Fcrit=qf(0.95, 1, 1196) Fcrit
anova(zB , zB.red2)
# getting the values for b0, b1, b2 and b3 summary(zB)
# interpretting model parameters plot(mydata$living.area~ mydata$bedrooms)
x9 <- mydata$bedrooms[order(mydata$bedrooms)] y9 <- mydata$living.area[order(mydata$bedrooms)] lines(lowess(x9, y9, delta=0.1), col="red")
##################################################### # B.4 Calcuating R-squared and root MSE ##################################################### # Get the predicted values in the original units, using exp(predictB) mydata$predictB.orig=exp(predictB)
SSy=sum((mean(mydata$sale.price)-mydata$sale.price)^2) SSy
SSE=sum((mydata$sale.price-mydata$predictB.orig)^2) SSE
pseudo.R2=1-SSE/SSy pseudo.R2
N=nrow(mydata)
75

rootMSE=sqrt(SSE/N-4) rootMSE
##############################################################################
########################################################
##############################################################################
########################################################
#### MODEL C Model C MODEL C Model C MODEL C Model C MODEL C Model C ############################################################################## ######################################################## ############################################################################## ########################################################
# creating the MLR
zC <- lm(log.sale.price ~ log.lot.size+bedrooms+log.lot.size*bedrooms, data=mydata)
# Residual plots residC <- resid(zC) predictC <- predict(zC)
plot(residC ~ predictC, pch=16) abline(h=0, lty=2) abline(v=11.75, lty=2, col="red") abline(v=12, lty=2, col="red") abline(v=12.25, lty=2, col="red") abline(v=12.5, lty=2, col="red") abline(v=12.75, lty=2, col="red") abline(v=13, lty=2, col="red")
# Q-Q normalitry plot
qqnorm(residC, ylab= "residuals", xlab = "Normal scores", pch=16) qqline(residC)
# Histogram of the residuals hist(residC)
# Normality test
shapiro.test(residC) #Shapiro-Wilk test
################################################### # C.1 test tye significance of the regression uising F-test
76

################################################### # Testing the significance of regression
df1.C=3
df2.C=N-df1.C-1 SSreg.C=sum((predictC-mean(mydata$log.sale.price))^2) SSreg.C
SSE.C=sum((mydata$log.sale.price-predictC)^2) SSE.C
SSy.C=sum((mydata$log.sale.price-mean(mydata$log.sale.price))^2) SSy.C
MSreg.C=SSreg.C/df1.C MSreg.C
MSE.C=SSE.C/(df2.C) MSE.C
Fstat.C=MSreg.C/MSE.C Fstat.C
p_value=pf(Fstat.C, df1.C, df2.C, lower.tail = FALSE) p_value
Fcrit=qf(0.95, 3, 1196)
Fcrit
anova(zC)
# C.2 performing t-test summary(zC)
t_critical=qt(0.975,1196)
t_critical
# interpretting the model parameters plot(mydata$lot.size~ mydata$bedrooms)
x10 <- mydata$bedrooms[order(mydata$bedrooms)] y10 <- mydata$lot.size[order(mydata$bedrooms)] lines(lowess(x10, y10, delta=0.1), col="red")
##################################################### # C.3 Calcuating R-squared and root MSE ##################################################### # Get the predicted values in the original units, using exp(predictC) mydata$predictC.orig=exp(predictC)
77

SSy=sum((mean(mydata$sale.price)-mydata$sale.price)^2) SSy
SSE=sum((mydata$sale.price-mydata$predictC.orig)^2) SSE
pseudo.R2=1-SSE/SSy pseudo.R2
N=nrow(mydata) rootMSE=sqrt(SSE/N-4) rootMSE
##################################################### # 3.3 Discussion ##################################################### # Correlation between log of living area nad bedrooms plot(mydata$log.living.area~mydata$bedrooms)
x12<- mydata$bedrooms[order(mydata$bedrooms)]
y12 <- mydata$log.living.area[order(mydata$bedrooms)] lines(lowess(x12, y12, delta=0.1), col="red")
# Adding explanatory vaiable lot size to modle A to see its effect zimp <- lm(sqrt.sale.price ~ living.area+log.lot.size, data=mydata) predictimp=predict(zimp)
ss=predictimp^2
ssregimp=sum((mydata$sale.price-ss)^2) 1-(ssregimp/SSy)
78
