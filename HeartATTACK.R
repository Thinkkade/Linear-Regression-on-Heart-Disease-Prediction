library(ggplot2)


# Read in our data
f <- file.choose("heartAttack.csv")
heart <- read.csv(f)

#Examine the 
head(heart)

lm(heart$target~heart$age)
lm.t=lm(heart$target~heart$age)
summary(lm.t)



# Plot the chart.
head(fortify(lm.t))

# Give the chart file a name.
png(file = "linearregression.png")


plot(('x=.fitted, y=.resid'), col = "blue",main = "target & age Regression",
     abline((lm.t),cex = 1.3,pch = 16,xlab = "age",ylab = "target"))

# Save the file.
dev.off()

#Residual Plots
head(fortify(lm.t))
residPlot<-
  ggplot(aes(x=.fitted,y=.resid),data=lm.t)+geom_point()+geom_hline(yintercept=0)+labs(x="fitted Value",y="residual")


#To get the coefficients of the model
coef(lm.t)

#To get the residuals of the model
resid(lm.t)

#To get the fitted values
fitted(lm.t)

    

library(ggplot2)

lm(heart$target~heart$age+heart$sex+heart$cp+heart$trestbps+heart$chol)
lm.t=lm(heart$target~heart$age+heart$sex+heart$cp+heart$trestbps+heart$chol)
summary(lm.t)

#Residual Plots
head(fortify(lm.t))
residPlot<-ggplot(aes(x=.fitted,y=.resid),data=lm.t)+geom_point()+geom_hline(yintercept=0)+labs(x="fitted Value",y="residual")

lm(finascore~attend)
lm.t=lm(finascore~attend)
summary(lm.t)

#Residual Plots
head(fortify(lm.t))
residPlot<-ggplot(aes(x=.fitted,y=.resid),data=lm.t)+geom_point()+geom_hline(yintercept=0)+labs(x="fitted Value",y="residual")

#QQ-Plot of Residuals
lm.finalscore=lm(finascore~ca)
finascore.zresid=rstandard(lm.finalscore)
qqnorm(finascore.zresid, ylab="standardized Residuals", xlab="Normal Scores", main="Final Scores")
qqline(finascore.zresid)





