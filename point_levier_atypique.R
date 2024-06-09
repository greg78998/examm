### On fait une régression 



### point levier 

data

lm <- lm(y ~. , data= data)

summary(lm)

residu <- residuals(lm)

rstudent(residu)

res <- rstudent(lm)

plot(res, pch = 15, cex =0.5, ylab="Residus",ylim = c(-3,3))
abline(h=c(-2,0,2), lty = c(2,1,2))


lm$fitted.values
hist(lm$fitted.values)
