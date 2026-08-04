# Delete memory
rm(list=ls())

# Required packages
library(knitr)

# Challenger data set
distress_ct = c(0,1,0,0,0,0,0,0,1,1,1,0,0,2,0,0,0,0,0,0,2,0,1)
temperature = c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58)
field_check_pressure = c(50,50,50,50,50,50,100,100,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200)
n.rings <- rep(6,length(distress_ct))

launch <- data.frame(cbind(n.rings,distress_ct, temperature, field_check_pressure))

#launch <- launch[9:23,]

# Replacing values higher than 1 by 1 (as we are only interested in whether or not a failure happened)
#launch$distress_ct = ifelse(launch$distress_ct<1,0,1)

# Displaying the data
kable(launch)

# Fitting a logistic regression model
model <- glm(cbind(distress_ct,n.rings-distress_ct) ~temperature + field_check_pressure, family = binomial(link='logit'), data = launch)
summary(model)

#------------------------------------------------------------------
# Confidence intervals based on the profile likelihood
#------------------------------------------------------------------
kable(confint(model, level=0.95, type="LR"),digits = 4)

#------------------------------------------------------------------
# Wald Confidence intervals
#------------------------------------------------------------------
kable(confint.default(model, level=0.95),digits = 4)

#------------------------------------------------------------------
# Normal confidence intervals
#------------------------------------------------------------------
NCI <- cbind(coefficients(model)- 1.96*summary(model)$coefficients[,2],
             coefficients(model)+ 1.96*summary(model)$coefficients[,2] )

colnames(NCI) <- c("2.5 %", "97.5 %")
rownames(NCI) <- c("(Intercept)", "temperature", "pressure")
kable(NCI,digits = 4)


##################################################################################
# Brief interpretation of the model
##################################################################################

# Predicted probability of failure for the Challenger
plogis(coefficients(model)%*%c(1,31,200))

# Predicted probability of failure for minimum temperature experienced previously
plogis(coefficients(model)%*%c(1,53,200))


# The following plot presents 
curve(predict(model, data.frame(temperature = x, field_check_pressure = 200),
              type = "response"),  from = 30, to = 85, ylab = "Probability of Failure", xlab = "Temperature (F)",
      cex.axis = 1.5, cex.lab = 1.5, lwd = 2, col = 'red')