# Install and load the survival package

# install.packages("survival")
library(survival)

# sort the aml data by time
aml <- aml[order(aml$time),]

print(aml)

# Create graph of length of time that each subject was in the study
with(aml, plot(time, type="h"))

# Create the life table survival object for aml
# The functions survfit() and Surv() create a life table survival object.
# The life table object is passed to the plot() function to create the KM plot.
aml.survfit <- survfit(Surv(time, status == 1) ~ 1, data=aml)

# Plot the Kaplan-Meier curve for aml.
# By default, R includes the confidence interval. 
plot(aml.survfit, xlab = "Time (weeks)", ylab="Proportion surviving", main="Survival in AML")

# The summary() function displays the life table
summary(aml.survfit)

# Create aml life tables and KM plots broken out by treatment (x,  "Maintained" vs. "Not maintained")
surv.by.aml.rx <- survfit(Surv(time, status == 1) ~ x, data = aml)

summary(surv.by.aml.rx)

# Plot KM 
plot(surv.by.aml.rx, xlab = "Time", ylab="Survival",col=c("black", "red"), lty = 1:2, main="Kaplan-Meier Survival vs. Maintenance in AML")

# Add legend
legend(100, .6, c("Maintained", "Not maintained"), lty = 1:2, col=c("black", "red"))

# Perform the log rank test using the R function survdiff().

surv.diff.aml <- survdiff(Surv(time, status == 1) ~ x, data=aml)

surv.diff.aml

# Cox Proportional Hazards regression
# melanoma data set from ISwR package, described in Dalgaard Chapter 12. 
# install the ISwR package and load the library into R.
# The ISwR package currently only appears to be available for older versions of R

install.packages("ISwR")

library(ISwR)

help(melanom) # description of the melanoma data

# The log rank test is a special case of the cox proportional hazard regression analysis.
# The same analysis can be performed using the R function coxph().
# melanoma example using a log-rank test.
surv.diff.sex <- survdiff(Surv(days, status == 1) ~ sex, data = melanom)

surv.diff.sex

# melanoma analysis using Cox proportional hazards regression
coxph.sex <- coxph(Surv(days, status == 1) ~ sex, data = melanom)

summary(coxph.sex)

# melanoma Cox analysis including covariate ulcer thickness

# Plot the thickness values and log(thickness)
hist(melanom$thick)

hist(log(melanom$thick))

# The Cox PH analysis of melanoma data including covariate log(thick)

coxph.sex.thick <- coxph(Surv(days, status == 1) ~ sex + log(thick), data = melanom)

summary(coxph.sex.thick)

# Examine thickness by sex
boxplot(log(melanom$thick) ~ melanom$sex)

t.test(log(melanom$thick) ~ melanom$sex)

# Test of proportional hazards assumption
coxph.sex <- coxph(Surv(days, status == 1) ~ sex, data = melanom)

cox.zph(coxph.sex)

install.packages("rpart")
library(rpart)

head(stagec)

# Pass a survival object from Surv() to the function rpart() to perform the analysis.
fit <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2 + grade + gleason + ploidy, data=stagec)

# plot the resulting tree

plot(fit, uniform=T, branch=.4, compress=T)
text(fit, use.n=T)
# The print() function provides details of the tree not shown above
print(fit)