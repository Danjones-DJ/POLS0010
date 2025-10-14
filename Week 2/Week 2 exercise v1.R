library(skimr); library(tidyverse); library(ggplot2); library(car); library(haven)

dataset = read_dta("2012_hse_and_shes_combined.dta")
dataset$bmival = as.numeric(dataset$bmival)
dataset$age = as.numeric(dataset$age)
dataset$sex = as_factor(dataset$Sex)


# Plot it -----------------------------------------------------------------

ggplot(dataset, aes(age, bmival)) + 
  geom_point(shape=1)


# Remove invalids ---------------------------------------------------------

subset = subset(dataset, bmival>0 & age >= 18)

# Plot again!

ggplot(subset, aes(age, bmival)) + 
  geom_point(shape=1) + 
  geom_smooth(method="lm")

# Straight line bad. I want curvey ----------------------------------------
ggplot(subset, aes(age, bmival)) + 
  geom_point(shape=1) + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2))

# -------------------------------------------------------------------------
#linear
bmi.model = lm(bmival~age+sex, data=subset)
summary(bmi.model)
#quad
bmi.model.q = lm(bmival~age+I(age^2)+sex, data=subset)
summary(bmi.model.q)

# compare models
anova(bmi.model, bmi.model.q)
# yay q is better


# -------------------------------------------------------------------------

residualPlots(bmi.model, ~1, fitted=TRUE)


