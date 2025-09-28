library(tidyverse); library(skimr); library(forcats); library(GGally); library(ggplot2); library(car)

# Load the data

aps24 <- read_csv("APS2024.csv") %>% janitor::clean_names()
election24 <- read_csv("GE2024.csv") %>% janitor::clean_names()

# Merge the data
election24aps24 <- merge(election24, aps24, by="ons_id")

# Create england binary variable
election24aps24$england <- election24aps24$country_name
election24aps24$england <- recode(election24aps24$england, Scotland = "Other", Wales = "Other")
election24aps24$england <- fct_drop(election24aps24$england)
table(election24aps24$england)

# create con.per
election24aps24$con.per <- election24aps24$con / election24aps24$valid_votes

# Create a linear model predicting con.per using england
fit_2 <- lm(con.per ~ england, data=election24aps24)
summary(fit_2)

# Key note:
# As we are using a binary predictor (either England, or "other"), the table outputs englandOther. 
# Thus, it is doing a t-tets between the two groups (England vs Other)
# The intercept is the mean of the reference group (England), and the coefficient for englandOther is the difference between the two groups.

mean(election24aps24$con.per[election24aps24$england=="England"], na.rm=TRUE)
mean(election24aps24$con.per[election24aps24$england=="England"], na.rm=TRUE) -  mean(election24aps24$con.per[election24aps24$england=="Other"], na.rm=TRUE)

# Comparatively, if we had a factor (not a binary) variable we would then have as many "dummy"s as there are groups - 1 (the reference group)
# Let's test this!

e24aps24 <- election24aps24
e24aps24$england <- e24aps24$country_name # not recoding
fit_2_2 <- lm(con.per ~ england, data=e24aps24)
summary(fit_2_2)

# Now we have 3 groups (England, Scotland, Wales), so we have 2 dummies (Scotland and Wales), with England as the reference group.

election24aps24$region_name <- as_factor(election24aps24$region_name) 
summary(election24aps24$region_name) # south east is biggest, so use as rg

fit_3 <- lm(con.per ~ relevel(region_name, "South East"), data=election24aps24)
summary(fit_3)

# Testing a multiple regression model
fit_4 <- lm(con.per ~ eco_active+england, data=election24aps24)
summary(fit_4)

# Model Building and Comparison
reduced <- lm(con.per ~ eco_active+england, data=election24aps24)
full <- lm(con.per ~ eco_active+england+age50andover, data=election24aps24)

anova(reduced, full) 
# So, in our comparison we find that age50andover is a non-zero predictor
# Thus, we would prefer the full model


# MASS and Stepwise Selection ---------------------------------------------
library(MASS)
step.model <- lm(con.per ~eco_active+england+age50andover+british+employment+white, data=election24aps24)
summary(step.model)

step <- stepAIC(step.model, direction="both")
step$anova

# The stepwise selection has removed ecoactive and white as predictors

final.model <- lm(con.per ~ england + age50andover + british + employment, data=election24aps24)
summary(final.model)


# Correlation Matrix ------------------------------------------------------

election24aps24_sm <- subset(election24aps24, select=c(con.per, age50andover, british, employment))
cor(election24aps24_sm, use="complete.obs")
pairs(election24aps24_sm)

# GGally
ggpairs(election24aps24_sm)
# Car

vif(final.model)
1/vif(final.model)