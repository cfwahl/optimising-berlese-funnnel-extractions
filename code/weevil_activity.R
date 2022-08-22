
## WEEVIL MOVEMENT MANUSCRIPT

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

#load libraries
#library(tidyverse)
library(car)
library(emmeans)
# MULTINOMIAL LOGISTIC REGRESSION
library(foreign)
library(nnet)
library(reshape2)
library(mlogit)
# broken stick regression
library(segmented)

# probit analysis was done in JMP, see results in "Probit weevils exit analysis.csv"


# weevil berlese escape success -----------------------------------------------------------

# load the file Weevil_escape.csv
weevil<-read_csv(file.choose())

##  generalized liner model for proportion of weevils to exit funnel 
bugs.glm <- glm(weevil$Escape_Success~Light*Biomass, 
                family=gaussian(link = "identity"),
                data = weevil)

summary(bugs.glm)

#summary does not include p-value, if needed for publication
Anova(bugs.glm)

#residual plots
plot(bugs.glm)

# pairwise comparisons for all light and biomass interactions
# since the two treatments varied from everything, I used smallest value 
# produced when saying there were significant differences among treatments 
emmeans(bugs.glm, list(pairwise ~ Light*Biomass), adjust = "tukey")


# weevil activity ---------------------------------------------------------

# load the file Weevil_Activity_Data.csv
activity<-read_csv(file.choose())

##  Generalized liner model  ##
# glm for Temperature 
bugs.glm <- glm(activity$Button_Temp~Humidity*Activity , 
                 family=gaussian(link = "identity"),
                 data = activity) 

summary(bugs.glm)

#summary does not include p-value, if needed for publication
Anova(bugs.glm)

#residual plots
plot(bugs.glm)

# pairwise comparisons for humidity and activity interactions
emmeans <- emmeans(bugs.glm, list(pairwise ~ Humidity*Activity), adjust = "tukey")
test(emmeans)
summary(emmeans)

emmeans(bugs.glm, list(pairwise ~ Activity), adjust = "tukey")


# linear model for RH 
bugs.lm <- lm(activity$RH_1~Humd_Treat+Time, data = activity) 
summary(bugs.lm)

#summary does not include p-value, if needed for publication
Anova(bugs.lm)

#residual plots
plot(bugs.glm)

# pairwise comparisons for all cover and nutrient interactions
emmeans <- emmeans(bugs.glm, list(pairwise ~ Humd_Treat), adjust = "tukey")
test(emmeans)
summary(emmeans)

# multinominal logistic regression ----------------------------------------

# load the file Weevil_Activity_Data.csv
activity<-read_csv(file.choose())

# subset for only low humidity samples
activity2 <- subset(activity, Humidity=="Low")

# subset for only medium humidity samples
activity3 <- subset(activity, Humidity=="Medium")

# subset for only high humidity samples
activity4 <- subset(activity, Humidity=="High")

with(activity, table(Humidity, Activity))

with(activity, do.call(rbind, tapply(Button_Temp, Activity, function(x) 
  c(M = mean(x), SD = sd(x)))))

# choose baseline category
activity4$Activity2 <- relevel(factor(activity4$Activity), ref = "F")
#activity$Humidity2 <- relevel(factor(activity$Humidity), ref = "Low")


# run the model, change as needed for each humidity treatment
test <- multinom(Activity2 ~ Button_Temp 
                 , data = activity4)

summary(test)


z <- summary(test)$coefficients/summary(test)$standard.errors
z

# two tailed Z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


# broken stick analysis ---------------------------------------------------

# load the file Master_Temp_RH_Plate_Data.csv
broken <- read_csv(file.choose())

# subset data
high <- subset(broken, Humd_Treat == "High")
med <- subset(broken, Humd_Treat == "Medium")
low <- subset(broken, Humd_Treat == "Low")

# high humidity treatment
fit.high <- lm(RH_1 ~ Temp_1, high)
summary(fit.high)

# med humidity treatment
fit.med <- lm(RH_1 ~ Temp_1, med)
summary(fit.med)

# low humidity treatment
fit.low <- lm(RH_1 ~ Temp_1, low)
summary(fit.low)


#### HIGH 

#create scatter plot of x vs. y
plot(high$Temp_1, high$RH_1, pch=16, col='steelblue')

#fit piecework regression model to original model, estimating a breakpoint at x=81%
segmented.fit.high <- segmented(fit.high, seg.Z = ~Temp_1, psi=34)

#view summary of segmented model
summary(segmented.fit.high)

#figures
plot(high$RH_1 ~ high$Temp_1, broken)  # plot the points
plot(segmented.fit.high, add = TRUE)     # plot the broken line
abline(v = segmented.fit.high$psi[2])    # plot the vertical at the break point

# get the breakpoints
segmented.fit.high$psi

# get the slopes
slope(segmented.fit.high)


#### MEDIUM

#create scatter plot of x vs. y
plot(med$Temp_1, med$RH_1, pch=16, col='steelblue')

#fit piece wise regression model to original model, estimating a breakpoint at x=9
segmented.fit.med <- segmented(fit.med, seg.Z = ~Temp_1, psi=43)

#view summary of segmented model
summary(segmented.fit.med)

#figures
plot(med$RH_1 ~ med$Temp_1, broken)   # plot the points
plot(segmented.fit.med, add = TRUE)     # plot the broken line
abline(v = segmented.fit.med$psi[2])    # plot the vertical at the break point

# get the breakpoints
segmented.fit.med$psi

# get the slopes
slope(segmented.fit.med)


#### LOW

#create scatterplot of x vs. y
plot(low$Temp_1, low$RH_1, pch=16, col='steelblue')

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit.low <- segmented(fit.low, seg.Z = ~Temp_1, psi=35)

#view summary of segmented model
summary(segmented.fit.low)

#figures
plot(low$RH_1 ~ low$Temp_1, broken)    # plot the points
plot(segmented.fit.low, add = TRUE)     # plot the broken line
abline(v = segmented.fit.low$psi[2])    # plot the vertical at the breakpoint

# get the breakpoints
segmented.fit.low$psi

# get the slopes
slope(segmented.fit.low)
