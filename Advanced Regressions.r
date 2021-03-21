library(tidyverse)

carsdata <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header=F, stringsAsFactors=F)
#we explicitly import strings as characters instead of factors so that we can define and order the factors manually

colnames(carsdata) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

dim(carsdata)
head(carsdata)

# Disabled as not sure what it's meant to do

# for (i in seq_along(carsdata)){
# print(unique(carsdata[c(i)]))
#    }

apply(X = carsdata, MARGIN = 2, FUN = function(x) length(unique(x)))

apply(X = carsdata, MARGIN = 2, FUN = function(x) table(x))

str(ordered(carsdata$buying)) #orders alphabetically

str(factor(carsdata$buying, levels=c('low','med','high','vhigh'), ordered=TRUE)) #custom ordering

# Define the order of the factors for the logistic regression
carsdata$buying <- factor(carsdata$buying, levels=c('low','med','high','vhigh'), ordered=TRUE)
carsdata$maint <- factor(carsdata$maint, levels=c('low','med','high','vhigh'), ordered=TRUE)
carsdata$doors <- factor(carsdata$doors, levels=c('2','3','4','5more'), ordered=TRUE)
carsdata$persons <- factor(carsdata$persons, levels=c('2','4','more'), ordered=TRUE)
carsdata$lug_boot <- factor(carsdata$lug_boot, levels=c('small','med','big'), ordered=TRUE)
carsdata$safety <- factor(carsdata$safety, levels=c('low','med','high'), ordered=TRUE)
carsdata$class <- factor(carsdata$class, levels=c('unacc','acc','good','vgood'), ordered=TRUE)

str(carsdata)

summary(carsdata)

set.seed(100)
trainRows <- sample(x = 1:nrow(carsdata), size = 0.7*nrow(carsdata))

trainData <- carsdata[trainRows,]
testData <- carsdata[-trainRows,]

options('contrasts')
#options(contrasts = c("contr.treatment", "contr.poly"))

#install.packages('MASS')

library(MASS)

ls()

rm(polrMod)

polrMod <- polr(class ~ safety + lug_boot + doors + buying + maint, data=trainData, Hess = TRUE)
#HESS to obtain standard errors

summary(polrMod)

head(coef(summary(polrMod)))

# Obtaining p values
summary_table <- coef(summary(polrMod))
pval <- pnorm(abs(summary_table[,'t value']), lower.tail = FALSE)*2
summary_table <- cbind(summary_table,'p value' = round(pval,3))

head(summary_table)

broom::tidy(polrMod) %>%
mutate(pval = (1 - pnorm(abs(statistic), 0, 1))*2,
       pval = round(pval,3)) %>% 
cbind(p_val = summary_table[,4])

levels(carsdata$doors)

broom::tidy(polrMod) %>% 
mutate(odds_ratio = round(exp(estimate),2),
       perc = scales::percent(odds_ratio - 1))

predictedClass <- predict(object = polrMod, newdata = testData)
head(predictedClass)

#The advantage with this over normal logistic regression it that this can handle a multiclass problem
#while a logistic is limited to binary class problems

predictedScores <- predict(object = polrMod, newdata = testData, type = 'probs')
head(predictedScores)

# Confusion matrix
table(testData$class, predictedClass)

# Misclassification error
mean(as.character(testData$class) != as.character(predictedClass))

# Pseudo R2 of the model
DescTools::PseudoR2(polrMod)

# R2 of the model (this output is in fact categorical hence this might be incorrect)
caret::R2(pred = as.integer(predictedClass), obs = as.integer(testData$class))

# Output though is not applicable to our case since it is not binary

carsdata <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header=F)

colnames(carsdata) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

str(carsdata)

set.seed(100)
trainRows <- sample(x = 1:nrow(carsdata), size = 0.7*nrow(carsdata))

trainData <- carsdata[trainRows,]
testData <- carsdata[-trainRows,]

modelLog <- glm(formula = class ~ . , family = binomial(link = 'logit'), data = trainData)
summary(modelLog)

# Change the base factors
carsdata$buying <- factor(carsdata$buying, levels=c('low','med','high','vhigh'))
carsdata$maint <- factor(carsdata$maint, levels=c('low','med','high','vhigh'))
carsdata$doors <- factor(carsdata$doors, levels=c('2','3','4','5more'))
carsdata$persons <- factor(carsdata$persons, levels=c('2','4','more'))
carsdata$lug_boot <- factor(carsdata$lug_boot, levels=c('small','med','big'))
carsdata$safety <- factor(carsdata$safety, levels=c('low','med','high'))
carsdata$class <- factor(carsdata$class, levels=c('unacc','acc','good','vgood'))

set.seed(100)
trainRows <- sample(x = 1:nrow(carsdata), size = 0.7*nrow(carsdata))

trainData <- carsdata[trainRows,]
testData <- carsdata[-trainRows,]

modelLog <- glm(formula = class ~ . , family = binomial(link = 'logit'), data = trainData)
summary(modelLog)

# Output though is not applicable to our case since it is not binary

library(nnet)

carsdata <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header=F)

colnames(carsdata) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

str(carsdata)

#Set the correct reference levels
carsdata$buying <- relevel(x = carsdata$buying, ref = 'low')
carsdata$maint <- relevel(x = carsdata$maint, ref = 'low')
carsdata$doors <- relevel(x = carsdata$doors, ref = '2')
carsdata$persons <- relevel(x = carsdata$persons, ref = '2')
carsdata$lug_boot <- relevel(x = carsdata$lug_boot, ref = 'small')
carsdata$safety <- relevel(x = carsdata$safety, ref = 'low')
carsdata$class <- relevel(x = carsdata$class, ref = 'unacc')

str(carsdata)

set.seed(100)
trainRows <- sample(x = 1:nrow(carsdata), size = 0.7*nrow(carsdata))

trainData <- carsdata[trainRows,]
testData <- carsdata[-trainRows,]

modelMulti <- multinom(formula = class ~ . , data = trainData)
summary(modelMulti)

summary(modelMulti)[]

# CALCULATE P VALUES

# Z SCORE = COEFFICIENT / STD ERROR
z_scores <- summary(modelMulti)$coefficients/summary(modelMulti)$standard.errors

# DERIVE P VALUES
p_values <- (1 - pnorm(abs(z_scores),0,1))*2

p_values
# It seems to provide a pvalue for each level of the outcome variable

predictedClass_M <- predict(object = modelMulti, newdata = testData)
head(predictedClass_M)

table(predictedClass_M, testData$class)

mean(predictedClass_M != testData$class)


