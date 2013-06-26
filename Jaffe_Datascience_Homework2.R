##Aaron Jaffe Regression Assignment ##

#Load in kaggle salary data train(10k) and test sets
train <- read.csv("~/datascience_assignments/regression_assignment/kaggle_salary/train.csv")
View(train)
test <- read.csv("~/datascience_assignments/regression_assignment/kaggle_salary/test.csv")
View(test)

train <- train[order(train$SalaryNormalized) , ] # sort train to get a sense of the salary rante
summary(train$SalaryNormalized)


hist(train$SalaryNormalized) # produce a histogram to get a visual sense of the data
hist(log(train$SalaryNormalized)) # Log give a much nicer distribution

#create training set using folding technique
set.seed(42)
train$fold <- sample(1:10, nrow(train), replace=TRUE)
model.train <- subset(train, fold != 3) 
model.test <- subset(train, fold == 3) 


#Creating first linear modeling to see if there is a correlation between salary and category, location contracttype/contracttime
model <- lm(SalaryNormalized ~ Category + LocationNormalized + ContractType*ContractTime, data=model.train )
summary(model) #r^2 is .247 - not great

mae <- function(x,y) {mean ( abs(x-y))} # create mean absolute error function
mae(fitted(model), model.train$SalaryNormalized) # mae of 9126.559

#Now create a log distribution model
model <- lm(log(SalaryNormalized) ~ Category + LocationNormalized + ContractType*ContractTime, data=model.train )
summary(model) # r^2 improves to .31
mae(exp(fitted(model)), model.train$SalaryNormalized) #mae also decreases to 8723

#Now trying model with a glm model from gaussian family specifying a log function
model <- glm(log(SalaryNormalized) ~ Category + LocationNormalized + ContractType*ContractTime, data=model.train, family = gaussian(link = 'identity'))
summary(model)
mae(exp(fitted(model)), model.train$SalaryNormalized) #mae stame - Aaron - I'm not quite sure if I just did exactly what I had above - can you advise? 


#create formula to find error from folding using log model
error_from_fold <- function(n){
  model <- lm(log(SalaryNormalized) ~ Category + ContractType*ContractTime, data=subset(train, fold != n))
  test <- subset(train, fold == n)
  summary <- summary(model)
  error <- mae(exp(predict(model, test)), test$SalaryNormalized)
  return(error)
}

error_from_fold(2) #Aaron I removed location from the LM becuase it was returning errors - could you help show me how I would remove those terms from the training dataset so they don't conflict with the test?


install.packages('DAAG')
library('DAAG')
cv.lm.model <- cv.lm(df = model.train, form.lm = formula(SalaryNormalized ~ Category + ContractType*ContractTime), m=3, dots = FALSE, seed=29) #cv.lm experiment
cv.lm.model

#Creating model using tm to pull out a set of words to put into regression
install.packages ('tm')
library('tm')

src <- DataframeSource(data.frame(model.train$FullDescription))
c <- Corpus(src)
dtm <- DocumentTermMatrix(c)

findFreqTerms(dtm, 500) # looking at frequent terms to pick out relevant words

#create new dataset
text_data <- cbind(model.train, as.matrix(dtm[ , "nights"]), as.matrix(dtm[ , "nurse"]), as.matrix(dtm[ , "apprentice"]), as.matrix(dtm[ , "doctor"]), as.matrix(dtm[ , "analyst"]), as.matrix(dtm[ , "executive"]), as.matrix(dtm[ , "assistant"]), as.matrix(dtm[ , "manager"]), as.matrix(dtm[ , "administration"]))

#create new training and test set using folding
text.train <- subset(text_data, fold != 5)
text.test <- subset(text_data, fold == 5)

#create new model
text.model <- lm(log(SalaryNormalized) ~ Category + LocationNormalized + ContractType*ContractTime + nights + doctor + nurse + apprentice + analyst + executive + assistant + manager + administration, data=text.train)
summary(text.model) # r improves a bit to .367
mae(exp(fitted(text.model)), text.train$SalaryNormalized) # MAE decerases to 8294 

#create that trains on full data set for final preductions
text.model <- lm(log(SalaryNormalized) ~ Category + ContractType*ContractTime + nights + doctor + nurse + apprentice + analyst + executive + assistant + manager + administration, data=text_data)
summary(text.model) #r^2 of .3675
prediction <- exp(predict(text.model, text.test))

# Remove terms in Category that block the prediction from running 
test.original <- read.csv("~/datascience_assignments/regression_assignment/kaggle_salary/test.csv")
c.src <- DataframeSource(data.frame(train$Category))
c.c <- Corpus(c.src)
c.dtm <- DocumentTermMatrix(c.c)
findFreqTerms(c.dtm, 100)
test.original$Category[!(test.original$Category %in% train$Category)] <- "Accounting & Finance Jobs"

#Remove terms in LovationNormalized that block prediction from running
c.src <- DataframeSource(data.frame(train$LocationNormalized))
c.c <- Corpus(c.src)
c.dtm <- DocumentTermMatrix(c.c)
findFreqTerms(c.dtm, 100)
test.original$LocationNormalized[!(test.original$LocationNormalized %in% train$LocationNormalized)] <- "London"

#put prediction values into sheet
test.src <- DataframeSource(data.frame(test.original$FullDescription))
test.c <- Corpus(test.src)
test.dtm <- DocumentTermMatrix(test.c)
test.text <- cbind(test.original, as.matrix(test.dtm[ , "nights"]), as.matrix(test.dtm[ , "nurse"]), as.matrix(test.dtm[ , "apprentice"]), as.matrix(test.dtm[ , "doctor"]), as.matrix(test.dtm[ , "analyst"]), as.matrix(test.dtm[ , "executive"]), as.matrix(test.dtm[ , "assistant"]), as.matrix(test.dtm[ , "manager"]), as.matrix(test.dtm[ , "administration"]))
prediction <- exp(predict(text.model, test.text))
test.original$SalaryNormalized <- prediction

#Having trouble here with number of rows 
submission <- data.frame(Id=test.original$Id, Salary=test.original$SalaryNormalized)
write.csv(submission, "Aaron_Jaffe_DS_HW2.csv", row.names=FALSE) 