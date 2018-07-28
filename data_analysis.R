install.packages("magrittr")
install.packages("broom")
install.packages("tidyverse")
library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests

data = read.csv("./diabetes.csv")
attach(data)
summary(data)

#fit <- rpart(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness
#             + BMI + DiabetesPedigreeFunction + Age, data = data)

# visually see how it is build

fit <- rpart(BMI ~ Pregnancies + Glucose + BloodPressure + SkinThickness
              + DiabetesPedigreeFunction + Age, data = data)
fit <- rpart(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness
                 + DiabetesPedigreeFunction + Age, data = data)

plot(fit, uniform=TRUE) 
text(fit, cex=.6)

print("Making predictions for the first few Outcomes :")
print(head(data))

print("The predictions are")
print(predict(fit, head(data)))
print("The actual BMI are")
print(head(data$BMI))

library(modelr)
mae(model = fit, data = data)


# split our data so that 30% is in the test set and 70% is in the training set
splitData <- resample_partition(data, c(test = 0.3, train = 0.7))

# of cases in test & training set
lapply(splitData, dim)



fit2 <- rpart(BMI ~ Pregnancies + Glucose + BloodPressure + SkinThickness
             + DiabetesPedigreeFunction + Age, data = splitData$train)

mae(model = fit2, data = splitData$test)


get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  # turn the predictors & target into a formula to pass to rpart()
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  # build our model
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  # get the mae
  mae <- mae(model, testing_data)
  return(mae)
}


target <- "Outcome"
predictors <-  c("Pregnancies","Glucose","BloodPressure","SkinThickness",
                 "Insulin","Age","BMI")

# get the MAE for maxdepths between 1 & 10
for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}

library("randomForest")
# fit a random forest model to our training set
fitRandomForest <- randomForest(BMI ~ Pregnancies + Glucose + BloodPressure + SkinThickness
              + DiabetesPedigreeFunction + Age, data = splitData$train)

# get the mean average error for our new model, based on our test data
mae(model = fitRandomForest, data = splitData$test)


library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests

# read in the test and train data
library(modelr)
splitData <- resample_partition(data, c(test = 0.3, train = 0.7))
train <- splitData$train
test <- splitData$test

# fit our model
model <- randomForest(BMI ~ Pregnancies + Glucose + BloodPressure + SkinThickness
                      + DiabetesPedigreeFunction + Age, data = train)

predict(model,test)

ggplot(data = data, aes(x = BMI, y = Age)) +
  geom_point()

