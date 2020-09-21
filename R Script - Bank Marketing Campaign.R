data <- read.csv('/Users/preetiyerkuntwar/Documents/Bank Marketing Prediction Data/Bank_Personal_Loan_Modelling.csv')

data$Income <- log1p(data$Income)
data$CCAvg <- log1p(data$CCAvg)

drops <- c("ID","ZIP.Code")
data <- data[ , !(names(data) %in% drops)]


input_ones <- data[which(data$Personal.Loan == 1), ]  # all 1's
input_zeros <- data[which(data$Personal.Loan == 0), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's


logitMod <- glm(Personal.Loan ~ ., data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData)) 

predicted <- predict(logitMod, testData, type="response") 


hist(predicted)

install.packages("InformationValue")
library(InformationValue)

optCutOff <- optimalCutoff(data$Personal.Loan, predicted)[1] 


summary(logitMod)


misClassError(testData$Personal.Loan, predicted, threshold = optCutOff)

plotROC(testData$Personal.Loan, predicted)


write.csv(trainingData, "/Users/preetiyerkuntwar/Documents/Bank Marketing Prediction Data/trainingData.csv")




