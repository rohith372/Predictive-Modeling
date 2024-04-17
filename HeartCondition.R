# install packages
# install.packages("tidyverse")
# install.packages("dummy")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("rpart.plot")

# load the libraries
library(tidyverse)
library(dummy)
library(corrplot)
library(olsrr)
library(smotefamily)
library(e1071)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(class)

# Set the working directory 
setwd("/Users/rohithpamidimukkala/Desktop/UofA")

# Import the csv data into a tibble called heartDiseaseData
heartDiseaseData<-read_csv(file = "heart_2020_reduced.csv",
                           col_types = "lnlllnnllffflfnlll",
                           col_names = TRUE)

# Display the heartDiseaseData tibble on the console
heartDiseaseData

# Display the structure heartDiseaseData tibble on the console
str(heartDiseaseData)

# Display the summary heartDiseaseData tibble on the console
summary(heartDiseaseData)

# Create a function called displayAllHistograms that takes in a tibble parameter
# that will display a histogram for all numeric features in the tibble
displayAllHistograms <- function(tibbleDataset){
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping= aes(x=value, fill=key),
                              color= "black")+
    facet_wrap(~key, scales="free")+
    theme_minimal()
}

# Call the displayAllHistograms() function, passing in heartDiseaseData 
# as an argument
displayAllHistograms(heartDiseaseData)

# Dummy code variables
heartDiseaseDataAgeCategory <- data.frame(heartDiseaseData$AgeCategory)
heartDiseaseDataRace <- data.frame(heartDiseaseData$Race)
heartDiseaseDataDiabetic <- data.frame(heartDiseaseData$Diabetic)
heartDiseaseDataGenHealth <- data.frame(heartDiseaseData$GenHealth)
heartDiseaseData <- data.frame(heartDiseaseData)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataAgeCategory, int = TRUE))) %>%
  select(-AgeCategory)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataRace, int = TRUE))) %>%
  select(-Race)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataDiabetic, int = TRUE))) %>%
  select(-Diabetic)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataGenHealth, int = TRUE))) %>%
  select(-GenHealth)


# Display a correlation matrix of heartDiseaseData rounded to 2 decimal places
round(cor(heartDiseaseData),2)

# Display a correlation plot
corrplot(cor(heartDiseaseData), method = "number", type = "lower", tl.cex = 0.4,
         number.cex = 0.25, bg="white")


# Split the data into training and testing
# The set.seed()function is used to ensure that we can get the same result
# every time we run a random sampling process
set.seed(203)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(heartDiseaseData),
                    round(nrow(heartDiseaseData) *0.75),
                    replace=FALSE)

# Put the records from the 75% sample into heartDiseaseDataTraining
heartDiseaseDataTraining <- heartDiseaseData[sampleSet, ]

# Put all other records(25%) into heartDiseaseDataTesting
heartDiseaseDataTesting <- heartDiseaseData[-sampleSet, ]

# Do we have class imbalance in training dataset
summary(heartDiseaseDataTraining$HeartDisease)

# Deal with class imbalance in the training dataset
heartDiseaseDataTrainingSmoted <-
  tibble(SMOTE(X = data.frame(heartDiseaseDataTraining),
               target= heartDiseaseDataTraining$HeartDisease,
               dup_size=5)$data)
summary(heartDiseaseDataTrainingSmoted)

# Convert variables into logical types
heartDiseaseDataTrainingSmoted <- heartDiseaseDataTrainingSmoted %>% 
  mutate(HeartDisease=as.logical(HeartDisease),
         Smoking=as.logical(Smoking),
         AlcoholDrinking=as.logical(AlcoholDrinking),
         Stroke=as.logical(Stroke),
         DiffWalking=as.logical(DiffWalking),
         Sex=as.logical(Sex),
         PhysicalActivity=as.logical(PhysicalActivity),
         Asthma=as.logical(Asthma),
         KidneyDisease=as.logical(KidneyDisease),
         SkinCancer=as.logical(SkinCancer))

# Get rid of the class column in the tibble (added by SMOTE())
heartDiseaseDataTrainingSmoted <- heartDiseaseDataTrainingSmoted %>%
  select(-class)

# Check for class imbalance on the smoted dataset
summary(heartDiseaseDataTrainingSmoted)

# Generate the logistic regression model THIS IS WHERE SMOTED GOES
heartDiseaseDataModel <- glm(data=heartDiseaseDataTrainingSmoted,
                             family = binomial,
                             formula = HeartDisease~.)

# Display the output of the logistic regression model
summary(heartDiseaseDataModel)

# Use the model to predict outcomes in the testing dataset
heartDiseaseDataPredictions <- predict(heartDiseaseDataModel,
                                       heartDiseaseDataTesting,
                                       type = "response")

# Display heartDiseaseDataPredictions on the console
heartDiseaseDataPredictions

# Treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1.
heartDiseaseDataPredictions <-
  ifelse(heartDiseaseDataPredictions >= 0.5,1,0)

# Display mobilePhonePredictions on the console
heartDiseaseDataPredictions

# Create a confusion matrix
heartDiseaseDataConfusionMatrix <- table(heartDiseaseDataTesting$HeartDisease,
                                         heartDiseaseDataPredictions)

# Display confusion matrix
heartDiseaseDataConfusionMatrix

# Calculate the false positive rate
heartDiseaseDataConfusionMatrix[1,2]/
  (heartDiseaseDataConfusionMatrix [1,2] +
     heartDiseaseDataConfusionMatrix[1,1])

# Calculate the false negative rate
heartDiseaseDataConfusionMatrix[2,1]/
  (heartDiseaseDataConfusionMatrix[2,1] +
     heartDiseaseDataConfusionMatrix[2,2])

# Calculate the prediction accuracy by dividing the number of true
# positive and true negatives by the total amount of predictions in
# the testing dataset
sum(diag(heartDiseaseDataConfusionMatrix)) / nrow(heartDiseaseDataTesting)


################################################################################
# Neural Networks

# Import the dataset
heartDiseaseData<-read_csv(file = "heart_2020_reduced.csv",
                           col_types = "lnlllnnllffflfnlll",
                           col_names = TRUE)

# Scale BMI, PhysicalHealth, MentalHealth, SleepTime
heartDiseaseData <- heartDiseaseData %>%
  mutate(BMIScaled = (BMI - min(BMI))/(max(BMI)-min(BMI)),
         PhysicalHealthScaled = (PhysicalHealth - min(PhysicalHealth))/
           (max(PhysicalHealth)-min(PhysicalHealth)),
         MentalHealthScaled = (MentalHealth - min(MentalHealth))/
           (max(MentalHealth)-min(MentalHealth)),
         SleepTimeScaled = (SleepTime - min(SleepTime))/
           (max(SleepTime)-min(SleepTime)))

# Dummy code variables
heartDiseaseDataAgeCategory <- data.frame(heartDiseaseData$AgeCategory)
heartDiseaseDataRace <- data.frame(heartDiseaseData$Race)
heartDiseaseDataDiabetic <- data.frame(heartDiseaseData$Diabetic)
heartDiseaseDataGenHealth <- data.frame(heartDiseaseData$GenHealth)
heartDiseaseData <- data.frame(heartDiseaseData)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataAgeCategory, int = TRUE))) %>%
  select(-AgeCategory)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataRace, int = TRUE))) %>%
  select(-Race)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataDiabetic, int = TRUE))) %>%
  select(-Diabetic)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataGenHealth, int = TRUE))) %>%
  select(-GenHealth)

# Split the data into training and testing
# The set.seed()function is used to ensure that we can get the same result
# every time we run a random sampling process
set.seed(203)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(heartDiseaseData),
                    round(nrow(heartDiseaseData) *0.75),
                    replace=FALSE)

# Put the records from the 75% sample into heartDiseaseDataTraining
heartDiseaseDataTraining <- heartDiseaseData[sampleSet, ]

# Put all other records(25%) into heartDiseaseDataTesting
heartDiseaseDataTesting <- heartDiseaseData[-sampleSet, ]

# General Neural Network Model
heartDiseaseNeuralNet <- neuralnet(
  formula = HeartDisease ~ BMIScaled + SleepTimeScaled + PhysicalHealthScaled,
  data = heartDiseaseDataTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display Neural Network Results
print(heartDiseaseNeuralNet$result.matrix)

# Visualize Neural Network
plot(heartDiseaseNeuralNet)

# Generate probabilities for testing dataset
heartDiseaseProbabilities <- compute(heartDiseaseNeuralNet,
                                     heartDiseaseDataTesting)

# Display the probabilities
print(heartDiseaseProbabilities$net.result)

# Convert probabilities to 0/1
heartDiseasePrediction <-
  ifelse(heartDiseaseProbabilities$net.result > 0.5, 1, 0)

# Display predictions
print(heartDiseasePrediction)

# Display Confusion Matrix for this model
heartDiseaseDataConfusionMatrix <- table(heartDiseaseDataTesting$HeartDisease,
                                         heartDiseasePrediction)
print(heartDiseaseDataConfusionMatrix)

# Calculate the false positive rate
heartDiseaseDataConfusionMatrix[1,2]/
  (heartDiseaseDataConfusionMatrix [1,2] +
     heartDiseaseDataConfusionMatrix[1,1])

# Calculate the false negative rate
heartDiseaseDataConfusionMatrix[2,1]/
  (heartDiseaseDataConfusionMatrix[2,1] +
     heartDiseaseDataConfusionMatrix[2,2])

# Display predictive accuracy for this model
predictiveAccuracy <- sum(diag(heartDiseaseDataConfusionMatrix)) / 
  nrow(heartDiseaseDataTesting)
print(predictiveAccuracy)

################################################################################
# K-Nearest Neighbor

# Import the csv data into a tibble called heartDiseaseData
heartDiseaseData<-read_csv(file = "heart_2020_reduced.csv",
                           col_types = "fnfffnnfffffffnfff",
                           col_names = TRUE)

# Display the heartDiseaseData tibble on the console
heartDiseaseData

# Display the structure heartDiseaseData tibble on the console
str(heartDiseaseData)

# Display the summary heartDiseaseData tibble on the console
summary(heartDiseaseData)

heartDiseaseData <- heartDiseaseData %>% drop_na()

# Box Plot of Sleep Time Compared Between Heart Disease and No Heart Disease
ggplot(heartDiseaseData,aes(x=HeartDisease,y=SleepTime,fill=HeartDisease))+
  geom_boxplot()+
  scale_fill_manual(breaks = c("Yes", "No"), 
                    values=c("#AB0520", "#0C234B"))+
  ggtitle("Boxplot of Sleep Time - Comparison Between Heart Disease and No Heart 
          Disease")

# Query Looking at sleeptime and genhealth
print(heartDiseaseData %>%
        group_by(GenHealth) %>%
        summarize(averageSleepTime = mean(SleepTime)) %>%
        arrange(desc(averageSleepTime)), m = Inf)

# Converting data to numeric
# heartDiseaseData<-heartDiseaseData %>% 
#   mutate(HeartDisease = as.numeric(ifelse(HeartDisease=="Yes",1,0)),
#     Smoking = as.numeric(ifelse(Smoking=="Yes",1,0)),
#     AlcoholDrinking = as.numeric(ifelse(AlcoholDrinking=="Yes",1,0)),
#     Stroke = as.numeric(ifelse(Stroke=="Yes",1,0)),
#     DiffWalking = as.numeric(ifelse(DiffWalking=="Yes",1,0)),
#     Sex=as.numeric(ifelse(HeartDisease=="Male",1,0)),
#     PhysicalActivity = as.numeric(ifelse(PhysicalActivity=="Yes",1,0)),
#     Asthma = as.numeric(ifelse(Asthma=="Yes",1,0)),
#     KidneyDisease = as.numeric(ifelse(KidneyDisease=="Yes",1,0)),
#     SkinCancer = as.numeric(ifelse(SkinCancer=="Yes",1,0)))

# Dummy coding categorical variable
heartDiseaseDataAgeCategory <- data.frame(heartDiseaseData$AgeCategory)
heartDiseaseDataRace <- data.frame(heartDiseaseData$Race)
heartDiseaseDataDiabetic <- data.frame(heartDiseaseData$Diabetic)
heartDiseaseDataGenHealth <- data.frame(heartDiseaseData$GenHealth)
heartDiseaseData <- data.frame(heartDiseaseData)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataAgeCategory, 
                                          int = TRUE))) %>%
  select(-AgeCategory)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataRace,
                                          int = TRUE))) %>%
  select(-Race)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataDiabetic, 
                                          int = TRUE))) %>%
  select(-Diabetic)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataGenHealth, 
                                          int = TRUE))) %>%
  select(-GenHealth)

summary(heartDiseaseData)

# Separating dependent variable into a new tibble
heartDiseaseDataLabels<-heartDiseaseData%>% select(HeartDisease)
heartDiseaseDataLabels

heartDiseaseData<-heartDiseaseData%>%select(-HeartDisease)
heartDiseaseData

# Splitting the data into training and test data
set.seed(127)
sampleSet<-sample(nrow(heartDiseaseData), 
                  round(nrow(heartDiseaseData)*0.75),
                  replace=FALSE)

# Training data
heartDiseaseTraining<-heartDiseaseData[sampleSet,]
heartDiseaseTraining


heartDiseaseTrainingLabels<-heartDiseaseDataLabels[sampleSet,]
heartDiseaseTrainingLabels

# Testing data
heartDiseaseTesting<-heartDiseaseData[-sampleSet,]
heartDiseaseTesting

heartDiseaseTestingLabels<-heartDiseaseDataLabels[-sampleSet,]
heartDiseaseTestingLabels

# Generate the model
heartDiseasePrediction<-knn(train=heartDiseaseTraining,
                            test = heartDiseaseTesting,
                            cl=heartDiseaseTrainingLabels$HeartDisease,
                            k=11)
print(heartDiseasePrediction)

summary(heartDiseasePrediction)

# Generate the confusion matrix
heartDiseaseConfusionMatrix<-table(heartDiseaseTestingLabels$HeartDisease,
                                   heartDiseasePrediction)
heartDiseaseConfusionMatrix

# Calculate the false positive rate
heartDiseaseConfusionMatrix[1,2]/
  (heartDiseaseConfusionMatrix [1,2] +
     heartDiseaseConfusionMatrix[1,1])

# Calculate the false negative rate
heartDiseaseConfusionMatrix[2,1]/
  (heartDiseaseConfusionMatrix[2,1] +
     heartDiseaseConfusionMatrix[2,2])

# Calculate predictive accuracy of the model
predictiveAccuracy<-sum(diag(heartDiseaseConfusionMatrix))/
  nrow(heartDiseaseTesting)
predictiveAccuracy

# Create the k value matrix
kValueMatrix<-matrix(data=NA,nrow=0,ncol=2)

# Add column names to the k value matrix
colnames(kValueMatrix)<-c("k-Value","Predictive Accuracy")
kValueMatrix

# Loop through odd values of k
for(kValue in 1:35){
  # Only calculate predictive accuracy if the k value is odd
  if(kValue%%2!=0){
    # Generate the model
    heartDiseasePrediction<-knn(train=heartDiseaseTraining,
                                test = heartDiseaseTesting,
                                cl=heartDiseaseTrainingLabels$HeartDisease,
                                k=kValue)
    
    # Generate the confusion matrix
    heartDiseaseConfusionMatrix<-table(heartDiseaseTestingLabels$HeartDisease,
                                       heartDiseasePrediction)
    # Calculate the predictive accuracy
    predictiveAccuracy<-sum(diag(heartDiseaseConfusionMatrix))/
      nrow(heartDiseaseTesting)
    predictiveAccuracy
    
    # Add a new row the kValueMatrix
    kValueMatrix<-rbind(kValueMatrix,c(kValue,predictiveAccuracy))
    
  }
}

# Display and view the kValueMatrix to determine the best k value
print(kValueMatrix)

################################################################################
# Naive Bayes
# The following lines of code are to build the Naive Bayes Model
# Import the binned csv data into a tibble called heartDiseaseDataBinned
heartDiseaseDataBinned <- read_csv(file = "heart_2020_binned.csv",
                                   col_types = "fnfffnnfffffffnfff",
                                   col_names = TRUE)

# Display the heartDiseaseDataBinned tibble on the console
heartDiseaseDataBinned

# Display the structure heartDiseaseDataBinned tibble on the console
str(heartDiseaseDataBinned)

# Display the summary heartDiseaseDataBinned tibble on the console
summary(heartDiseaseDataBinned)

# Split the heartDiseaseDataBinned data into training and testing  
heartDiseaseNavieBayesSample <- sample(nrow(heartDiseaseDataBinned),
                                       round (nrow(heartDiseaseDataBinned) 
                                              * 0.75),
                                       replace = FALSE)

# Training tibble
heartDiseaseNavieBayesTrainingData <- heartDiseaseDataBinned[
  heartDiseaseNavieBayesSample, ]

# Testing tibble
heartDiseaseNavieBayesTestingData <- heartDiseaseDataBinned[
  -heartDiseaseNavieBayesSample, ]

# Train the naive bayes model
naiveBayesheartDiseaseModel <- naiveBayes(formula = HeartDisease ~ .,
                                          data = 
                                            heartDiseaseNavieBayesTrainingData,
                                          laplace = 1)

# Build out the probability model
heartDiseaseProbability <- predict(naiveBayesheartDiseaseModel,
                                   heartDiseaseNavieBayesTestingData,
                                   type = "raw")

# Display heartDiseaseProbability on the console
print(heartDiseaseProbability)

# Predict classes for each record in the testing dataset
naiveBayesheartDiseasePrediction <- predict(naiveBayesheartDiseaseModel,
                                            heartDiseaseNavieBayesTestingData,
                                            type = "class")

# Display naiveBayesheartDiseasePrediction on the console
print(naiveBayesheartDiseasePrediction)

# Evaluate the model by forming a confusion matrix for Naive Bayes Model
naiveBayesheartDiseaseMatrix <- table(
  heartDiseaseNavieBayesTestingData$HeartDisease,
  naiveBayesheartDiseasePrediction)

# Display the confusion matrix on the console
print(naiveBayesheartDiseaseMatrix)

# Calculate the false positive rate
naiveBayesheartDiseaseMatrix[1,2]/
  (naiveBayesheartDiseaseMatrix [1,2] +
     naiveBayesheartDiseaseMatrix[1,1])

# Calculate the false negative rate
naiveBayesheartDiseaseMatrix[2,1]/
  (naiveBayesheartDiseaseMatrix[2,1] +
     naiveBayesheartDiseaseMatrix[2,2])

# Calculate the model predictive accuracy for Naive Bayes Model
naiveBayespredictiveAccuracy <- sum(diag(naiveBayesheartDiseaseMatrix)) /
  nrow(heartDiseaseNavieBayesTestingData)

# Display the predictive accuracy for Naive Bayes Model
naiveBayespredictiveAccuracy


# Query to find the percentage of people with and without Heart disease based
# on their BMI range
print(heartDiseaseDataBinned %>%
        mutate(WeightCategoryByBMI = case_when(
          BMI <= 18.50 ~ "Underweight",
          BMI <= 24.99  ~ "Normal weight",
          BMI <= 29.99  ~ "Over weight",
          BMI > 30 ~ "Obese"
        ))) %>%
  group_by(WeightCategoryByBMI) %>%
  summarize(
    PercentageOfPeopleWithHeartDisease =
      length(HeartDisease[HeartDisease=="Yes"])*100/
      length(HeartDisease),
    PercentageOfPeopleWithoutHeartDisease =
      length(HeartDisease[HeartDisease=="No"])*100/
      length(HeartDisease))

################################################################################
# Decision Trees

# Import the csv data into a tibble called heartDiseaseData
heartDiseaseData<-read_csv(file = "heart_2020_reduced.csv",
                           col_types = "lnlllnnllffflfnlll",
                           col_names = TRUE)

# Query
print(heartDiseaseData %>%
        group_by(HeartDisease) %>%
        summarize(averageBMI = mean(BMI)) %>%
        arrange(desc(averageBMI)), m = Inf)

heartDiseaseDataAgeCategory <- data.frame(heartDiseaseData$AgeCategory)
heartDiseaseDataRace <- data.frame(heartDiseaseData$Race)
heartDiseaseDataDiabetic <- data.frame(heartDiseaseData$Diabetic)
heartDiseaseDataGenHealth <- data.frame(heartDiseaseData$GenHealth)
heartDiseaseData <- data.frame(heartDiseaseData)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataAgeCategory, int = TRUE))) %>%
  select(-AgeCategory)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataRace, int = TRUE))) %>%
  select(-Race)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataDiabetic, int = TRUE))) %>%
  select(-Diabetic)
heartDiseaseData <- as_tibble(cbind(heartDiseaseData,
                                    dummy(heartDiseaseDataGenHealth, int = TRUE))) %>%
  select(-GenHealth)

# Set Seed
set.seed(203)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(heartDiseaseData),
                    round(nrow(heartDiseaseData) *0.75),
                    replace=FALSE)

# Put the records from the 75% sample into heartDiseaseDataTraining
heartDiseaseDataTraining <- heartDiseaseData[sampleSet, ]

# Put all other records(25%) into heartDiseaseDataTesting
heartDiseaseDataTesting <- heartDiseaseData[-sampleSet, ]

# create smote
#heartDiseaseDataTrainingSmoted <-
#  tibble(SMOTE(X = data.frame(heartDiseaseDataTraining),
#               target= heartDiseaseDataTraining$HeartDisease,
#               dup_size=9)$data)


# Decision Tree model
heartDiseaseDataDecisionTreeModel <- rpart(formula = HeartDisease ~ ., 
                                           method = "class",
                                           cp = 0.01,
                                           data = heartDiseaseDataTraining )
# Display the Decision Tree Model
rpart.plot(heartDiseaseDataDecisionTreeModel)

# prediction classes for each report in training dataset
heartDiseasePrediction <- predict(heartDiseaseDataDecisionTreeModel,
                                  heartDiseaseDataTesting,
                                  type = "class")

# display the prediction
print(heartDiseasePrediction)

# Evaluate the model by forming a confusion matrix 
heartDiseaseConfusionMatrix <- table(heartDiseaseDataTesting$HeartDisease,
                                     heartDiseasePrediction)

# Display confusion Matrix on console 
print(heartDiseaseConfusionMatrix)

# Calculate Model Predictive Accuracy 
predictiveAccuracy <- sum(diag(heartDiseaseConfusionMatrix))/
  nrow(heartDiseaseDataTesting)

# Display Predictive Accuracy 
print(predictiveAccuracy)


# Using a different complexity Level 
# Decision Tree model
heartDiseaseDataDecisionTreeModel <- rpart(formula = HeartDisease ~ ., 
                                           method = "class",
                                           cp = 0.001,
                                           data = heartDiseaseDataTraining )
# Display the Decision Tree Model
rpart.plot(heartDiseaseDataDecisionTreeModel)

# prediction classes for each report in training dataset
heartDiseasePrediction <- predict(heartDiseaseDataDecisionTreeModel,
                                  heartDiseaseDataTesting,
                                  type = "class")

# display the prediction
print(heartDiseasePrediction)

# Evaluate the model by forming a confusion matrix 
heartDiseaseConfusionMatrix <- table(heartDiseaseDataTesting$HeartDisease,
                                     heartDiseasePrediction)

# Display confusion Matrix on console 
print(heartDiseaseConfusionMatrix)

# Calculate the false positive rate
heartDiseaseConfusionMatrix[1,2]/
  (heartDiseaseConfusionMatrix [1,2] +
     heartDiseaseConfusionMatrix[1,1])

# Calculate the false negative rate
heartDiseaseConfusionMatrix[2,1]/
  (heartDiseaseConfusionMatrix[2,1] +
     heartDiseaseConfusionMatrix[2,2])

# Calculate Model Predictive Accuracy 
predictiveAccuracy <- sum(diag(heartDiseaseConfusionMatrix))/
  nrow(heartDiseaseDataTesting)

# Display Predictive Accuracy 
print(predictiveAccuracy)
