# Libraries Requried
library(ggplot2)
library(corrplot)
library(corrgram)
library(dplyr)
library(caTools)
library(Hmisc)
library(tidyr)
library(plotrix)

# Store the csv File data into data as data.frame
data <- read.csv("C:/Users/virag/Desktop/Data Science - R Programming/Project.csv")
# Data cleaning and setting Roll_No as character to avoid that column in prediction 
data$Roll_No <- as.character(data$Roll_No)
data <- data %>% mutate(LA_1=replace(LA_1,is.na(LA_1),0))
data <- data %>% mutate(IAT_1=replace(IAT_1,is.na(IAT_1),0))
data <- data %>% mutate(Prac_1=replace(Prac_1,is.na(Prac_1),0))
data <- data %>% mutate(Asg_1=replace(Asg_1,is.na(Asg_1),0))
data <- data %>% mutate(Sem_1=replace(Sem_1,is.na(Sem_1),0))
# Printing the summary and type of data
summary(data)
str(data)

# Correlation and Corrplots
# Grab only numeric columns
num.cols <- sapply(data, is.numeric)
# Filter to numeric columns for correlation
cor.data <- cor(data[,num.cols])
# Correlation Data
corrplot::corrplot(cor.data, method='color')
corrgram(data, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie,text.panel=panel.txt)
# Histogram
qplot(Sem_1, data=data, geom='histogram', bins=20, fill=..count.., xlab='Sem 1 Pointers', ylab = 'No. of Students')

# Set the seed
set.seed(101)
# Split the data into test and train sets
sample <- sample.split(data$Sem_1, SplitRatio = 0.83)
# Training data
train <- subset(data[2:6],sample==TRUE)
summary(train)
# Test data
test <- subset(data[2:6],sample==FALSE)
summary(test)

#Apply Multi Variate Regression and create model using train data
model <- lm(Sem_1~., train)
summary(model)

# Grab residuals
res <- residuals(model)
# Convert to data frame for plot
res <- as.data.frame(res)
# Plot residuals
qplot(res, data=res, geom='histogram', bins = 30, xlab = 'Residuals', ylab = 'No. of Students')
par(mfrow=c(2,2))
plot(model)

# Test our model by predicting on our testing set
sem1prec <- predict(model, test)
summary(sem1prec)

# Create a dataset of actual and predicted results to check model performance
results <- cbind(sem1prec, test$Sem_1)
colnames(results) <- c('Predicted', 'Real')
results <- as.data.frame(results)
results

# Plot the predicted pointer
DF <- rbind(data.frame(fill="blue", obs=results$Predicted),
            data.frame(fill="green", obs=results$Real))
ggplot(DF, aes(x=obs, fill=fill)) + ggtitle("Pointers Predicted") +
  xlab("Pointers") + ylab("No. of Students") +
  geom_histogram(binwidth=0.25, colour="black", position="dodge") +
  scale_x_continuous(breaks=seq(0, 10, 0.5)) +
  scale_fill_manual(name="Legend",values=c("red","green"),labels=c("Predicted","Real"))

# Check the performance of our model
sse <- sum((results$Predicted - results$Real)^2)
sst <- sum((mean(data$Sem_1) - results$Real)^2)
R2 <- 1-sse/sst
R2

# Create function to replace negative values with 0
to_zero <- function(x){
  if(x<0){
    return(0)
  } else {
    return(x)
  }
}
# Apply the function to predicted results
results$Predicted <- sapply(results$Predicted, to_zero)
# Check the range of predicted values
range(results$Predicted)

# Improved Prediction Accuracy
sse <- sum((results$Predicted - results$Real)^2)
sst <- sum((mean(data$Sem_1) - results$Real)^2)
R2 <- 1-sse/sst
RMSE <- sqrt(mean((results$Real - results$Predicted)^2))
R2
RMSE
