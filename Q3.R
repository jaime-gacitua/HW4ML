library(mlbench)
library(e1071)
library(caret)
library(ggplot2)

data(BreastCancer)
raw.data <- BreastCancer

## Count number of missing values
sapply(raw.data, function(x) sum(is.na(x)))

## We are missing 16 elements in the column Bare.nuclei.
table(raw.data$Bare.nuclei)
# This is a  column of type factor, with levels from 1 to 10 will add the new type 11
levels(raw.data$Bare.nuclei) <- c(levels(raw.data$Bare.nuclei), 11)
raw.data$Bare.nuclei[is.na(raw.data$Bare.nuclei)] <- as.factor(11) 
table(raw.data$Bare.nuclei)
# Remove column ID
raw.data <- raw.data[,2:11]

# Function to calculate SVM
calculateSVM <- function(data, t){

  # Divide test set and training set according to proportion t
  sub <- sample(nrow(data), size = floor(nrow(raw.data) * t))
  trainData <- data.frame(data[sub,])
  testData <- data.frame(data[-sub,])
  
  # Fitting best SVM through cross validation
  tune.out=tune(svm, Class~., data = trainData , 
                kernel="linear", ranges=list(
                  cost=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 100)))
  bestmod=tune.out$best.model

  # Predicting values in the test set,
  ypred=predict(bestmod,testData)
  result = confusionMatrix(ypred, testData$Class)
  return(result)
}

# Part a
t <- as.numeric(0.7)
acc <- calculateSVM(raw.data, t)

# Part b

## Number of repetitions and reset data frame for storing values
N <- as.numeric(50)
accB <- data.frame()

## Loop
for(i in 1:N){
  res <- calculateSVM(raw.data, t)
  accB = rbind(accB, res$overall[1])  
}

## Summarize
colnames(accB) <- 'accuracy'
qplot(accB$accuracy, geom='histogram', main='Test Set Errrors for N=50 experiments of SVM')
sapply(accB, mean)
sapply(accB, sd)

# Part c
## Create sequence of t's
N <- as.numeric(50)
t.values <- seq(from = 0.5, to = 0.95, by = 0.05)
## Initialize data matrix
means.sd <- matrix(ncol = 3, nrow = length(t.values))

j <- as.numeric(1)
## Loop and record data
for(t in t.values){
  # Reset matrix
  accC <- matrix(ncol = 1, nrow = N)
  for(i in 1:N){
    res <- calculateSVM(raw.data, t)
    accC[i,1] <- res$overall[1]
    # display progress
    cat("loop", t, "rep ", i, "\n")
    flush.console()
  }
  means.sd[j,1] <- t
  means.sd[j,2] <- mean(accC)
  means.sd[j,3] <- sd(accC)
  j <- j+1
}

?plot

plot(means.sd[,1], means.sd[,2], ylim = c(0.9, 1), xlab = 't', ylab = 'accuracy', 
     main = 'Mean Accuracy of SVM for different sizes of training set (t%) ')
lines(means.sd[,1],means.sd[,2]+ 1.96*means.sd[,3]/sqrt(N),col="red",lty=2)
lines(means.sd[,1],means.sd[,2]-1.96*means.sd[,3]/sqrt(N),col="red",lty=2)








