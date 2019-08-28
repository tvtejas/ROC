ROC<-function(data,x,y)
{
  library(caTools)
  set.seed(0)
  x=as.matrix(x)
  split <- sample.split(y, SplitRatio = 0.75)
  
  split  ##It returns the array of true and false
  
  #Get training data and test data
  train <- subset(data, split == TRUE)
  test <- subset(data, split == FALSE)
  threshold <- c(1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.0)  #Threshold vector
  sensitivity <- c()  #sensitivity
  specificity <- c()  #Specificity
  fpr <- c()
  logit <- glm(y~x, data = train, family = binomial)
  pred <- predict(logit, test, type ="response")
  for (i in 1:length(threshold))
  {
    
    pred_class = ifelse(pred > threshold[i], 1, 0)
    conf = prop.table(table(y, pred_class))
    
    if(threshold[i] == 1)
    {
      sensitivity[i] <- 0
      specificity[i] <- conf[1][1] / (conf[1][1] + 0)
      fpr[i] <- 1 - specificity[i]
    }
    else if(threshold[i] == 0)
    {
      sensitivity[i] <- conf[2][1] / (conf[2][1] + 0)
      specificity[i] <- 0
      fpr[i] <- 1 - specificity[i]
    }
    else
    {
      sensitivity[i] <- conf[2, 2]/(conf[2, 2] + conf[2, 1])
      specificity[i] = conf[1, 1]/(conf[1, 1] + conf[1, 2])
      fpr[i] <- 1 - specificity[i]
    }
  }
  plot(fpr, sensitivity, type = 'line',xlab = "Flase Positive Rate",ylab="True Positive Rate")
  
  
}

