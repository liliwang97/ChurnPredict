# ChurnPredictFun
library(data.table)

churnPred <- function(data, cusID){
  if (cusID %in% data$CustomerId) {
    model_churn <- glm(Exited ~ CreditScore+Gender+Age+Tenure+Balance+NumOfProducts+HasCrCard+IsActiveMember+EstimatedSalary,
                       data=data, family="binomial")
    predOut <- predict(object = model_churn, newdata = data, type = "response")
    data[,churnProb:=predOut]
    res <- data[CustomerId==cusID, churnProb]
    return(res)
  }
  else{
    print("error")
  }
}
