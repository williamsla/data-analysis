dev.off() #To reset your graphics device

if(!require("rpart")){
  install.packages("rpart", dependencies = TRUE)
}
if(!require("rpart.plot")){
  install.packages("rpart.plot", dependencies = TRUE)
}
if(!require("party")){
  install.packages("party", dependencies = TRUE)
}

setwd('/home/williams/projetos/UFAL/mineração/atividade2/5DecisionTree')
# import the CSV file
data <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# drop the id feature
data <- data[-1]

# examine the structure of the wbcd data frame
str(data)

# table of diagnosis
table(data$diagnosis)

# recode diagnosis as a factor
data$diagnosis <- factor(data$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(data$diagnosis)) * 100, digits = 1)

# create training and test data
#cria indices randômicos
set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
data_train <- data[ind==1,]
data_test <- data[ind==2,]

library(rpart)
library(rpart.plot)
library(party)

tree = rpart(diagnosis ~ ., data=data_train, method="class")

rpart.plot(tree, extra = 104, nn = TRUE)

evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  
  xtab = table(prediction, data$diagnosis)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$diagnosis)/length(data$diagnosis)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(tree, data_test, "class")
