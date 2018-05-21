if(!require('rstudioapi')){
  install.packages("rstudioapi") # run this if it's your first time using it to install
}
library(rstudioapi)

if(!require("e1071")){
  install.packages("e1071")
}
library("e1071")

#definindo diretório de datasets
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

data <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(data)

# drop the id feature
data <- data[-1]

# recode diagnosis as a factor
data$diagnosis <- factor(data$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

range_train <- 1:400
range_test <- 401:569
data_train <- data[range_train,]
data_test <- data[range_test,]

svm_model <- svm(diagnosis ~ ., data=data_train)
summary(svm_model)

pred <- predict(svm_model, data_test)

table(data_test$diagnosis, pred)
