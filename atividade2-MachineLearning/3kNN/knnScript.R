if(!require('rstudioapi')){
  install.packages("rstudioapi") # run this if it's your first time using it to install
}
library(rstudioapi)

#definindo diretório de datasets
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

data <- read.csv("dataset-Properties_philly_Kraggle_v2.csv", stringsAsFactors = FALSE)
data <- na.omit(data)

columns <- c("Violent.Crime.Rate","Opening.Bid", "OPA","Postal.Code", "Ward","Sheriff.Cost", "Advertising",
             "Record.Deed", "PGW", "Avg.Walk.Transit.score","School.Score",
             "yearBuilt")

data <- data[,columns]

#str(data)

data$Violent.Crime.Rate[data$Violent.Crime.Rate >= 1 ] <- 'High'
data$Violent.Crime.Rate[data$Violent.Crime.Rate > 0.5 &  data$Violent.Crime.Rate < 1] <- 'Medium'
data$Violent.Crime.Rate[data$Violent.Crime.Rate <= 0.5] <- 'Low'

table(data$Violent.Crime.Rate)

#percentual por categoria de perigo
round(prop.table(table(data$Violent.Crime.Rate)) * 100, digits = 1)  

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_n <- as.data.frame(lapply(data[2:12], normalize))

range_train <- 1:400
range_test <- 401:610
data_train <- data_n[range_train,]
data_test <- data_n[range_test,]

train_labels <- data[range_train, 1]
test_labels <- data[range_test, 1]


# load the "class" library
if (!require("class")) {
  install.packages("class", dependencies = TRUE)
  library(class)
}

#The value for k is generally chosen as the square root of the number of observations.
#square root of 600 is equal to 24.9

data_test_pred <- knn(train = data_train, test = data_test,cl = train_labels, k=24)

#Avaliando a acurácia

if (!require("gmodels")) {
  install.packages("gmodels", dependencies = TRUE)
  library(gmodels)
}

CrossTable(x = test_labels, y = data_test_pred, prop.chisq=FALSE)