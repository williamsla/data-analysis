if(!require('rstudioapi')){
  install.packages("rstudioapi") # run this if it's your first time using it to install
}
library(rstudioapi)

#definindo diretório de datasets
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

range_train = 1:691
range_test = 692:891

dataset = read.csv('train.csv', stringsAsFactors = T)

str(dataset_train)

dataset$Sex <- as.numeric(dataset$Sex)
dataset$Embarked <- as.numeric(dataset$Embarked)
dataset$Cabin <- as.numeric(dataset$Cabin)

dataset <- subset(dataset, select = -c(PassengerId, Name, Ticket))
  
dataset_train = dataset[range_train, ]
dataset_test = dataset[range_test, ]

#verificando correlação
correlation <- cor(dataset, use="complete.obs", method="pearson")

library(corrplot)
corrplot(correlation, method = "number", type = "upper", insig = "blank", 
         diag = F,
         order = "FPC",
         tl.col = "black", tl.cex = 0.5,
         cl.ratio = 0.3)

#Survived por alta correlação com Pclass, Sex, Cabin e Fare
model_lm <- lm(Survived ~ Pclass+Sex+Age+Cabin+Fare, data = dataset_train, na.action = na.omit)

pred <- predict(model_lm, newdata = dataset_test)

#considerando uma estimativa maior ou igual a 0.6 como sobrevivente, e menor como não-sobrevivente
pred[pred >= 0.6] <- 1
pred[pred < 0.6] <- 0

summary(model_lm)

if (!require("gmodels")) {
  install.packages("gmodels", dependencies = TRUE)
  library(gmodels)
}
CrossTable(x=dataset_test$Survived, y = pred, prop.chisq = F)