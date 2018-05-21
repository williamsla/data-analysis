#Getting started with Naive Bayes
#Install the package
if(!require("XML")){
  install.packages("XML")
}

if(!require("e1071")){
  install.packages("e1071")
}

if(!require("mlr")){
  install.packages("mlr")
}

#Loading the library
library(e1071)
data("Titanic")

Titanic_df=as.data.frame(Titanic)
#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) #This will repeat each combination equal to the frequency of each combination

#Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]
#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL

#Cria o modelo Naive Bayes com base na coluna Survived
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Realiza predições a partir do modelo e dos dados
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)

library(mlr)

#Cria uma tarefa de classificação para aprender, especificando a característica alvo
task = makeClassifTask(data = Titanic_dataset, target = "Survived")

#Inicializa o classificador
selected_model = makeLearner("classif.naiveBayes")

#Treinamento
NB_mlr = train(selected_model, task)

#modelo aprendido
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Titanic_dataset[,1:3]))

##matriz de Confusão
table(predictions_mlr[,1],Titanic_dataset$Survived)