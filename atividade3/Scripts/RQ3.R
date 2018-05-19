##### Library #####
if(!require('rstudioapi')){
  install.packages("rstudioapi") # run this if it's your first time using it to install
}
library(rstudioapi)

if(!require('clusterSim')){
  install.packages('clusterSim')
}
library(clusterSim)


if (!require("stringr")) {
  install.packages("stringr", dependencies = TRUE)
}
library(stringr)

if (!require("tidyr")) {
  install.packages("tidyr", dependencies = TRUE)
  library(tidyr)
}


##### Functions #####
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

effectiveness <- function(file_name) {
  
  #creating source for storing the graphs
  project_name <- str_replace(file_name, ".csv", "")
  image_path <- paste("images", project_name, sep = "/")
  if(!dir.exists(image_path)){
    dir.create(image_path, recursive = TRUE)
  }
  print(image_path)
  
  data <- read.csv(file_name);
  
  #verificando se existe NA
  data <- na.omit(data) #removendo linhas que possuem alguma célula NA

  #removendo a coluna Fold. Ela é desnecessária
  data = subset(data, select = -c(Fold))
  str(data)
  
  data$Affected <- as.numeric(data$Affected)
  
  data_n <- as.data.frame(lapply(data, normalize))
  #print( data_n[data_n$Affected == 1])
  
  head(data_n)
  
  #set.seed(3)
  folds <- createFolds(data_n$Affected, k = 2)
  
  results <- executeRandomForest(data_n, folds)
  print(results)
}

#definindo diretório de datasets
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

source <- source("Effectiveness.R", local = T)

src <- "/home/williams/projetos/UFAL/mineração/atividade3/Vulnerability Dataset"

categories <- c("unbalanced", "random_undersampling")
for (category in categories) {
    setwd(paste(src, category, sep = "/"))
 
    files <- list.files(include.dirs = FALSE)
    files <- files[!file.info(files)$isdir]
    
    for (file in files) {
        effectiveness(file)
        return()
    }
}

