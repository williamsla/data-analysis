if (!require("stringr")) {
  install.packages("stringr", dependencies = TRUE)
  library(stringr)
}

if (!require("tidyr")) {
  install.packages("tidyr", dependencies = TRUE)
  library(tidyr)
}


detectionVulnerabilities <- function(file_name) {
  
  project_name <- str_replace(file_name, ".csv", "")
  image_path <- paste("images", project_name, sep = "/")
  if(!dir.exists(image_path)){
    dir.create(image_path, recursive = TRUE)
  }
  print(image_path)
  
  #lendo CSV
  data <- read.csv(file_name);

  #verificando se existe NA
  if(anyNA(data)){
    data <- na.omit(data) #removendo linhas que possuem alguma célula NA
  }
  
  #removendo a coluna Fold. Ela é desnecessária
  data = subset(data, select = -c(Fold))
  
  groups <- split(data, data$Affected)
  data_neutral <- groups$'NEUTRAL'
  data_vulnerable <- groups$'VULNERABLE'
  
  for (colname in colnames(data)) {
    if(tolower(colname) == "affected") next
    
    metric_group_neutral <- data_neutral[,colname]
    metric_group_vulnerable <- data_vulnerable[,colname]
    
    test <- wilcox.test(metric_group_neutral, metric_group_vulnerable )
    
    if(test$p.value <= 0.01){
        while (!is.null(dev.list()))  dev.off()
      
        
        img_name <- str_replace(file_name, ".csv", paste(colname,"png", sep = "."))
        img_name <- paste("RQ2",img_name, sep = "-")
        
        png(filename = paste(image_path, img_name, sep="/"))
      
        boxplot(data[,colname]~Affected, data = data,
                main = paste(colname, "Affected", sep = " X "), 
                ylab = colname, xlab="Affected", col= c("gray","yellow"))
    }
  }
  dev.off()
}

src <- "/home/williams/projetos/UFAL/mineração/atividade3/Vulnerability Dataset"

categories <- c("unbalanced", "random_undersampling")
for (category in categories) {
    setwd(paste(src, category, sep = "/"))
  
    files <- list.files(include.dirs = FALSE)
    files <- files[!file.info(files)$isdir]

    for (file in files) {

        detectionVulnerabilities(file)
  }
}
