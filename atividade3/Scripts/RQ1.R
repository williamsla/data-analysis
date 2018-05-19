correlation <- function(src_base, dataset_type, file_name){
  setwd(src_base)
  
  if (!require("corrplot")) {
    install.packages("corrplot", dependencies = TRUE)
    library(corrplot)
  }
  if(!require("tidyr")){
    install.packages("tidyr", dependencies = TRUE)
    library(tidyr)
  }

  #lendo CSV
  filePath <- paste(src_base,dataset_type,file_name, sep="/")
  data <- read.csv(filePath);
  
  #verificando se existe NA
  if(anyNA(data)){
    data <- na.omit(data) #removendo linhas que possuem alguma célula NA
  }
  
  #removendo a coluna Fold. Ela é desnecessária
  data = subset(data, select = -c(Fold))
  
  str(data)
  
  table(data$Affected)
  #converting factor Affected to number
  #1 = NEUTRAL and 2 = VULNERABLE
  data$Affected <- as.numeric(data$Affected)
  
  print(table(data$Affected))
  
  cor_data <- cor(data, use="all.obs", method="pearson")
  
  #nível de confiança (probabilidade de estar correto)
  p_mat <- cor.mtest(cor_data, conf.level = .99)
  
   
  library(stringr)
  img_name <- str_replace(file_name, "csv", "png")
  img_name <- paste("RQ1", img_name, sep = "-")
  img_path <- paste("images", img_name, sep="/")
  png(img_path)
  
  #nível de significância (probabilidade de erro)
  corrplot(cor_data, method = "color", type = "upper",
           insig = "label_sig", p.mat = p_mat$p, sig.level = 0.01, 
           order = "hclust", hclust.method = "single",
           tl.col = "black", tl.cex = 0.7,
           cl.ratio = 0.15, cl.cex = 0.7,
           is.corr = FALSE, diag = F)
  dev.off()
}

src_balanced <- "/home/williams/projetos/UFAL/mineração/atividade3/Vulnerability Dataset"
src_unbalanced <- "/home/williams/projetos/UFAL/mineração/atividade3/Vulnerability Dataset"
balanced <- "random_undersampling"
unbalanced <- "unbalanced"

#glibc
correlation(src_balanced, balanced, "glibc_data_balanced.csv")
correlation(src_unbalanced, unbalanced, "glibc_data.csv")

#httpd
correlation(src_balanced, balanced, "httpd_data_balanced.csv")
correlation(src_unbalanced, unbalanced, "httpd_data.csv")

#kernel
correlation(src_balanced, balanced, "kernel_data_balanced.csv")
correlation(src_unbalanced, unbalanced, "kernel_data.csv")

#mozilla
correlation(src_balanced, balanced, "mozilla_data_balanced.csv")
correlation(src_unbalanced, unbalanced, "mozilla_data.csv")

#xen
correlation(src_balanced, balanced, "xen_data_balanced.csv")
correlation(src_unbalanced, unbalanced, "xen_data.csv")

