setwd("~/Metas 2017/Análise IDK/Falso Positivo")
library(dplyr)

dados <- read.csv("Base_AVI_07.csv",fill = TRUE,header = TRUE,row.names = NULL,sep = ";",stringsAsFactors = FALSE) 
only_input <- dados[grep("Typed Input",dados$Entry.Type),]
dados_fp <- read.csv("FP_GERAL_201707.csv",sep = ";",stringsAsFactors = FALSE)
only_input <- cbind(only_input,is_fp = FALSE)
x <- distinct(as.data.frame(dados_fp$Agent.Session.ID))
for (i in 1:nrow(only_input)){
  for (j in 1:nrow(x)){
    if (only_input$Agent.Session.ID[i] == x[j,1]){
      only_input$is_fp[i] <- TRUE
    }
  }
}

write.csv2(dados,"Arquivo_Julho.csv")
  
#lapply(dados$is_fp,function(x) if (dados$Agent.Session.ID %in% x) dados$is_fp <- TRUE else FALSE )