setwd("~/Metas 2017/Análise IDK")
dados <- read.csv("Base_AVI_03.csv",fill = TRUE,header = TRUE,row.names = NULL,sep = ";",stringsAsFactors = FALSE) 
filtrado <- dados[grep("",dados$Input,ignore.case = T),]
filtrado2 <- filtrado[grep("portabilidade",filtrado$Input,invert = T,ignore.case = T),]
write.csv2(filtrado,"Aumento_CEP.csv")


