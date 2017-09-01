setwd("~/Metas 2017/Análise IDK") # determina o diretório
library(tm)
library(skmeans)
library(ggplot2) 
library(dplyr)
library(Rtsne)
library(ptstem) # stemming em português

dados <- read.csv("teste.csv",fill = TRUE,header = TRUE,row.names = NULL,sep = ";",stringsAsFactors = FALSE) 

#seleciona somente as interações com IDK
filtrado <- dados[grep("Clarifiers|^[[:space:]]*$",dados$Knowledge.Category,invert = T),]
filtrado <- filtrado[grep("IDK",filtrado$Knowledge.Category),]
x <- sample(1:nrow(filtrado),5000) # DIMINUIR
filtrado <- filtrado[x,]

tokeniza <- function(x){
  x <- gsub("xxxxxxxxxxxxxxxxx","_token",x,ignore.case = T) # nº protocolo
  x <- gsub("xxxxxxxxxxxxxxxx","_token",x,ignore.case = T) # nº cartão
  x <- gsub("xxxx.xxxx.xxxx.xxxx","_token",x,ignore.case = T) # nº cartão com ponto
  x <- gsub("xxxxxxxxxxx","_token",x,ignore.case = T) #  CPF
  x <- gsub("xxxxxxxxx","_token",x,ignore.case = T) # número
  x <- gsub("xxxxx-xxx","_token",x,ignore.case = T) # CEP
  x <- gsub("R$xxxx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("R$xxxx","_token",x,ignore.case = T) #valor
  x <- gsub("xxxx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("xx/xx/xxxx","_token",x,ignore.case = T) #data
  x <- gsub("xxxx","_token",x,ignore.case = T)# Final cartão
  x <- gsub("R$xxx,xx","_token",x,ignore.case = T) # valor
  x <- gsub("R$xxx","_token",x,ignore.case = T) # valor
  x <- gsub("xxx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("xxx","_token",x,ignore.case = T) #valor
  x <- gsub("xx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("xx/xx","_token",x,ignore.case = T) # data
  x <- gsub("xx","_token",x,ignore.case = T) #valor
  x <- gsub("x.xxx,xx","_token",x,ignore.case = T)#valor
  x <- gsub("agosto/xxxx","_token",x,ignore.case = T) #data
  x <- gsub("1º|2ª|3º","_token",x,ignore.case = T)# numeral
  x <- gsub("2.0","_token",x,ignore.case = T)# cartão 2.0
  
  return(x)
}

removeAccents <- function(x){
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  out <- x
  for(i in seq_along(unwanted_array))
    out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)
  return(out)
}

filtrado$Input <- removeAccents(filtrado$Input)
filtrado$Input <- tokeniza(filtrado$Input)
filtrado$Input <- tolower(filtrado$Input)
filtrado$Input <- ptstem(filtrado$Input,algorithm = "rslp",complete = FALSE)# Portuguese Stem

# Cria um corpus e limpa os dados sujos
corpus_idk <- Corpus(VectorSource(filtrado$Input))
#cleanset <- tm_map(corpus_idk,removeNumbers)
cleanset <- tm_map(corpus_idk,removePunctuation)
myStop <- stopwords('portuguese')[stopwords('portuguese') != 'não']
cleanset <- tm_map(cleanset, removeWords, myStop)
cleanset <- tm_map(cleanset,stripWhitespace)

# cria uma matrix de documentos x termos
dtm <- DocumentTermMatrix(cleanset)
dtm <- removeSparseTerms(dtm,0.99)
dtm_tfxidf <- weightTfIdf(dtm)
rowTotals <- apply(dtm_tfxidf , 1, sum)
dtm.new   <- dtm_tfxidf[rowTotals> 0, ] 

# transforma em matrix e calcula a distância do cosseno
m <- as.matrix(dtm.new)
skm <- skmeans_xdist(m)

#writeLines(as.character(cleanset[[8]]))

# determina o número ótimo de cluster
wss <- (nrow(skm)-1)*sum(apply(skm,2,var))
for (i in 2:50) wss[i] <- sum(kmeans(skm,centers=i)$withinss)
plot(1:50, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


# clustering
groups <- skmeans(skm,k = 43)

table(groups$cluster)

#plot cluster
cols = rainbow(43)
tsne <- Rtsne(skm,dims = 2,perplexity = 20,check_duplicates = FALSE,max_iter = 5000,theta = 0.0)
nomes <- row.names(skm)
df_plot <- cbind(tsne$Y,nomes,groups$cluster)
colnames(df_plot) <- c('x', 'y', 'names','cluster')
write.csv2(df_plot,"arquivo.csv")
data <- read.csv(file.choose(), sep=';')
ggplot(data, aes(x=x, y=y, label=names, colour= as.factor(cluster))) + geom_text(size=3) + scale_color_manual(values = cols) + theme_bw()

# Verificar os inputs em cada cluster.
filtrado$Input[groups$cluster == 9]


