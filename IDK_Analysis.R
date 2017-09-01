setwd("~/Metas 2017/Análise IDK") # determina o diretório
library(tm)
library(skmeans)
library(ggplot2) # para plotar histograma
library(dplyr)
library(Rtsne)
library(ptstem) # stemming em português
library(wordcloud)

# faz a leitura do arquivo csv. Precisa processar no Excel antes e já dividir em colunas
dados <- read.csv("teste.csv",fill = TRUE,header = TRUE,row.names = NULL,sep = ";",stringsAsFactors = FALSE) 

# conta a quantidade de caracteres em cada interação
tamanho_frases <- as.numeric(lapply(dados$Input,nchar))

# histograma do tamanho de cada interação
qplot(tamanho_frases,geom = "histogram",binwidth = 1)
# descobrir ponto no histograma
#df <- as.data.frame(as.numeric(tamanho_frases)) # transforma em data frame
#selected_rows <- (df[,1] == 17) # seleciona ponto
#dados_uteis <- dados[selected_rows, ] # retorna os dados
#distinct(as.data.frame(dados_uteis$Input)) # todas as frases distintas
#cont <- dados_uteis$Input == 'Conversar com especialistas de cartão de crédito'
#dados_Teste <- dados_uteis[cont,]

#transforma num data frame
df <- as.data.frame(as.numeric(tamanho_frases))

# Seleciona linhas que contêm entre 5 e 150 caracteres
selected_rows <- (df[,1] >= 5 & df[,1] <= 150)

# seleciona os dados úteis para análise
dados_uteis <- dados[selected_rows,]

# Nessa parte é feita a agregação dos dados na tabela, de modo a entender toda a conversa 
# do cliente, a respota do AVI e em quais categorias as conversas dele caíram

dados_agg <- aggregate(cbind(Input,Response.Text,Knowledge.Category)~Agent.Session.ID, data=dados_uteis, paste, collapse="\n") 

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


# remove acentos
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

filtrado <- dados_agg[grep("IDK",dados_agg$Knowledge.Category),]


filtrado$Input <- removeAccents(filtrado$Input)
filtrado$Input <- tokeniza(filtrado$Input)
filtrado$Input <- tolower(filtrado$Input)

filtrado_copia <- filtrado$Input

filtrado$Input <- ptstem(filtrado$Input,algorithm = "rslp",complete = FALSE)# Portuguese Stem


# Transforma os dados em um corpus para text mining
corpus_idk <- Corpus(VectorSource(filtrado$Input))

#printar uma linha: writeLines(as.character(cleanset[[1]]))

# Remove números 
# cleanset <- tm_map(corpus_idk,removeNumbers)

# Remove pontuação
cleanset <- tm_map(corpus_idk,removePunctuation)

# transforma o texto em letras minúsculas
cleanset <- tm_map(corpus_idk,content_transformer(tolower))

# Remove stop words
myStop <- stopwords('portuguese')[stopwords('portuguese') != 'não']
cleanset <- tm_map(cleanset, removeWords, myStop)

#remove espaços em branco extras
cleanset <- tm_map(cleanset,stripWhitespace)

#Transforma os dados em documentTermMatrix
dtm <- DocumentTermMatrix(cleanset)
dtm <- removeSparseTerms(dtm,0.99)
dtm_tfxidf <- weightTfIdf(dtm)
rowTotals <- apply(dtm_tfxidf , 1, sum)
dtm.new   <- dtm_tfxidf[rowTotals> 0, ] 

#Cria um cluster
#cluster_idk <- kmeans(dtm,10)


m <- as.matrix(dtm.new)
#m <- 1- cosine(m) # distância
#distM <- dist(m, method = "cosine")
# groups <- hclust(distM,method="ward.D")
# plot(groups, cex=0.9, hang=-1)
# rect.hclust(groups, k=5)

# Calcula a distância do cosseno
skm <- skmeans_xdist(m)

# determina o número ótimo de cluster
wss <- (nrow(skm)-1)*sum(apply(skm,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(skm,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Clustering
#groups <- kmeans(skm,centers = 11)
groups <- skmeans(skm,k = 18)

# Verificar quantos documentos tem em cada cluster
table(groups$cluster)

#plotar clusters
# cmd <- cmdscale(distM)
# cols = c("red","blue","pink","black","yellow","green","grey","brown","darkred","darkblue","purple","orange")
# grupos <- levels(factor(groups$cluster))
# plot(cmd,type = "n")
# for(i in seq_along(grupos)){
#   points(cmd[factor(groups$cluster) == grupos[i], ], col = cols[i], pch = 16) 
# }

#colours <- c("black","red","green","blue","orange","gray","darkred","#009900","#e6005c","purple","#e6e600")
cores = c("red","blue","pink","black","#d1d13a","green","grey","brown","darkred","darkblue","purple","orange")
cols = rainbow(18)
tsne <- Rtsne(skm,dims = 2,perplexity = 15,check_duplicates = FALSE,max_iter = 5000)
nomes <- row.names(skm)
df_plot <- cbind(tsne$Y,nomes,groups$cluster)
colnames(df_plot) <- c('x', 'y', 'names','cluster')
write.csv2(df_plot,"arquivo.csv")
data <- read.csv(file.choose(), sep=';')
ggplot(data, aes(x=x, y=y, label=names, colour= as.factor(cluster)),ggtitle("Cluster")) + geom_text(size=3) + scale_color_manual(values = cols) + theme_bw()


# Verificar os inputs em cada cluster.
filtrado_copia[groups$cluster == 4]


#wordcloud
freqr <- colSums(as.matrix(dtm))
wordcloud(names(freqr),freqr,min.freq=70,colors = cores)

#associação
findAssocs(dtm,"nao",0.1)