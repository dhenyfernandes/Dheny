setwd("~/Metas 2017/An�lise IDK/Apresentacao Elaine") # determina o diret�rio
library(tm)
library(skmeans)
library(ggplot2) # para plotar histograma
library(dplyr)
library(Rtsne)
library(ptstem) # stemming em portugu�s

# faz a leitura do arquivo csv. Precisa processar no Excel antes e j� dividir em colunas
dados <- read.csv("teste.csv",fill = TRUE,header = TRUE,row.names = NULL,sep = ";",stringsAsFactors = FALSE) 

# conta a quantidade de caracteres em cada intera��o
tamanho_frases <- as.numeric(lapply(dados$Input,nchar))

# histograma do tamanho de cada intera��o
qplot(tamanho_frases,geom = "histogram",binwidth = 1)
# descobrir ponto no histograma
df <- as.data.frame(as.numeric(tamanho_frases)) # transforma em data frame
selected_rows <- (df[,1] == 27) # seleciona ponto
dados_uteis <- dados[selected_rows, ] # retorna os dados
frases_distintas <- distinct(as.data.frame(dados_uteis$Input)) # todas as frases distintas

contagem <- function(x){
  cont <- vector(mode="numeric", length=nrow(x))
  for (i in 1:nrow(x)){
    cont[i] <- sum(as.numeric(dados_uteis$Input == x[i,1])) 
  }
  return(cont)
}
qtde <- contagem(frases_distintas)
qtde_frases_distinct <- cbind(frases_distintas,qtde)
qtde_frases_distinct <- qtde_frases_distinct[order(qtde,decreasing = T),]

filtrado <- dados_uteis[grep("Aumentar o limite do cart�o",dados_uteis$Input,ignore.case = T),]

# --------------------------------------------------------------------#
#An�lise separada por tipo de resposta que o AVI deu
#distinct_answer <- distinct(as.data.frame(filtrado$Response.Text))
#answer <- filtrado[grep("Por favor, acesse sua conta ou seu cart�o, procure na busca por",filtrado$Response.Text,ignore.case = T),]
#comparison <- semi_join(dados,answer,by = "Agent.Session.ID") # traz a conversa toda
# --------------------------------------------------------------------#

comparison <- semi_join(dados,filtrado,by = "Agent.Session.ID") # traz a conversa toda

# --------------------------------------------------------------------#
only_input <- comparison[grep("Typed Input",comparison$Entry.Type),]
lenght_input <- as.numeric(lapply(only_input$Input,nchar))
qplot(lenght_input,geom = "histogram",binwidth = 1)
df_input <- as.data.frame(as.numeric(lenght_input)) 
linhas_selecionadas <- (df_input[,1] > 4) 
input_selected <- only_input[linhas_selecionadas, ] 

# Remover frases que contenham 2 tokens ou menos
# --------------------------------------------------------------------#

# transforma n�meros, datas e valores em "qualquer padr�o" em _token
tokeniza <- function(x){
  x <- gsub("xxxxxxxxxxxxxxxxx","_token",x,ignore.case = T) # n� protocolo
  x <- gsub("xxxxxxxxxxxxxxxx","_token",x,ignore.case = T) # n� cart�o
  x <- gsub("xxxx.xxxx.xxxx.xxxx","_token",x,ignore.case = T) # n� cart�o com ponto
  x <- gsub("xxxxxxxxxxx","_token",x,ignore.case = T) #  CPF
  x <- gsub("xxxxxxxxx","_token",x,ignore.case = T) # n�mero
  x <- gsub("xxxxx-xxx","_token",x,ignore.case = T) # CEP
  x <- gsub("R$xxxx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("R$xxxx","_token",x,ignore.case = T) #valor
  x <- gsub("xxxx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("xx/xx/xxxx","_token",x,ignore.case = T) #data
  x <- gsub("xxxx","_token",x,ignore.case = T)# Final cart�o
  x <- gsub("R$xxx,xx","_token",x,ignore.case = T) # valor
  x <- gsub("R$xxx","_token",x,ignore.case = T) # valor
  x <- gsub("xxx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("xxx","_token",x,ignore.case = T) #valor
  x <- gsub("xx,xx","_token",x,ignore.case = T) #valor
  x <- gsub("xx/xx","_token",x,ignore.case = T) # data
  x <- gsub("xx","_token",x,ignore.case = T) #valor
  x <- gsub("x.xxx,xx","_token",x,ignore.case = T)#valor
  x <- gsub("agosto/xxxx","_token",x,ignore.case = T) #data
  x <- gsub("1�|2�|3�","_token",x,ignore.case = T)# numeral
  x <- gsub("2.0","_token",x,ignore.case = T)# cart�o 2.0
  
  return(x)
}

# remove acentos
removeAccents <- function(x){
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', '�'='A', '�'='A', '�'='A', '�'='A', '�'='A', '�'='A', '�'='A', '�'='C', '�'='E', '�'='E',
                            '�'='E', '�'='E', '�'='I', '�'='I', '�'='I', '�'='I', '�'='N', '�'='O', '�'='O', '�'='O', '�'='O', '�'='O', '�'='O', '�'='U',
                            '�'='U', '�'='U', '�'='U', '�'='Y', '�'='B', '�'='Ss', '�'='a', '�'='a', '�'='a', '�'='a', '�'='a', '�'='a', '�'='a', '�'='c',
                            '�'='e', '�'='e', '�'='e', '�'='e', '�'='i', '�'='i', '�'='i', '�'='i', '�'='o', '�'='n', '�'='o', '�'='o', '�'='o', '�'='o',
                            '�'='o', '�'='o', '�'='u', '�'='u', '�'='u', '�'='y', '�'='y', '�'='b', '�'='y' )
  out <- x
  for(i in seq_along(unwanted_array))
    out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)
  return(out)
}
dados_agg_copia <- aggregate(cbind(Input,Response.Text,Knowledge.Category)~Agent.Session.ID, data=comparison, paste, collapse="\n") 

analize <- function(x){
  to_analize <- list()
  agent <- list()
  for (i in 1:nrow(x)){
    if (x$Input[i] == "Parcelar a fatura" & x$Entry.Type[i] == "Click Unit Link"){
      agent[[length(agent)+1]] <- x$Agent.Session.ID[i]
      to_analize[[length(to_analize)+1]] <- x$Input[i-1]
    }
  }
  retorno <- cbind(agent,to_analize)
  return(as.data.frame(retorno))
}

analizar <- analize(comparison)
# colnames(analizar) <- c("Agent.Session.ID","Input")
# analizar <- transform(analizar,Agent.Session.ID = as.character(Agent.Session.ID))
# resultado <- anti_join(filtrado,analizar, by = "Agent.Session.ID")

comparison$Input <- removeAccents(comparison$Input)
comparison$Input <- tolower(comparison$Input)
comparison$Input <- tokeniza(comparison$Input)
comparison$Input <- ptstem(comparison$Input,algorithm = "rslp",complete = FALSE)# Portuguese Stem

#Agrega os dados pelo ID
dados_agg <- aggregate(cbind(Input,Response.Text,Knowledge.Category)~Agent.Session.ID, data=comparison, paste, collapse="\n") 


# Transforma os dados em um corpus para text mining
corpus_idk <- Corpus(VectorSource(dados_agg$Input))

#printar uma linha: writeLines(as.character(cleanset[[1]]))

# Remove n�meros 
cleanset <- tm_map(corpus_idk,removeNumbers)

# Remove pontua��o
cleanset <- tm_map(corpus_idk,removePunctuation)

# Remove stop words
myStop <- stopwords('portuguese')[stopwords('portuguese') != 'n�o']
cleanset <- tm_map(cleanset, removeWords, myStop)

#remove espa�os em branco extras
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
#m <- 1- cosine(m) # dist�ncia
#distM <- dist(m, method = "cosine")
# groups <- hclust(distM,method="ward.D")
# plot(groups, cex=0.9, hang=-1)
# rect.hclust(groups, k=5)

# Calcula a dist�ncia do cosseno
skm <- skmeans_xdist(m)

groups <- skmeans(skm,k = 9)

# Verificar quantos documentos tem em cada cluster
table(groups$cluster)

#plotar 
cols = rainbow(9)
tsne <- Rtsne(skm,dims = 2,perplexity = 10,check_duplicates = FALSE,max_iter = 5000)
nomes <- row.names(skm)
df_plot <- cbind(tsne$Y,nomes,groups$cluster)
colnames(df_plot) <- c('x', 'y', 'names','cluster')
write.csv2(df_plot,"ponto1.csv")
data <- read.csv(file.choose(), sep=';')
ggplot(data, aes(x=x, y=y, label=names, colour= as.factor(cluster))) + geom_text(size=3) + scale_color_manual(values = cols) + theme_bw()

#DEIXAR uma c�pia de dados_agg sem estemizar para comparar

# Verificar os inputs em cada cluster.
dados_agg_copia$Input[groups$cluster == 2]
