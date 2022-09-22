#######################
#######################
# Analise de Conglomerados Nao Hierarquica com Kmeans
# Kmeans e sensivel aos outliers
# Fonte: https://www.statology.org/k-means-clustering-in-r/
#######################
#######################


#######################
# pacotes
#######################
library(factoextra)
library(cluster)
library(dplyr)
library(openxlsx)

#######################
# preparacao dos dados
#######################

#armazenar os dados em um dataframe
df <- read.xlsx(
  xlsxFile = "D:\\Le Bomb\\kgalois\\FMU\\2022.2\\2022.2-fmu-analise-multivariada-dados\\Material Complementar\\metodos-multivariados-aula-04-exemplos-livro-manly.xlsx",
  sheet = 'empregados-europa', skipEmptyRows = FALSE)

#ver as primeiras linhas do nosso dataframe
head(df)

#remover linhas com NaN
df <- na.omit(df)

#usar a primeira coluna como nome/label das linhas
df <- data.frame(df, row.names = 1)

#remove dados categoricos
df <- df[,-c(1)]

#remove outliers
#row_names_df_to_remove<-c("row-name")
#f <- df[!(row.names(df) %in% row_names_df_to_remove),]

#calcula z-scores
#df <- scale(df)

#ver as primeiras linhas do nosso dataframe apos tratamento
head(df)

#######################
# implementacao do metodo
#######################

#plota num de cluster x soma de quadrados
fviz_nbclust(df, kmeans, method = "wss")

#calcula gap statistic baseada no numero de clusters
gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plota numero de clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#torna este example reproduzivel
set.seed(1)

#performa k-means com k clusters
km <- kmeans(df, centers = 3, nstart = 25)

#visualiza os resultados
km

#plota o resultado do modelo k-means
fviz_cluster(km, data = df)

#calcula a media para cada cluster
aggregate(df, by=list(cluster=km$cluster), mean)

#acrescenta a informacao dos clusters aos dados
final_data <- cbind(df, cluster = km$cluster)

#visualiza os dados
head(final_data)

#salva dataframe em formato xlsx. cuidado com os enderecos locais.
write.xlsx(final_data, "D:\\Le Bomb\\kgalois\\FMU\\2022.2\\2022.2-fmu-analise-multivariada-dados\\Material Complementar\\teste-kmeans.xlsx")
