#######################
#######################
# Analise de Conglomerados Hierarquica com Varios Metodos
# Fonte: https://www.statology.org/hierarchical-clustering-in-r/
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

#calcula z-scores
#df <- scale(df)

#ver as primeiras linhas do nosso dataframe apos tratamento
head(df)

#######################
# implementacao do metodo
#######################

#define os metodos de ligacao
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#funcao para calcular os coeficientes de agrupamento
ac <- function(x) {
  agnes(df, method = x)$ac
}

#calcula o coeficiente de aglomeracao para cada metodo de ligacao
#quanto mais proximo de 1 melhor a qualidade dos clusters
sapply(m, ac)

#performa uma aglomeracao hierarquica com variancia minima
clust <- agnes(df, method = "ward")

#plota o dendrograma
pltree(clust, cex = 0.6, hang = -1, main = "Dendrograma") 

#calcula a gap statistic para cada numero de cluster (k.max deve ser menor q o numero de variaveis)
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#plota clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#calcula a matriz de distancias
d <- dist(df, method = "euclidean")

#performa analise de conglomerados hierarquica usando o metodo de Ward
final_clust <- hclust(d, method = "ward.D2" )

#corta o dendograma em k clusters
groups <- cutree(final_clust, k=10)

#calcula o numero de observacoes em cada cluster
table(groups)

#adiciona o label do cluster aos dados originais
final_data <- cbind(df, cluster = groups)

#ver as primeiras linhas do nosso dataframe final
head(final_data)

#calcula a media em cada cluster
aggregate(final_data, by=list(cluster=final_data$cluster), mean)

#salva dataframe em formato xlsx. cuidado com os enderecos locais.
write.xlsx(final_data, "D:\\Le Bomb\\kgalois\\FMU\\2022.2\\2022.2-fmu-analise-multivariada-dados\\Material Complementar\\teste.xlsx")