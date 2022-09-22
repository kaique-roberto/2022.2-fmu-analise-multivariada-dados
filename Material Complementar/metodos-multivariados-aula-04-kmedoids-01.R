#######################
#######################
# Analise de Conglomerados Nao Hierarquica com Kmeans
# Kmeans e sensivel aos outliers
# Fonte: https://www.statology.org/k-medoids-in-r/
#######################
#######################


#######################
# pacotes
#######################
library(factoextra)
library(cluster)
library(dplyr)
library(openxlsx)
library(dados)

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
row_names_df_to_remove<-c("Gibraltar","Albânia","Republica Tcheca","Hungria")
df <- df[!(row.names(df) %in% row_names_df_to_remove),]

#calcula z-scores
#df <- scale(df)

#ver as primeiras linhas do nosso dataframe apos tratamento
head(df)

#######################
# implementacao do metodo
#######################

#plota num de cluster x soma de quadrados
fviz_nbclust(df, pam, method = "wss")

#calcula gap statistic baseada no numero de clusters
gap_stat <- clusGap(df,
                    FUN = pam,
                    K.max = 10,
                    B = 50)

#plota numero de clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#torna este example reproduzivel
set.seed(1)

#performa k-medoids com k clusters
kmed <- pam(df, k=3)

#visualiza os resultados
kmed

#plota o resultado do modelo k-medoids
fviz_cluster(kmed, data = df)

#acrescenta a informacao dos clusters aos dados
final_data <- cbind(df, cluster = kmed$cluster)

#visualiza os dados
head(final_data)

#salva dataframe em formato xlsx. cuidado com os enderecos locais.
write.xlsx(final_data, "D:\\Le Bomb\\kgalois\\FMU\\2022.2\\2022.2-fmu-analise-multivariada-dados\\Material Complementar\\teste-kmedoids.xlsx")
