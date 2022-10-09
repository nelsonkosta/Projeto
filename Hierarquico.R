########################################################################################################################################
##Verificação do melhor algoritmo para agrupamento - Dataset das caixas

library(factoextra)
library(gridExtra)
library(NbClust)
library(tidyverse)
library(clValid)

#load data
dfcaixas <- caixas[,c(5,7,9,13)]

#remove rows with missing values
dfcaixas <- na.omit(dfcaixas)

#scale each variable to have a mean of 0 and sd of 1
dfcaixas <- scale(dfcaixas)

#métodos de clustering

clmethods <- c("hierarchical","kmeans")
intern <- clValid(dfcaixas, nClust = 3:6, clMethods = clmethods, validation = "internal")
summary(intern)

#Hierarchical Clustering com 3 clusters é a melhor opção (optimal score)

#Score para a estabilidade

stab <- clValid(dfcaixas, nClust = 3:6, clMethods = clmethods,
                validation = "stability")
optimalScores(stab)
#hierarchical com 4 clusters tem melhor score para estabilidade, seguido de hierárquico com 3 clusters

#############################################
#Hierarchical Clustering

methodsHc <- c("single", "complete", "average")

methodsDist <- c("euclidean", "maximum", "manhattan")

#Testagem do método da distância
res.dist <- function(x){
  dist(dfcaixas, method = methodsDist[x])
}

#Qual combinação representa o melhor agrupamento dos dados?

correlacao <- function(x,y){
  res.dist <- dist(dfcaixas, method = methodsDist[x])
  res.hc <- hclust(res.dist, method = methodsHc[y])
  res.coph <- cophenetic(res.hc)
  cor(res.dist, res.coph)
}
for(i in 1:3){
  for (j in 1:3){
    a <- paste("D:", methodsDist[i], "Hc:", methodsHc[j], "Cor:", correlacao(i, j))
    print(a)
  }
}

#average + euclidean representa um melhor agrupamento de dados



#Cortar o dendrograma em tres grupos

res.dist <- dist(dfcaixas, method = methodsDist[1])
res.hc <- hclust(res.dist, method = methodsHc[3])
grp <- cutree(res.hc, k = 3)

#silhouette plot
sil <- silhouette(grp, dist(dfcaixas))
fviz_silhouette(sil)

#validação do agrupamento realizado de maneira interna

b <- NbClust(dfcaixas, distance = methodsDist[1], min.nc = 3, max.nc = 6, method = methodsHc[3], index = "silhouette")

b[1:2]

#Coloca retângulo no dendrograma apartando os clusters
plot(res.hc)
#rect.hclust(res.hc, k = 3, border = 3:6)
#abline(h = 305, col = 'red')

#Visualizar graficamente os clusters

fviz_cluster(list(data = dfcaixas, cluster = grp))

#Contagem de clusters no Hierarchical Clustering
library(dplyr)
seeds_df_cl <- mutate(caixas, cluster = grp)
count(seeds_df_cl,cluster)

cor(dist(dfcaixas, method = 'euclidean'), cophenetic(res.hc)) #coeficiente cofenético igual a 0.9663 o que traduz um bom agrupamento dos dados


###############CONCLUSÕES DOS CLUSTERS

caixas[141,] #A ENTRADA 141 DIZ RESPEITO A UMA OBSERVAÇÃO DA CAIXA GOURMET M 14766 EM 2021 e faz parte das caixas mais vendidas do Nivel 3 de Vendas 

caixas[189,] #A ENTRADA 189 DIZ RESPEITO A UMA OBSERVAÇÃO DA CAIXA ELISEE HYPE 15075 EM 2020 e faz parte das caixas mais vendidas do Nivel 1 de vendas

#Os restantes produtos agregam-se num mesmo cluster(1)


########################################################################################################################################
##Verificação do melhor algoritmo para agrupamento - Dataset dos sacos

library(factoextra)
library(gridExtra)
library(NbClust)
library(tidyverse)
library(clValid)

#load data
dfsacos <- sacos[,c(5,7,9,11,13)]

#remove rows with missing values
dfsacos <- na.omit(dfsacos)

#scale each variable to have a mean of 0 and sd of 1
dfsacos <- scale(dfsacos)

#métodos de clustering

clmethods <- c("hierarchical","kmeans")
intern <- clValid(dfsacos, nClust = 3:6, clMethods = clmethods, validation = "internal")
summary(intern)

#K-Means com 3 clusters é a melhor opção (optimal score)

#Score para a estabilidade

stab <- clValid(dfsacos, nClust = 3:6, clMethods = clmethods,
                validation = "stability")
optimalScores(stab)
#hierarchical com 3 clusters tem melhor score para estabilidade

#############################################
#Hierarchical Clustering

methodsHc <- c("single", "complete", "average")

methodsDist <- c("euclidean", "maximum", "manhattan")

#Testagem do método da distância
res.dist <- function(x){
  dist(dfsacos, method = methodsDist[x])
}

#Qual combinação representa o melhor agrupamento dos dados?

correlacao <- function(x,y){
  res.dist <- dist(dfsacos, method = methodsDist[x])
  res.hc <- hclust(res.dist, method = methodsHc[y])
  res.coph <- cophenetic(res.hc)
  cor(res.dist, res.coph)
}
for(i in 1:3){
  for (j in 1:3){
    a <- paste("D:", methodsDist[i], "Hc:", methodsHc[j], "Cor:", correlacao(i, j))
    print(a)
  }
}

#average + euclidean representa um melhor agrupamento de dados



#Cortar o dendrograma em tres grupos

res.dist <- dist(dfsacos, method = methodsDist[1])
res.hc <- hclust(res.dist, method = methodsHc[3])
grp <- cutree(res.hc, k = 3)

#silhouette plot
sil <- silhouette(grp, dist(dfsacos))
fviz_silhouette(sil)

#validação do agrupamento realizado de maneira interna

b <- NbClust(dfsacos, distance = methodsDist[1], min.nc = 3, max.nc = 6, method = methodsHc[3], index = "silhouette")

b[1:2]

#Coloca retângulo no dendrograma apartando os clusters
plot(res.hc)
#rect.hclust(res.hc, k = 3, border = 3:6)
#abline(h = 305, col = 'red')

#Visualizar graficamente os clusters

fviz_cluster(list(data = dfsacos, cluster = grp))

#Contagem de clusters no Hierarchical Clustering
library(dplyr)
seeds_df_cl <- mutate(sacos, cluster = grp)
count(seeds_df_cl,cluster)

cor(dist(dfsacos, method = 'euclidean'), cophenetic(res.hc)) #coeficiente cofenético igual a 0.9773 o que traduz um bom agrupamento dos dados

#______________________________
########CONCLUSÕES DOS CLUSTERS
#Identificação de clusters
sacos[605,]#A ENTRADA 605 DIZ RESPEITO A UMA OBSERVAÇÃO DO SACO COLORIS 61100 EM 2018 (CLUSTER ÚNICO)
sacos[sacos$Codigo=="61100",] #Saco COLORIS 61100 - Faz parte do TOP de sacos mais vendidos (venda bastante expressiva)
sacos[608,] #A ENTRADA 608 DIZ RESPEITO A UMA OBSERVAÇÃO DO SACO COLORIS 61100 EM 2021


sacos[364,] #A ENTRADA 364 DIZ RESPEITO A UMA OBSERVAÇÃO DO SACO COLORIS 61000 EM 2021
sacos[sacos$Codigo=="61000",] #Saco COLORIS 61000 - Faz parte do TOP de sacos mais vendidos (venda bastante expressiva)

sacos[710,] #A ENTRADA 2004 DIZ RESPEITO A UMA OBSERVAÇÃO DO SACO KRAFT LISO 61120 EM 2021 (TOP de sacos mais vendidos)
sacos[568,] #A ENTRADA 1862 DIZ RESPEITO A UMA OBSERVAÇÃO DO SACO KRAFT LISO 61068 EM 2021 (TOP de sacos mais vendidos)

#Os restantes produtos agregam-se num mesmo cluster(1)


########################################################################################################
#CLUSTERING HIERÁRQUICO - Dataset dos sacos
#Ajusta o Hierarchical Clustering aos dados com o método "Ward.D"

#load data
dfsacos <- sacos[,c(5,7,9,11,13)]

#remove rows with missing values
dfsacos <- na.omit(dfsacos)

#scale each variable to have a mean of 0 and sd of 1
dfsacos <- scale(dfsacos)

hc1 = hclust(d = dist(dfsacos, method = 'euclidean'), method = 'ward.D')
plot(hc1)
y_hc1 = cutree(hc1, 3)
y_hc1

#silhouette plot
sil <- silhouette(y_hc1, dist(dfsacos))
fviz_silhouette(sil)

#Coloca retângulo no dendrograma apartando os clusters
plot(hc1)
rect.hclust(hc1, k = 3, border = 3:6)
abline(h = 230, col = 'red')

fviz_cluster(list(data = dfsacos, cluster = y_hc1))

#Contagem de clusters no Hierarchical Clustering
library(dplyr)
seeds_df_cl1 <- mutate(sacos, cluster = y_hc1)
count(seeds_df_cl1,cluster)

#Cálculo do coeficiente cofenético

res.coph1 <- cophenetic(hc1) #computa a distancia cofenetica
res.coph1

cor(dist(dfsacos, method = 'euclidean'), res.coph1) #correlação entre a distancia cofenetica e a distancia original
#O coeficiente de correlaçao mostra que usando um metodo de linkage diferente cria uma arvore que representa a distancia original ligeiramente melhor

##################################################################################################################
#CLUSTERING HIERÁRQUICO - Dataset das caixas
#Ajusta o Hierarchical Clustering aos dados com o método "Ward.D"

#load data
dfcaixas <- caixas[,c(5,7,9,13)]

#remove rows with missing values
dfcaixas <- na.omit(dfcaixas)

#scale each variable to have a mean of 0 and sd of 1
dfcaixas <- scale(dfcaixas)

hc2 = hclust(d = dist(dfcaixas, method = 'euclidean'), method = 'ward.D')
plot(hc2)
y_hc2 = cutree(hc2, 3)
y_hc2

#silhouette plot
sil <- silhouette(y_hc2, dist(dfcaixas))
fviz_silhouette(sil)

#Coloca retângulo no dendrograma apartando os clusters
plot(hc2)
rect.hclust(hc2, k = 3, border = 3:6)
abline(h = 172, col = 'red')

fviz_cluster(list(data = dfcaixas, cluster = y_hc2))

#Contagem de clusters no Hierarchical Clustering
library(dplyr)
seeds_df_cl2 <- mutate(caixas, cluster = y_hc2)
count(seeds_df_cl2,cluster)

#Cálculo do coeficiente cofenético

res.coph2 <- cophenetic(hc2) #computa a distancia cofenetica
res.coph2

cor(dist(dfcaixas, method = 'euclidean'), res.coph2) #correlação entre a distancia cofenetica e a distancia original
#O coeficiente de correlaçao mostra que usando um metodo de linkage diferente cria uma arvore que representa a distancia original 
