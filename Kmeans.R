####################################################################################################################
#kMeans no dataset sacos


library(factoextra)
library(cluster)



#load data
dfsacos <- sacos[,c(5,7,9,11,13)]

#remove rows with missing values
dfsacos <- na.omit(dfsacos)

#scale each variable to have a mean of 0 and sd of 1
dfsacos <- scale(dfsacos)

#view first six rows of dataset
head(dfsacos)

km <- kmeans(dfsacos, 2)

#Método de elbow
fviz_nbclust(dfsacos, kmeans, method = "wss") #Número ótimo de 3 clusters

#Método Silhueta
fviz_nbclust(dfsacos, kmeans, method="silhouette") #Número ótimo de 2 clusters

#Função nb_clust: 30 índices para escolha do melhor numero de clusters
library("NbClust")
nb<- NbClust(dfsacos, distance="euclidean", min.nc= 2, max.nc= 10, method="kmeans")
fviz_nbclust(nb) #Número ótimo de 3 clusters

#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 3 clusters
km <- kmeans(dfsacos, centers = 3, nstart = 25)

#silhouette plot

sil <- silhouette(km$cluster, dist(dfsacos))
fviz_silhouette(sil)


#Representação gráfica do algoritmo k-Means
fviz_cluster(km, data = dfsacos)

#Fornece informação sobre a média de uma variável nos clusters
#aggregate(df, by=list(cluster=km$cluster), mean)

#Coloca a coluna de clusters no dataset dos sacos
final_data <- cbind(sacos, cluster = km$cluster)
head(final_data)

####################################################################################################################
#kMeans no dataset caixas


library(factoextra)
library(cluster)



#load data
dfcaixas <- caixas[,c(5,7,9,13)]

#remove rows with missing values
dfcaixas <- na.omit(dfcaixas)

#scale each variable to have a mean of 0 and sd of 1
dfcaixas <- scale(dfcaixas)

#view first six rows of dataset
head(dfcaixas)

km <- kmeans(dfcaixas, 2)

#Método de elbow
fviz_nbclust(dfcaixas, kmeans, method = "wss") #Número ótimo de 5 clusters

#Método Silhueta
fviz_nbclust(dfcaixas, kmeans, method="silhouette") #Número ótimo de 2 clusters

#Função nb_clust: 30 índices para escolha do melhor numero de clusters
library("NbClust")
nb<- NbClust(dfcaixas, distance="euclidean", min.nc= 2, max.nc= 10, method="kmeans")
fviz_nbclust(nb) #Número ótimo de 2 clusters


#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 5 clusters
km <- kmeans(dfcaixas, centers = 5, nstart = 25)

#silhouette plot
sil <- silhouette(km$cluster, dist(dfcaixas))
fviz_silhouette(sil)

#Representação gráfica do algoritmo k-Means
fviz_cluster(km, data = dfcaixas)

#Fornece informação sobre a média de uma variável nos clusters
#aggregate(df, by=list(cluster=km$cluster), mean)

#Coloca a coluna de clusters no dataset das caixas
final_data1 <- cbind(caixas, cluster = km$cluster)
head(final_data1)