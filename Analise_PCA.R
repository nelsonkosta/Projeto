###################################################################################################################
#PCA- dataset caixas


scale1 <- scale(caixas[,(5:14)])
pcaixas = prcomp(scale1[,-c(7,8)])
pcaixas
summary(pcaixas)
pcaixas$rotation
fviz_eig(pcaixas)
get_eigenvalue(pcaixas)
#A primeira componente explica 33% da variância total dos dados. Portanto, 1/3 das informações contidas nas 8 variáveis da base de dados podem ser encapsuladas apenas nessa componente. A segunda componente explica quase 25% da variância total.
#Com apenas duas componentes  58% de variancia dos dados é explicada.


fviz_pca_ind(pcaixas,
             col.ind = "cos2", #Cor pela qualidade de representação
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1,2),
             repel = T, # Texto não sobreposto
             legend.title = "Representação"
)

# Gráfico das variáveis
fviz_pca_var(pcaixas,
             col.var = "contrib", # Cor por contribuições para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribuição"
)

# Gráfico das variáveis e indivíduos
fviz_pca_biplot(pcaixas, repel = TRUE,
                col.var = "#2E9FDF", # cor das variáveis
                col.ind = "#696969"  # cor dos produtos
)

fviz_pca_ind(pcaixas,
             col.ind = caixas$Referencia, # cor por familia de produto
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             legend.title = "Familia de produtos",
             repel = TRUE
)


# Gráfico das variáveis e indivíduos
fviz_pca_biplot(pcaixas, repel = TRUE,
                col.var = "black", # cor das variáveis
                col.ind = as.factor(caixas$Referencia),  # cor por referencia
                addEllipses = TRUE,
                legend.title = "Famílias de Produtos"
)

#Contribuiçao das variaveis para o PC1

fviz_contrib(pcaixas, choice = "var", axes = 1, top = 10,
             title= "Contribuição das variáveis para a DIM 1")

#Contribuiçao das variaveis para o PC2
fviz_contrib(pcaixas, choice = "var", axes = 2, top = 10,
             title= "Contribuição das variáveis para a DIM 2")

#Contribuiçao das variaveis para o PC3
fviz_contrib(pcaixas, choice = "var", axes = 3, top = 10,
             title= "Contribuição das variáveis para a DIM 3")


#Representação das 3 componentes principais
library(pca3d)
pca3d(pcaixas, group = caixas$Referencia)

bp3 <- bpca(pcaixas,d=1:3)
plot.bpca.3d(bp3)

#rotacionar o gráfico
plot.bpca.3d(bp3,rgl=T)


###################################################################################################################
#PCA- dataset sacos

scale2 <- scale(sacos[,(5:14)])
#psacos = prcomp(scale2[,-c(7,8)])
psacos = prcomp(scale2)
psacos
summary(psacos)
psacos$rotation
fviz_eig(psacos)
get_eigenvalue(psacos)

fviz_pca_ind(psacos,
             col.ind = "cos2", #Cor pela qualidade de representação
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1,2),
             repel = T, # Texto não sobreposto
             legend.title = "Representação"
)


# Gráfico das variáveis
fviz_pca_var(psacos,
             col.var = "contrib", # Cor por contribuições para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribuição"
)

# Gráfico das variáveis e indivíduos
fviz_pca_biplot(psacos, repel = TRUE,
                col.var = "#2E9FDF", # cor das variáveis
                col.ind = "#696969"  # cor dos produtos
)

fviz_pca_ind(psacos,
             col.ind = sacos$Referencia, # cor por familia de produto
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             legend.title = "Familia de produtos",
             repel = TRUE
)


# Gráfico das variáveis e indivíduos
fviz_pca_biplot(psacos, repel = TRUE,
                col.var = "black", # cor das variáveis
                col.ind = as.factor(sacos$Referencia),  # cor por referencia
                addEllipses = TRUE,
                legend.title = "Famílias de Produtos"
)

#Contribuiçao das variaveis para o PC1

fviz_contrib(psacos, choice = "var", axes = 1, top = 10,
             title= "Contribuição das variáveis para a DIM 1")

#Contribuiçao das variaveis para o PC2
fviz_contrib(psacos, choice = "var", axes = 2, top = 10,
             title= "Contribuição das variáveis para a DIM 2")

#Contribuiçao das variaveis para o PC3
fviz_contrib(psacos, choice = "var", axes = 3, top = 10,
             title= "Contribuição das variáveis para a DIM 3")
