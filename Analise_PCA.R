###################################################################################################################
#PCA- dataset caixas


scale1 <- scale(caixas[,(5:14)])
pcaixas = prcomp(scale1[,-c(7,8)])
pcaixas
summary(pcaixas)
pcaixas$rotation
fviz_eig(pcaixas)
get_eigenvalue(pcaixas)
#A primeira componente explica 33% da vari?ncia total dos dados. Portanto, 1/3 das informa??es contidas nas 8 vari?veis da base de dados podem ser encapsuladas apenas nessa componente. A segunda componente explica quase 25% da vari?ncia total.
#Com apenas duas componentes  58% de variancia dos dados ? explicada.


fviz_pca_ind(pcaixas,
             col.ind = "cos2", #Cor pela qualidade de representa??o
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1,2),
             repel = T, # Texto n?o sobreposto
             legend.title = "Representa??o"
)

# Gr?fico das vari?veis
fviz_pca_var(pcaixas,
             col.var = "contrib", # Cor por contribui??es para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribui??o"
)

# Gr?fico das vari?veis e indiv?duos
fviz_pca_biplot(pcaixas, repel = TRUE,
                col.var = "#2E9FDF", # cor das vari?veis
                col.ind = "#696969"  # cor dos produtos
)

fviz_pca_ind(pcaixas,
             col.ind = caixas$Referencia, # cor por familia de produto
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             legend.title = "Familia de produtos",
             repel = TRUE
)


# Gr?fico das vari?veis e indiv?duos
fviz_pca_biplot(pcaixas, repel = TRUE,
                col.var = "black", # cor das vari?veis
                col.ind = as.factor(caixas$Referencia),  # cor por referencia
                addEllipses = TRUE,
                legend.title = "Fam?lias de Produtos"
)

#Contribui?ao das variaveis para o PC1

fviz_contrib(pcaixas, choice = "var", axes = 1, top = 10,
             title= "Contribui??o das vari?veis para a DIM 1")

#Contribui?ao das variaveis para o PC2
fviz_contrib(pcaixas, choice = "var", axes = 2, top = 10,
             title= "Contribui??o das vari?veis para a DIM 2")

#Contribui?ao das variaveis para o PC3
fviz_contrib(pcaixas, choice = "var", axes = 3, top = 10,
             title= "Contribui??o das vari?veis para a DIM 3")


#Representa??o das 3 componentes principais
library(pca3d)
pca3d(pcaixas, group = caixas$Referencia)

bp3 <- bpca(pcaixas,d=1:3)
plot.bpca.3d(bp3)

#rotacionar o gr?fico
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
             col.ind = "cos2", #Cor pela qualidade de representa??o
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1,2),
             repel = T, # Texto n?o sobreposto
             legend.title = "Representa??o"
)


# Gr?fico das vari?veis
fviz_pca_var(psacos,
             col.var = "contrib", # Cor por contribui??es para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribui??o"
)

# Gr?fico das vari?veis e indiv?duos
fviz_pca_biplot(psacos, repel = TRUE,
                col.var = "#2E9FDF", # cor das vari?veis
                col.ind = "#696969"  # cor dos produtos
)

fviz_pca_ind(psacos,
             col.ind = sacos$Referencia, # cor por familia de produto
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             legend.title = "Familia de produtos",
             repel = TRUE
)


# Gr?fico das vari?veis e indiv?duos
fviz_pca_biplot(psacos, repel = TRUE,
                col.var = "black", # cor das vari?veis
                col.ind = as.factor(sacos$Referencia),  # cor por referencia
                addEllipses = TRUE,
                legend.title = "Fam?lias de Produtos"
)

#Contribui?ao das variaveis para o PC1

fviz_contrib(psacos, choice = "var", axes = 1, top = 10,
             title= "Contribui??o das vari?veis para a DIM 1")

#Contribui?ao das variaveis para o PC2
fviz_contrib(psacos, choice = "var", axes = 2, top = 10,
             title= "Contribui??o das vari?veis para a DIM 2")

#Contribui?ao das variaveis para o PC3
fviz_contrib(psacos, choice = "var", axes = 3, top = 10,
             title= "Contribui??o das vari?veis para a DIM 3")
