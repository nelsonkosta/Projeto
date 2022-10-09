#Análise Multivarida de Dados sobre tipologia de produtos numa empresa
#Projeto de Estágio - Nelson Costa

#União das bases de dados relativas a todos os anos
Litel <- rbind(litel2015,litel2016,litel2017,litel2018,litel2019,litel2020,litel2021)
Litel

#Dimensão da base de dados
dim(Litel)

#número de códigos únicos na base de dados
length(unique(Litel$Codigo))

#Aplica uma função a sub conjuntos do data frame
# dá nos o numero de observações por código
ncodigo <- by(Litel, Litel$Codigo, function(x){dim(x)[1]})
ncodigo= c(as.vector(ncodigo))

#Numero de observações na base de dados relativa a cada código
table(ncodigo) #conclusao: 26 códigos com 2 observaçoes, 8 codigos com 3 observaçoes, 2 codigos com 4 observaçoes...

#Descrição estatística sumária das variáveis da base de dados
summary(Litel)

#Verificação de NA's na coluna Qt_Tot
Litel[is.na(Litel$Qt_Tot),]

#Ordena a base de dados por código de produto
Litel <- Litel[order(Litel$Codigo),]

#-----------------------------------------------------------------------------------------------------------------------------

#Gráfico com as quantidades totais de todos os sacos vendidos ao longo dos anos 

plot(sacos$Ano, sacos$Qt_Tot, xlab="Anos",ylab="Quantidade Total", col="red", pch=19)

by(sacos,sacos$Codigo,function(x){lines(x$Ano, x$Qt_Tot, col="blue")})

#Gráfico com as quantidades totais de todas as caixas vendidas ao longo dos anos

plot(caixas$Ano, caixas$Qt_Tot, xlab="Anos",ylab="Quantidade Total", col="red", pch=19)

by(caixas,caixas$Codigo,function(x){lines(x$Ano, x$Qt_Tot, col="blue")})

plot(Litel$Ano, Litel$Qt_Tot, xlab="Anos",ylab="Quantidade Total", col="red", pch=19)

by(Litel,Litel$Codigo,function(x){lines(x$Ano, x$Qt_Tot, col="blue")})

#Apresenta dois gráficos lado a lado
par(mfrow=c(1,2))

#Gráfico com as quantidades totais de todos os produtos de catalogo vendidos ao longo dos anos 
x <- c(sum(Litel$Qt_Tot[Litel$Ano==2015]), sum(Litel$Qt_Tot[Litel$Ano==2016]), sum(Litel$Qt_Tot[Litel$Ano==2017]), sum(Litel$Qt_Tot[Litel$Ano==2018]), sum(Litel$Qt_Tot[Litel$Ano==2019]), sum(Litel$Qt_Tot[Litel$Ano==2020]), sum(Litel$Qt_Tot[Litel$Ano==2021]))
y <- c(2015,2016,2017,2018,2019,2020,2021)
plot(y, x, xlab="Anos",ylab="Qt_Tot", col="red", pch=17)
lines(y,x, col="red",type="b",pch=17, lwd=5 )

#Gráfico com os valores de venda totais de todos os produtos de catalogo ao longo dos anos
z <- c(sum(Litel$Vl_Tot[Litel$Ano==2015]), sum(Litel$Vl_Tot[Litel$Ano==2016]), sum(Litel$Vl_Tot[Litel$Ano==2017]), sum(Litel$Vl_Tot[Litel$Ano==2018]), sum(Litel$Vl_Tot[Litel$Ano==2019]), sum(Litel$Vl_Tot[Litel$Ano==2020]), sum(Litel$Vl_Tot[Litel$Ano==2021]))
y <- c(2015,2016,2017,2018,2019,2020,2021)
plot(y, z, xlab="Anos",ylab="Vl_Tot", col="red", pch=17)
lines(y,z, col="green",type="b",pch=17, lwd=5 )

#Separação da base de dados em caixas e sacos

#grafico circular e barras
pie(table(Litel$Tipo), col = c("blue", "purple"))
barplot(table(Litel$Tipo), col = "limegreen", names=c("Caixas", "Sacos"))

#base de dados relativa a sacos
sacos <- Litel[Litel$Tipo=="SACO",]

#base de dados relativa a caixas
caixas <- Litel[Litel$Tipo=="CAIXA",]

#Base de dados apenas com os 6 sacos mais vendidos (TOP de vendas de sacos)

sacos_mais_vend <- sacos[sacos$Qt_Tot>250000,]

sacos_mais_vend

unique(sacos_mais_vend$Codigo)
hist(sacos_mais_vend$Qt_Tot)
boxplot(sacos_mais_vend$Qt_Tot)

#Base de dados de sacos - Nivel 1 (sacos vendidos em menor quantidade)

sacos_vend_1 <- sacos[sacos$Qt_Tot<5000,]
hist(sacos_vend_1$Qt_Tot)
boxplot(sacos_vend_1$Qt_Tot)

#Bases de dados de sacos - Nivel 2 (sacos vendidos - quantidade intermédia)

sacos_vend_2 <- sacos[sacos$Qt_Tot<40000 & sacos$Qt_Tot>5000,]
hist(sacos_vend_2$Qt_Tot)
boxplot(sacos_vend_2$Qt_Tot)

#Base de dados de sacos - Nivel 3 (sacos vendidos em maior quantidade)

sacos_vend_3 <- sacos[sacos$Qt_Tot<250000 & sacos$Qt_Tot>40000 ,]
hist(sacos_vend_3$Qt_Tot)
boxplot(sacos_vend_3$Qt_Tot)


#Base de dados relativa a notas de crédito de caixas
caixas_vend_0 <- caixas[caixas$Qt_Tot<0 ,]
caixas_vend_0$Codigo[caixas_vend_0$Qt_Tot<0] #existe uma nota de crédito relativa à caixa com o codigo «15131»
caixas_vend_0$Referencia[caixas_vend_0$Qt_Tot<0] #A caixa com o código «15131» pertence à família de caixas "CLASSIC"

#Base de dados de caixas - Nivel 1 (caixas vendidas em menor quantidade)
caixas_vend_1 <- caixas[caixas$Qt_Tot>0 & caixas$Qt_Tot<=1000,]
hist(caixas_vend_1$Qt_Tot)
boxplot(caixas_vend_1$Qt_Tot)

#Base de dados de caixas vendidas - Nivel 2 (caixas vendidas - quantidade intermédia)
caixas_vend_2 <- caixas[caixas$Qt_Tot>1000 & caixas$Qt_Tot<10000,]
hist(caixas_vend_2$Qt_Tot)
boxplot(caixas_vend_2$Qt_Tot)

#Base de dados de caixas - Nivel 3 (caixas vendidas em maior quantidade)
caixas_vend_3 <- caixas[caixas$Qt_Tot>10000,]
hist(caixas_vend_3$Qt_Tot)
boxplot(caixas_vend_3$Qt_Tot)

#DETERMINAÇÃO DOS SACOS MAIS VENDIDOS (QT_TOT)
#TOP de vendas de sacos Qt_Total > 250000

# Plot do Qt_Tot de sacos mais vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="61000"], sacos$Qt_Tot[sacos$Codigo=="61000"], ylim = c(200000,1500000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP - Sacos Mais Vendidos")
lines(sacos$Ano[sacos$Codigo=="61000"], sacos$Qt_Tot[sacos$Codigo=="61000"], col="black")
points(sacos$Ano[sacos$Codigo=="61100"], sacos$Qt_Tot[sacos$Codigo=="61100"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="61100"], sacos$Qt_Tot[sacos$Codigo=="61100"], col="black")
points(sacos$Ano[sacos$Codigo=="61068"], sacos$Qt_Tot[sacos$Codigo=="61068"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="61068"], sacos$Qt_Tot[sacos$Codigo=="61068"], col="black")
points(sacos$Ano[sacos$Codigo=="61120"], sacos$Qt_Tot[sacos$Codigo=="61120"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="61120"], sacos$Qt_Tot[sacos$Codigo=="61120"], col="black")
points(sacos$Ano[sacos$Codigo=="61032"], sacos$Qt_Tot[sacos$Codigo=="61032"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="61032"], sacos$Qt_Tot[sacos$Codigo=="61032"], col="black")
points(sacos$Ano[sacos$Codigo=="61069"], sacos$Qt_Tot[sacos$Codigo=="61069"], col="red", pch=19)
lines(sacos$Ano[sacos$Codigo=="61069"], sacos$Qt_Tot[sacos$Codigo=="61069"], col="black")

legend( x = "top",
        legend = c("COLORIS (61000)","COLORIS WIDE (61100)", "KRAFT LISO (61068)", "KRAFT LISO (61120)", "COLORIS (61032)", "KRAFT LISO (61069)"),
        col = c("blue","green", "purple", "pink", "yellow", "red"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de sacos - Nivel 1 (sacos vendidos em menor quantidade)
#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2015

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2015], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2015]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2015 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2016

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2016], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2016]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2016 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2017

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2017], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2017]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2017 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2018

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2018], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2018]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2018 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2019

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2019], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2019]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2019 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2020

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2020], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2020]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2020 - Nivel 1", xlab="Codigo", ylab="Quantidade")


#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2021

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2021], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2021]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2021 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de sacos mais vendidos ao longo dos anos

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas ao longo dos anos

h <- aggregate(x= sacos_vend_1$Qt_Tot, 
               by= list(sacos_vend_1$Codigo), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos ao longo dos anos - NIVEL 1

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos ao longo dos anos - Nivel 1", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de sacos mais vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="30666"], sacos$Qt_Tot[sacos$Codigo=="30666"], ylim = c(0,15000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Sacos Mais Vendidos - Nivel 1")
lines(sacos$Ano[sacos$Codigo=="30666"], sacos$Qt_Tot[sacos$Codigo=="30666"], col="black")
points(sacos$Ano[sacos$Codigo=="30674"], sacos$Qt_Tot[sacos$Codigo=="30674"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="30674"], sacos$Qt_Tot[sacos$Codigo=="30674"], col="black")
points(sacos$Ano[sacos$Codigo=="61204"], sacos$Qt_Tot[sacos$Codigo=="61204"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="61204"], sacos$Qt_Tot[sacos$Codigo=="61204"], col="black")
points(sacos$Ano[sacos$Codigo=="61218"], sacos$Qt_Tot[sacos$Codigo=="61218"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="61218"], sacos$Qt_Tot[sacos$Codigo=="61218"], col="black")
points(sacos$Ano[sacos$Codigo=="62073"], sacos$Qt_Tot[sacos$Codigo=="62073"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="62073"], sacos$Qt_Tot[sacos$Codigo=="62073"], col="black")

legend( x = "top",
        legend = c("KRAFT GIFT (30666)","KRAFT GIFT (30674)", "COLORIS (61204)", "COLORIS (61218)", "GEMINUS 1 (62073)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de sacos - Nivel 2 (sacos vendidos em quantidade - intermédia)
#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2015

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2015], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2015]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2015 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2016

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2016], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2016]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2016 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2017

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2017], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2017]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2017 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2018

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2018], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2018]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2018 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2019

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2019], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2019]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2019 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2020

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2020], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2020]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2020 - Nivel 2", xlab="Codigo", ylab="Quantidade")


#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2021

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2021], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2021]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2021 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de sacos mais vendidos ao longo dos anos

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas ao longo dos anos

h <- aggregate(x= sacos_vend_2$Qt_Tot, 
               by= list(sacos_vend_2$Codigo), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos ao longo dos anos - NIVEL 2

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos ao longo dos anos - Nivel 2", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de sacos mais vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="61064"], sacos$Qt_Tot[sacos$Codigo=="61064"], ylim = c(10000,100000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Sacos Mais Vendidos - Nivel 2")
lines(sacos$Ano[sacos$Codigo=="61064"], sacos$Qt_Tot[sacos$Codigo=="61064"], col="black")
points(sacos$Ano[sacos$Codigo=="61007"], sacos$Qt_Tot[sacos$Codigo=="61007"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="61007"], sacos$Qt_Tot[sacos$Codigo=="61007"], col="black")
points(sacos$Ano[sacos$Codigo=="61458"], sacos$Qt_Tot[sacos$Codigo=="61458"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="61458"], sacos$Qt_Tot[sacos$Codigo=="61458"], col="black")
points(sacos$Ano[sacos$Codigo=="61008"], sacos$Qt_Tot[sacos$Codigo=="61008"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="61008"], sacos$Qt_Tot[sacos$Codigo=="61008"], col="black")
points(sacos$Ano[sacos$Codigo=="61037"], sacos$Qt_Tot[sacos$Codigo=="61037"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="61037"], sacos$Qt_Tot[sacos$Codigo=="61037"], col="black")

legend( x = "top",
        legend = c("COLORIS (61064)","COLORIS (61007)", "COLORIS (61458)", "COLORIS (61008)", "COLORIS (61037)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )

#Base de dados de sacos - Nivel 3 (sacos vendidos em maior quantidade)
#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2015

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2015], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2015]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2015 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2016

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2016], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2016]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2016 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2017

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2017], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2017]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2017 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2018

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2018], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2018]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2018 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2019

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2019], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2019]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2019 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2020

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2020], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2020]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2020 - Nivel 3", xlab="Codigo", ylab="Quantidade")


#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2021

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2021], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2021]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos em 2021 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de sacos mais vendidos ao longo dos anos

#Agrega códigos de sacos e devolve o máximo de quantidades vendidas ao longo dos anos

h <- aggregate(x= sacos_vend_3$Qt_Tot, 
               by= list(sacos_vend_3$Codigo), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de sacos mais vendidos ao longo dos anos - NIVEL 3

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos mais vendidos ao longo dos anos - Nivel 3", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de sacos mais vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="60000"], sacos$Qt_Tot[sacos$Codigo=="60000"], ylim = c(0,250000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Sacos Mais Vendidos - Nivel 3")
lines(sacos$Ano[sacos$Codigo=="60000"], sacos$Qt_Tot[sacos$Codigo=="60000"], col="black")
points(sacos$Ano[sacos$Codigo=="61042"], sacos$Qt_Tot[sacos$Codigo=="61042"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="61042"], sacos$Qt_Tot[sacos$Codigo=="61042"], col="black")
points(sacos$Ano[sacos$Codigo=="61249"], sacos$Qt_Tot[sacos$Codigo=="61249"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="61249"], sacos$Qt_Tot[sacos$Codigo=="61249"], col="black")
points(sacos$Ano[sacos$Codigo=="90955"], sacos$Qt_Tot[sacos$Codigo=="90955"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="90955"], sacos$Qt_Tot[sacos$Codigo=="90955"], col="black")
points(sacos$Ano[sacos$Codigo=="91767"], sacos$Qt_Tot[sacos$Codigo=="91767"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="91767"], sacos$Qt_Tot[sacos$Codigo=="91767"], col="black")

legend( x = "top",
        legend = c("KRAFT LISO (60000)","KRAFT VERJURADO (61042)", "KRAFT LISO (61249)", "SACOS TAKE AWAY (90955)", "SACOS TAKE AWAY (91767)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de sacos - Nivel 1 (sacos vendidos em menor quantidade)
#DETERMINAÇÃO DOS SACOS MENOS VENDIDOS POR ANO (QT_TOT)

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2015

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2015], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2015]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2015 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2016

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2016], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2016]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2016 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2017

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2017], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2017]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2017 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2018

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2018], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2018]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2018 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2019

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2019], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2019]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2019 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2020

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2020], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2020]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2020 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2021

h <- aggregate(x= sacos_vend_1$Qt_Tot[sacos_vend_1$Ano==2021], 
               by= list(sacos_vend_1$Codigo[sacos_vend_1$Ano==2021]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2021 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de sacos menos vendidos ao longo dos anos

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas ao longo dos anos

h <- aggregate(x= sacos_vend_1$Qt_Tot, 
               by= list(sacos_vend_1$Codigo), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos ao longo dos anos - NIVEL 1

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos ao longo dos anos - Nivel 1", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de sacos menos vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="30665"], sacos$Qt_Tot[sacos$Codigo=="30665"], ylim = c(0,2000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Sacos Menos Vendidos - Nivel 1")
lines(sacos$Ano[sacos$Codigo=="30665"], sacos$Qt_Tot[sacos$Codigo=="30665"], col="black")
points(sacos$Ano[sacos$Codigo=="30681"], sacos$Qt_Tot[sacos$Codigo=="30681"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="30681"], sacos$Qt_Tot[sacos$Codigo=="30681"], col="black")
points(sacos$Ano[sacos$Codigo=="30688"], sacos$Qt_Tot[sacos$Codigo=="30688"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="30688"], sacos$Qt_Tot[sacos$Codigo=="30688"], col="black")
points(sacos$Ano[sacos$Codigo=="30689"], sacos$Qt_Tot[sacos$Codigo=="30689"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="30689"], sacos$Qt_Tot[sacos$Codigo=="30689"], col="black")
points(sacos$Ano[sacos$Codigo=="30691"], sacos$Qt_Tot[sacos$Codigo=="30691"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="30691"], sacos$Qt_Tot[sacos$Codigo=="30691"], col="black")

legend( x = "top",
        legend = c("COLORIS GIFT (30665)","COLORIS GIFT (30681)", "COLORIS GIFT (30688)", "COLORIS GIFT (30689)", "KRAFT GIFT (30691)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )

#Base de dados de sacos - Nivel 2 (sacos vendidos em quantidade intermédia)
#DETERMINAÇÃO DOS SACOS MENOS VENDIDOS POR ANO (QT_TOT)

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2015

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2015], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2015]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2015 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2016

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2016], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2016]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2016 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2017

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2017], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2017]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2017 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2018

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2018], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2018]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2018 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2019

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2019], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2019]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2019 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2020

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2020], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2020]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2020 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2021

h <- aggregate(x= sacos_vend_2$Qt_Tot[sacos_vend_2$Ano==2021], 
               by= list(sacos_vend_2$Codigo[sacos_vend_2$Ano==2021]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2021 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de sacos menos vendidos ao longo dos anos

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas ao longo dos anos

h <- aggregate(x= sacos_vend_2$Qt_Tot, 
               by= list(sacos_vend_2$Codigo), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos ao longo dos anos - NIVEL 2

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos ao longo dos anos - Nivel 2", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de sacos menos vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="61455"], sacos$Qt_Tot[sacos$Codigo=="61455"], ylim = c(100,18000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Sacos Menos Vendidos - Nivel 2")
lines(sacos$Ano[sacos$Codigo=="61455"], sacos$Qt_Tot[sacos$Codigo=="61455"], col="black")
points(sacos$Ano[sacos$Codigo=="61552"], sacos$Qt_Tot[sacos$Codigo=="61552"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="61552"], sacos$Qt_Tot[sacos$Codigo=="61552"], col="black")
points(sacos$Ano[sacos$Codigo=="68705"], sacos$Qt_Tot[sacos$Codigo=="68705"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="68705"], sacos$Qt_Tot[sacos$Codigo=="68705"], col="black")
points(sacos$Ano[sacos$Codigo=="30666"], sacos$Qt_Tot[sacos$Codigo=="30666"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="30666"], sacos$Qt_Tot[sacos$Codigo=="30666"], col="black")
points(sacos$Ano[sacos$Codigo=="61299"], sacos$Qt_Tot[sacos$Codigo=="61299"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="61299"], sacos$Qt_Tot[sacos$Codigo=="61299"], col="black")

legend( x = "top",
        legend = c("COLORIS WIDE (61455)","CHRISTMAS SNOWMAN (61552)", "LUXURY BASIC (68705)", "KRAFT GIFT (30666)", "BACUS (61299)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )

#Base de dados de sacos - Nivel 3 (sacos vendidos em maior quantidade)
#DETERMINAÇÃO DOS SACOS MENOS VENDIDOS POR ANO (QT_TOT)

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2015

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2015], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2015]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2015 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2016

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2016], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2016]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2016 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2017

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2017], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2017]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2017 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2018

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2018], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2018]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2018 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2019

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2019], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2019]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2019 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2020

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2020], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2020]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2020 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas no ano de 2021

h <- aggregate(x= sacos_vend_3$Qt_Tot[sacos_vend_3$Ano==2021], 
               by= list(sacos_vend_3$Codigo[sacos_vend_3$Ano==2021]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos em 2021 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de sacos menos vendidos ao longo dos anos

#Agrega códigos de sacos e devolve o mínimo de quantidades vendidas ao longo dos anos

h <- aggregate(x= sacos_vend_3$Qt_Tot, 
               by= list(sacos_vend_3$Codigo), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de sacos menos vendidos ao longo dos anos - NIVEL 3

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Sacos menos vendidos ao longo dos anos - Nivel 3", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de sacos menos vendidos (forma isolada)

plot(sacos$Ano[sacos$Codigo=="61009"], sacos$Qt_Tot[sacos$Codigo=="61009"], ylim = c(10000,120000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Sacos Menos Vendidos - Nivel 3")
lines(sacos$Ano[sacos$Codigo=="61009"], sacos$Qt_Tot[sacos$Codigo=="61009"], col="black")
points(sacos$Ano[sacos$Codigo=="61107"], sacos$Qt_Tot[sacos$Codigo=="61107"], col="green", pch=19)
lines(sacos$Ano[sacos$Codigo=="61107"], sacos$Qt_Tot[sacos$Codigo=="61107"], col="black")
points(sacos$Ano[sacos$Codigo=="61216"], sacos$Qt_Tot[sacos$Codigo=="61216"], col="purple", pch=19)
lines(sacos$Ano[sacos$Codigo=="61216"], sacos$Qt_Tot[sacos$Codigo=="61216"], col="black")
points(sacos$Ano[sacos$Codigo=="61105"], sacos$Qt_Tot[sacos$Codigo=="61105"], col="pink", pch=19)
lines(sacos$Ano[sacos$Codigo=="61105"], sacos$Qt_Tot[sacos$Codigo=="61105"], col="black")
points(sacos$Ano[sacos$Codigo=="61433"], sacos$Qt_Tot[sacos$Codigo=="61433"], col="yellow", pch=19)
lines(sacos$Ano[sacos$Codigo=="61433"], sacos$Qt_Tot[sacos$Codigo=="61433"], col="black")

legend( x = "top",
        legend = c("COLORIS (61009)","COLORIS (61107)", "COLORIS (61216)", "COLORIS (61105)", "KRAFT LISO (61433)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )



#Base de dados de caixas - Nivel 1 (caixas vendidas em menor quantidade)
#Agrega códigos de sacos e devolve o máximo de quantidades vendidas no ano de 2015

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2015], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2015]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2015 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2016

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2016], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2016]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2016 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2017

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2017], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2017]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidos em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2017 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2018

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2018], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2018]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2018 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2019

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2019], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2019]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2019 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2020

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2020], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2020]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2020 - Nivel 1", xlab="Codigo", ylab="Quantidade")


#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2021

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2021], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2021]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2021 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de caixas mais vendidos ao longo dos anos

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas ao longo dos anos

h <- aggregate(x= caixas_vend_1$Qt_Tot, 
               by= list(caixas_vend_1$Codigo), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas ao longo dos anos - NIVEL 1

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas ao longo dos anos - Nivel 1", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de caixas mais vendidas (forma isolada)

plot(caixas$Ano[caixas$Codigo=="15075"], caixas$Qt_Tot[caixas$Codigo=="15075"], ylim = c(100,8000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Caixas Mais Vendidas - Nivel 1")
lines(caixas$Ano[caixas$Codigo=="15075"], caixas$Qt_Tot[caixas$Codigo=="15075"], col="black")
points(caixas$Ano[caixas$Codigo=="15419"], caixas$Qt_Tot[caixas$Codigo=="15419"], col="green", pch=19)
lines(caixas$Ano[caixas$Codigo=="15419"], caixas$Qt_Tot[caixas$Codigo=="15419"], col="black")
points(caixas$Ano[caixas$Codigo=="15798"], caixas$Qt_Tot[caixas$Codigo=="15798"], col="purple", pch=19)
lines(caixas$Ano[caixas$Codigo=="15798"], caixas$Qt_Tot[caixas$Codigo=="15798"], col="black")
points(caixas$Ano[caixas$Codigo=="14616"], caixas$Qt_Tot[caixas$Codigo=="14616"], col="pink", pch=19)
lines(caixas$Ano[caixas$Codigo=="14616"], caixas$Qt_Tot[caixas$Codigo=="14616"], col="black")
points(caixas$Ano[caixas$Codigo=="14622"], caixas$Qt_Tot[caixas$Codigo=="14622"], col="yellow", pch=19)
lines(caixas$Ano[caixas$Codigo=="14622"], caixas$Qt_Tot[caixas$Codigo=="14622"], col="black")

legend( x = "top",
        legend = c("ELISÉE HYPE (15075)","SPARKLING (15419)", "B2C PACKAGING (15798)", "VINTAGE (14616)", "GOURMET M (14622)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de caixas - Nivel 2 (caixas vendidas em quantidade - intermédia)
#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2015

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2015], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2015]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2015 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2016

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2016], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2016]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2016 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2017

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2017], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2017]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2017 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2018

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2018], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2018]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2018 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2019

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2019], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2019]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2019 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2020

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2020], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2020]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2020 - Nivel 2", xlab="Codigo", ylab="Quantidade")


#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2021

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2021], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2021]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2021 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de caixas mais vendidas ao longo dos anos

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas ao longo dos anos

h <- aggregate(x= caixas_vend_2$Qt_Tot, 
               by= list(caixas_vend_2$Codigo), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas ao longo dos anos - NIVEL 2

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas ao longo dos anos - Nivel 2", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de caixas mais vendidas (forma isolada)

plot(caixas$Ano[caixas$Codigo=="15783"], caixas$Qt_Tot[caixas$Codigo=="15783"], ylim = c(0,25000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Caixas Mais Vendidas - Nivel 2")
lines(caixas$Ano[caixas$Codigo=="15783"], caixas$Qt_Tot[caixas$Codigo=="15783"], col="black")
points(caixas$Ano[caixas$Codigo=="15467"], caixas$Qt_Tot[caixas$Codigo=="15467"], col="green", pch=19)
lines(caixas$Ano[caixas$Codigo=="15467"], caixas$Qt_Tot[caixas$Codigo=="15467"], col="black")
points(caixas$Ano[caixas$Codigo=="15782"], caixas$Qt_Tot[caixas$Codigo=="15782"], col="purple", pch=19)
lines(caixas$Ano[caixas$Codigo=="15782"], caixas$Qt_Tot[caixas$Codigo=="15782"], col="black")
points(caixas$Ano[caixas$Codigo=="15836"], caixas$Qt_Tot[caixas$Codigo=="15836"], col="pink", pch=19)
lines(caixas$Ano[caixas$Codigo=="15836"], caixas$Qt_Tot[caixas$Codigo=="15836"], col="black")
points(caixas$Ano[caixas$Codigo=="15834"], caixas$Qt_Tot[caixas$Codigo=="15834"], col="yellow", pch=19)
lines(caixas$Ano[caixas$Codigo=="15834"], caixas$Qt_Tot[caixas$Codigo=="15834"], col="black")

legend( x = "top",
        legend = c("KRAFT (15783)","LINEAS PRETO (15467)", "KRAFT (15782)", "KRAFT (15836)", "KRAFT (15834)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de caixas - Nivel 3 (caixas vendidas em maior quantidade)
#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2015

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2015], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2015]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2015 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2016

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2016], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2016]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2016 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2017

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2017], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2017]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2017 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2018

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2018], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2018]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidos em 2018 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2019

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2019], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2019]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2019 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2020

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2020], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2020]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidos em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2020 - Nivel 3", xlab="Codigo", ylab="Quantidade")


#Agrega códigos de caixas e devolve o máximo de quantidades vendidas no ano de 2021

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2021], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2021]), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas em 2021 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de caixas mais vendidas ao longo dos anos

#Agrega códigos de caixas e devolve o máximo de quantidades vendidas ao longo dos anos

h <- aggregate(x= caixas_vend_3$Qt_Tot, 
               by= list(caixas_vend_3$Codigo), 
               FUN=max)

#Ordenar os valores anteriores apresentando primeiro os maiores valores

h <- h[order(h$x, decreasing=TRUE),]
h

#Apresenta os 5 códigos de caixas mais vendidas ao longo dos anos - NIVEL 3

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas mais vendidas ao longo dos anos - Nivel 3", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de caixas mais vendidas (forma isolada)

plot(caixas$Ano[caixas$Codigo=="14766"], caixas$Qt_Tot[caixas$Codigo=="14766"], ylim = c(0,90000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Caixas Mais Vendidas - Nivel 3")
lines(caixas$Ano[caixas$Codigo=="14766"], caixas$Qt_Tot[caixas$Codigo=="14766"], col="black")
points(caixas$Ano[caixas$Codigo=="15129"], caixas$Qt_Tot[caixas$Codigo=="15129"], col="green", pch=19)
lines(caixas$Ano[caixas$Codigo=="15129"], caixas$Qt_Tot[caixas$Codigo=="15129"], col="black")
points(caixas$Ano[caixas$Codigo=="15433"], caixas$Qt_Tot[caixas$Codigo=="15433"], col="purple", pch=19)
lines(caixas$Ano[caixas$Codigo=="15433"], caixas$Qt_Tot[caixas$Codigo=="15433"], col="black")
points(caixas$Ano[caixas$Codigo=="15799"], caixas$Qt_Tot[caixas$Codigo=="15799"], col="pink", pch=19)
lines(caixas$Ano[caixas$Codigo=="15799"], caixas$Qt_Tot[caixas$Codigo=="15799"], col="black")
points(caixas$Ano[caixas$Codigo=="15434"], caixas$Qt_Tot[caixas$Codigo=="15434"], col="yellow", pch=19)
lines(caixas$Ano[caixas$Codigo=="15434"], caixas$Qt_Tot[caixas$Codigo=="15434"], col="black")

legend( x = "top",
        legend = c("GOURMET M (14766)","CLASSIC (15129)", "CLASSIC (15433)", "B2C PACKAGING (15799)", "CLASSIC (15434)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de caixas - Nivel 1 (caixas vendidas em menor quantidade)
#DETERMINAÇÃO DAS CAIXAS MENOS VENDIDAS POR ANO (QT_TOT)

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2015

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2015], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2015]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2015 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2016

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2016], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2016]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2016 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2017

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2017], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2017]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2017 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2018

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2018], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2018]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2018 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2019

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2019], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2019]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2019 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2020

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2020], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2020]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2020 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2021

h <- aggregate(x= caixas_vend_1$Qt_Tot[caixas_vend_1$Ano==2021], 
               by= list(caixas_vend_1$Codigo[caixas_vend_1$Ano==2021]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2021 - Nivel 1", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de caixas menos vendidas ao longo dos anos

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas ao longo dos anos

h <- aggregate(x= caixas_vend_1$Qt_Tot, 
               by= list(caixas_vend_1$Codigo), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas ao longo dos anos - NIVEL 1

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas ao longo dos anos - Nivel 1", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de caixas menos vendidos (forma isolada)

plot(caixas$Ano[caixas$Codigo=="15089"], caixas$Qt_Tot[caixas$Codigo=="15089"], ylim=c(0,2000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Caixas Menos Vendidas - Nivel 1")
lines(caixas$Ano[caixas$Codigo=="15089"], caixas$Qt_Tot[caixas$Codigo=="15089"], col="black")
points(caixas$Ano[caixas$Codigo=="15453"], caixas$Qt_Tot[caixas$Codigo=="15453"], col="green", pch=19)
lines(caixas$Ano[caixas$Codigo=="15453"], caixas$Qt_Tot[caixas$Codigo=="15453"], col="black")
points(caixas$Ano[caixas$Codigo=="15459"], caixas$Qt_Tot[caixas$Codigo=="15459"], col="purple", pch=19)
lines(caixas$Ano[caixas$Codigo=="15459"], caixas$Qt_Tot[caixas$Codigo=="15459"], col="black")
points(caixas$Ano[caixas$Codigo=="15460"], caixas$Qt_Tot[caixas$Codigo=="15460"], col="pink", pch=19)
lines(caixas$Ano[caixas$Codigo=="15460"], caixas$Qt_Tot[caixas$Codigo=="15460"], col="black")
points(caixas$Ano[caixas$Codigo=="15468"], caixas$Qt_Tot[caixas$Codigo=="15468"], col="yellow", pch=19)
lines(caixas$Ano[caixas$Codigo=="15468"], caixas$Qt_Tot[caixas$Codigo=="15468"], col="black")

legend( x = "top",
        legend = c("ELISÉE HYPE (15089)","GOURMET HAUTE M (15453)", "GOURMET HAUTE L (15459)", "GOURMET HAUTE L (15460)", "LINEAS OURO (15468)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )

#Base de dados de caixas - Nivel 2 (caixas vendidas em quantidade intermédia)
#DETERMINAÇÃO DAS CAIXAS MENOS VENDIDOS POR ANO (QT_TOT)

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2015

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2015], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2015]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2015 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2016

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2016], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2016]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidos em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2016 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2017

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2017], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2017]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2017 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2018

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2018], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2018]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2018 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2019

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2019], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2019]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2019 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2020

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2020], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2020]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2020 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2021

h <- aggregate(x= caixas_vend_2$Qt_Tot[caixas_vend_2$Ano==2021], 
               by= list(caixas_vend_2$Codigo[caixas_vend_2$Ano==2021]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2021 - Nivel 2", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de caixas menos vendidas ao longo dos anos

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas ao longo dos anos

h <- aggregate(x= caixas_vend_2$Qt_Tot, 
               by= list(caixas_vend_2$Codigo), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas ao longo dos anos - NIVEL 2

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas ao longo dos anos - Nivel 2", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de caixas menos vendidas (forma isolada)

plot(caixas$Ano[caixas$Codigo=="16386"], caixas$Qt_Tot[caixas$Codigo=="16386"], xlim = c(2015,2021), ylim = c(0,3000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Caixas Menos Vendidas - Nivel 2")
lines(caixas$Ano[caixas$Codigo=="16386"], caixas$Qt_Tot[caixas$Codigo=="16386"], col="black")
points(caixas$Ano[caixas$Codigo=="16981"], caixas$Qt_Tot[caixas$Codigo=="16981"], col="green", pch=19)
lines(caixas$Ano[caixas$Codigo=="16981"], caixas$Qt_Tot[caixas$Codigo=="16981"], col="black")
points(caixas$Ano[caixas$Codigo=="16389"], caixas$Qt_Tot[caixas$Codigo=="16389"], col="purple", pch=19)
lines(caixas$Ano[caixas$Codigo=="16389"], caixas$Qt_Tot[caixas$Codigo=="16389"], col="black")
points(caixas$Ano[caixas$Codigo=="15419"], caixas$Qt_Tot[caixas$Codigo=="15419"], col="pink", pch=19)
lines(caixas$Ano[caixas$Codigo=="15419"], caixas$Qt_Tot[caixas$Codigo=="15419"], col="black")
points(caixas$Ano[caixas$Codigo=="14746"], caixas$Qt_Tot[caixas$Codigo=="14746"], col="yellow", pch=19)
lines(caixas$Ano[caixas$Codigo=="14746"], caixas$Qt_Tot[caixas$Codigo=="14746"], col="black")

legend( x = "top",
        legend = c("NODUS sem janela (16386)","AFFERO XMAS (16981)", "CALIX sem janela (16389)", "SPARKLING (15419)", "PORTET CLASSIC (14746)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )


#Base de dados de caixas - Nivel 3 (caixas vendidas em maior quantidade)
#DETERMINAÇÃO DAS CAIXAS MENOS VENDIDAS POR ANO (QT_TOT)

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2015

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2015], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2015]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2015

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2015 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2016

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2016], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2016]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2016

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2016 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2017

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2017], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2017]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2017

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2017 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2018

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2018], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2018]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2018

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2018 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2019

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2019], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2019]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2019

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2019 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2020

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2020], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2020]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2020

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2020 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas no ano de 2021

h <- aggregate(x= caixas_vend_3$Qt_Tot[caixas_vend_3$Ano==2021], 
               by= list(caixas_vend_3$Codigo[caixas_vend_3$Ano==2021]), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas em 2021

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas em 2021 - Nivel 3", xlab="Codigo", ylab="Quantidade")

#Apresenta os 5 códigos de caixas menos vendidas ao longo dos anos

#Agrega códigos de caixas e devolve o mínimo de quantidades vendidas ao longo dos anos

h <- aggregate(x= caixas_vend_3$Qt_Tot, 
               by= list(caixas_vend_3$Codigo), 
               FUN=min)

#Ordenar os valores anteriores apresentando primeiro os menores valores

h <- h[order(h$x),]
h

#Apresenta os 5 códigos de caixas menos vendidas ao longo dos anos - NIVEL 3

head(h,5)
h <-  h[1:5,]
plot(factor(h[,1]),h[,2], main="Caixas menos vendidas ao longo dos anos - Nivel 3", xlab="Codigo", ylab="Quantidade")


# Plot do Qt_Tot de caixas menos vendidas (forma isolada)

plot(caixas$Ano[caixas$Codigo=="14766"], caixas$Qt_Tot[caixas$Codigo=="14766"],ylim = c(0,90000), xlab="Anos",ylab="Quantidade Total", col="blue", pch=19, main = "TOP 5 - Caixas Menos Vendidas - Nivel 3")
lines(caixas$Ano[caixas$Codigo=="14766"], caixas$Qt_Tot[caixas$Codigo=="14766"], col="black")
points(caixas$Ano[caixas$Codigo=="15782"], caixas$Qt_Tot[caixas$Codigo=="15782"], col="green", pch=19)
lines(caixas$Ano[caixas$Codigo=="15782"], caixas$Qt_Tot[caixas$Codigo=="15782"], col="black")
points(caixas$Ano[caixas$Codigo=="15836"], caixas$Qt_Tot[caixas$Codigo=="15836"], col="purple", pch=19)
lines(caixas$Ano[caixas$Codigo=="15836"], caixas$Qt_Tot[caixas$Codigo=="15836"], col="black")
points(caixas$Ano[caixas$Codigo=="15804"], caixas$Qt_Tot[caixas$Codigo=="15804"], col="pink", pch=19)
lines(caixas$Ano[caixas$Codigo=="15804"], caixas$Qt_Tot[caixas$Codigo=="15804"], col="black")
points(caixas$Ano[caixas$Codigo=="15129"], caixas$Qt_Tot[caixas$Codigo=="15129"], col="yellow", pch=19)
lines(caixas$Ano[caixas$Codigo=="15129"], caixas$Qt_Tot[caixas$Codigo=="15129"], col="black")

legend( x = "top",
        legend = c("GOURMET M (14766)","KRAFT (15782)", "KRAFT (15836)", "B2C PACKAGING (15804)", "CLASSIC (15129)"),
        col = c("blue","green", "purple", "pink", "yellow"), lwd = 2, lty = c(0,0),
        pch = 19 )

#-------------------------------------------------------------------------------------------------------------------
#Valores padrão estabelecidos pelo orientador para divisão da base de dados
#Base de dados de caixas - Nivel 4 (caixas vendidas em menor quantidade)
caixas_vend_4 <- caixas[caixas$Qt_Tot>=5000,]
hist(caixas_vend_4$Qt_Tot)
boxplot(caixas_vend_4$Qt_Tot)

#Base de dados de caixas - Nivel 5 (caixas vendidas em menor quantidade)
caixas_vend_5 <- caixas[caixas$Qt_Tot<=5000,]
hist(caixas_vend_5$Qt_Tot)
boxplot(caixas_vend_5$Qt_Tot)

#Valores padrão estabelecidos pelo orientador para divisão da base de dados
#Base de dados de sacos - Nivel 4

sacos_vend_4 <- sacos[sacos$Qt_Tot<50000 ,]
hist(sacos_vend_4$Qt_Tot)
boxplot(sacos_vend_4$Qt_Tot)

#Base de dados de sacos - Nivel 5

sacos_vend_5 <- sacos[sacos$Qt_Tot>50000 ,]
hist(sacos_vend_5$Qt_Tot)
boxplot(sacos_vend_5$Qt_Tot)
#------------------------------------------------------------------------------------------------------------------
