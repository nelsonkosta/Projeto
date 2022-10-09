#Tira os acentos dos dados das caixas

unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

library(gsubfn)
caixas$Referencia <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,caixas$Referencia)
caixas$Referencia
caixas

#Tira os acentos dos dados dos sacos

sacos$Referencia <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,sacos$Referencia)
sacos$Referencia
sacos

#ANÁLISE ESTATÍSTICA DESCRITIVA DAS VARIÁVEIS

length(unique(caixas$Referencia))
length(unique(sacos$Referencia))
summary(caixas$Qt_Pt)
var(caixas$Qt_Pt)

boxplot(caixas$Qt_Tot ~ caixas$Referencia, main="", ylab="Qt_pt",
        col=("yellow"), xlim=c(0,5.3), ylim=c(0,10000))

#VARIÁVEL TIPO
length(unique(Litel$Tipo)) # Numero de observações únicas da variável tipo do dataset completo (caixas e sacos)

#grafico circular e barras
pie(table(Litel$Tipo), col = c("blue", "purple"))
barplot(table(Litel$Tipo), col = "grey", names=c("Caixas", "Sacos"))


#VARIÁVEL REFERENCIA

#NÚMERO DE OBSERVAÇÕES ÚNICAS
caixas.tb <- table(caixas$Referencia) #Tabela descritiva do número de observações de cada categoria de caixas
sacos.tb <- table(sacos$Referencia) # Tabela descritiva do número de observações de cada categoria de sacos

length(unique(caixas$Referencia)) # Número de observações únicas da variável referencia para as caixas
length(unique(sacos$Referencia)) # Número de observações únicas da variável referencia para os sacos

names(caixas.tb)[caixas.tb == max(caixas.tb)] #Moda - referência que aparece mais vezes
names(sacos.tb)[sacos.tb == max(sacos.tb)] #Moda - referência que aparece mais vezes

#Variável Medida
caixas.tb <- table(caixas$Medida) #Tabela descritiva do número de observações por medida de caixas
sacos.tb <- table(sacos$Medida) # Tabela descritiva do número de observações por medida de sacos

length(unique(caixas$Medida)) # Numero de observações únicas da variável medida para as caixas
length(unique(sacos$Medida)) # Numero de observações únicas da variável medida para os sacos

names(caixas.tb)[caixas.tb == max(caixas.tb)] #Moda - medida de caixas que aparece mais vezes
names(sacos.tb)[sacos.tb == max(sacos.tb)] #Moda - medida de sacos que aparece mais vezes

#Variável Codigo

caixas.tb <- table(caixas$Codigo) #Tabela descritiva do número de observações de códigos de caixas
sacos.tb <- table(sacos$Codigo) # Tabela descritiva do número de observações de códigos de sacos

length(unique(sacos$Codigo)) # Quantidade de sacos da base de dados
length(unique(caixas$Codigo)) #Quantidade de caixas da base de dados

names(caixas.tb)[caixas.tb == max(caixas.tb)] #Moda - código de caixas que aparece mais vezes
names(sacos.tb)[sacos.tb == max(sacos.tb)] #Moda - código de sacos que aparece mais vezes


#Média, Mediana e Quartis das variáveis quantitativas
summary(sacos)
summary(caixas)

#Variâncias das variaveis quantitativas
var(sacos$Qt_Pt)
var(caixas$Qt_Pt)
var(sacos$Vl_Pt)
var(caixas$Vl_Pt)
var(sacos$Qt_Sp)
var(caixas$Qt_Sp)
var(sacos$Vl_Sp)
var(caixas$Vl_Sp)
var(sacos$Qt_FR)
var(caixas$Qt_FR)
var(sacos$Vl_Fr)
var(caixas$Vl_Fr)
var(sacos$Qt_Eng)
var(caixas$Qt_Eng)
var(sacos$Vl_Eng)
var(caixas$Vl_Eng)
var(sacos$Qt_Ger)
var(caixas$Qt_Ger)
var(sacos$Vl_Ger)
var(caixas$Vl_Ger)
var(sacos$Qt_Tot)
var(caixas$Qt_Tot)
var(sacos$Vl_Tot)
var(caixas$Vl_Tot)

#Desvio padrão das variáveis quantitativas
sd(sacos$Qt_Pt)
sd(caixas$Qt_Pt)
sd(sacos$Vl_Pt)
sd(caixas$Vl_Pt)
sd(sacos$Qt_Sp)
sd(caixas$Qt_Sp)
sd(sacos$Vl_Sp)
sd(caixas$Vl_Sp)
sd(sacos$Qt_FR)
sd(caixas$Qt_FR)
sd(sacos$Vl_Fr)
sd(caixas$Vl_Fr)
sd(sacos$Qt_Eng)
sd(caixas$Qt_Eng)
sd(sacos$Vl_Eng)
sd(caixas$Vl_Eng)
sd(sacos$Qt_Ger)
sd(caixas$Qt_Ger)
sd(sacos$Vl_Ger)
sd(caixas$Vl_Ger)
sd(sacos$Qt_Tot)
sd(caixas$Qt_Tot)
sd(sacos$Vl_Tot)
sd(caixas$Vl_Tot)

#Variável Ano
caixas.tb <- table(caixas$Ano) #Numero de observações de caixas por ano
sacos.tb <- table(sacos$Ano) #Numero de observações de sacos por ano

length(unique(sacos$Ano)) # Quantidade de anos na base de dados com obs. de sacos
length(unique(caixas$Ano)) #Quantidade de anos na base de dados com obs. de caixas

names(caixas.tb)[caixas.tb == max(caixas.tb)] #Moda - ano com mais observações de caixas
names(sacos.tb)[sacos.tb == max(sacos.tb)] #Moda - ano(s) com mais observações de sacos

#Scatterplots relação entre variáveis
pairs(caixas[,c(5,7,9,11,13,15)], main="Relações entre as variáveis no dataset das caixas")
pairs(sacos[,c(5,7,9,11,13,15)], main="Relações entre as variáveis no dataset dos sacos")
pairs(caixas[,c(6,8,10,12,14,16)], main="Relações entre as variáveis no dataset das caixas")
pairs(sacos[,c(6,8,10,12,14,16)], main="Relações entre as variáveis no dataset dos sacos")         

#Histogramas dos Sacos
#Histogramas e boxplots dos Sacos (Qt_Pais)

par(mfrow=c(1,1))
#hist(sacos$Qt_Pt, breaks = 100, xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Portugal" )
hist(sacos$Qt_Pt, breaks = 400, xlim =c(0,25000), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Portugal" )
hist(sacos$Qt_Pt, breaks = 50, xlim =c(25000,280000), ylim = c(0,25), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Portugal" )
boxplot(sacos$Qt_Pt[sacos$Qt_Pt<25000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Portugal")
boxplot(sacos$Qt_Pt[sacos$Qt_Pt>25000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Portugal")


#hist(sacos$Qt_Sp, breaks = 100, xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Espanha" )
hist(sacos$Qt_Sp, breaks = 400, xlim =c(0,24000), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Espanha" )
hist(sacos$Qt_Sp, breaks = 50, xlim =c(24000,260000), ylim = c(0,15), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Espanha" )
boxplot(sacos$Qt_Sp[sacos$Qt_Sp<24000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Espanha")
boxplot(sacos$Qt_Sp[sacos$Qt_Sp>24000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Espanha")


#hist(sacos$Qt_FR, breaks = 100, xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em França" )
hist(sacos$Qt_FR, breaks = 400, xlim =c(-1000,60000), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em França" )
hist(sacos$Qt_FR, breaks = 50, xlim =c(60000,1100000), ylim = c(0,100), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em França")
boxplot(sacos$Qt_FR[sacos$Qt_FR<60000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em França")
boxplot(sacos$Qt_FR[sacos$Qt_FR>60000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em França")



#hist(sacos$Qt_Eng, breaks= 100, xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Inglaterra" )
hist(sacos$Qt_Eng, breaks = 600, xlim =c(0,10000), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Inglaterra" )
hist(sacos$Qt_Eng, breaks = 50, xlim =c(10000,120000), ylim = c(0,6), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Inglaterra")
boxplot(sacos$Qt_Eng[sacos$Qt_Eng<10000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Inglaterra")
boxplot(sacos$Qt_Eng[sacos$Qt_Eng>10000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos em Inglaterra")


#hist(sacos$Qt_Ger, breaks= 100, xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos na Alemanha" )
hist(sacos$Qt_Ger, breaks = 700, xlim =c(0,1250), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos na Alemanha" )
hist(sacos$Qt_Ger, breaks = 50, xlim =c(1250,20000), ylim = c(0,20), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos na Alemanha")
boxplot(sacos$Qt_Ger[sacos$Qt_Ger<1250], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos na Alemanha")
boxplot(sacos$Qt_Ger[sacos$Qt_Ger>1250], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos na Alemanha")


#hist(sacos$Qt_Tot, breaks= 100, xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos no Total")
hist(sacos$Qt_Tot, breaks = 700, xlim =c(0,130000), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos no Total" )
hist(sacos$Qt_Tot, breaks = 50, xlim =c(130000,1350000), ylim = c(0,20), xlab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos no Total")
boxplot(sacos$Qt_Tot[sacos$Qt_Tot<130000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos no Total")
boxplot(sacos$Qt_Tot[sacos$Qt_Tot>130000], ylab = "Quantidade de Vendas", main = "Quantidade de Sacos Vendidos no Total")



#Histogramas e boxplots dos Sacos (Vl_Pais)

par(mfrow=c(1,1))
#hist(sacos$Vl_Pt, breaks = 100, xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Portugal" )
hist(sacos$Vl_Pt, breaks = 400, xlim =c(0,2880), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Portugal" )
hist(sacos$Vl_Pt, breaks = 50, xlim =c(2880,26000), ylim = c(0,25), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Portugal" )
boxplot(sacos$Vl_Pt[sacos$Vl_Pt<2880], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Portugal")
boxplot(sacos$Vl_Pt[sacos$Vl_Pt>2880], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Portugal")


#hist(sacos$Vl_Sp, breaks = 100, xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Espanha" )
hist(sacos$Vl_Sp, breaks = 400, xlim =c(0,2350), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Espanha" )
hist(sacos$Vl_Sp, breaks = 50, xlim =c(2350,24000), ylim = c(0,50), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Espanha" )
boxplot(sacos$Vl_Sp[sacos$Vl_Sp<2350], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Espanha")
boxplot(sacos$Vl_Sp[sacos$Vl_Sp>2350], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Espanha")


#hist(sacos$Vl_Fr, breaks = 100, xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em França" )
hist(sacos$Vl_Fr, breaks = 500, xlim =c(-100,5500), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em França" )
hist(sacos$Vl_Fr, breaks = 50, xlim =c(5500,68000), ylim = c(0,40), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em França" )
boxplot(sacos$Vl_Fr[sacos$Vl_Fr<5500], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em França")
boxplot(sacos$Vl_Fr[sacos$Vl_Fr>5500], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em França")


#hist(sacos$Vl_Eng, breaks= 100, xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Inglaterra" )
hist(sacos$Vl_Eng, breaks = 800, xlim =c(0,1150), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Inglaterra" )
hist(sacos$Vl_Eng, breaks = 50, xlim =c(1150,17000), ylim = c(0,20), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Inglaterra" )
boxplot(sacos$Vl_Eng[sacos$Vl_Eng<1150], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Inglaterra")
boxplot(sacos$Vl_Eng[sacos$Vl_Eng>1150], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos em Inglaterra")



#hist(sacos$Vl_Ger, breaks= 100, xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos na Alemanha" )
hist(sacos$Vl_Ger, breaks = 300, xlim =c(0,110), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos na Alemanha" )
hist(sacos$Vl_Ger, breaks = 50, xlim =c(110,1630), ylim = c(0,40), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos na Alemanha" )
boxplot(sacos$Vl_Ger[sacos$Vl_Ger<110], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos na Alemanha")
boxplot(sacos$Vl_Ger[sacos$Vl_Ger>110], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos na Alemanha")


#hist(sacos$Vl_Tot, breaks= 100, xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos no Total")
hist(sacos$Vl_Tot, breaks = 300, xlim =c(0,19600), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos no Total" )
hist(sacos$Vl_Tot, breaks = 50, xlim =c(19600,110000), ylim = c(0,6), xlab = "Valor de Vendas", main = "Valor de Sacos Vendidos no Total" )
boxplot(sacos$Vl_Tot[sacos$Vl_Tot<19600], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos no Total")
boxplot(sacos$Vl_Tot[sacos$Vl_Tot>19600], ylab = "Valor de Vendas", main = "Valor de Sacos Vendidos no Total")

#Histogramas e boxplots das Caixas (Qt_Pais)

par(mfrow=c(1,1))
#hist(caixas$Qt_Pt, breaks = 100, xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Portugal" )
hist(caixas$Qt_Pt, breaks = 100, xlim =c(0,3900), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Portugal" )
hist(caixas$Qt_Pt, breaks = 100, xlim =c(3900,8500), ylim = c(0,8), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Portugal" )
boxplot(caixas$Qt_Pt[caixas$Qt_Pt<3900], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Portugal")
boxplot(caixas$Qt_Pt[caixas$Qt_Pt>3900], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Portugal")

#hist(caixas$Qt_Sp, breaks = 100, xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Espanha" )
hist(caixas$Qt_Sp, breaks = 100, xlim =c(-3000,4000), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Espanha" )
hist(caixas$Qt_Sp, breaks = 100, xlim =c(4000,30000), ylim = c(0,8), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Espanha" )
boxplot(caixas$Qt_Sp[caixas$Qt_Sp>0 & caixas$Qt_Sp<4000], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Espanha")
boxplot(caixas$Qt_Sp[caixas$Qt_Sp>4000], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Espanha")

#hist(caixas$Qt_FR, breaks = 100, xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em França" )
hist(caixas$Qt_FR, breaks = 400, xlim =c(-850,2680), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em França" )
hist(caixas$Qt_FR, breaks = 100, xlim =c(2680,20000), ylim = c(0,8), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em França" )
#boxplot(caixas$Qt_FR[caixas$Qt_FR<0], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em França")
boxplot(caixas$Qt_FR[caixas$Qt_FR>0 & caixas$Qt_FR<2680], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em França")
boxplot(caixas$Qt_FR[caixas$Qt_FR>2680], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em França")

#hist(caixas$Qt_Eng, breaks= 800, xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas em Inglaterra" )
#Os dados de venda de caixas para a Inglaterra são compostos unicamente por zeros.


#hist(caixas$Qt_Ger, breaks= 100, xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas na Alemanha" )
hist(caixas$Qt_Ger, breaks = 300, xlim =c(0,100), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas na Alemanha" )
hist(caixas$Qt_Ger, breaks = 50, xlim =c(100,800), ylim = c(0,8), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas na Alemanha" )
boxplot(caixas$Qt_Ger, ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas na Alemanha")

#hist(caixas$Qt_Tot, breaks= 100, xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas no Total")
hist(caixas$Qt_Tot, breaks= 400, xlim = c(-1000, 11000), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas no Total")
hist(caixas$Qt_Tot, breaks= 50, xlim = c(11000, 90000), ylim = c(0,15), xlab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas no Total")
boxplot(caixas$Qt_Tot[caixas$Qt_Tot<11000], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas no Total")
boxplot(caixas$Qt_Tot[caixas$Qt_Tot>11000], ylab = "Quantidade de Vendas", main = "Quantidade de Caixas Vendidas no Total")

#Histogramas e boxplots das Caixas (Vl_Pais)

par(mfrow=c(1,1))
#hist(caixas$Vl_Pt, breaks = 100, xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Portugal" )
hist(caixas$Vl_Pt, breaks = 300, xlim =c(-100,2100), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Portugal" )
hist(caixas$Vl_Pt, breaks = 50, xlim =c(2100,10000), ylim = c(0,10), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Portugal" )
boxplot(caixas$Vl_Pt[caixas$Vl_Pt<2100], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Portugal")
boxplot(caixas$Vl_Pt[caixas$Vl_Pt>2100], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Portugal")


#hist(caixas$Vl_Sp, breaks = 100, xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Espanha" )
hist(caixas$Vl_Sp, breaks = 300, xlim =c(-1000,1310), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Espanha" )
hist(caixas$Vl_Sp, breaks = 50, xlim =c(1310,9000), ylim = c(0,30), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Espanha" )
boxplot(caixas$Vl_Sp[caixas$Vl_Sp<1310], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Espanha")
boxplot(caixas$Vl_Sp[caixas$Vl_Sp>1310], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Espanha")


#hist(caixas$Vl_Fr, breaks = 100, xlab = "Valor de Vendas", main = "Valor de Caixas vendidas em França" )
hist(caixas$Vl_Fr, breaks = 800, xlim =c(-200,3450), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em França" )
hist(caixas$Vl_Fr, breaks = 50, xlim =c(3450,65000), ylim = c(0,20), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em França" )
#boxplot(caixas$Vl_Fr[caixas$Vl_Fr<0], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em França")
boxplot(caixas$Vl_Fr[caixas$Vl_Fr>0 & caixas$Vl_Fr<3450], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em França")
boxplot(caixas$Vl_Fr[caixas$Vl_Fr>3450], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas em França")



#hist(caixas$Vl_Eng, breaks= 100, xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas em Inglaterra" )
#Os dados de venda de caixas para a Inglaterra são compostos unicamente por zeros.

#hist(caixas$Vl_Ger, breaks= 100, xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas na Alemanha" )
hist(caixas$Vl_Ger, breaks = 900, xlim =c(0,40), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas na Aleamanha" )
hist(caixas$Vl_Ger, breaks = 50, xlim =c(40,340), ylim = c(0,5), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas na Alemanha" )
boxplot(caixas$Vl_Ger, ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas na Alemanha")


#hist(caixas$Vl_Tot, breaks= 100, xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas no Total")
hist(caixas$Vl_Tot, breaks = 900, xlim =c(-100,6500), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas no Total" )
hist(caixas$Vl_Tot, breaks = 50, xlim =c(6500,68000), ylim = c(0,20), xlab = "Valor de Vendas", main = "Valor de Caixas Vendidas no Total" )
boxplot(caixas$Vl_Tot[caixas$Vl_Tot<6500], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas no total")
boxplot(caixas$Vl_Tot[caixas$Vl_Tot>6500], ylab = "Valor de Vendas", main = "Valor de Caixas Vendidas no total")
