# Aula 1 - Introdução

# Gráfico de Dispersão
# Gráfico de Barras
# Gráfico de Setor
# Histogramas
# Gráfico de Linhas
# Gráfico de Caixa (Box)
# Gráfico de Distribuição Normal
# WordCloud

# Pacote lattice
# Pacote ggplot2

############################################################################################
# Aula 2 - Gŕafico de Barras
curso<-c("Eng. Computação", "Eng. Software", "Sistemas de Informação", "TADS")
aprovado<-c(1751, 2186, 947, 200)
reprovado<-c(2528, 2132, 1843, 280)
curso
aprovado
reprovado
#save(curso,file = "curso")
#save(aprovado,file = "aprovado")
#save(reprovado,file = "reprovado")
#Gráfico de Barras
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", xlab= "Cursos",
        ylab="Aprovação")
#Sub-Título
barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", xlab= "Cursos",
        ylab="Aprovação", sub="Teste Gráfico")
#Alterando o tamanho do título
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", cex.main=0.9)
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", cex.main=1.5)
#Alterando o tamanho dos valores do eixo
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", cex.axis=0.5)
#Alterando o tamanho dos nomes dos eixos
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", cex.lab=0.5)
#Resumindo:
#cex.main= valor, para o título
#cex.lab = valor, para os eixos
#cex.axis = valor, para o números dos eixos
#cex.sub = valor, para o subtítulo

#Eliminando Eixo
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", axes = F)

#Girando o Gráfico
barplot(aprovado, names.arg=curso, main="Aprovação por Curso", ylab= "Cursos", 
        xlab="Aprovação", sub="Teste Gráfico",horiz = T)

#Cores
#Densidade
barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",density = 20)

barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",density=20, angle=90)

colors()

barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",col = "gold3")

#números de 1 a 8 que correspondem respectivamente às cores 1-preto,
#2-vermelho, 3-verde, 4-azul, 5-ciano, 6-magenta, 7-amarelo e 8-cinza.

barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",col = c(2,3,4,5))

#RGB
barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",col = rgb(0.5,0.7,0.6))

#Borda
barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",col = rgb(0.5,0.7,0.6), border = F)

barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",col = rgb(0.5,0.7,0.6), density = 20,border = F)

barplot(aprovado, names.arg=curso, main= "Aprovação por Curso", ylab= "Aprovação",
        xlab="Cursos", sub="Teste Gráfico",col = rgb(0.5,0.7,0.6), density = 20,border = T)


ind_aprov = matrix(c(curso,aprovado,reprovado),nrow=4, ncol=3,dimnames=list(c("C1","C2","C3","C4"),c("Curso","Aprovados","Reprovados")))
ind_aprov
ind_aprov = matrix(c(aprovado,reprovado),nrow=4, ncol=2, dimnames=list(curso,c("Aprovados","Reprovados")))
ind_aprov
#save(ind_aprov,file = "ind_aprov")
barplot(ind_aprov, main= "Aprovação por Curso", ylab= "Índices",
        xlab="Estado")

barplot(ind_aprov, main= "Aprovação por Curso", ylab= "Índices",
        xlab="Estado",beside = T)

barplot(ind_aprov, main= "Aprovação por Curso", ylab= "Índices",
        xlab="Estado",beside = T, legend.text=rownames(ind_aprov))

barplot(ind_aprov, main= "Aprovação por Curso", ylab= "Índices",
        xlab="Estado",beside = T, legend.text=rownames(ind_aprov), ylim = c(0,3000), xlim = c(0,20))

barplot(ind_aprov, main= "Aprovação por Curso", ylab= "Índices",
        xlab="Estado",beside = T, legend.text=rownames(ind_aprov), ylim = c(0,3000),
        xlim = c(0,20), col = c(2,3,4,5))

getwd()
setwd("/home/fermat/Área de Trabalho/RStudio/R - Básico")

# uma opção para figuras em jpeg
dev.copy(device = jpeg, file = "exemplo1.jpeg", width = 600, height = 500, res = 100)
dev.off()

# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo1.1.pdf", width = 600, paper = "USr")
dev.off()


help("plot")

#############################################################################################
# Aula 03 - Gráfico de Setor
matriculas = c(51971, 33625, 25018,18890, 13457, 10266, 6989)
names(matriculas)=c("medicina","veterinaria","eng. civil","eng. mecanica","eng. eletrica",
                    "eng.computacao", "tads")
matriculas

pie(matriculas)

pie(matriculas, main = "Matrículas em Cursos Superiores - UFG")

porc = round(matriculas*100/sum(matriculas), 2)

rotulos = paste("(",porc,"%)", sep="")

pie(matriculas, main = "Matrículas em Cursos Superiores - UFG", labels=rotulos,
    col=rainbow(7))
legend(1, 1, names(matriculas), col=rainbow(7), pch=rep(20,6))

legend(1.4, 1, names(matriculas), col=rainbow(7), pch=rep(20,6),cex = 0.6)
help("pch")

pie(matriculas, main = "Matrículas em Cursos Superiores - UFG",
    labels=rotulos, cex=0.7, col=rainbow(7))
legend(1.4, 1, names(matriculas), col=rainbow(7), pch=rep(20,6),cex = 0.6)
# uma opção para figuras em jpeg
dev.copy(device = jpeg, file = "exemplo2.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo2.2.pdf", width = 600, paper = "USr")
dev.off()


pie(matriculas, main = "Matrículas em Cursos Superiores - UFG",labels=NA)
text(locator(length(names(matriculas))), rotulos)
     
pie(matriculas, main = "Matrículas em Cursos Superiores - UFG", cex=0.9)
pie(matriculas, main = "Matrículas em Cursos Superiores - UFG",init.angle=180)

####################################################################################
# Aula 04 - Histogramas
# Um histograma é uma representação gráfica da distribuição de frequências de uma
#massa de medições, normalmente um gráfico de barras verticais. Tal gráfico é composto
#por retângulos justapostos em que a base de cada um deles corresponde ao intervalo de
#classe e a sua altura à respectiva frequência.

setwd("/home/fermat/Área de Trabalho/RStudio/R - Básico")
load("corrida")
corrida
hist(corrida$Pace)
hist(corrida$Posicao)
hist(corrida$Pace, freq = F)
hist(corrida$Pace, main="Histograma - Pace da Corrida",density = 20)
hist(corrida$Pace, main="Histograma - Pace da Corrida",density = 20, angle = 70)
hist(corrida$Pace, main="Histograma - Pace da Corrida",col=c("blue","red","orange","green","pink"))
hist(corrida$Pace, main="Histograma - Pace da Corrida",border=c("blue","red","orange","green","pink"))
hist(corrida$Pace, main="Histograma - Pace da Corrida",col=c("blue","red","orange","green","pink"),
     border = F)
hist(corrida$Pace, main="Histograma - Pace da Corrida",col=c("blue","red","orange","green","pink"),
     border = F,xlab="Pace",ylab="Frequencia")

hist(corrida$Pace, main="Histograma - Pace da Corrida",col=c("blue","red","orange","green","pink"),
     border = F,xlab="Pace",ylab="Frequencia",breaks = 3)

dev.copy(device = jpeg, file = "exemplo3.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo3.3.pdf", width = 600, paper = "USr")
dev.off()

#Ramo das folhas
stem(corrida$Pace)


#######################################################################################
# Aula 05 - Gráfico de Caixa(box)
#O boxplot é um gráfico que possibilita representar a distribuição de um conjunto de
#dados com base em alguns de seus parâmetros descritivos, quais sejam: a mediana (q2), o
#quartil inferior (q1), o quartil superior (q3) e do intervalo interquartil (IQR = q3 - q1)

data(rock)
rock
attach(rock)
boxplot(shape)

boxplot(shape, main="BoxPlot do Shape(Rock)" , ylab="perimetro/sqrt(area)")

boxplot(corrida$Pace)
boxplot(corrida$Posicao)
boxplot(corrida)
# Cercas com valores máximo e mínimo
boxplot(shape, range=0)
#Outlier
boxplot(shape, outline=FALSE)

data(PlantGrowth)
PlantGrowth
attach(PlantGrowth)
boxplot(weight~group)
title("Boxplot para rendimento de plantas segundo o tratamento", xlab =
        "tratamento", ylab = "peso")

data(iris)
iris
boxplot(iris$Sepal.Length~iris$Species)

boxplot(iris[1:4],main="Iris", xlab="Variáveis",
        col="aquamarine3", horizontal = F, notch = F)

dev.copy(device = jpeg, file = "exemplo4.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo4.4.pdf", width = 600, paper = "USr")
dev.off()

x1 = sample(1:9,size=13,replace = T)
x1
x2 = sample(1:6, size = 31,replace = T)
x2
boxplot(x1, x2, varwidth=TRUE)
# Nomear os grupos:
boxplot(x1, x2, names=c("grupo1","grupo2"))
# Modificar a largura da caixa para mais estreita 
boxplot(x1, x2, boxwex=0.3)
# Ajeitar o tamanho das linhas limites:
boxplot(x1, x2, staplewex=0.1)
# Para colocar cor nas bordas:
boxplot(x1, x2, border="red")

boxplot.stats(iris$Sepal.Width)

###########################################################################################
# Aula 06 - Gráfico de Dispersão
# Os diagramas de dispersão são representações de duas variáveis que são organizadas 
# em um gráfico, para observar o padrão de relacionamento entre as mesmas. É um método
# gráfico que permite verificar a existência ou não de relação entre duas variáveis de 
# natureza quantitativa.

data("women")
women
cor(women$height,women$weight)

plot(women$height,women$weight, main = "Relação entre Peso | Altura",xlab = "Peso",
     ylab = "Altura")

#Colocando a reta
plot(women$height,women$weight, main = "Relação entre Peso | Altura",xlab = "Peso",
     ylab = "Altura", abline(lm(women$weight~women$height), col=2))
#Mudando caracter
plot(women$height,women$weight, main = "Relação entre Peso | Altura",xlab = "Peso",
     ylab = "Altura", abline(lm(women$weight~women$height), col=2), pch=17)
#Largura da linha
plot(women$height,women$weight, main = "Relação entre Peso | Altura",xlab = "Peso",
     ylab = "Altura", abline(lm(women$weight~women$height), col=2), lwd=3, col="blue")
#Mudando valores dos eixos
plot(women$height,women$weight, main = "Relação entre Peso | Altura",
     xlab ="Peso",ylab="Altura", abline(lm(women$weight~women$height)), col=2,
     xlim=range(50:80), ylim=range(120:180))

Orange

plot(Orange$age,Orange$circumference)
plot(Orange$age,Orange$circumference, abline(lm(Orange$circumference~Orange$age),col=3))
plot(Orange$age,Orange$circumference,col="red", abline(lm(Orange$circumference~Orange$age),col=3),
     xlab="Idade em dias", ylab="Circunferência em mm",
     main="Dispersão entre Idade e Circunferência",pch=20)

plot(jitter(Orange$age),Orange$circumference,col="red", abline(lm(Orange$circumference~Orange$age),col=3),
     xlab="Idade em dias", ylab="Circunferência em mm",
     main="Dispersão entre Idade e Circunferência",pch=20)

plot(jitter(Orange$age),Orange$circumference,col=Orange$Tree, abline(lm(Orange$circumference~Orange$age),col=3),
     xlab="Idade em dias", ylab="Circunferência em mm",
     main="Dispersão entre Idade e Circunferência",pch=20)

setwd("/home/fermat/Área de Trabalho/RStudio/R - Básico")
dev.copy(device = jpeg, file = "exemplo5.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo5.5.pdf", width = 600, paper = "USr")
dev.off()


par(mfrow=c(2, 3))
plot(Orange$age[Orange$Tree==1], Orange$circumference[Orange$Tree==1],
     xlab= "Idade", ylab= "Circumferência", sub= "Árvore1")
plot(Orange$age[Orange$Tree==2], Orange$circumference[Orange$Tree==2],
     xlab= "Idade", ylab= "Circumferência", sub= "Árvore2")
plot(Orange$age[Orange$Tree==3], Orange$circumference[Orange$Tree==3], 
     xlab= "Idade", ylab= "Circumferência", sub= "árvore3")
plot(Orange$age[Orange$Tree==4], Orange$circumference[Orange$Tree==4],
     xlab= "Idade", ylab= "Circumferência", sub= "árvore4")
plot(Orange$age[Orange$Tree==5], Orange$circumference[Orange$Tree==5],
     xlab= "Idade", ylab= "Circumferência", sub= "árvore5")
plot(Orange$age, Orange$circumference, xlab="Idade", ylab="Circumferência", sub="Árvores")

#########################################################################################
# Aula 07 - Nuvem de Palavras
#installed.packages()
#install.packages("wordcloud", dependencies = T)
library(wordcloud)

#install.packages("RColorBrewer")
library(RColorBrewer)

nuvem = read.csv(file.choose(),header = T,sep = ",")
nuvem
class(nuvem)
class(nuvem$Palavra)
nuvem$Palavra

wordcloud(words = nuvem$Palavra, freq = nuvem$Freq)
wordcloud(words = nuvem$Palavra, freq = nuvem$Freq, random.color = T,
          random.order = T, colors = rainbow(8), rot.per = 0.5)
wordcloud(words = nuvem$Palavra, freq = nuvem$Freq)

#install.packages("wordcloud2",dependencies = T)
library(wordcloud2)
nuvem2 = read.csv(file.choose(),header = T,sep = ",")

wordcloud2(nuvem2)
wordcloud2(nuvem2, color = "random-light", backgroundColor = "grey")
wordcloud2(nuvem2, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
rotateRatio = 1)

setwd("/home/fermat/Área de Trabalho/RStudio/R - Básico")
dev.copy(device = jpeg, file = "exemplo6.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo6.6.pdf", width = 600, paper = "USr")
dev.off()


######################################################################################
# Aula 08 - Gráfico de Linhas
AirPassengers
plot(AirPassengers, main="Transporte Aéreo",xlab="Tempo(Anos)",
     ylab="Quantidade(Milhões)", type = "l", col="red")

plot(AirPassengers, main="Transporte Aéreo",xlab="Tempo(Anos)",
     ylab="Quantidade(Milhões)", type = "b", col="red")

#type: "p" para pontos, "l" para linhas, "b" para pontos e linhas, 
#"c" para linhas descontínuas nos pontos, "o" para pontos sobre as linhas,
#"n" para nenhum gráfico, apenas a janela.

airmiles
class(airmiles)

plot(airmiles, main="Milhas Aérea",xlab="Tempo(Anos)",
     ylab="Quantidade(Milhas)", type = "l", col="blue")


par(mfrow=c(2,1))
plot(AirPassengers, main="Transporte Aéreo",xlab="Tempo(Anos)",
     ylab="Quantidade(Milhões)", type = "l", col="red")
plot(airmiles, main="Milhas Aérea",xlab="Tempo(Anos)",
     ylab="Quantidade(Milhas)", type = "l", col="blue")

meses = c(1:12)
meses
t1 = sample(-10:10,size = 12, replace = T)
t1
t2 = sample(-10:10,size = 12, replace = T)
t2
t3 = sample(-10:10,size = 12, replace = T)
t3

plot(meses,t1,type="l",main="Temperatura em ºC",col="blue",xlab="Meses",
     ylab="Variação em ºC",, ylim=c(-10,20))
lines(meses, t2, col="red", type="l")
lines(meses, t3, col="green", type="l")

legend(10.65, 20.5, c("1º Temp","2º Temp","3º Temp"), col =c("blue","red","green"),
       pch=rep(20,2),cex = 0.7)

plot(meses,t1,type="b",main="Temperatura em ºC",col="blue",xlab="Meses",
     ylab="Variação em ºC",, ylim=c(-10,20))
lines(meses, t2, col="red", type="b")
lines(meses, t3, col="green", type="b")
legend(10.65, 20.5, c("1º Temp","2º Temp","3º Temp"), col =c("blue","red","green"),
       pch=rep(20,2),cex = 0.7)

setwd("/home/fermat/Área de Trabalho/RStudio/R - Básico")
dev.copy(device = jpeg, file = "exemplo7.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo7.7.pdf", width = 600, paper = "USr")
dev.off()

#########################################################################################

# Aula 09 - Gráfico da Distribuição Normal

rnorm(30)
rnorm(40, mean=3, sd=sqrt(2))

curve(dnorm(x), xlim=c(-5,5), main="Distribuição Normal", ylab="y")

set.seed(5)
x = rnorm(500, mean=3, sd=sqrt(2))
set.seed(5)
y = rnorm(30)

set.seed(5); hist(x,freq = F)
set.seed(5); curve(dnorm(x, mean=3, sd=sqrt(2)),col=2, lty=2, lwd=2, add=TRUE)

curve(dnorm(x, mean=2, sd=sqrt(3)), lwd=2, from=-6, to=17, 
      main="Comparando a distribuição normal com médias diferentes")
curve(dnorm(x, mean=8, sd=sqrt(3)), col=2, lwd=2, add=T)
# Adicionando legenda:
legend('topright', legend=c(expression(theta==2), expression(theta==8)), text.col=c(1,2),
       cex=1)

curve(dnorm(x,mean=4, sd=sqrt(5)), lwd=2, from=-15, to=25, 
      main="Comparando a distribuição normal com variâncias diferentes")
curve(dnorm(x, mean=4, sd=sqrt(15)), col=2, lwd=2, add=T)
legend('topright', legend=c(expression(beta^2==5), expression(beta^2==15)),
       text.col=c(1,2), cex=1.5)

setwd("/home/fermat/Área de Trabalho/RStudio/R - Básico")
dev.copy(device = jpeg, file = "exemplo8.jpeg", width = 600, height = 500, res = 100)
dev.off()
# uma opção para figures em pdf
dev.copy(device = pdf, file = "exemplo8.8.pdf", width = 600, paper = "USr")
dev.off()
