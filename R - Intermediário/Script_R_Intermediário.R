###########################################################################
# Aula 01 - Introdução
# Conteúdo:
# 1.Amostras
# 1.1. strata() amostra estratificada
# 1.2. s.sy() amostra sistemática
# 1.3. sample() amostra aleatória simples
# 1.4. moda - EXTRA

# 2. Medidas de centralidade variabilidade
# 2.1. sd()
# 2.2. quantile()
# 2.3. mean()
# 2.4. var()
# 2.5. median()

# 3. Distribuição Normal
# 3.1. rnorm()
# 3.2. dnorm()
# 3.3. pnorm()
# 3.4. qqnorm() e qqline()
# 3.5. shapiro.test()

# 4. Distribuição Binomial
# 4.1. dbinom() - encontrar a probabilidade
# 4.2. pbinom() - probabilidade cumulativa

# 5. Teste T de Student
# 6. Regressão Linear Simples
# 7. Regressão Linear Múltipla
# 8. Regressão Logistica
# 9. Qui quadrado
# 10. Distribuição de Poison
# 11. Anova
# 12. Outliers
# 13. Índice de Gini

#################################################################################3
# Aula 02 - Amostras: strata()

#install.packages("sampling")
library(sampling)
Orange
summary(Orange)
amostra = strata(Orange,c("Tree"),size = c(4,4,4,4,4), method = "srswor")
amostra
summary(amostra)
data()
ChickWeight
tail(ChickWeight)

summary(ChickWeight)
round(220/578*50,2)
round(120/578*50,2)
round(118/578*50,2)
amostra1 = strata(ChickWeight,c("Diet"),size = c(19.03,10.38,10.38,10.21), method = "srswor")
amostra1
summary(amostra1)

#################################################################################
# Aula 03 - Amostras: sample()

Orange
summary(Orange)

amostra3 = sample(c("a","b","c"), size = 30, replace = T, prob = c(0.5,0.5,0.5))
amostra3
length(amostra3[amostra3=="a"])
length(amostra3[amostra3=="b"])
length(amostra3[amostra3=="c"])

amostra4 = sample(Orange$Tree, size = 15, replace = T)
amostra4

#################################################################################
# Aula 04 - Amostras: s.sy()

#install.packages("TeachingSampling")
library(TeachingSampling)

summary(Orange)
amostra5 = S.SY(35, 4)
amostra5

amostra6 = Orange[amostra5,]
amostra6

##################################################################################
# Aula 05 - Medidas de centralidade variabilidade: sd()
#O desvio padrão é uma medida que expressa o grau de dispersão de um conjunto de dados.
#Ou seja, o desvio padrão indica o quanto um conjunto de dados é uniforme.
#Quanto mais próximo de 0 for o desvio padrão, mais homogêneo são os dados.

Orange
sd(Orange$age)
sd(Orange$circumference)

##################################################################################
# Aula 06 - Medidas de centralidade variabilidade: quantile()
# Quartis são valores que dividem uma amostra de dados em quatro partes iguais. 
#Com eles você pode rapidamente avaliar a dispersão e a tendência central de um conjunto
#de dados, que são etapas importantes na compreensão dos seus dados.
Orange
quantile(Orange$age)
quantile(Orange$circumference)
quantile(Orange$circumference)[1]

##################################################################################
# Aula 07 - Medidas de centralidade variabilidade: mean()
#A Média Aritmética de um conjunto de dados é obtida somando todos os valores e dividindo
#o valor encontrado pelo número de dados desse conjunto.
#É muito utilizada em estatística como uma medida de tendência central.
iris
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)

##################################################################################
# Aula 08 - Medidas de centralidade variabilidade: var()
# Variância é uma medida de dispersão e é usada também para expressar o quanto um 
# conjunto de dados se desvia da média.
#O desvio padrão (DP) é definido como a raiz quadrada da variância (V).

joao = c(63,60,59,55,62)
var(joao)
sd(joao)
sqrt(var(joao))

pedro = c(54,59,60,NA,57,61)
pedro
var(pedro)
var(pedro, na.rm = T)

##################################################################################
# Aula 09 - Medidas de centralidade variabilidade: median()
#A Mediana (Md) representa o valor central de um conjunto de dados. Quando o número 
#elementos de um conjunto é par, a mediana é encontrada pela média dos dois valores 
#centrais. Assim, esses valores são somados e divididos por dois.
iris
median(iris$Sepal.Length)
median(iris$Petal.Length)

##################################################################################
# Aula 10 EXTRA - Moda
cars
tab_freg = table(cars$speed)
tab_freg

tab_freg1 = table(sample(1:250, 500, replace = T))
tab_freg1
max(tab_freg1)
tab_freg1[tab_freg1==max(tab_freg1)]

###################################################################################
# Aula 11 - Distribuição Normal: rnorm()
help("rnorm")
x = round(rnorm(n=100, mean = 50, sd = 2),2)
x
mean(x)
sd(x)
hist(x)
x = round(rnorm(100),2)
x
hist(x)
mean(x)
sd(x)

###################################################################################
# Aula 12 - Distribuição Normal: dnorm()
set.seed(1)
x = round(rnorm(n=100, mean = 50, sd = 2.33),2)
x
hist(x, freq = F)
set.seed(1)
y = dnorm(x)
y
curve(dnorm(x, mean=50, sd=2.33),col=2, lty=2, lwd=2, add=TRUE)

###################################################################################
# Aula 13 - Distribuição Normal: pnorm()
x
sd(x)
mean(x)
# Probabilidade de tirar valor menor que 48
a = pnorm(48,mean(x),sd(x))
a
# Probabilidade de tirar valor maior que 51
b = pnorm(51,mean(x),sd(x))
c = pnorm(51,mean(x),sd(x),lower.tail = F)
c
1 - b
b+c

# Probabilidade de tirar um valor menor que 48 e maior que 51
pnorm(48,mean(x),sd(x)) + pnorm(51,mean(x),sd(x),lower.tail = F)
a+c

# Probabilidade de tirar um valor entre 48 e 51
pnorm(51,mean(x),sd(x)) - pnorm(48,mean(x),sd(x))

1 - pnorm(51,mean(x),sd(x),lower.tail = F) - pnorm(48,mean(x),sd(x))
1-c-a

###################################################################################
# Aula 14 - Distribuição Normal: qqnorm() e qqline()
x
qqnorm(x)
qqline(x)

###################################################################################
# Aula 15 - Distribuição Normal: shapiro.test()
x
shapiro.test(x)

###################################################################################
# Aula 16 - Distribuição Binomial: dbinom() - encontrar a probabilidade

# É a distribuição de probabilidade discreta do número de sucessos numa sequência de
#n tentativas tais que:
#1.Cada tentativa tem exclusivamente como resultado duas possibilidades,
#sucesso ou fracasso (binomial, a que se chama de tentativa de Bernoulli), e;
#2.Cada tentativa é independente das demais, e;
#3.A probabilidade de sucesso p a cada tentativa permanece constante independente das demais,e;
#4.A variável de interesse, ou pretendida, é o número de sucessos k nas n tentativas.

#Seja X uma variável aleatória que contém o número de caras saídas em 12 lançamentos de uma
#moeda honesta. A probabilidade de sair 5 caras em 12 lançamentos. 

dbinom(5,12,0.5)
dbinom(2,12,0.5)

#Qual a probabilidade de lançar um dado não viciado 15 vezes e tirar o número 2, 4 vezes:
dbinom(4,15,1/6)

#Três dados comuns e honestos serão lançados. A probabilidade de que o número 6 seja 
#obtido mais de uma vez é: A probabilidade de que seja obtido 2 vezes mais a probabilidade
#de que seja obtido 3 vezes. Usando a distribuição binomial de probabilidade:
dbinom(2,3,1/6)
dbinom(3,3,1/6)
dbinom(2,3,1/6)+dbinom(3,3,1/6)

sum(dbinom(2:3,3,1/6))

dbinom(10,70,0.2)


###################################################################################
# Aula 17 - Distribuição Binomial: pbinom() - probabilidade cumulativa

dbinom(0,6,0.5)
dbinom(1,6,0.5)
dbinom(2,6,0.5)
dbinom(3,6,0.5)
dbinom(4,6,0.5)
dbinom(5,6,0.5)
dbinom(6,6,0.5)

pbinom(6,6,0.5)

pbinom(2,6,0.5)
dbinom(0,6,0.5)+dbinom(1,6,0.5)+dbinom(2,6,0.5)

sum(dbinom(0:2,6,0.5))

###################################################################################
# Aula 18 - Teste T de Student: compara duas médias e mostra se as diferenças entre essas
#médias são significativas. Em outras palavras, permite que você avalie se essas diferenças
#ocorreram por um mero por acaso ou não.

controle     = c(21, 28, 24, 23, 23, 19, 28, 20, 22, 20, 26, 26)
experimental = c(26, 27, 23, 25, 25, 29, 30, 31, 36, 23, 32, 22)

t.test(controle,experimental)

# paired = FALSE: por default o teste admite que os grupos não sejam pareados
# conf.level = 0.95: por default o teste admite um nível de confiança de 95% (ou α= 5%)
# alternative = two.sided: por default o teste é bicaudal, podemos escolher também “greater” ou “less”

antes  = c(21, 28, 24, 23, 23, 19, 28, 20, 22, 20, 26, 26)
depois = c(26, 27, 23, 25, 25, 29, 30, 31, 36, 23, 32, 22)
t.test(antes,depois,paired = TRUE)

altura = c(152.0, 153.1, 154.6, 157.8, 158.8, 159.6, 161.1, 161.6, 162.7, 163.7, 164.1,
           165.5, 165.8, 168.4, 168.4, 169.1, 169.1, 170.2, 172.4, 172.9, 173.1, 173.3, 
           175.6, 176.9, 179.0)
mean(altura)

t.test(altura, mu=160)

homens = c(15,9,7,13,10,11,14,8,12,5,10,10,6,6,5,13,12,5,12,6)
mulheres = c(13,11,11,10,11,11,15,14,10,9,13,11,9,9,12,9,12,9,15,13,9,15,11,13,11,11,12,11,11,11)
mean(mulheres)-mean(homens)

t.test(homens,mulheres)

###################################################################################
# Aula 19 - Regressão Linear Simples

Orange

cor(Orange$age,Orange$circumference)

modelo = lm(age ~ circumference, data = Orange)

modelo

plot(modelo)

plot(age ~ circumference, data = Orange)
abline(modelo)

modelo$coefficients

predict(modelo, data.frame(circumference=150))

summary(modelo)

modelo$residuals

modelo$fitted.values

plot(modelo$fitted.values, Orange$circumference)

###################################################################################
# Aula 20 - Regressão Linear Múltipla

trees
cor(trees)
cor(trees[1:3])

modelo1 = lm(Volume ~ Girth, data = trees)
modelo1
summary(modelo1)
summary(modelo1)$r.squared # R²
summary(modelo1)$adj.r.squared
plot(Volume ~ Girth, data = trees)
abline(modelo1)
predict(modelo1,data.frame(Girth = 23))

modelo2 = lm(Volume ~ Girth + Height, data = trees)
summary(modelo2)$r.squared
summary(modelo2)$adj.r.squared
predict(modelo2,data.frame(Girth = 23, Height = 60))
###################################################################################
# Aula 21 - Regressão Logistica

aprovacao = read.csv2(file.choose(),header = T, sep = ",", dec = ".")
aprovacao
plot(aprovacao$Freq,aprovacao$Aprovado, pch=20)
summary(aprovacao)
#as.numeric(aprovacao$Freq)
cor(aprovacao$Freq,aprovacao$Aprovado)

modelo3 = glm(Aprovado ~ Freq, data = aprovacao, family = "binomial")
summary(modelo3)

plot(aprovacao$Freq,aprovacao$Aprovado, pch=20)
points(aprovacao$Freq,modelo3$fitted, col="red")

preverAprovacao = read.csv2(file.choose(),header = T, sep = ",",dec = ".")
preverAprovacao

preverAprovacao$Aprovacao = predict(modelo3, newdata = preverAprovacao, type = "response")
preverAprovacao$Aprovacao

###################################################################################
# Aula 22 - Qui quadrado
# Utilizado para testar se a frequência observada na amostra difere significativamente da
# frequência esperada especificada por uma distribuição de probabilidade.  Trabalha com a
# hipótese nula de que as variáveis são estatisticamente independentes. Portanto, considerando
# um nível de 5%, se encontrarmos um p-valor menor que 0,05 temos que as variáveis possuem alguma
# relação e não são independentes.

help("chisq.test")

obs = c(10, 7, 10, 6, 14, 8, 11, 11, 12, 11)
chisq.test(obs)$expected
chisq.test(obs)
#O valor de probabilidade indica que as freqüências observadas não diferem das freqüências esperadas


baixo=c(12,22,9)
alto=c(32,14,6)
lider=cbind(baixo,alto)
lider

# H0: Não existe influência da altura sobre ser um lider ou não
# H1: A altura da pessoa influencia em ser um lider ou não
chisq.test(lider)$expected
chisq.test(lider)$residuals
chisq.test(lider)$stdres
chisq.test(lider)

###################################################################################
# Aula 23 - Distribuição de Poisson
# Distribuição de Poisson é uma distribuição de probabilidade discreta. Expressa a probabilidade 
# de uma série de eventos ocorrem em um período fixo de tempo, área, volume, quadrante, etc. 
# Esta distribuição segue as mesmas premissas da distribuição binomial: i) as tentativas são 
# independentes; ii) a variável aleatória é o número de eventos em cada amostra; e iii) a probabilidade
# é constante em cada intervalo.
help("Poisson")

# Ex. A média do número de aprovados de uma turma de Data Mining é de  23 alunos em um semestre.
# Qual a probabilidade de serem aprovados 17 alunos?

dpois(17,lambda = 23)
# e 25 alunos?
dpois(25,lambda=23)

plot(dpois(1:40,23),type = "h")

# A probabilidade acumulada de 19 ou menos alunos serem aprovados?
ppois(19,lambda = 23)
# Aprobabilidade acumulada de mais de 19 alunos serem aprovados?
ppois(19,lambda = 23, lower.tail = F)
ppois(19,lambda = 23, lower.tail = F) + ppois(19,lambda = 23)

plot(ppois(1:40,23),type = "s")

# Qual o número de aprovados(x) se a procentagem acumulada for de 0.78?
qpois(.78,lambda = 23)
qpois(.434567,23)


x = rpois(70,lambda = 23)
x

###################################################################################
# Aula 24 - Anova
# A Análise de Variância (ANOVA) trata-se de um método estatı́stico que permite realizar comparações
# simultâneas entre duas ou mais médias, ou seja, permite testar hipóteses sobre médias de distintas
# populações.

help("aov")

dados = read.csv(file.choose(), header = TRUE, sep = ",", dec = ".")

summary(dados)
View(dados)

teste1 = aov(vento ~ area,dados)
teste1
summary(teste1)

teste2 = TukeyHSD(teste1)
teste2

PlantGrowth
teste3 = aov(weight ~ group, PlantGrowth)
summary(teste3)

teste4 = TukeyHSD(teste3)
teste4

# Fonte1: http://lite.acad.univali.br/rcurso/anova/index.html
# Fonte2: http://www.sthda.com/english/wiki/one-way-anova-test-in-r


###########################################################################################
# Aula 25 - Outliers
#install.packages("outliers")
#library(outliers)

cars
cars1 = cars[1:30,]
cars1
cars_outliers = data.frame(speed=c(19,19,20,20,20,1), dist=c(190, 186, 210, 220, 218,195))
cars2 = rbind(cars1, cars_outliers) 
cars2

boxplot(cars2)
boxplot(cars2$speed)
boxplot(cars2$dist)
boxplot.stats(cars2$speed)$out
boxplot.stats(cars2$dist)$out

outlier(cars2$speed)
outlier(cars2$dist)
help("outlier")

par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), main="Com Outliers",
     xlab="speed", ylab="dist", pch=20, col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Sem Outliers",
     xlab="speed", ylab="dist", pch=20, col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

# Média, mediana, moda
par(mfrow=c(1, 1))

x = cars2$dist
qnt = quantile(x, probs=c(.25, .75), na.rm = T)
caps = quantile(x, probs=c(.05, .95), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = caps[1]
x[x > (qnt[2] + H)] = caps[2]
x
boxplot(x)

y = cars2$speed
qnt = quantile(y, probs=c(.25, .75), na.rm = T)
caps = quantile(y, probs=c(.05, .95), na.rm = T)
H = 1.5 * IQR(y, na.rm = T)
y[y < (qnt[1] - H)] = caps[1]
y[y > (qnt[2] + H)] = caps[2]
y
boxplot(y)

########################################################################################################
# 13. Índice de Gini
# O Coeficiente de Gini é uma medida de desigualdade desenvolvida pelo estatístico italiano Corrado Gini,
# e publicada no documento "Variabilità e mutabilità" ("Variabilidade e mutabilidade" em italiano),
# em 1912. Pode ser usado para qualquer distribuição embora seja comumente utilizado para medir a
# desigualdade de distribuição de renda.

# O Coeficiente de Gini consiste em um número entre 0 e 1, onde 0 corresponde à completa igualdade (no
# caso do rendimento, por exemplo, toda a população recebe o mesmo salário) e 1 corresponde à completa
# desigualdade (onde uma pessoa recebe todo o rendimento e as demais nada recebem).

# O índice de Gini é o coeficiente expresso em pontos percentuais (é igual ao coeficiente multiplicado 
# por 100).

# O Coeficiente de Gini é amplamente utilizado em diversos campos de estudo, como a sociologia, economia,
# ciências da saúde, ecologia, engenharia e agricultura. Por exemplo, em ciências sociais e economia,
# além do coeficiente de Gini relacionado à renda, estudiosos publicaram coeficientes relacionados à
# educação e oportunidades.

#install.packages("ineq")
library(ineq)

AirPassengers

indice = ineq(AirPassengers,type="Gini")
indice

plot(Lc(AirPassengers))

plot(Lc(AirPassengers),col="darkred",lwd=2)

Gini(AirPassengers)

plot(Gini(AirPassengers))

#install.packages("reldist")
library(reldist)

gini(AirPassengers)

cidade = c ("Bagé", "Caxias do Sul", "Pelotas", "Caçapava do Sul", "Candiota", "Dom Pedrito",
             "Rosário do Sul", "São Gadriel", "Hulha Negra", "Candiota") 
renda = sample ( 1: 100000, 100, replace = TRUE) 
cidades = data.frame (cidade, renda)

library(ggplot2)
ggplot (cidades, aes (renda)) + stat_density (geom = "path", position = "identity") + 
  facet_wrap (~ cidade, ncol = 2)

Gini(cidades[which(cidades$cidade == "Bagé"), ]$renda)

indices_gini = aggregate(renda ~ cidade, data = cidades, FUN = "gini")

names(indices_gini) = c("cidade", "índice_gini")

indices_gini
