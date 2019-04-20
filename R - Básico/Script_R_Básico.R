#Aula 01 - Script
a = 4
b = 5
c = a * b
c

#Aula 02 - Comentário
#class(c)                                                            
#c = "Selva, Brasil!"
# Este é um exemplo de comentário no R
f = 0
#f = c / 2
f
# ctl + enter = executa
# ctl + l = limpa

#Aula 03 - Imagem do R

save.image()

save.image("Script.RData")

load(file="Script.RData")

#Aula 04 - Pacotes

installed.packages()

install.packages("ggplot2")

install.packages("ggplot2", dependencies = TRUE)

library(ggplot2)

detach("package:ggplot2")

remove.packages("ggplot2")


#Aula 05 - Tipos de Dados
#character, numeric, integer
#Atribuição implícita 

a = 10
a
a <- 5
a
7 -> a
a
class(a)

is.numeric(a)
is.array(a)

b = "Estatística: "
b

c = Estatistica

d = "R"

e = paste(b,d)
e

var1 = 10L
var2 = as.integer(10)
class(var1); class(var2)

#as.integer, as.numeric, as.character, as.vector ...

x = 5345L
y = 45667778L
med = y / x
med

objects()

#rm remove um objeto
rm(med)
med # Erro

rm(list=objects())


#Aula 06 - Mais Dicas
#Verificando diretório atual
getwd()

#Alterando diretorio
setwd("/home/fermat/Área de Trabalho/RStudio")

#Ler arquivos
dir()

#Opções
getOption("OutDec")
5/4
options(OutDec = ",")
getOption("OutDec")
5/4
options(OutDec = ".")

help(options)


#Aula 07 - Operadores
#Aritméticos: 
# + soma
2 + 4
# - subtração
2 - 4
# * multiplicação
2 * 4
# / divisão
2 / 4
# ^ potenciação
2^4

x = FALSE
x
x = TRUE
x
# Lógicos
# | ou
2 | 4
# & e
2 & 4
# isTRUE(a), testa se é verdadeiro
x = 3
isTRUE(x == 4)
# != não igual
2 != 4
2 != 2
#!x Não é
!x
# < menor que
1 < 2
2 < 1
# > maior que
1 > 2
2 > 1
# <= menor ou igual que
2 <= 7
# >= maior ou igual que
3 >= 4
# == exatamente igual
2 == 2
2 == 8

#Diversos
# %% modo
9 %% 2
8 %% 2
9 %% 3
# %/% divisão de inteiros
15 %/% 4
#pi
pi
# abs() valor absoluto, recebe um valor como parâmetro
abs(-17)
# log, logaritomo, recebe um valor e a base como parâmetros
log(5,base = 3)
var1 = 20
log(var1, base = 10)
# factorial, fatorial, recece um valor como parâmentro
factorial(4)
factorial(var1)
# sqrt, raiz quadrada
sqrt(36)
sqrt(var1)
# sin, seno
sin(45)
# cos, cosseno
cos(60)
# tan, tangente
tan(90)






# Aula 08 - Número complexos
z = -3 + 5i
# verifica se z é um número complexo
class(z)
is.complex(z)
Re(z)	#Parte real de z
Im(z)	#Parte imaginária de z
Mod(z)	#Módulo de z
Arg(z)	#Argumento de z
Conj(z)	#Complexo conjugado de z

# Aula 09 - Fórmula
# y = 2x + 1
teste = y ~ x + z + w
teste = y ~.

####################################################
# Aula 10 - Datas
dia = as.Date("2019-02-26")
dia
dia2 = as.Date("2019-02-09")
dia2
dif = dia - dia2
dif
#Formatação de datas
######################################
# d #          Dia do mês           #   
# a #    Dia da semana abreviado    #
# A #   Dia da semana por extenso   #
# m # Representação numérica do mês #
# b #         Mês abreviado         #
# B #        Mês por extenso        #
# y #       Ano com 2 digítos       #
# Y #    Ano com quatro digitos     #
#####################################

format(dia, format="%d/%m/%Y")
format(dia2, format("%A, %d de %B de %Y"))


###################################################

# Aula 11 - Estruturas
#######################################################################
#      Vetores     # Um conjunto simples de valoreos de um mesmo tipo #   
#######################################################################
#     Matrizes     # Conjunto de linhas e colunas(cada coluna deve ter# 
#                  # o mesmo tipo de dado)                            #
#######################################################################
#      Arrays      # Podem ter mais de uma dimensão                   #
#######################################################################
#      Listas      # Permite agrupar lista de diferentes objetos, que #
#                  # podem ter diferentes tipo e tamanhos             #
#######################################################################
#    Data Frame    # Semenhante a uma tabela de um banco de dados     #
#######################################################################
# Séries Temporais # Estruturas de séries de dados temporais          #
#######################################################################
#     Fatores      # Estruturas para variáveis categóricas            #
#######################################################################

# Aula 12 - Vetores

vet = c("essas são", "algumas strings")
vet
vet1 = c(1, 2.5, 4.5) #DOUBLE
vet1
# Com o sufixo L temos números inteiros em vez de double
vet2 = c(1L, 6L, 10L)
vet2
# Use TRUE ou FALSE (T ou F) para vetores lógicos
vet3 = c(TRUE, FALSE, T, F)
vet3

typeof(vet)
typeof(vet3)

vet4 = seq(from=1,by=7,length=100)
vet4

#Acessando um valor pelo indíce
vet4[5]
#Acessando vários valores
vet4[c(2,5,7,10)]
#Acessando valores por um intervalo
vet4[c(10:34)]
#Filtrando busca
vet4[vet4 > 100 & vet4 < 333]
#Removendo valores
vet4[-1]
vet4[-100]
vet4[c(-100, -1)]

#Maior elemento
max(vet4)
#Menor elemento
min(vet4)
#Tamanho vetor
length(vet4)

#Ordenando vetor
vet4 = sort(vet4)
sort(vet3)
#Ordem decrescente
vet4 = sort(vet4, decreasing = T)

#Ordenando pelo indice
vet4 = order(vet4)

vet4
#Outras coisas legais
vet5 = seq(from=1,by=7,length=10)
vet6 = seq(from=1,by=9,length=10)
vet5 + vet6
vet7 = vet5 + vet6
vet7

vet7 + 3
vet8 = vet7 + 3
vet8

vet9 = c(9,NA,8,NA,7)
vet10 = c(6,5,4,3,2)
vet10 + vet9

vet9[2] = 0
vet9[4] = 0
vet9

#Omitindo valores ocultos
vet11 = c(19,NA,18,NA,17)
sd(vet11)
#na.rm = T
sd(vet11,na.rm = T)

#Coerção
#####################################################################
# Aula 13 - Arrays e Matrizes
#Carrege o conjuto nativo do R attitude
attitude

# Acessando valores pelos índices -> attitude[linha,coluna]
attitude[1,5]
attitude[3,4]
# Acessando conjunto de linhas e colunas
attitude[1:3,c(1,5:6)]
attitude[c(2,4,6), c(1,3,5)]
# Omitindo os indices
attitude[,2]
attitude[3,]
# Acessando pelos nomes
attitude["1","rating"]
# Criando uma nova matriz/array
mat = attitude[c(2,4,6), c(1,3,5)]
mat
mat1 = matrix(c("a","b","c","d","e","f"),nrow = 3, ncol = 2, byrow = T)
mat1
mat2 = matrix(c("a","b","c","d","e","f"),nrow = 3, ncol = 2, byrow = F)
mat2
# Nomeando linhas/colunas
dim(mat)
dimnames(mat1) = list(c("L1","L2","L3"),c("C1","C2"))
mat1
mat1["L2","C1"]

mat3 = matrix(c("a","b","c","d","e","f"),nrow = 3, ncol = 2, byrow = F, 
              dimnames=list(c("Linha1","Linha2","Linha3"),c("Coluna1","Coluna2")))
mat3              

# Alterando valores pelo índice
mat3[1,2]="G"
mat3
mat3["Linha3","Coluna1"] = "H"
mat3
################################################################################
# Aula 14 - Listas
mat = matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3, ncol = 3)
mat
vet = c("a","b","c")
vet
x = 8L
x

lista1 = list(mat,vet,x)
lista1

lista2 = list(attitude,mat,vet,x,"teste",15, lista1)
lista2

# Acessando a lista1
lista1[1]
lista1[2]
lista1[3]

# Acessando a lista2
lista2[1]
lista2[2]
lista2[3]
lista2[4]
lista2[5]
lista2[6]
lista2[7]

# Testados os tipos de uma lista
class(lista1[[1]])
class(lista2[[6]])

# Acessando valores da lista
lista1[[1]][2,3]
lista1[[2]][3]
lista1[[3]][1]

lista2[[7]][[1]][3,2]
lista2[[7]][[2]][1]

# Alterando valores em uma lista
lista2[[7]][[1]][3,2] = 15
lista2[[7]][[2]][1] = "x"

lista2[[7]][[1]][3,2]
lista2[[7]][[2]][1]

# Retirando valores da lista
lista2[[7]][[1]][- 3,2]
lista2[[7]][[2]][-1]
lista2[[7]][[2]] = lista2[[7]][[2]][-1]
lista2[[7]][[2]][1] 
lista2[[7]][[2]]

# Adicionando elementos
length(lista2)
length(lista1)
lista1
lista1[[4]] = c(1,2,3,4,5) 

lista1 
length(lista1[[4]])
lista1[[4]][[6]] = 6
lista1

#Renomeando indices de uma lista
names(lista1) = c("Matriz1","Vetor1","Valo1","Vetor2")
lista1
lista1$Vetor2
lista1$Matriz1[3,2]

#######################################################################
# Aula 15 - Séries Temporais
#Criando uma série

minhaSerie = ts(c(100:231), start=c(2000,1), end=c(2010,12), frequency = 12)
minhaSerie
class(minhaSerie)

plot.ts(minhaSerie,main="Exemplo1")

AirPassengers
plot.ts(AirPassengers,main="Exemplo2")

###############################################################
# Aula 16 - Fatores

fator = factor(c("aventura", "terror", "comédia", "drama"))
fator

levels(fator)


fator1 = factor(c("2", "3", "1"), ordered = TRUE)
fator1
as.numeric(fator1)
levels(fator1)
class(fator1)

##############################################################
# Aula 17 - Data Frame
#Data frames são listas em que todos os elementos têm o mesmo comprimento
x=5:1
y = c("e","d","c","b","a")
w = F
df = data.frame(x,y,w)
df

df1 = data.frame(x=1:5, y = c("a","b","c","d","e"), w = F)
df1

# Acessando pelo índice
df1[2,1]
df1[3,3]

# Renomeando
# colnames(df1) = c("C1","C2","C3")
rownames(df1) = c("L1","L2","L3","L4","L5")
df1
# Acessando pelo nome
df1["L5","y"]

df1$x
df1$y
df1$w
# Verificando o suporte de diferentes classes ou tipos
class(df1$x)
class(df1$y)
class(df1$w)


df2 = data.frame(x=1:10, y = c("a","b","c","d","e","f","g","h","i","j"), w = T,
                 row.names=c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10"))
df2

# Comdinado data frames
df3 = rbind(df1, df2); #rbind(df2,df1)
df3

df4 = cbind(df1, df2); #cbind(df2,df1)
df4

# Alterando de Factor para character
str(df1)

df5 = data.frame(x=1:5, y = c("a","b","c","d","e"), w = F, stringsAsFactors = F)
str(df5)

# Alterando valores pela função edit e fix
df5
df6 = edit(df5)
df6
df6[6,2]="f"
df6
df6["6","w"] = FALSE
df6

df5
fix(df5)
df5
############################################################################
# Aula 18 - Attach, Detach, With

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))
corrida
corrida$Pace
sum(Pace)
sum(corrida$Pace)

# Attach
attach(corrida)
sum(Pace)
sum(Pace)/10

# Detach
detach(corrida)
sum(Pace)

# With
with(corrida,sum(Pace))
with(corrida,sum(Pace))/10

############################################################
# Aula 19 - Sequências
seq1 = c(10:55)
seq1

seq2 = seq(from=10,to=100,by=5)
seq2
help(seq)
seq(1, 9, by = pi)



#######################################################################
# Aula 20 - Tamanho
length(seq1)
length(seq2)

length(seq1) = length(seq2)
seq1



#######################################################################
# Aula 21 - Scan
# OBS: + eficiente se estiver trabalhando direto no console
#vet1 = scan()
#1 2 3
#4 5

#vet1

#vet2 = scan(what = "boolean")
#1 2 3
#4 5 6

#vet2


##################################################################################
# Aula 22 - Conversão de tipos
#as.numeric()
#as.character()
#as.vector()
#as.matrix()
#as.data.frame()
#as.Date()
#as.factor()
#as.list()

x = 7
x
class(x)
x = as.character(x)
x
class(x)

data1 = "2019-03-02"
data1
class(data1)
data1 = as.Date(data1)
data1
class(data1)

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))
corrida
class(corrida)
corrida = as.list(corrida)
corrida
class(corrida)


#########################################################################################

# Aula 23 - Importação de Dados


conjunto = read.table("/home/fermat/Rodrigo/TADS/TCC/Dados utilizados Rodrigo.csv", sep = ",", dec = ".", header = T)
conjunto

conjunto1 = read.table(file.choose(),sep = ",", dec = ".", header = T)
conjunto1

########################################################
#   header  # Indica de possui cabeçalho, tipo boolean #
#    sep    # Separador de dados                       #
# row.names # Nomes para as linhas                     #
# col.names # Nomes para as colunas                    #
#    dec    # Separador decimal                        #
########################################################

list.files("/home/fermat/Rodrigo/TADS/TCC")

help("read.csv2")


########################################################################################

# Aula 24 - Lendo e salvando em Disco
getwd()
setwd("/home/fermat/Área de Trabalho/")
#setwd("/home/fermat")

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))
corrida

save(corrida, file = "corrida")

rm(corrida)
corrida

load("corrida")
corrida

vet = c(1:40)
vet

save(vet, file = "/home/fermat/Área de Trabalho/RStudio/vetor")
rm(vet)
vet
load("/home/fermat/Área de Trabalho/RStudio/vetor")
vet

save(corrida,vet, file = "arquivos")

########################################################################################

# Aula 25 - Head e Tail

iris

head(iris)
tail(iris)
head(iris, 10L)
tail(iris, 13L)

##########################################################################################

# Aula 26 - Sumarização

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))
summary(corrida)

conjunto = read.table("/home/fermat/Rodrigo/TADS/TCC/Dados utilizados Rodrigo.csv", sep = ",", dec = ".", header = T)
conjunto

summary(conjunto)

##############################################################################################
# Aula 27 - Nomes de Linhas e Colunas

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))

conjunto = read.table("/home/fermat/Rodrigo/TADS/TCC/Dados utilizados Rodrigo.csv", sep = ",", dec = ".", header = T)
corrida
conjunto

rownames(corrida)
colnames(corrida)

rownames(conjunto)
colnames(conjunto)

#########################################################################################
# Aula 28 - Funções Cumulativas

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))


cumsum(corrida)
class(corrida)

cumsum(AirPassengers)
class(AirPassengers)
AirPassengers

cummax(corrida$Pace)
cummin(corrida)
cumprod(AirPassengers)
factorial(10)
corrida

########################################################################################
# Aula 29 - Aplicações de Funções

corrida

apply(corrida, 1, sd)
apply(corrida, 2, sd)

apply(corrida,1, median)
apply(corrida,2, median)

########################################################################################
# Aula 30 - Tabelas de Contingência

infert
length(infert$education)
tail(infert)
table(infert$education,infert$spontaneous)

#########################################################################################
# Aula 31 - Números Aleatórios
sample(20)

conjunto = sample(1:100, size = 6, replace = T)
conjunto

set.seed(1)
sample(1:100, size = 6, replace = T)
set.seed(1)
sample(1:100, size = 6, replace = T)

ruas = sample(30:1000, size = 300)
ruas
length(ruas)

amostra = sample(1:300, 15)
amostra
amostragem = ruas[c(amostra)]
amostragem
ruas[79]



###################################################################################
# Aula 32 - Estruturas de Programação

#############################################################
# Estruturas Condicionais  # if - else                      #
#############################################################
# Estruturas de Repetição  # repet, while, for, break, next #
#############################################################
# Criando Funções                                           #
#############################################################

####################################################################################
# Aula 33 - Estruturas Condicionais - if, else
a = 4

b = 6

if (a > b){ 

    print(paste("O maior numero é",a,", e a diferença é",a-b))

}else{
  
  c = a+b
  print(paste("O maior numero é",b,", e a soma é",c))

}

# ifelse (condição , se verdadeiro, se falso)

x = c(6:-4)
x
sqrt(x)
sqrt(ifelse(x >= 0, x, NA))


y=1:10
z=10:20
y
z
if (length(y)==length(z) && y[1]!=0) {
  c = y*z
  c
}else{ 
  c = 1:100
  c
}

####################################################################################
# Aula 34 - Estruturas de Repetição  # repeat, while, for, break, next

#for(variável in sequência)
x = 0

for (x in 1:10) {
  print(x)
}

corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))
corrida

for(i in corrida[,2]) {
  print(i)
}

vetorPace = 0
posicao=0

for(i in corrida$Pace){
  vetorPace[posicao+1] = i
  posicao = posicao + 1
}
vetorPace

# repeat
y = 1
repeat {
  
  print (y)
  
  y=y+1
  
  if (y > 10) break()
}

z = 0
repeat{
  if(z==11) break() else
    z = z + 1
    print(z)
}
# while

a = 1

while (a <= 10 ){
  
  print (a)
  
  a = a + 1
}

##################################################################################
# Aula 35 - Funções no R
corrida = data.frame(Pace = c(5.55,4.59,5.21,5.05,4.45,5.17,5.01,5.34,5.45,5.56)
                     ,Posicao = c(9,2,6,4,1,5,3,7,8,10), row.names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))
corrida

funcaoA = function(a){
  for (i in 1:a) {
    b = i^2
    print(b)
    }
}
funcaoA(6)

funcaoB = function(){
  for(i in 1:5) {
    print(i^2)
  }
}
funcaoB()

funcaoC = function(a,b,c) {
  resultado = a * b + c
  print(resultado)
}

funcaoC(5,3,11)

funcaoC(a = 11, b = 5, c = 3)

funcaoD = function(corrida1, vetorPace1, posicao1){
  for (i in corrida1$Pace){
    vetorPace1[posicao1+1] = i
    posicao1 = posicao1 + 1
  }
  #print(vetorPace1)
  return(vetorPace1)
}
vetorPace = 0
posicao = 0
funcaoD(corrida, vetorPace, posicao)

help("function")
