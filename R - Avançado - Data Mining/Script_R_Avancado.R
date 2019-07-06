#####################################################################################
# Aula 01 - Introdução
# Conteúdo:
# 1. Regras de Associação:
# 1.1. Apriori
# 1.2. ECLAT

# 2. Agrupamentos
# 2.1. K-means
# 2.2. Fuzzy c-means
# 2.3. K-medoids
# 2.4. DBSCAN
# 2.5. Hier?rquico

# 3. Classificação:
# 3.1. Neive Bayes
# 3.2. Árvores de Decisão - Rpart
# 3.3. Regras - (PRISM, OneR, CN2)
# 3.4. Aprendizagem Baseado em Instâncias - KNN
# 3.5. M?quina de Vetores de Suporte - SVM
# 3.6. Regressão Logistica
# 3.7. RNA

# 4. Regressão:
# 4.1. Regressão linear (simples e múltipla)
# 4.2. Regressão polinomial
# 4.3. Regressão com árvores de decissão e random forest
# 4.4. Regressão com vetores de suporte
# 4.5. Regressão com redes neurais artificiais

# 5. Séries Temporais
# 6. Mineração de Textos
# 7. Redes Sociais e Grafos

####################################################################################################
# 1. Regras de Associação:
# 1.1. Apriori

#install.packages("arules")
library(arules)

base = read.transactions(file.choose(),header = F,sep = ",",rm.duplicates = T)
summary(base)

itemFrequencyPlot(base)

itemFrequencyPlot(base,top=5)

regras = apriori(base, parameter = list(sup=0.1, conf=0.8))

inspect(regras)

base1 = read.transactions(file.choose(),header = F,sep = ",",rm.duplicates = T)
summary(base1)

itemFrequencyPlot(base1)
itemFrequencyPlot(base1,top=10)

regras1 = apriori(base1, parameter = list(sup=0.001, conf=0.5))
inspect(regras1)
4 * 3 
12 / 7501
inspect(sort(regras1, by="confidence")[30:60])

#################################################################################################
# 1. Regras de Associa??o:
# 1.2. ECLAT

base = read.transactions("GroceryStoreDataSet.csv", header = F, sep = ",", rm.duplicates = T)

image(base)

itemSet = eclat(base, parameter = list(support=0.3))
inspect(itemSet)

itemSet= eclat(base, parameter = list(support=0.2 ,maxlen=2))
inspect(itemSet)

itemSet= eclat(base, parameter = list(support=0.15 ,minlen=2))
inspect(itemSet)

inspect(sort(itemSet))



#########################################################################################################
# 2. Agrupamentos
# 2.1. K-means
library(readr)
vinho = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
View(vinho)
structure(vinho)

#Alcohol
#Malic acid
#Ash
#Alcalinity of ash  
#Magnesium
#Total phenols
#Flavanoids
#Nonflavanoid phenols
#Proanthocyanins
#Color intensity
#Hue
#OD280/OD315 of diluted wines
#Proline            



colnames(vinho)=c('Tipo','Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols', 
                  'Flavanoids','Nonflavanoid_phenols','Proantocianinas','Proanthocyanins',
                  'Hue','OD280/OD315','Proline') 


View(vinho)

#install.packages("tidyverse")
library(tidyverse)
#install.packages("corrplot")
library(corrplot)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("GGally")
library(GGally)
#install.packages("knitr")
library(knitr)

str(vinho)

vinho1 = vinho[,-1]
vinho1

#Histograma para cada atributo
vinho1 %>% gather(Attributes, value, 1:13) %>% ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) + facet_wrap(~Attributes, scales="free_x") +
  labs(x="Valores", y="Frequencia",title="Atributos do Conjuto Vinho - Histogramas") + theme_bw()

#Densidade para cada atributo
vinho1 %>% gather(Attributes, value, 1:13) %>% ggplot(aes(x=value, fill=Attributes)) + 
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) + facet_wrap(~Attributes, scales="free_x") +
  labs(x="Valores", y="Densidade",title="Atributos do Conjuto Vinho - Densidade") +
  theme_bw()

#Boxplot para cada atrituto  
vinho1 %>% gather(Attributes, values, c(1:4, 6:12)) %>% ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) + labs(title="Atributos do Conjuto Vinho - Boxplots") + theme_bw() +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank()) + ylim(0, 35) + coord_flip()

#Matriz de Correla??o 
corrplot(cor(vinho1), type="upper", method="ellipse", tl.cex=0.9)

#Rela??o entre Phenols e Flavanoids
ggplot(vinho1, aes(x=Total_phenols, y=Flavanoids)) + geom_point() + geom_smooth(method="lm", se=FALSE) +
  labs(title="Atributos dos Vinhos", subtitle="Rela??o entre Phenols e Flavanoids") + theme_bw()

#Normaliza??o
vinhoNorm = as.data.frame(scale(vinho1))
#rm(vinhosNorm)
#Conjuto original
c1 = ggplot(vinho1, aes(x=Alcohol, y=Malic_acid)) + geom_point() +
  labs(title="Conjunto Original") +
  theme_bw()

#Conjunto Normalizado 
c2 = ggplot(vinhoNorm, aes(x=Alcohol, y=Malic_acid)) +
  geom_point() +
  labs(title="Conjunto Normalizado") +
  theme_bw()

# Subplot
grid.arrange(c1, c2, ncol=2)


# Execu??o k-means com k=2
set.seed(1234)
vinho_k2 = kmeans(vinhoNorm, centers=2)


#A fun??o kmeans () retorna um objeto da classe "kmeans" com informa??es sobre a parti??o:
#cluster. Um vetor de inteiros indicando o cluster ao qual cada ponto ? alocado.
#centers. Uma matriz de centros de cluster.
#size. O n?mero de pontos em cada cluster.

#Cluster ao qual cada ponto ? alocado
vinho_k2$cluster
# Centros de clusters
vinho_k2$centers
# Pontos por clusters
vinho_k2$size


#Al?m disso, a fun??o kmeans () retorna algumas propor??es que nos informam qu?o compacto
#? um cluster e qu?o diferentes s?o os v?rios clusters entre si.

#betweenss. A soma entre os quadrados dos aglomerados. Em uma segmenta??o ?tima, espera-se que
#essa propor??o seja a mais alta poss?vel, j? que gostar?amos de ter clusters heterog?neos.

#withinss. Vetor da soma de quadrados dentro do cluster, um componente por cluster. 
#Em uma segmenta??o ideal, espera-se que essa propor??o seja a menor poss?vel para cada cluster,
#desde que n?s gostar?amos de ter homogeneidade dentro dos clusters.

#tot.withinss. Soma total de quadrados dentro do cluster.

#tots. A soma total de quadrados.

# Between-cluster
vinho_k2$betweenss

# Within-cluster
vinho_k2$withinss

# Total within-cluster soma dos quadrados 
vinho_k2$tot.withinss

# Total soma dos quadrados
vinho_k2$totss

#Quantos clusters usar?
#Para estudar graficamente qual valor de k nos d? a melhor parti??o, podemos plotar
#betweenss e tot.withinss e escolhas de k


bts = numeric()
tws = numeric()

#algoritmo para diferentes valores de k 
set.seed(1234)

for(i in 1:10){
  # Para cada k, calcula betweenss e tot.withinss
  bts[i] = kmeans(vinhoNorm, centers=i)$betweenss
  tws[i] = kmeans(vinhoNorm, centers=i)$tot.withinss
}

# Soma entre os quadrados pelas escolhas de k
c3=qplot(1:10, bts, geom=c("point", "line"), 
         xlab="N?mero de clusters", ylab="Soma entre os quadrados") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Total within-cluster soma dos quadrados pelas escolhas k
c4=qplot(1:10, tws, geom=c("point", "line"),
         xlab="Numero de clusters", ylab="Soma total de quadrados dentro do cluster") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
grid.arrange(c3, c4, ncol=2)

#Qual ? o valor ideal para k? Deve-se escolher um n?mero de clusters para que a adi??o de
#outro cluster n?o forne?a uma parti??o muito melhor dos dados. Em algum momento, o ganho
#cair?, dando um ?ngulo no gr?fico (crit?rio de cotovelo). O n?mero de clusters ? 
#escolhido neste momento. No nosso caso, 3 ? o valor apropriado para k.

#Executando k-means com k=3
set.seed(1234)

vinho_k3 = kmeans(vinhoNorm, centers=3)

previsao = vinho_k3$cluster
previsao
# M?dia dos valores para cada cluster
aggregate(vinho1, by=list(previsao), mean)



#agrupamento 
ggpairs(cbind(vinho1, Cluster=as.factor(previsao)),
        columns=1:13, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()

plot(vinhoNorm[,1:13],col=previsao)

plot(previsao)

library(cluster)
clusplot(vinhoNorm,previsao, color = T, lines = F, labels = 4)
#help("clusplot")
table(vinho$Tipo,previsao)

erro = 6 / 177
erro
acerto = 1 - erro
acerto

##########################################################################################################
# 2. Agrupamentos
# 2.2. Fuzzy c-means
library(readr)
vinho = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")

colnames(vinho)=c('Tipo','Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols', 
                  'Flavanoids','Nonflavanoid_phenols','Proantocianinas','Proanthocyanins',
                  'Hue','OD280/OD315','Proline')
vinho1 = vinho[,-1]
vinhoNorm = as.data.frame(scale(vinho1))

library(e1071)
vinho_cm = cmeans(vinhoNorm,3,dist = 'euclidean',method = 'cmeans')

vinho_cm$size
vinho_cm$centers

previsao = vinho_cm$cluster
previsao

vinho_cm$membership

clusplot(vinhoNorm,previsao, color = T, lines = F, labels = 4)

table(vinho$Tipo,previsao)

erro = 116 / 177
erro
acerto = 1 - erro
acerto

##########################################################################################################
# 2. Agrupamentos
# 2.3. K-medoids
library(readr)
vinho = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")

colnames(vinho)=c('Tipo','Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols', 
                  'Flavanoids','Nonflavanoid_phenols','Proantocianinas','Proanthocyanins',
                  'Hue','OD280/OD315','Proline')
vinho1 = vinho[,-1]
vinhoNorm = as.data.frame(scale(vinho1))

#install.packages("fpc")
library(fpc)

vinho_km = pam(vinhoNorm,3)

clusplot(vinho_km,color=T,labels=4)
vinho_km$silinfo
vinho_km$medoids
vinho_km$clusinfo

previsao = vinho_km$clustering
previsao

table(vinho$Tipo,previsao)

erro = 17 / 177
erro
acerto = 1 - erro
acerto

##########################################################################################################
# 2. Agrupamentos
# 2.4. DBSCAN
#Density-Based Spatial Clustering of Applications with Noise
#Baseado em densidade, agrupando os pontos similares no mesmo espa?o
#N?o ? necess?rio especificar o n?mero de clusters
#Em geral apresenta melhores resultados que o k-means
#Mais r?pido que o k-means
#Tenta encontrar os pontos que s?o separados por uma dist?ncia n?o maior do que um limiar
#(threshold distance)
#Encontra padr?es n?o lineares
#Robusto contra outliers
#O resultado pode ser mais consistente que o k-means porque a inicializa??o dos "centroides"
#n?o afeta tanto o algoritmo
#Dependendo da inicializa??o, um ponto pode pertencer aos cluster dependendo da ordem
#Pode ser dif?cil encontrar um bom valor para o par?metro da dist?ncia
library(readr)
vinho = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")

colnames(vinho)=c('Tipo','Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols', 
                  'Flavanoids','Nonflavanoid_phenols','Proantocianinas','Proanthocyanins',
                  'Hue','OD280/OD315','Proline')
vinho1 = vinho[,-1]
vinhoNorm = as.data.frame(scale(vinho1))

#install.packages("dbscan")
library(dbscan)

vinho_db = dbscan::dbscan(vinhoNorm,eps=2,minPts=4)
previsao = vinho_db$cluster
previsao
unique(previsao)
table(previsao)

plot(vinhoNorm[1:2],col=previsao)

flor =  dbscan(iris[,1:4],eps = 0.6,MinPts = 4)
unique(flor$cluster)

plot(iris[1:4],col=flor$cluster)



###########################################################################################################
# 2. Agrupamentos
# 2.5. Hier?rquico
# N?o ? necess?rio especificar o n?mero de clusters, tamb?m ? criada uma estrutura em 
#formato de ?rvore que indica o n?mero de clusters. Possui uma abordagem aglomerativa:
#cada registro pertence ao seu pr?prio cluster e pares de clusters s?o unidos. Os resultados
#podem ser apresentados em um dendograma

library(readr)
vinho = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")

colnames(vinho)=c('Tipo','Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols', 
                  'Flavanoids','Nonflavanoid_phenols','Proantocianinas','Proanthocyanins',
                  'Hue','OD280/OD315','Proline')
vinho1 = vinho[,-1]
vinhoNorm = as.data.frame(scale(vinho1))

vinho_hc = hclust(d = dist(vinhoNorm, method = 'euclidean'), method = 'ward.D')

plot(vinho_hc)

previsao = cutree(vinho_hc,3)
previsao
plot(vinhoNorm,col=previsao)

library(cluster)
clusplot(vinhoNorm,previsao, color = T, lines = F, labels = 4)

table(vinho$Tipo,previsao)
erro = 13 / 177
erro
acerto = 1 - erro
acerto

##########################################################################################
# 3. Classificação
# 3.1. Neive Bayes
library(readr)
vinho = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")

colnames(vinho)=c('Tipo','Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols', 
                  'Flavanoids','Nonflavanoid_phenols','Proantocianinas','Proanthocyanins',
                  'Hue','OD280OD315','Proline')


vinho$Tipo = factor(vinho$Tipo,levels = c(1,2,3))

library(caTools)

divisao = sample.split(vinho$Tipo, SplitRatio = 0.75)

base_treinamento = subset(vinho,divisao==TRUE)
base_teste = subset(vinho,divisao==FALSE)

library(e1071)

classificador = naiveBayes(x = base_treinamento[-1], y = base_treinamento$Tipo)
print(classificador)

previsao = predict(classificador,newdata = base_teste[-1])
print(previsao)

previsao

matriz = table(base_teste$Tipo,previsao)
matriz
12+17+12+3
erro =  3 / 44
erro
acerto = 1 - erro
acerto

library(caret)
confusionMatrix(matriz)


df = data.frame(Alcohol=c(11.00),Malic_acid=c(1.25),Ash=c(1.79),Alcalinity_of_ash=c(17.00),
                Magnesium=c(150),Total_phenols=c(1.99),Flavanoids=c(1.27),
                Nonflavanoid_phenols=c(0.5),Proantocianinas=c(1.3),Proanthocyanins=c(6.66),
                Hue=c(0.57),OD280OD315=c(4.85),Proline=c(457))

previsao = predict(classificador,newdata = df)
previsao
#









##########################################################################################
# 3. Classificação
# 3.2. Árvores de Decisão - Rpart

#DadosRS  = https://dados.fee.tche.br/

dados = read.csv2(file.choose(), header = T, sep = ",")
#dados = read.csv2("DM.csv", header = T, sep = ",")
dados
colnames(dados) = c("cidade","a_colhida","rend_medio","a_dest_a_colheita","quant_produzida","valor_da_producao")
dados1 = dados

dados1$cidade=NULL
cor(dados[2:6])
dados1
#install.packages("arules")
library(arules)
#discretizando coluna valor_da_producao
dados1$valor_da_producao = discretize(dados1$valor_da_producao,method = "frequency",
                                      breaks = 5, labels = c("Muito Baixo","Baixo","M?dio","Alta","Muito Alto"))
dados1
tail(dados1)
library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

#analisando alguns gr?ficos
par(mfrow=c(1,4))
for (i in 2:5) {
  hist(base_treinamento[,i], main=names(base_treinamento)[i])
}

for (i in 2:5) {
  plot(base_treinamento[,i], col=base_treinamento$valor_da_producao , main=names(base_treinamento)[i])
}

plot(base_treinamento[,-1],col = base_treinamento$valor_da_producao)

par(mfrow=c(1,1))
boxplot(base_treinamento[,-1])

#install.packages("rpart")
library(rpart)
#criando um classificador
classificador = rpart(formula = valor_da_producao ~ ., data = base_treinamento)
print(classificador)
#poda
#prune(classificador,0.02)
#install.packages("rpart.plot")
library(rpart.plot)
#mostrando árvore
rpart.plot(classificador)

#install.packages("caret")
library(caret)
#realizando previsoes, mostrando matriz de confusao e analisando resultados 
previsoes = predict(classificador, newdata = base_teste[-5], type = 'class')
previsoes
matriz_confusao = table(base_teste[,5],previsoes)
print(matriz_confusao)
#install.packages("e1071")
library(e1071)
confusionMatrix(matriz_confusao)
#






############################################################################################
# 3. Classificass?o
# 3.2. ?rvores de Decis?o - RandomForest 

#DadosRS  = https://dados.fee.tche.br/

dados = read.csv2(file.choose(), header = T, sep = ",")
#dados = read.csv2("DM.csv", header = T, sep = ",")
dados
colnames(dados) = c("cidade","a_colhida","rend_medio","a_dest_a_colheita","quant_produzida","valor_da_producao")
dados1 = dados

dados1$cidade=NULL
cor(dados[2:6])
dados1
#install.packages("arules")
library(arules)
#discretizando coluna valor_da_producao
dados1$valor_da_producao = discretize(dados1$valor_da_producao,method = "frequency",
                                      breaks = 5, labels = c("Muito Baixo","Baixo","M?dio","Alta","Muito Alto"))
dados1
tail(dados1)
library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

library(randomForest)

set.seed(11)
classificador = randomForest(x = base_treinamento[-5], 
                             y = base_treinamento$valor_da_producao, ntree = 10)

previsoes = predict(classificador,base_teste[-5])

previsoes

matriz_confusao = table(base_teste[,5],previsoes)
matriz_confusao
library(caret)
confusionMatrix(matriz_confusao)

##########################################################################################
# 3. Classificassão
# 3.3. Regras - (PRISM, OneR, CN2)

##########################################################################################
# 3. Classificass?o
# 3.4. Aprendizagem Baseado em Inst?ncias - KNN

##########################################################################################
# 3. Classificass?o
# 3.5. M?quina de Vetores de Suporte - SVM

##########################################################################################
# 3. Classificass?o
# 3.6. Regress?o Logist?ca

##########################################################################################
# 3. Classificass?o
# 3.7. RNA

