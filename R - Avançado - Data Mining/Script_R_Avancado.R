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
# 3.1. Naive Bayes
# 3.2. ?rvores de Decisão - Rpart - RandomForest - Party
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
?sort



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
# 3.1. Naive Bayes
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

##########################################################################################
# 3. Classificação
# 3.2. ?rvores de Decisão - Rpart

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
                                      breaks = 5, labels = c("Muito Baixo","Baixo","Médio","Alta","Muito Alto"))
dados1
tail(dados1)
library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

#analisando alguns gráficos
par(mfrow=c(1,4))
for (i in 2:5) {
  hist(base_treinamento[,i], main=names(base_treinamento)[i])
}

for (i in 2:5) {
  plot(base_treinamento[,i], col=base_treinamento$valor_da_producao ,
       main=names(base_treinamento)[i])
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


############################################################################################
# 3. Classificação
# 3.2. Árvores de Decisão - RandomForest 

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
                                      breaks = 5, labels = c("Muito Baixo","Baixo","Médio","Alta","Muito Alto"))
dados1
tail(dados1)
library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

library(randomForest)
help(randomForest)

set.seed(1)
classificador = randomForest(x = base_treinamento[-5], 
                             y = base_treinamento$valor_da_producao, ntree = 40, mtry = 3)

classificador
previsoes = predict(classificador,base_teste[-5])

previsoes

matriz_confusao = table(base_teste[,5],previsoes)
matriz_confusao
library(caret)
confusionMatrix(matriz_confusao)


############################################################################################
# 3. Classificação
# 3.3. Árvores de Decisão - Party
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
                                      breaks = 5, labels = c("Muito Baixo","Baixo","Médio","Alta","Muito Alto"))
dados1
tail(dados1)
library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

library(party)
set.seed(1)
classificador = ctree(formula = valor_da_producao ~ ., data = base_treinamento)

previsoes = predict(classificador,base_teste)

previsoes

matriz_confusao = table(base_teste[,5],previsoes)

library(caret)

confusionMatrix(matriz_confusao)

plot(classificador)

plot(classificador,type="simple")

##########################################################################################
# 3. Classificação - Regras
# 3.3.1. OneR

library(readr)
diagnosticos = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data")


colnames(diagnosticos)=c('ID','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion',
                         'Single Epithelial Cell Size', 'Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses',
                         'Class')

diag = data.frame(diagnosticos)

diag[,1] = NULL
diag$Class[diag$Class==2]='benign'
diag$Class[diag$Class==4]='malignant'

library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(diag$Class, SplitRatio = 0.75)
base_treinamento = subset(diag, divisao == TRUE)
base_teste = subset(diag, divisao == FALSE)

#install.packages("OneR")
library(OneR)
help("OneR")
set.seed(1)
classificador = OneR(base_treinamento, verbose = T)

print(classificador)

summary(classificador)

plot(classificador)

previsao = predict(classificador,base_teste)

plot(previsao)

eval_model(previsao,base_teste)

##########################################################################################
# 3. Classificação - Regras
# 3.3.2. PRISM e ZeroR

#Sugestão: Estudar a teoria de funcionamento

# Por onde começar: https://christophm.github.io/interpretable-ml-book/rules.html#bayesian-rule-lists

#Holte, Robert C. “Very simple classification rules perform well on most commonly used datasets.” Machine learning 11.1 (1993): 63-90.↩

#Cohen, William W. “Fast effective rule induction.” Machine Learning Proceedings (1995). 115-123.↩

#Letham, Benjamin, et al. “Interpretable classifiers using rules and Bayesian analysis: Building a better stroke prediction model.” The Annals of Applied Statistics 9.3 (2015): 1350-1371.↩

#Borgelt, C. “An implementation of the FP-growth algorithm.” Proceedings of the 1st International Workshop on Open Source Data Mining Frequent Pattern Mining Implementations - OSDM ’05, 1–5. http://doi.org/10.1145/1133905.1133907 (2005).↩

#Yang, Hongyu, Cynthia Rudin, and Margo Seltzer. “Scalable Bayesian rule lists.” Proceedings of the 34th International Conference on Machine Learning-Volume 70. JMLR. org, 2017.↩

#Fürnkranz, Johannes, Dragan Gamberger, and Nada Lavrač. “Foundations of rule learning.” Springer Science & Business Media, (2012)

#ZeroR (Linha Base)

#Qual o percentual mínimo de acerto que um algoritmo de aprendizagem de máquina deve ter!
library(readr)
diagnosticos = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data")


colnames(diagnosticos)=c('ID','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion',
                         'Single Epithelial Cell Size', 'Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses',
                         'Class')
diag = diagnosticos

diag[,1] = NULL
diag$Class[diag$Class==2]='benign'
diag$Class[diag$Class==4]='malignant'

library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(diag$Class, SplitRatio = 0.75)
base_treinamento = subset(diag, divisao == TRUE)
base_teste = subset(diag, divisao == FALSE)

table(base_teste$Class)

acerto = 114 / 174
acerto
percentual = acerto * 100
percentual

##########################################################################################
# 3. Classificação - Regras 
# 3.3.3. CN2

library(readr)
diagnosticos = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data")


colnames(diagnosticos)=c('ID','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion',
                         'Single Epithelial Cell Size', 'Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses',
                         'Class')

diag = diagnosticos

diag[,1] = NULL
diag$Class[diag$Class==2]='benign'
diag$Class[diag$Class==4]='malignant'

library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(diag$Class, SplitRatio = 0.75)
base_treinamento = subset(diag, divisao == TRUE)
base_teste = subset(diag, divisao == FALSE)

#install.packages('RoughSets')
library(RoughSets)

dt_treinamento = SF.asDecisionTable(base_treinamento, decision.attr = 10)
dt_teste = SF.asDecisionTable(base_teste, decision.attr = 10)

classificador = RI.CN2Rules.RST(dt_treinamento, K = 5)

discretizacao = D.discretization.RST(dt_treinamento, nOfIntervals = 5)

dt_treinamento = SF.applyDecTable(dt_treinamento, discretizacao)
dt_teste = SF.applyDecTable(dt_teste, discretizacao)

classificador = RI.CN2Rules.RST(dt_treinamento, K = 5)
print(classificador)

previsao = predict(classificador, newdata = dt_teste[-10])

matriz_confusao = table(dt_teste[, 10], unlist(previsao))
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)

##########################################################################################
# 3. Classificação
# 3.4. Aprendizagem Baseado em Instâncias - KNN

# Outros métodos geram modelos e os dados podem ser descartados, métodos 
# baseados em instâncias apenas armazenam os treinamentos, sendo que a
# previsão só é realizanda quando um novo registro precisa ser classificado

dados = read.csv2(file.choose(), header = T, sep = ",")

dados
colnames(dados) = c("cidade","a_colhida","rend_medio","a_dest_a_colheita","quant_produzida","valor_da_producao")
dados1 = dados

dados1$cidade=NULL
library(arules)
#discretizando coluna valor_da_producao
dados1$valor_da_producao = discretize(dados1$valor_da_producao,method = "frequency",
                                      breaks = 5, labels = c("Muito Baixo","Baixo",
                                                             "Médio","Alta","Muito Alto"))

dados1[,1:4] = scale(dados1[,1:4])


library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

#install.packages("class")
library(class)

previsao = knn(base_treinamento[,-5], base_teste[,-5], cl = base_treinamento[,5], k = 5)
help(knn)

print(previsao)

matriz_confusao = table(base_teste[,5],previsao)
matriz_confusao
library(caret)
confusionMatrix(matriz_confusao)

##########################################################################################
# 3. Classificação
# 3.5. Máquina de Vetores de Suporte - SVM

dados = read.csv2(file.choose(), header = T, sep = ",")

dados
colnames(dados) = c("id","sexo","idade","rendimentoEstimado","comprou")
dados

dados1 = dados
#dados1$id = NULL
#dados1$sexo = NULL

dados1 = dados1[3:5]

#Encoding da classe meta para factor
dados1$comprou = factor(dados1$comprou, levels = c(0,1))

#Scaling
dados1[-3] = scale(dados1[-3])

library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$comprou, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

library(e1071)

classificador = svm(formula = comprou ~ .,
                    data = base_treinamento,
                    type = "C-classification",
                    kernel = "radial",
                    cost = 500)

classificador
previsao = predict(classificador, newdata = base_teste[-3])
previsao

matriz_confusao = table(base_teste[,3],previsao)
matriz_confusao
library(caret)
confusionMatrix(matriz_confusao)
plot(previsao)


library(ElemStatLearn) 
#Plotando os resultados do conjunto de treinamento 
btr = base_treinamento 
X1 = seq(min(btr[, 1]) - 1, max(btr[, 1]) + 1, by = 0.01) 
X2 = seq(min(btr[, 2]) - 1, max(btr[, 2]) + 1, by = 0.01) 

grafico = expand.grid(X1, X2) 
colnames(grafico) = c('idade', 'rendimentoEstimado') 
grafico_a = predict(classificador, newdata = grafico) 

plot(btr[, -3], 
     main = 'SVM (Conjunto de Treinamento)', 
     xlab = 'Idade', ylab = 'Rendimento Estimado', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(grafico_a), length(X1), length(X2)), add = TRUE) 

points(grafico, pch = '.', col = ifelse(grafico_a == 1, 'coral1', 'aquamarine')) 

points(btr, pch = 21, bg = ifelse(btr[, 3] == 1, 'green4', 'red3')) 

#https://www.datacamp.com/community/tutorials/support-vector-machines-r

##########################################################################################
# 3. Classificação
# 3.6. Regressão Logistíca

Social
colnames(Social) = c("id","sexo","idade","rendimentoEstimado","comprou")
Social

dados = Social
#dados$id = NULL
#dados$sexo = NULL

dados = dados[3:5]

#Encoding da classe meta para factor
#dados$comprou = factor(dados$comprou, levels = c(0,1))

#Scaling
dados[-3] = scale(dados[-3])

library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados$comprou, SplitRatio = 0.75)
base_treinamento = subset(dados, divisao == TRUE)
base_teste = subset(dados, divisao == FALSE)

classificador = glm (formula = comprou ~ ., data = base_treinamento, family = binomial)
summary(classificador)

step(classificador)

summary(classificador)$coefficients

odd.ratio = exp(coef(classificador))
odd.ratio
previsao = predict(classificador,newdata = base_teste[-3], type = "response")
previsao

previsoes = ifelse(previsao > 0.5, 1, 0)
previsoes

matriz_confusao = table(base_teste$comprou,previsoes)
matriz_confusao
library(caret)
confusionMatrix(matriz_confusao)

previsao2 = cbind(base_teste,previsao)
previsao2

library(ROCR)
previsao_valores = prediction(previsao ,previsao2$comprou)

# calculo da auc (area under the curve)
auc = performance(previsao_valores,"auc")

# Plota curva ROC
performance = performance(previsao_valores, "tpr", "fpr")
plot(performance, col = "blue", lwd = 5)

#Calculo Estatística KS
ks = max(attr(performance, "y.values")[[1]] - (attr(performance, "x.values")[[1]]))
ks

##########################################################################################
# 3. Classificação
# 3.7. RNA
# 3.7.1. Neural Net

# 1º Exemplo
treinoEntrada =  as.data.frame(runif(50, min=0, max=100))
treinoSaida = sqrt(treinoEntrada)

base_treinamento = cbind(treinoEntrada,treinoSaida)

colnames(base_treinamento) = c("Entrada","Saida")

#install.packages("neuralnet")
library(neuralnet)
classificador = neuralnet(Saida ~ Entrada, base_treinamento, hidden = c(50,50),
                          threshold = 0.001)
print(classificador)

plot(classificador)

base_teste = as.data.frame((1:10)^2)

previsao = compute(classificador, base_teste)

ls(previsao)

print(previsao$net.result)

tabela = cbind(base_teste,sqrt(base_teste),
                     as.data.frame(previsao$net.result))

colnames(tabela) = c("Entrada","Saida Esperada","Saida RNA")
print(tabela)

#################################################################################
#2º Exemplo

treinamento = iris[sample(1:nrow(iris), 50),]
base_treinamento = treinamento

# Binarize the categorical output
base_treinamento = cbind(base_treinamento, treinamento$Species == 'setosa')
base_treinamento = cbind(base_treinamento, treinamento$Species == 'versicolor')
base_treinamento = cbind(base_treinamento, treinamento$Species == 'virginica')

head(base_treinamento)

names(base_treinamento)[6] = 'setosa'
names(base_treinamento)[7] = 'versicolor'
names(base_treinamento)[8] = 'virginica'

head(base_treinamento)
library(neuralnet)
classificador = neuralnet(setosa+versicolor+virginica ~ Sepal.Length + Sepal.Width
                          + Petal.Length + Petal.Width, data = base_treinamento, 
                          hidden = c(10,10))

print(classificador)
plot(classificador)

#install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(classificador)

previsao = compute(classificador, iris[-5])$net.result
previsao

# Colocar várias saídas binárias na saída categórica
func = function(x) {
  return(which(x == max(x)))
}
x = apply(previsao, c(1), func)

predicao = c('setosa', 'versicolor', 'virginica')[x]
predicao

matriz_confusao = table(iris$Species,predicao)
matriz_confusao
library(caret)
confusionMatrix(matriz_confusao)

##########################################################################################
# 3. Classificação
# 3.7. RNA
# 3.7.2. h2o

# Deep Learning                            Naıve Bayes
# Principal Components Analysis (PCA)      K-means
# Stacked Ensembles                        Generalized Linear Models (GLM)
# Gradient Boosting Machine (GBM)          Generalized Low Rank Model (GLRM)
# Distributed Random Forest (DRF)          Word2vec

# www.h2o.ai

#install.packages("h2o",dependencies = T)
library(h2o)
#help("h2o.init")

dados = read.csv2(file.choose(), header = T, sep = ",")


colnames(dados) = c("id","cred_fornecido","sexo","educacao","estado_civil","idade",
                    "pag_passado1","pag_passado2","pag_passado3","pag_passado4","pag_passado5",
                    "pag_passado6","valor_em_conta1","valor_em_conta2","valor_em_conta3",
                    "valor_em_conta4","valor_em_conta5","valor_em_conta6","valor_pag_anterior1",
                    "valor_pag_anterior2","valor_pag_anterior3","valor_pag_anterior4",
                    "valor_pag_anterior5","valor_pag_anterior6","pagou")

dados
dados$id = NULL

library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados$pagou, SplitRatio = 0.75)
base_treinamento = subset(dados, divisao == TRUE)
base_teste = subset(dados, divisao == FALSE)

h2o.init(nthreads = -1)

base_treinamento = as.h2o(base_treinamento)
base_teste = as.h2o(base_teste)

help("h2o.deeplearning")

classificador = h2o.deeplearning(y = "pagou",
                                 training_frame = base_treinamento,
                                 activation = "Rectifier",
                                 hidden = c(150,150,150),
                                 epochs = 1000)

classificador
plot(classificador)

previsoes = h2o.predict(classificador,base_teste[-24])

previsoes

previsoes  = (previsoes > 0.5)
previsoes = as.vector(previsoes)

base_teste = as.data.frame(base_teste)

matriz_confusao = table(base_teste[,24],previsoes)

matriz_confusao

library(caret)
confusionMatrix(matriz_confusao)
h2o.shutdown()
y


########################################################################
# 4. Regress?o:
# 4.1. Regress?o linear (simples e m?ltipla)

#install.packages("datarium")
data("marketing", package = "datarium")
head(marketing, 4)

dados = marketing
colnames(dados) = c("youtube","facebook","jornal","vendas")
summary(dados)

library(ggplot2)
ggplot(dados, aes(x = youtube, y = vendas)) +
  geom_point() +
  stat_smooth()

scatter.smooth(x=dados$vendas, y=dados$youtube, main="Vendas ~ Youtube")

par(mfrow=c(1, 2))

boxplot(dados$youtube, main="Youtube",
        sub=paste("Outlier: ", boxplot.stats(dados$youtube)$out))

boxplot(dados$vendas, main="Vendas",
        sub=paste("Outlier: ", boxplot.stats(dados$vendas)$out))

library(e1071)

plot(density(dados$youtube), main="Gr?fico de Densidade: Youtube"
     , ylab="Frequency", sub=paste("Skewness:",
                                   round(e1071::skewness(dados$youtube), 2)))

polygon(density(dados$youtube), col="red")

plot(density(dados$vendas), main="Gr?fico de Densidade: Vendas"
     , ylab="Frequency", sub=paste("Skewness:",
                                   round(e1071::skewness(dados$vendas), 2)))

polygon(density(dados$vendas), col="red")

par(mfrow=c(1, 1))

cor(dados$vendas, dados$youtube)

modelo = lm(vendas ~ youtube, data = dados)
print(modelo)

summary(modelo)

modelo$coefficients

modelo$residuals

modelo$fitted.values

confint(modelo)

ggplot(dados, aes(youtube, vendas)) +
  geom_point() +
  stat_smooth(method = lm)

plot(modelo)

predict(modelo, data.frame(youtube=150))

modelo = lm(vendas ~ youtube + facebook + jornal, data = dados)

print(modelo)
summary(modelo)
summary(modelo)$r.squared
summary(modelo)$adj.r.squared

predict(modelo ,data.frame(youtube = 150, facebook = 202, jornal = 78))

########################################################################
# 4. Regress?o:
# 4.2. Regress?o polinomial
dim(cars)

head(cars)

# prepara??o dos dados
x = with(cars, speed)
y = with(cars, dist)
tamanho = dim(cars)[1]
speed.novo = seq(min(x), max(x), length.out = tamanho)

# scatter plot com regress?o
plot(y ~ x, data = cars, xlab = "Velocidade", 
     ylab = "Dist?ncia at? parar")

# ajuste reta de regress?o
modelo = lm(y ~ x, data = cars)
abline(modelo, lty = 2, lwd = 2, col = "blue")
modelo

# ajuste regress?o c?bica
modelo.1 = lm(y ~ poly(x, 3))
modelo.1

# dist = 42.98 + 145.55Speed + 23.00Speed? + 13.80Speed?

# scatter plot com ajuste polinomial
plot(y ~ x, data = cars, xlab = "Velocidade",
     ylab = "Dist?ncia at? parar")

abline(modelo, lty = 2, lwd = 2, col = "red")

lines(speed.novo, predict(modelo.1, data.frame(x = speed.novo)),
      col = "green", lty = 2, lwd = 2)
#

########################################################################
# 4. Regress?o:
# 4.3.1 Regressão com árvores de decis?o e 4.3.2 random forest

dados = boston

#LON e LAT s?o a longitude e latitude do centro do setor censit?rio. 
#MEDV ? o valor m?dio das casas ocupadas pelos propriet?rios, medido em milhares de d?lares. 
#CRIM ? a taxa de criminalidade per capita. 
#O ZN est? relacionado a quanto da terra ? zoneada para grandes propriedades residenciais. 
#INDUS ? a propor??o da ?rea utilizada para a ind?stria. 
#CHAS ? 1 se um setor censit?rio estiver pr?ximo ao rio Charles.
#0 NOX ? a concentra??o de ?xidos nitrosos no ar, uma medida da polui??o do ar. 
#RM ? o n?mero m?dio de quartos por habita??o.
#AGE ? a propor??o de unidades ocupadas pelos propriet?rios constru?das antes de 1940.
#DIS ? uma medida de qu?o longe o trato est? dos centros de emprego em Boston. 
#RAD ? uma medida de proximidade com estradas importantes. 
#IMPOSTO ? o imposto predial por US $ 10.000 em valor. 
#PTRATIO ? a propor??o de alunos por professor por cidade.

dados$TOWN = NULL
dados$TRACT = NULL
plot(dados$LON, dados$LAT)

points(dados$LON[dados$CHAS == 1],
     dados$LAT[dados$CHAS == 1], col = "blue", pch = 19)

summary(dados$NOX)

points(dados$LON[dados$NOX>=0.55],
       dados$LAT[dados$NOX>=0.55], col="green", pch=20)

summary(dados$MEDV)

points(dados$LON[dados$MEDV>=22.5],
       dados$LAT[dados$MEDV>=22.5], col="red", pch=20)

library(rpart)
library(rpart.plot)
library(caTools)
set.seed(123)
split = sample.split(dados$MEDV, SplitRatio = 0.7)
base_treinamento = subset(dados, split==TRUE)
base_teste = subset(dados, split==FALSE)

classificador = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE +
                        DIS + RAD + TAX + PTRATIO, data=base_treinamento)

summary(classificador)
prp(classificador)

previsao = predict(classificador, newdata = base_treinamento[-3])
previsao
plot(dados$LON, dados$LAT)

points(dados$LON[dados$MEDV>=21.2],
       dados$LAT[dados$MEDV>=21.2], col="red", pch=20)

points(dados$LON[previsao>21.2],dados$LAT[previsao>=21.2],
       col="blue", pch="$")

#install.packages("miscTools")
library(miscTools)
rsq_treinamento = rSquared(base_treinamento[['MEDV']], 
                          resid = base_treinamento[['MEDV']] - previsao)
rsq_treinamento

previsoes = predict(classificador, newdata = base_teste[-3])

mean(abs(base_teste[['MEDV']] - previsoes))

rsq_teste = rSquared(base_teste[['MEDV']],
                    resid = base_teste[['MEDV']] - previsoes)
rsq_teste

########################################################################
# 4. Regress?o:
# 4.3.1 Regressão com árvores de decissão e 4.3.2 random forest

dados = boston

#LON e LAT s?o a longitude e latitude do centro do setor censit?rio. 
#MEDV ? o valor m?dio das casas ocupadas pelos propriet?rios, medido em milhares de d?lares. 
#CRIM ? a taxa de criminalidade per capita. 
#O ZN est? relacionado a quanto da terra ? zoneada para grandes propriedades residenciais. 
#INDUS ? a propor??o da ?rea utilizada para a ind?stria. 
#CHAS ? 1 se um setor censit?rio estiver pr?ximo ao rio Charles.
#0 NOX ? a concentra??o de ?xidos nitrosos no ar, uma medida da polui??o do ar. 
#RM ? o n?mero m?dio de quartos por habita??o.
#AGE ? a propor??o de unidades ocupadas pelos propriet?rios constru?das antes de 1940.
#DIS ? uma medida de qu?o longe o trato est? dos centros de emprego em Boston. 
#RAD ? uma medida de proximidade com estradas importantes. 
#IMPOSTO ? o imposto predial por US $ 10.000 em valor. 
#PTRATIO ? a propor??o de alunos por professor por cidade.

dados$TOWN = NULL
dados$TRACT = NULL
dados$Medv = dados$MEDV
dados$MEDV = NULL
#plot(dados$LON, dados$LAT)

#points(dados$LON[dados$CHAS == 1],dados$LAT[dados$CHAS == 1], col = "blue", pch = 19)

#summary(dados$NOX)

#points(dados$LON[dados$NOX>=0.55],dados$LAT[dados$NOX>=0.55], col="green", pch=20)

#summary(dados$MEDV)

#points(dados$LON[dados$MEDV>=21.2],dados$LAT[dados$MEDV>=21.2], col="red", pch=20)

library(caTools)
set.seed(123)
split = sample.split(dados$Medv, SplitRatio = 0.7)
base_treinamento = subset(dados, split==TRUE)
base_teste = subset(dados, split==FALSE)

library(randomForest)

classificador = randomForest(x = base_treinamento[1:13], y = base_treinamento$Medv,
                             ntree = 50)

previsoes = predict(classificador, newdata = base_treinamento[-14])
previsoes

library(miscTools)
rsq_treinamento = rSquared(base_treinamento[['Medv']],
                          resid = base_treinamento[['Medv']] - previsoes)
rsq_treinamento

previsoes = predict(classificador, newdata = base_teste[-14])

mean(abs(base_teste[['Medv']] - previsoes))

rsq_teste = rSquared(base_teste[['Medv']], resid = base_teste[['Medv']] - previsoes)

rsq_teste
#

########################################################################
# 4. Regress?o:
# 4.4. Regressão com vetores de suporte
dados = regression

library(e1071)
library(ggplot2)
ggplot() + geom_point(aes(x = dados$X, y = dados$Y), colour = 'blue')

modelo = lm(Y ~ X , dados)

previsoes = predict(modelo, newdata = dados[-2])

ggplot() + geom_point(aes(x = dados$X, y = dados$Y), colour = 'blue') +
  geom_line(aes(x = dados$X, y = previsoes), colour = 'red')

rsq = rSquared(dados[['Y']], resid = dados[['Y']] - previsoes)
rsq

classificador = svm(formula = Y ~ X, data = dados,
                    type = 'eps-regression', kernel = 'radial')
summary(classificador)

previsoes = predict(classificador, newdata = dados[-2])

#library(miscTools)
rsq1 = rSquared(dados[['Y']], resid = dados[['Y']] - previsoes)
rsq1

ggplot() + geom_point(aes(x = dados$X, y = dados$Y), colour = 'blue') +
  geom_line(aes(x = dados$X, y = previsoes), colour = 'red')

df = data.frame(X = c(40))
previsao = predict(classificador, newdata = df)
previsao

########################################################################
# 4. Regress?o:
# 4.5. Regressão com redes neurais artificiais
dados = boston
#LON e LAT s?o a longitude e latitude do centro do setor censit?rio. 
#MEDV ? o valor m?dio das casas ocupadas pelos propriet?rios, medido em milhares de d?lares. 
#CRIM ? a taxa de criminalidade per capita. 
#O ZN est? relacionado a quanto da terra ? zoneada para grandes propriedades residenciais. 
#INDUS ? a propor??o da ?rea utilizada para a ind?stria. 
#CHAS ? 1 se um setor censit?rio estiver pr?ximo ao rio Charles.
#0 NOX ? a concentra??o de ?xidos nitrosos no ar, uma medida da polui??o do ar. 
#RM ? o n?mero m?dio de quartos por habita??o.
#AGE ? a propor??o de unidades ocupadas pelos propriet?rios constru?das antes de 1940.
#DIS ? uma medida de qu?o longe o trato est? dos centros de emprego em Boston. 
#RAD ? uma medida de proximidade com estradas importantes. 
#IMPOSTO ? o imposto predial por US $ 10.000 em valor. 
#PTRATIO ? a propor??o de alunos por professor por cidade.

dados$TOWN = NULL
dados$TRACT = NULL
#dados$Medv = dados$MEDV
#dados$MEDV = NULL

library(caTools)
set.seed(123)
split = sample.split(dados$MEDV, SplitRatio = 0.7)
base_treinamento = subset(dados, split==TRUE)
base_teste = subset(dados, split==FALSE)

library(h2o)

h2o.init(nthreads = -1)

base_treinamento = as.h2o(base_treinamento)
base_teste = as.h2o(base_teste)

#help("h2o.deeplearning")

classificador = h2o.deeplearning(y = "MEDV",
                                 training_frame = base_treinamento,
                                 activation = "Rectifier",
                                 hidden = c(30,30),
                                 epochs = 100)

classificador
plot(classificador)

previsoes = h2o.predict(classificador,base_teste[-3])

previsoes
previsoes = as.vector(previsoes)

library(miscTools)
mean(abs(base_teste[['MEDV']] - previsoes))
rsqd = rSquared(base_teste[['MEDV']],
                          resid = base_teste[['MEDV']] - previsoes)

h2o.shutdown()

########################################################################
# 5. S?ries Temporais
AirPassengers
lynx
Nile
USAccDeaths

start(USAccDeaths)
end(USAccDeaths)


plot(AirPassengers)
plot(lynx)
plot(Nile)
plot(USAccDeaths)


plot(USAccDeaths,xlab='Anos',ylab='Numero de Mortes')

plot(USAccDeaths,type='o')

plot.ts(cbind(USAccDeaths,AirPassengers),main='Mortes X Transporte A?reo',xlab='Anos')
plot.ts(cbind(USAccDeaths,AirPassengers),main='Mortes X Transporte A?reo',xlab='Anos', nc=2) #lado a lado
ts.plot(USAccDeaths,AirPassengers,gpars=list(xlab='', ylab='',lty=1:1))

aggregate(USAccDeaths, nfrequency = 4, FUN = sum) # somas trimestrais

aggregate(USAccDeaths, nfreq = 1,FUN=mean) # medias anuais

plot(aggregate(USAccDeaths))

plot(aggregate(USAccDeaths, nfrequency = 4, FUN = sum))

#Por meses
monthplot(USAccDeaths, col.base = 2, labels = month.abb)

#Extraindo uma Janela
janela = window(USAccDeaths, start=c(1973,5), end=c(1975,7))
janela

diff(USAccDeaths)
log(USAccDeaths)

# An?lise da Fun??o de Autocorrela??o (FAC) e Autocorrela??o Parcial
#(FACp) com defasagem 25:
a = acf(USAccDeaths, lag.max=25)
a
p = pacf(USAccDeaths, lag.max=25)
p
da = acf(diff(USAccDeaths), lag.max=25)
da
dp = pacf(diff(USAccDeaths), lag.max=25)
dp
#Obtendo a sazionalidade
plot(stl(log(USAccDeaths), "periodic"))

#Fim 1? Parte - Inicio 2? Parte

#Decompondo
decompose(USAccDeaths)

d = decompose(USAccDeaths)

d$seasonal
d$trend
d$random

plot(d$seasonal)
plot(d$trend)
plot(d$random)
plot(d)

#M?dias M?veis, ARIMA
mean(USAccDeaths)
janela = window(USAccDeaths, start=c(1976,1), end=c(1976,12))
mean(janela)

#install.packages("forecast")
library(forecast)
mm = ma(USAccDeaths,order = 6)
mm
previsao = forecast(mm, h = 6)
previsao
plot(previsao)

# Comando geral: arima(data,order=c(p,d,q)
x = arima(USAccDeaths,order=c(0,1,1)) 

# Estimando o modelo ARIMA sazonal
y = arima(USAccDeaths,order=c(0,1,1),seasonal=list(order=c(0,1,1),period = 6)) 

previsao1 = predict(y,n.ahead=4)
previsao1

ar = auto.arima(USAccDeaths)
ar

previsao2 = forecast(ar, h = 6)
previsao2
plot(previsao2)

########################################################################
# 6. Minera??o de Textos
#install.packages("tm",dependencies = T)
library(NLP)
library(tm)

#Carregando e analisando dados
getSources()

getReaders()

#VCorpus()
#PCorpus()

textos = VCorpus(DirSource("C:/Users/fermat/Documents/ScriptR/R - Avan?ado - Data Mining/textos",
                 encoding="UTF-8"),readerControl = list(reader=readPlain,language="por"))

inspect(textos)

inspect(textos[1:2])

inspect(textos[2])

inspect(textos[[1]])

meta(textos[[1]])

as.character(textos[[2]])

as.character(textos[[2]])[20]


#Minera??o de termos frequentes

stopwords('portuguese')

textos = tm_map(textos, removeWords,stopwords('portuguese'))

textos = tm_map(textos, stripWhitespace)

textos = tm_map(textos, removePunctuation)

textos = tm_map(textos, removeNumbers)

textos = tm_map(textos, content_transformer(tolower))

tdm = TermDocumentMatrix(textos)
tdm
m = as.matrix(tdm)
m
v = sort(rowSums(m),decreasing=TRUE)
v
d = data.frame(word = names(v),freq=v)
head(d, 50)


# install.packages("wordcloud",dependencies = T)
# install.packages("wordcloud2",dependencies = T)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order = T, rot.per=0.5, 
          colors = brewer.pal(8, "Dark2"))

wordcloud2(d)

wordcloud2(d, color = "random-light", backgroundColor = "grey")

wordcloud2(d, minRotation = -pi/6, maxRotation = -pi/6, minSize = 15,
           rotateRatio = 1)

######################################################################################

#install.packages("twitteR",dependencies = T)
library(twitteR)

# https://blog.difluir.com/2013/06/como-criar-uma-app-no-twitter/

# https://medium.com/@marlessonsantana/como-criar-apps-e-obter-os-tokens-necess%C3%A1rios-para-coletar-dados-do-twitter-instagram-linkedin-e-8f36602ea92a

# coloque suas chaves
api_key             = "xxxxxxxxxxxxxxxxxx"
api_secret          = "xxxxxxxxxxxxxxxxxx"
access_token        = "xxxxxxxxxxxxxxxxxx"
access_token_secret = "xxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# woeid = where on earth id
availableTrendLocations()

w = availableTrendLocations()

w

trendsPoA = getTrends(woeid = 455823)
# 10 primeiros apenas
trendsPoA
trendsPoA$name
trendsPoA$name[1:10]

trendsPoA$url
trendsPoA$query
trendsPoA$woeid

#Buscando Twitter

searchTwitteR('gremio',n = 50)

imp = searchTwitter('gremio', n = 50)
imp
imp[c(1, 25, 50)]

#twetar direto do R
tweet("Tweet gerado com twitteR da playlist de R Avan?ado do meu canal no YouTube")

trendsBR = getTrends(woeid = 23424768)

trendsBR$name

tweets = searchTwitter("@Prof_R_R_S", n = 20)

usuario = getUser('Prof_R_R_S')

usuario$created
usuario$favoritesCount
usuario$followersCount
usuario$name

TLusuario = userTimeline("Prof_R_R_S", n = 20)
TLusuario

df = twListToDF(TLusuario)

head(df)

########################################################################
# 7. Redes, Comunidades e Grafos - 1? Parte

#install.packages("igraph",dependencies = T)
library(igraph)

g1 = graph(edges = c(1,2, 2,3, 3,1), directed = F )

plot(g1)

class(g1)

g1

g2 = graph( edges=c(1,2, 2,3, 3, 1) )

plot(g2)

g2

g3 = graph( edges=c(1,2, 2,3, 3,1), n=5 )

plot(g3)   

g3

g4 = graph( c("Rodrigo", "C?ssio", "C?ssio", "Leonardo", "Leonardo", "Rodrigo"))

plot(g4)

g4

g5 = graph( c("Rodrigo", "Michele", "Michele", "Rodrigo","Michele","Leonardo","Michele","Leonardo",
              "Rodrigo", "Rodrigo"), 
             isolates=c("Larissa", "Vivi", "Julina", "C?ssio")) 

plot(g5)

plot(g5, edge.arrow.size = .3)

plot(g5, edge.arrow.size = .3, vertex.color = "gold")

plot(g5, edge.arrow.size = .3, vertex.color = "gold", vertex.size = 15)

plot(g5, edge.arrow.size = .3, vertex.color = "gold", vertex.size = 15, 
     vertex.frame.color = "gray")

plot(g5, edge.arrow.size = .3, vertex.color = "gold", vertex.size = 15, 
     vertex.frame.color = "gray", vertex.label.color = "black") 

plot(g5, edge.arrow.size = .3, vertex.color = "gold", vertex.size = 15, 
     vertex.frame.color = "gray", vertex.label.color = "black", 
     vertex.label.cex = 1.5)

plot(g5, edge.arrow.size = .3, vertex.color = "gold", vertex.size = 15, 
     vertex.frame.color = "gray", vertex.label.color = "black", 
     vertex.label.cex = 0.8, vertex.label.dist = 3) 

plot(g5, edge.arrow.size = .3, vertex.color = "gold", vertex.size = 15, 
     vertex.frame.color = "gray", vertex.label.color = "black", 
     vertex.label.cex = 0.8, vertex.label.dist = 3, edge.curved = 0.5) 

#Gr?ficos pequenos tamb?m podem ser gerados com uma descri??o desse tipo: - para empate n?o
# direcionado, +- ou -+ para empates direcionados apontando para a esquerda e direita, ++ para um
# empate sim?trico e ":" para conjuntos de v?rtices.

plot(graph_from_literal(a--b, b--c))

plot(graph_from_literal(a-+b, b+-c))

plot(graph_from_literal(a++b, b++c))

plot(graph_from_literal(a:b:c--c:d:e))

gl = graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)

plot(gl)

#Acessar v?rtices e arestas:

E(g5)

V(g5)

# Examinar a matriz de rede diretamente
g5[]

g5[1,]
g5[2,]
g5[,2]
g5[,3]
g5[3,2]

########################################################################
# 7. Redes, Comunidades e Grafos - 2? Parte
# Adicione atributos ? rede, v?rtices ou arestas:
V(g5)$name

V(g5)$gender = c("M", "F", "M", "F", "F", "F", "M")

E(g5)$type = "email" #Atribuindo "email" a todas as arestas

E(g5)$weight = 10 #Configura todas as arestas existentes para 10

edge_attr(g5)

vertex_attr(g5)

graph_attr(g5)

g5 = set_graph_attr(g5, "Nome", "Email de Servi?o")

g5 = set_graph_attr(g5, "Teste", "Testando")

graph_attr_names(g5)

graph_attr(g5, "Nome")

graph_attr(g5)

g5 = delete_graph_attr(g5, "Teste")

graph_attr(g5)

plot(g5, edge.arrow.size=.1, vertex.label.color="black", vertex.label.dist=3, 
     vertex.color=c("pink", "skyblue")[1+(V(g5)$gender=="M")] ) 

#O gr?fico g5 tem duas arestas, indo de Michele para Leonardo, e um loop de Rodrigo para si mesmo. 
#Podemos simplificar nosso gr?fico para remover loops e v?rias arestas entre os mesmos n?s.
#Use edge.attr.comb para indicar como atributos de borda devem ser combinados - op??es poss?veis
#incluem sum, mean, prod(produto), min, max, first/ last(seleciona atributo a primeira / ?ltima 
#de borda). A op??o "ignorar" diz que o atributo deve ser desconsiderado e descartado.

g5s = simplify(g5, remove.multiple = T, remove.loops = T, 
                edge.attr.comb = c(weight="sum", type="ignore"))

g5

plot(g5s, edge.arrow.size = .1, vertex.label.dist=3)

g5s

#A descri??o de um objeto igraph come?a com at? quatro letras:
  
#1.D ou U, para um gr?fico direcionado ou n?o direcionado
#2.N para um gr?fico nomeado (onde os n?s t?m um nameatributo)
#3.W para um gr?fico ponderado (onde as bordas t?m um weightatributo)
#4.B para um gr?fico bipartido (dois modos) (onde os n?s t?m um typeatributo)
#5.Os dois n?meros a seguir (7 5) se referem ao n?mero de n?s e arestas no gr?fico.

#A descri??o tamb?m lista os atributos de n? e borda, por exemplo:
  
#1.(g/c) - atributo de caractere no n?vel do gr?fico
#2.(v/c) - atributo de caractere no n?vel do v?rtice
#3.(e/n) - atributo num?rico no n?vel da borda

########################################################################
# 7. Redes, Comunidades e Grafos - 3? Parte : Gr?ficos e modelos de gr?ficos espec?ficos
library(igraph)

#Gr?fico vazio
gv = make_empty_graph(40)
plot(gv, vertex.size=10, vertex.label=NA)

#Gr?fico completo
gc = make_full_graph(40)
plot(gc, vertex.size=10, vertex.label=NA)

#Gr?fico estrela simples
es = make_star(40)
plot(es, vertex.size=10, vertex.label=NA) 

#Gr?fico de ?rvore
ga = make_tree(40, children = 3, mode = "undirected")
plot(ga, vertex.size=10, vertex.label=NA) 

#Gr?fico em anel
ganel = make_ring(40)
plot(ganel, vertex.size=10, vertex.label=NA)

#Modelo de gr?fico aleat?rio Erdos-Renyi
#('n' ? o n?mero de n?s, 'm' ? o n?mero de arestas).
er = sample_gnm(n=100, m=40)
plot(er, vertex.size=6, vertex.label=NA) 

#Modelo de mundo pequeno Watts-Strogatz
#Cria uma treli?a (com dim dimens?es e sizen?s atrav?s da dimens?o) e religa as arestas
#aleatoriamente com probabilidade p. A vizinhan?a na qual as arestas est?o conectadas ? nei.
#Voc? pode permitir loops e multiplearestas.
ws = sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(ws, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

#Modelo de anexo preferencial Barabasi-Albert para gr?ficos sem escala
#( n ? o n?mero de n?s, power ? o poder do anexo ( 1 ? linear); 
#m ? o n?mero de arestas adicionadas em cada etapa do tempo)
ba =  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

#igraph tamb?m pode fornecer alguns gr?ficos hist?ricos not?veis. Por exemplo:
z = graph("Zachary") # the Zachary carate club
plot(z, vertex.size=10, vertex.label=NA)

data(package = .packages(all.available = TRUE))

#A reconfigura??o de um gr?fico
#each_edge() ? um m?todo de religa??o que altera os pontos finais da aresta uniformemente
#aleatoriamente com uma probabilidade prob.
rg = rewire(ganel, each_edge(prob=0.1))
plot(rg, vertex.size=10, vertex.label=NA)

#Rewire para conectar v?rtices a outros v?rtices a uma certa dist?ncia.
rn = connect.neighborhood(ganel, 5)
plot(rn, vertex.size=8, vertex.label=NA)

#Combine gr?ficos (uni?o separada, assumindo conjuntos de v?rtices separados): %du%
plot(ganel, vertex.size=10, vertex.label=NA) 

plot(ga, vertex.size=10, vertex.label=NA)

plot(ganel %du% ga, vertex.size=10, vertex.label=NA)

########################################################################
# 7. Redes, Comunidades e Grafos - 4º Parte
# Conhecendo o conjunto de dados

########################################################################
# 7. Redes, Comunidades e Grafos - 5º Parte

library(igraph)

nos = Dados1_MIDIA_NOS

ligacoes = Dados1_MIDIA_ARESTAS

head(nos)

head(ligacoes)

nrow(nos);

length(unique(nos$id))

nrow(ligacoes);

nrow(unique(ligacoes[,c("origem", "destino")]))

ligacoes = aggregate(ligacoes[,3], ligacoes[,-3], sum)

ligacoes = ligacoes[order(ligacoes$origem, ligacoes$destino),]

colnames(ligacoes)[4] = "peso"

rownames(ligacoes) = NULL

nos2 = Dados2_MIDIA_USUARIO_NOS

ligacoes2 = Dados2_MIDIA_USUARIO_ARESTAS

head(nos2)

head(ligacoes2)

#Podemos ver que ligacoes2 é uma matriz de adjacência para uma rede de dois modos:
  
ligacoes2 = as.matrix(ligacoes2)

dim(ligacoes2)

dim(nos2)

########################################################################
# 7. Redes, Comunidades e Grafos - 6º Parte Transformando Redes em Objetos Igraph

library(igraph)

graf = graph_from_data_frame(d = ligacoes, vertices = nos, directed = T)

class(graf)

graf

E(graf) # Arestas
V(graf) # Vertices
E(graf)$tipo # Arestas do atributo "tipo"
V(graf)$midia # Vertices do atributo "midia"

plot(graf, edge.arrow.size=.1,vertex.label=NA)

graf = simplify(graf, remove.multiple = F, remove.loops = T)

plot(graf, edge.arrow.size=.1,vertex.label=NA)

as_edgelist(graf, names=T)

as_adjacency_matrix(graf, attr="peso")

as_data_frame(graf, what="edges")

as_data_frame(graf, what="vertices")

head(nos2)

head(ligacoes2)

graf2 = graph_from_incidence_matrix(ligacoes2)

table(V(graf2)$type)

graf2.bp = bipartite.projection(graf2)

as_incidence_matrix(graf2) %*% t(as_incidence_matrix(graf2))
t(as_incidence_matrix(graf2)) %*% as_incidence_matrix(graf2)

plot(graf2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.size=7, vertex.label = nos2$midia[!is.na(nos2$tipo.midia)])

plot(graf2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     vertex.size=7, vertex.label=nos2$midia[is.na(nos2$tipo.midia)])

########################################################################
# 7. Redes, Comunidades e Grafos - 7º Parte Plotagens Igraph
library(igraph)

plot(graf, edge.arrow.size=.1, edge.curved=.1)

plot(graf, edge.arrow.size=.1, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(graf)$midia, vertex.label.color="black",
     vertex.label.cex=.7) 


# Gerar cores baseadas no tipo de mídia:
colrs = c("gray50", "tomato", "gold")

V(graf)$color = colrs[V(graf)$tipo.midia]

# Set o nó com base no tamanho da audiencia:
V(graf)$size = V(graf)$tamanho.audiencia*0.7

V(graf)$label.color = "black"

V(graf)$label = NA

# Set a largura da araesta baseado no peso:
E(graf)$width = E(graf)$peso/6

#Alterar o tamanho da seta e a cor da aresta:
E(graf)$arrow.size = .1
E(graf)$edge.color = "gray80"
E(graf)$width = 1+E(graf)$peso/12
plot(graf)

#Substituir os atributos explicitamente no gráfico
plot(graf, edge.color="orange", vertex.color="gray50")

#Legenda
plot(graf)
legend(x=1.2, y=1, c("Jornal","Televisão", "Notícias On Line"),
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8,
       bty="n", ncol=1)

#Em redes semânticas, podemos plotar apenas os rótulos dos nós:
plot(graf, vertex.shape="none", vertex.label=V(graf)$midia,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray70")

#Colorir as arestas do gráfico com base na cor do nó de origem
edge.start = ends(graf, es=E(graf), names=F)[,1]

edge.col = V(graf)$color[edge.start]

plot(graf, edge.color=edge.col, edge.curved=.2)

########################################################################
# 7. Redes, Comunidades e Grafos - 8º Layouts de rede
graf.bg = sample_pa(80) 

V(graf.bg)$size = 8

V(graf.bg)$frame.color = "white"

V(graf.bg)$color = "orange"

V(graf.bg)$label = "" 

E(graf.bg)$arrow.mode = 0

plot(graf.bg)

plot(graf.bg, layout=layout_randomly)

l = layout_in_circle(graf.bg)

plot(graf.bg, layout=l)

l = cbind(1:vcount(graf.bg), c(1, vcount(graf.bg):2))

plot(graf.bg, layout=l)

l = layout_randomly(graf.bg)

plot(graf.bg, layout=l)

# Fruchterman-Reingold
l = layout_with_fr(graf.bg)

plot(graf.bg, layout=l)

par(mfrow=c(2,2), mar=c(0,0,0,0))

plot(graf.bg, layout=layout_with_fr)

plot(graf.bg, layout=layout_with_fr)

plot(graf.bg, layout=l)

plot(graf.bg, layout=l)

dev.off()

l = layout_with_fr(graf.bg)

l = norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))

plot(graf.bg, rescale=F, layout=l*0.4)

plot(graf.bg, rescale=F, layout=l*0.6)

plot(graf.bg, rescale=F, layout=l*0.8)

plot(graf.bg, rescale=F, layout=l*1.0)

dev.off()

l = layout_with_kk(graf.bg)

plot(graf.bg, layout=l)

layouts = grep("^layout_", ls("package:igraph"), value=TRUE)[-1]

layouts = layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))

for (layout in layouts) {
  
  print(layout)
  
  l = do.call(layout, list(graf)) 
  
  plot(graf, edge.arrow.mode=0, layout=l, main=layout)
}
dev.off()
########################################################################
# 7.Redes, Comunidades e Grafos - 9º Parte Melhorando os gráficos de Rede
library(igraph)
plot(graf)

hist(ligacoes$peso)

mean(ligacoes$peso)

sd(ligacoes$peso)
help(sd)

cut.off = mean(ligacoes$peso) 

graf.sp = delete_edges(graf, E(graf)[peso<cut.off])

plot(graf.sp) 

E(graf)$peso = 1.5

plot(graf, edge.color=c("dark red", "slategrey")[(E(graf)$tipo=="hyperlink")+1],
     vertex.color="gray40", layout=layout.circle)

graf.m = graf - E(graf)[E(graf)$tipo=="hyperlink"]
graf.h = graf - E(graf)[E(graf)$tipo=="mencao"]

par(mfrow=c(1,2))

plot(graf.h, vertex.color="orange", main="Hyperlink")

plot(graf.m, vertex.color="lightsteelblue2", main="Mencao")

l = layout_with_fr(graf)

plot(graf.h, vertex.color="orange", layout=l, main="Hyperlink")

plot(graf.m, vertex.color="lightsteelblue2", layout=l, main="Mencao")
dev.off()
########################################################################
# 7.Redes, Comunidades e Grafos - 10º Parte Plotagem interativa com tkplot
graf.tk = tkplot(graf) #tkid is the id of the tkplot that will open

l = tkplot.getcoords(graf.tk) # grab the coordinates from tkplot

tk_close(graf.tk, window.close = T)

plot(graf, layout=l)

########################################################################
# 7.Redes, Comunidades e Grafos - 11º Parte Outras maneiras de representar uma rede
library(igraph)

grafm = get.adjacency(graf, attr="peso", sparse=F)

colnames(grafm) = V(graf)$midia

rownames(grafm) = V(graf)$midia

cor = colorRampPalette(c("gold", "dark orange")) 

heatmap(grafm[,17:1], Rowv = NA, Colv = NA, col = cor(100), 
        scale="none", margins=c(10,10))


V(graf2)$color = c("steel blue", "orange")[V(graf2)$type+1]

V(graf2)$shape = c("square", "circle")[V(graf2)$type+1]

V(graf2)$label = ""

V(graf2)$label[V(graf2)$type==F] = nos2$midia[V(graf2)$type==F] 

V(graf2)$label.cex=.5

V(graf2)$label.font=2

plot(graf2, vertex.label.color="white", vertex.size=(2-V(graf2)$type)*10) 

#O Igraph também possui um layout especial para redes bipartidas

plot(graf2, vertex.label=NA, vertex.size=7, layout=layout_as_bipartite) 

#Usar texto como nós pode ser útil às vezes
plot(graf2, vertex.shape="none", vertex.label=nos2$midia,
    vertex.label.color=V(graf2)$color, vertex.label.font=2.5, 
    vertex.label.cex=.6, edge.color="gray70",  edge.width=2)

########################################################################
# 7.Redes, Comunidades e Grafos - 12º Parte Descritores de Rede e Nós
library(igraph)
#Densidade
edge_density(graf, loops=F)
ecount(graf)/(vcount(graf)*(vcount(graf)-1))

#Reciprocidade
reciprocity(graf)
dyad_census(graf)
2*dyad_census(graf)$mut/ecount(graf)

#Transitividade
transitivity(graf, type="global")
transitivity(as.undirected(graf, mode="collapse"))
transitivity(graf, type="local")
triad_census(graf)

#Diâmetro
diameter(graf, directed=F, weights=NA)
diameter(graf, directed=F)
diam = get_diameter(graf, directed=T)
diam

class(diam)
as.vector(diam)

vcol = rep("gray40", vcount(graf))

vcol[diam] = "gold"

ecol = rep("gray80", ecount(graf))

ecol[E(graf, path=diam)] = "orange" 

# E(net, path=diam) fencontra arestas ao longo de um caminho

plot(graf, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#Graus de nós
grau = degree(graf, mode="all") #mode = in, out, all
plot(graf, vertex.size=grau*3)

hist(grau, breaks=1:vcount(graf)-1, main="Histograma de graus do nó")

#Distribuição de Graus
grau.dist = degree_distribution(graf, cumulative=T, mode="all")

plot( x=0:max(grau), y=1-grau.dist, pch=19, cex=1.2, col="orange", 
      xlab="Grau", ylab="Frequencia Acumulada")

#Centralidade e centralização
degree(graf, mode="in")
centr_degree(graf, mode="in", normalized=T)

closeness(graf, mode="all", weights=NA) 
centr_clo(graf, mode="all", normalized=T) 

eigen_centrality(graf, directed=T, weights=NA)
centr_eigen(graf, directed=T, normalized=T)


betweenness(graf, directed=T, weights=NA)
edge_betweenness(graf, directed=T, weights=NA)
centr_betw(graf, directed=T, normalized=T)

#Hubs e authorities

hs = hub_score(graf, weights=NA)$vector
as = authority_score(graf, weights=NA)$vector

par(mfrow=c(1,2))

plot(graf, vertex.size=hs*50, main="Hubs")

plot(graf, vertex.size=as*30, main="Authorities")
dev.off()

#####################################################################################
# 7.Redes, Comunidades e Grafos - 13º Distâncias e caminhos
library(igraph)
mean_distance(graf, directed=F)
mean_distance(graf, directed=T)

distances(graf)
distances(graf, weights=NA)

dist.from.GN = distances(graf, v=V(graf)[midia=="GloboNews"], to=V(graf),weights=NA)

oranges = colorRampPalette(c("dark red", "gold"))
col = oranges(max(dist.from.GN)+1)
col = col[dist.from.GN+1]

plot(graf, vertex.color=col, vertex.label=dist.from.GN, edge.arrow.size=.1,
     vertex.label.color="white")


novo.caminho = shortest_paths(graf, 
                            from = V(graf)[midia=="Folha de S.Paulo"], 
                            to  = V(graf)[midia=="GloboNews"],
                            output = "both") # nós do caminho e arestas

#Gerando variável de cor da borda para traçar o caminho
ecol = rep("gray80", ecount(graf))
ecol[unlist(novo.caminho$epath)] = "orange"

#Gerando variável de largura da aresta para plotar o caminho
ew = rep(2, ecount(graf))
ew[unlist(novo.caminho$epath)] = 4

#Gerar variável de cor do nó para traçar o caminho
vcol = rep("gray40", vcount(graf))
vcol[unlist(novo.caminho$vpath)] = "gold"

plot(graf, vertex.color=vcol, edge.color=ecol, edge.width=ew,
     edge.arrow.mode=0)

#1 único no use incident()
#Vários nós use incident_edges()

inc.arestas = incident(graf,  V(graf)[midia=="Veja.com"], mode="all")

ecol = rep("gray80", ecount(graf))
ecol[inc.arestas] = "orange"

vcol = rep("grey40", vcount(graf))
vcol[V(graf)$midia=="Veja.com"] = "gold"

plot(graf, vertex.color=vcol, edge.color=ecol)

#adjacent_vertices()

nos.vizinho = neighbors(graf, V(graf)[midia=="Veja.com"], mode="in")

vcol[nos.vizinho] = "#ff9d00"
plot(graf, vertex.color=vcol)


#Operadores especiais para indexação de seqüências de arestas:,% -> %,% <- %
#E(rede) [X% -> %Y] seleciona arestas dos conjuntos de vértices X para conjunto de vértices Y
#E(rede) [X% <- %Y] seleciona arestas dos conjuntos de vértices Y para conjunto de vértices X
E(graf)[V(graf)[tipo.descricao=="Jornal"] %->% V(graf)[tipo.descricao=="Site"]]

cocitation(graf)

####################################################################################
# 7.Redes, Comunidades e Grafos - 14º Parte Subgrupos e comunidades
library(igraph)

graf.sym = as.undirected(graf, mode= "collapse", 
                         edge.attr.comb=list(weight="sum", "ignore"))

#Cliques
cliques(graf.sym) # lista de cliques       
sapply(cliques(graf.sym), length) # tamanho dos cliques
largest_cliques(graf.sym) # cliques com número máximo de nós

vcol = rep("grey80", vcount(graf.sym))
vcol[unlist(largest_cliques(graf.sym))] = "gold"

plot(as.undirected(graf.sym), vertex.label=V(graf.sym)$name, vertex.color=vcol)

#Detecção da comunidade
ceb = cluster_edge_betweenness(graf) 
dendPlot(ceb, mode="hclust")

plot(ceb, graf,edge.arrow.size =.1)

class(ceb)
length(ceb)     
membership(ceb) 
modularity(ceb)
crossing(ceb, graf)

clp = cluster_label_prop(graf)
plot(clp, graf,edge.arrow.size =.1)

cfg = cluster_fast_greedy(as.undirected(graf))
plot(cfg, as.undirected(graf))

V(graf)$community = cfg$membership
colrs = adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(graf, vertex.color=colrs[V(graf)$community],edge.arrow.size =.1)

#Decomposição do núcleo K
kc = coreness(graf, mode="all")
plot(graf, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc],edge.arrow.size =.1)
