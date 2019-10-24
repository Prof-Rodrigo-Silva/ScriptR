#####################################################################################
# Aula 01 - IntroduÃ§Ã£o
# ConteÃºdo:
# 1. Regras de AssociaÃ§Ã£o:
# 1.1. Apriori
# 1.2. ECLAT

# 2. Agrupamentos
# 2.1. K-means
# 2.2. Fuzzy c-means
# 2.3. K-medoids
# 2.4. DBSCAN
# 2.5. Hier?rquico

# 3. ClassificaÃ§Ã£o:
# 3.1. Naive Bayes
# 3.2. ?rvores de DecisÃ£o - Rpart - RandomForest - Party
# 3.3. Regras - (PRISM, OneR, CN2)
# 3.4. Aprendizagem Baseado em InstÃ¢ncias - KNN
# 3.5. M?quina de Vetores de Suporte - SVM
# 3.6. RegressÃ£o Logistica
# 3.7. RNA

# 4. RegressÃ£o:
# 4.1. RegressÃ£o linear (simples e mÃºltipla)
# 4.2. RegressÃ£o polinomial
# 4.3. RegressÃ£o com Ã¡rvores de decissÃ£o e random forest
# 4.4. RegressÃ£o com vetores de suporte
# 4.5. RegressÃ£o com redes neurais artificiais

# 5. SÃ©ries Temporais
# 6. MineraÃ§Ã£o de Textos
# 7. Redes Sociais e Grafos

####################################################################################################
# 1. Regras de AssociaÃ§Ã£o:
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
# 3. ClassificaÃ§Ã£o
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
# 3. ClassificaÃ§Ã£o
# 3.2. ?rvores de DecisÃ£o - Rpart

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
                                      breaks = 5, labels = c("Muito Baixo","Baixo","MÃ©dio","Alta","Muito Alto"))
dados1
tail(dados1)
library(caTools)
#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados1$valor_da_producao, SplitRatio = 0.75)
base_treinamento = subset(dados1, divisao == TRUE)
base_teste = subset(dados1, divisao == FALSE)

#analisando alguns grÃ¡ficos
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
#mostrando Ã¡rvore
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
# 3. ClassificaÃ§Ã£o
# 3.2. Ãrvores de DecisÃ£o - RandomForest 

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
                                      breaks = 5, labels = c("Muito Baixo","Baixo","MÃ©dio","Alta","Muito Alto"))
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
# 3. ClassificaÃ§Ã£o
# 3.3. Ãrvores de DecisÃ£o - Party
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
                                      breaks = 5, labels = c("Muito Baixo","Baixo","MÃ©dio","Alta","Muito Alto"))
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
# 3. ClassificaÃ§Ã£o - Regras
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
# 3. ClassificaÃ§Ã£o - Regras
# 3.3.2. PRISM e ZeroR

#SugestÃ£o: Estudar a teoria de funcionamento

# Por onde comeÃ§ar: https://christophm.github.io/interpretable-ml-book/rules.html#bayesian-rule-lists

#Holte, Robert C. â€œVery simple classification rules perform well on most commonly used datasets.â€ Machine learning 11.1 (1993): 63-90.â†©

#Cohen, William W. â€œFast effective rule induction.â€ Machine Learning Proceedings (1995). 115-123.â†©

#Letham, Benjamin, et al. â€œInterpretable classifiers using rules and Bayesian analysis: Building a better stroke prediction model.â€ The Annals of Applied Statistics 9.3 (2015): 1350-1371.â†©

#Borgelt, C. â€œAn implementation of the FP-growth algorithm.â€ Proceedings of the 1st International Workshop on Open Source Data Mining Frequent Pattern Mining Implementations - OSDM â€™05, 1â€“5. http://doi.org/10.1145/1133905.1133907 (2005).â†©

#Yang, Hongyu, Cynthia Rudin, and Margo Seltzer. â€œScalable Bayesian rule lists.â€ Proceedings of the 34th International Conference on Machine Learning-Volume 70. JMLR. org, 2017.â†©

#FÃ¼rnkranz, Johannes, Dragan Gamberger, and Nada LavraÄ. â€œFoundations of rule learning.â€ Springer Science & Business Media, (2012)

#ZeroR (Linha Base)

#Qual o percentual mÃ­nimo de acerto que um algoritmo de aprendizagem de mÃ¡quina deve ter!
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
# 3. ClassificaÃ§Ã£o - Regras 
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
# 3. ClassificaÃ§Ã£o
# 3.4. Aprendizagem Baseado em InstÃ¢ncias - KNN

# Outros mÃ©todos geram modelos e os dados podem ser descartados, mÃ©todos 
# baseados em instÃ¢ncias apenas armazenam os treinamentos, sendo que a
# previsÃ£o sÃ³ Ã© realizanda quando um novo registro precisa ser classificado

dados = read.csv2(file.choose(), header = T, sep = ",")

dados
colnames(dados) = c("cidade","a_colhida","rend_medio","a_dest_a_colheita","quant_produzida","valor_da_producao")
dados1 = dados

dados1$cidade=NULL
library(arules)
#discretizando coluna valor_da_producao
dados1$valor_da_producao = discretize(dados1$valor_da_producao,method = "frequency",
                                      breaks = 5, labels = c("Muito Baixo","Baixo",
                                                             "MÃ©dio","Alta","Muito Alto"))

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
# 3. ClassificaÃ§Ã£o
# 3.5. MÃ¡quina de Vetores de Suporte - SVM

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
# 3. ClassificaÃ§Ã£o
# 3.6. RegressÃ£o LogistÃ­ca

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

#Calculo EstatÃ­stica KS
ks = max(attr(performance, "y.values")[[1]] - (attr(performance, "x.values")[[1]]))
ks

##########################################################################################
# 3. ClassificaÃ§Ã£o
# 3.7. RNA
# 3.7.1. Neural Net

# 1Âº Exemplo
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
#2Âº Exemplo

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

# Colocar vÃ¡rias saÃ­das binÃ¡rias na saÃ­da categÃ³rica
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
# 3. ClassificaÃ§Ã£o
# 3.7. RNA
# 3.7.2. h2o

# Deep Learning                            NaÄ±ve Bayes
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
# 4. Regressão:
# 4.1. Regressão linear (simples e múltipla)

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

plot(density(dados$youtube), main="Gráfico de Densidade: Youtube"
     , ylab="Frequency", sub=paste("Skewness:",
                                   round(e1071::skewness(dados$youtube), 2)))

polygon(density(dados$youtube), col="red")

plot(density(dados$vendas), main="Gráfico de Densidade: Vendas"
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
# 4. Regressão:
# 4.2. Regressão polinomial
dim(cars)

head(cars)

# preparação dos dados
x = with(cars, speed)
y = with(cars, dist)
tamanho = dim(cars)[1]
speed.novo = seq(min(x), max(x), length.out = tamanho)

# scatter plot com regressão
plot(y ~ x, data = cars, xlab = "Velocidade", 
     ylab = "Distância até parar")

# ajuste reta de regressão
modelo = lm(y ~ x, data = cars)
abline(modelo, lty = 2, lwd = 2, col = "blue")
modelo

# ajuste regressão cúbica
modelo.1 = lm(y ~ poly(x, 3))
modelo.1

# dist = 42.98 + 145.55Speed + 23.00Speed² + 13.80Speed³

# scatter plot com ajuste polinomial
plot(y ~ x, data = cars, xlab = "Velocidade",
     ylab = "Distância até parar")

abline(modelo, lty = 2, lwd = 2, col = "red")

lines(speed.novo, predict(modelo.1, data.frame(x = speed.novo)),
      col = "green", lty = 2, lwd = 2)
#

########################################################################
# 4. Regressão:
# 4.3.1 RegressÃ£o com Ã¡rvores de decisão e 4.3.2 random forest

dados = boston

#LON e LAT são a longitude e latitude do centro do setor censitário. 
#MEDV é o valor médio das casas ocupadas pelos proprietários, medido em milhares de dólares. 
#CRIM é a taxa de criminalidade per capita. 
#O ZN está relacionado a quanto da terra é zoneada para grandes propriedades residenciais. 
#INDUS é a proporção da área utilizada para a indústria. 
#CHAS é 1 se um setor censitário estiver próximo ao rio Charles.
#0 NOX é a concentração de óxidos nitrosos no ar, uma medida da poluição do ar. 
#RM é o número médio de quartos por habitação.
#AGE é a proporção de unidades ocupadas pelos proprietários construídas antes de 1940.
#DIS é uma medida de quão longe o trato está dos centros de emprego em Boston. 
#RAD é uma medida de proximidade com estradas importantes. 
#IMPOSTO é o imposto predial por US $ 10.000 em valor. 
#PTRATIO é a proporção de alunos por professor por cidade.

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
# 4. Regressão:
# 4.3.1 RegressÃ£o com Ã¡rvores de decissÃ£o e 4.3.2 random forest

dados = boston

#LON e LAT são a longitude e latitude do centro do setor censitário. 
#MEDV é o valor médio das casas ocupadas pelos proprietários, medido em milhares de dólares. 
#CRIM é a taxa de criminalidade per capita. 
#O ZN está relacionado a quanto da terra é zoneada para grandes propriedades residenciais. 
#INDUS é a proporção da área utilizada para a indústria. 
#CHAS é 1 se um setor censitário estiver próximo ao rio Charles.
#0 NOX é a concentração de óxidos nitrosos no ar, uma medida da poluição do ar. 
#RM é o número médio de quartos por habitação.
#AGE é a proporção de unidades ocupadas pelos proprietários construídas antes de 1940.
#DIS é uma medida de quão longe o trato está dos centros de emprego em Boston. 
#RAD é uma medida de proximidade com estradas importantes. 
#IMPOSTO é o imposto predial por US $ 10.000 em valor. 
#PTRATIO é a proporção de alunos por professor por cidade.

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
# 4. Regressão:
# 4.4. RegressÃ£o com vetores de suporte
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
# 4. Regressão:
# 4.5. RegressÃ£o com redes neurais artificiais
dados = boston
#LON e LAT são a longitude e latitude do centro do setor censitário. 
#MEDV é o valor médio das casas ocupadas pelos proprietários, medido em milhares de dólares. 
#CRIM é a taxa de criminalidade per capita. 
#O ZN está relacionado a quanto da terra é zoneada para grandes propriedades residenciais. 
#INDUS é a proporção da área utilizada para a indústria. 
#CHAS é 1 se um setor censitário estiver próximo ao rio Charles.
#0 NOX é a concentração de óxidos nitrosos no ar, uma medida da poluição do ar. 
#RM é o número médio de quartos por habitação.
#AGE é a proporção de unidades ocupadas pelos proprietários construídas antes de 1940.
#DIS é uma medida de quão longe o trato está dos centros de emprego em Boston. 
#RAD é uma medida de proximidade com estradas importantes. 
#IMPOSTO é o imposto predial por US $ 10.000 em valor. 
#PTRATIO é a proporção de alunos por professor por cidade.

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
# 5. Séries Temporais
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

plot.ts(cbind(USAccDeaths,AirPassengers),main='Mortes X Transporte Aéreo',xlab='Anos')
plot.ts(cbind(USAccDeaths,AirPassengers),main='Mortes X Transporte Aéreo',xlab='Anos', nc=2) #lado a lado
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

# Análise da Função de Autocorrelação (FAC) e Autocorrelação Parcial
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

#Fim 1º Parte - Inicio 2º Parte

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

#Médias Móveis, ARIMA
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
# 6. Mineração de Textos
#install.packages("tm",dependencies = T)
library(NLP)
library(tm)

#Carregando e analisando dados
getSources()

getReaders()

#VCorpus()
#PCorpus()

textos = VCorpus(DirSource("C:/Users/fermat/Documents/ScriptR/R - Avançado - Data Mining/textos",
                 encoding="UTF-8"),readerControl = list(reader=readPlain,language="por"))

inspect(textos)

inspect(textos[1:2])

inspect(textos[2])

inspect(textos[[1]])

meta(textos[[1]])

as.character(textos[[2]])

as.character(textos[[2]])[20]


#Mineração de termos frequentes

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
tweet("Tweet gerado com twitteR da playlist de R Avançado do meu canal no YouTube")

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
# 7. Redes, Comunidades e Grafos - 1º Parte

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

g4 = graph( c("Rodrigo", "Cássio", "Cássio", "Leonardo", "Leonardo", "Rodrigo"))

plot(g4)

g4

g5 = graph( c("Rodrigo", "Michele", "Michele", "Rodrigo","Michele","Leonardo","Michele","Leonardo",
              "Rodrigo", "Rodrigo"), 
             isolates=c("Larissa", "Vivi", "Julina", "Cássio")) 

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

#Gráficos pequenos também podem ser gerados com uma descrição desse tipo: - para empate não
# direcionado, +- ou -+ para empates direcionados apontando para a esquerda e direita, ++ para um
# empate simétrico e ":" para conjuntos de vértices.

plot(graph_from_literal(a--b, b--c))

plot(graph_from_literal(a-+b, b+-c))

plot(graph_from_literal(a++b, b++c))

plot(graph_from_literal(a:b:c--c:d:e))

gl = graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)

plot(gl)

#Acessar vértices e arestas:

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
# 7. Redes, Comunidades e Grafos - 2º Parte
# Adicione atributos à rede, vértices ou arestas:
V(g5)$name

V(g5)$gender = c("M", "F", "M", "F", "F", "F", "M")

E(g5)$type = "email" #Atribuindo "email" a todas as arestas

E(g5)$weight = 10 #Configura todas as arestas existentes para 10

edge_attr(g5)

vertex_attr(g5)

graph_attr(g5)

g5 = set_graph_attr(g5, "Nome", "Email de Serviço")

g5 = set_graph_attr(g5, "Teste", "Testando")

graph_attr_names(g5)

graph_attr(g5, "Nome")

graph_attr(g5)

g5 = delete_graph_attr(g5, "Teste")

graph_attr(g5)

plot(g5, edge.arrow.size=.1, vertex.label.color="black", vertex.label.dist=3, 
     vertex.color=c("pink", "skyblue")[1+(V(g5)$gender=="M")] ) 

#O gráfico g5 tem duas arestas, indo de Michele para Leonardo, e um loop de Rodrigo para si mesmo. 
#Podemos simplificar nosso gráfico para remover loops e várias arestas entre os mesmos nós.
#Use edge.attr.comb para indicar como atributos de borda devem ser combinados - opções possíveis
#incluem sum, mean, prod(produto), min, max, first/ last(seleciona atributo a primeira / última 
#de borda). A opção "ignorar" diz que o atributo deve ser desconsiderado e descartado.

g5s = simplify(g5, remove.multiple = T, remove.loops = T, 
                edge.attr.comb = c(weight="sum", type="ignore"))

g5

plot(g5s, edge.arrow.size = .1, vertex.label.dist=3)

g5s

#A descrição de um objeto igraph começa com até quatro letras:
  
#1.D ou U, para um gráfico direcionado ou não direcionado
#2.N para um gráfico nomeado (onde os nós têm um nameatributo)
#3.W para um gráfico ponderado (onde as bordas têm um weightatributo)
#4.B para um gráfico bipartido (dois modos) (onde os nós têm um typeatributo)
#5.Os dois números a seguir (7 5) se referem ao número de nós e arestas no gráfico.

#A descrição também lista os atributos de nó e borda, por exemplo:
  
#1.(g/c) - atributo de caractere no nível do gráfico
#2.(v/c) - atributo de caractere no nível do vértice
#3.(e/n) - atributo numérico no nível da borda

########################################################################
# 7. Redes, Comunidades e Grafos - 3º Parte : Gráficos e modelos de gráficos específicos
library(igraph)

#Gráfico vazio
gv = make_empty_graph(40)
plot(gv, vertex.size=10, vertex.label=NA)

#Gráfico completo
gc = make_full_graph(40)
plot(gc, vertex.size=10, vertex.label=NA)

#Gráfico estrela simples
es = make_star(40)
plot(es, vertex.size=10, vertex.label=NA) 

#Gráfico de árvore
ga = make_tree(40, children = 3, mode = "undirected")
plot(ga, vertex.size=10, vertex.label=NA) 

#Gráfico em anel
ganel = make_ring(40)
plot(ganel, vertex.size=10, vertex.label=NA)

#Modelo de gráfico aleatório Erdos-Renyi
#('n' é o número de nós, 'm' é o número de arestas).
er = sample_gnm(n=100, m=40)
plot(er, vertex.size=6, vertex.label=NA) 

#Modelo de mundo pequeno Watts-Strogatz
#Cria uma treliça (com dim dimensões e sizenós através da dimensão) e religa as arestas
#aleatoriamente com probabilidade p. A vizinhança na qual as arestas estão conectadas é nei.
#Você pode permitir loops e multiplearestas.
ws = sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(ws, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

#Modelo de anexo preferencial Barabasi-Albert para gráficos sem escala
#( n é o número de nós, power é o poder do anexo ( 1 é linear); 
#m é o número de arestas adicionadas em cada etapa do tempo)
ba =  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

#igraph também pode fornecer alguns gráficos históricos notáveis. Por exemplo:
z = graph("Zachary") # the Zachary carate club
plot(z, vertex.size=10, vertex.label=NA)

data(package = .packages(all.available = TRUE))

#A reconfiguração de um gráfico
#each_edge() é um método de religação que altera os pontos finais da aresta uniformemente
#aleatoriamente com uma probabilidade prob.
rg = rewire(ganel, each_edge(prob=0.1))
plot(rg, vertex.size=10, vertex.label=NA)

#Rewire para conectar vértices a outros vértices a uma certa distância.
rn = connect.neighborhood(ganel, 5)
plot(rn, vertex.size=8, vertex.label=NA)

#Combine gráficos (união separada, assumindo conjuntos de vértices separados): %du%
plot(ganel, vertex.size=10, vertex.label=NA) 

plot(ga, vertex.size=10, vertex.label=NA)

plot(ganel %du% ga, vertex.size=10, vertex.label=NA)

########################################################################
# 7. Redes, Comunidades e Grafos - 4º Parte