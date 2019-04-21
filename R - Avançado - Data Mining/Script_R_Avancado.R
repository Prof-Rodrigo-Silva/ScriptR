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
# 2.5. Hierárquico

# 3. Classificação:
# 3.1. Neive Bays
# 3.2. Árvores de Decisão - Rpart
# 3.3. Regras - (PRISM, OneR, CN2)
# 3.4. Aprendizagem Baseado em Instâncias - KNN
# 3.5. Máquina de Vetores de Suporte - SVM
# 3.6. Regressão Logistíca
# 3.7. RNA

# 4. Regressão:
# 4.1. Regressão linear (simples e múltipla)
# 4.2. Regressão polinomial
# 4.3. Regressão com árvores de decisão e random forest
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
# 1. Regras de Associação:
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
