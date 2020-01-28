
#Inner_Join
id.loja = 1:12

nome.loja = c('Americanas', 'Netshoes', 'Sansung', 'Mototola', 'Vivo', 'Claro',
                   'Submarino', 'Dell', 'Acer', 'Nacional', 'Peruzzo', "Líder Premium")

idade.loja = c(10, 11, 49, 10, 45, 10, 40, 15, 44, 11, 49, 34)

uf.loja = c('DF', 'DF', 'CE', 'MG', 'PR', 'PR', 'MA', 'RJ', 'SC', 'SP', 'RS','RS')

id.seguimento = c(1, 2, 4, 4, 3, 3, 1, 5, 5, 6, 6, 8)

lojas = data.frame(id.loja, nome.loja, idade.loja, uf.loja,
                        id.seguimento)

########################################################################################

id.seguimento = 1:7

nome.seguimento = c('Varejo', 'Esportes', 'Telefonia', 'Dispositivos Móveis',
                    'Informática', 'Super Mercados','Viajens')

rendimento.seguimento = c(90000, 80000, 76000, 88000, 89000, 95000, 70000)

seguimentos = data.frame(id.seguimento, nome.seguimento, rendimento.seguimento)

base = merge(lojas, seguimentos)

names(seguimentos) = c("seguimento", "nome.seguimento", "rendimento.seguimento") 

base1 = merge(lojas, seguimentos, by.x = "id.seguimento", by.y = "seguimento")

#########################################################################################
#Outer Join:
#Left Join, Right Join, Full Join

id.produtor = 1:9
nome.produtor = c("Sementhes", "Tree Life", "T.I", "Campo Bom", "Vento Forte",
                  "Fazenda Esperança", "Trem Bam", "J.J Simons", "Minuano")
produto.a = c(500, 501, 500.78, 505.05, 502, 503, 500, 501, 501)
produto.b = c(819.92, 819.90, 819.89, 819.99, 820.01, 819.95, 819.98, 819.93, 820.00)
produto.c = c(1, 0, 0, 1, 0, 0, 1, 1, 0)
produtores = data.frame(id.produtor, nome.produtor, produto.a, produto.b, produto.c)
produtores

id.produtor = c(1, 2, 3, 5, 8, 13, 21, 34)
tipo.pesticida = c("S", "P", "S", "P", "S", "S", "P", "P")
controle = data.frame(id.produtor, tipo.pesticida)
controle


########################################################################################
#Left Join
base = merge(produtores, controle, all.x = TRUE)

########################################################################################
#Right Join
base1 = merge(produtores, controle, all.y = TRUE)

########################################################################################
#Full Join

base = merge(produtores, controle, all.x = TRUE, all.y = TRUE)

names(controle) = c("id", "tipo.pesticida")

base1 = merge(produtores, controle, all.x = TRUE, all.y = TRUE)
#O que ocorreu?

base2 = merge(produtores, controle, by.x = "id.produtor", by.y = "id",
              all.x = TRUE, all.y = TRUE)

####################################################################################
#Cross Join
base = merge(produtores, controle, all.x = T, all.y = T)

names(controle) = c("id", "tipo.pesticida")

base = merge(produtores, controle, all.x = T, all.y = T)
base1 = merge(produtores, controle)
rm(base)
####################################################################################
#Anti Join
library(dplyr)
id.produtor = c(1, 2, 3, 5, 8, 13, 21, 34)
tipo.pesticida = c("S", "P", "S", "P", "S", "S", "P", "P")
controle = data.frame(id.produtor, tipo.pesticida)
controle
base = anti_join(produtores,controle)

###################################################################################
#Semi Join
base = semi_join(produtores,controle)
#base = merge(produtores, controle, all.x = F, all.y = F)

###################################################################
#Self Join

