
library(sp)
library(stringr)

df = ManipulnadoStrings
class(df)

for(i in 1:length(df$Dados)){
  
  matricula  =str_sub(df$Dados[i], start = -4)
  
  df$Dados[i] = str_c(matricula,sep =" ",df$Dados[i])
  
  tamanho = str_length(df$Dados[i])
  
  aux = tamanho - 5
  
  df$Dados[i] = substr(df$Dados[i], 1, aux)
    
}
rm(matricula)
#
#
#
#


























library(sp)
library(stringr)

dados = ManipulnadoStrings

for (i in 1:length(dados$Dados)){
  #Pega os 4 últimos caracteres
  matriculas = str_sub(dados$Dados[i], start = - 4)
  #Colar os 4 últimos caracteres com um espeço de separção
  dados$Dados[i] = str_c(matriculas,sep = " ",dados$Dados[i])
  
  #Recorta a matricula que ficou no final da string
  tamanho = str_length(dados$Dados[i])
  aux = tamanho - 5
  dados$Dados[i] = substr(dados$Dados[i], 1,aux)
}