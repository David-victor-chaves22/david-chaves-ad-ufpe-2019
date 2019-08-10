#####################################################################################
#####################################################################################
#####################################################################################
#############                                                           #############
#############           UNIVERSIDADE FEDERAL DE PERNAMBUCO              ############# 
############# PROGRAMA DE POS-GRADUCAO EM CIENCIA POLITICA - MESTRADO   ############# 
#############                RECIFE, 09 de agosto de 2019               ############# 
#############          DISCENTE: DAVID VICTOR DE MELO CHAVES            ############# 
#############                 DOCENTE: DAVI MOREIRA                     ############# 
#############                                                           ############# 
#####################################################################################
#####################################################################################
#####################################################################################

# Link do Github: https://github.com/David-victor-chaves22/david-chaves-ad-ufpe-2019.git

### Tabalho final David Chaves ###

# abrindo pacotes para processamento de dados
  
library(car)
library(sjstats)
library(fields)
library(foreign)
library(readstata13)
library(ggplot)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
  
  
  
#abrir data frames
candidatos <- read_delim("candidatos.csv", 
                           delim = ",", 
                           locale = locale(decimal_mark=",",grouping_mark="."))
colnames(candidatos)
  
  
  
votos <- read_delim("votos.csv", 
                      delim = ",", 
                      locale = locale(decimal_mark=",",grouping_mark="."))
colnames(votos)
  
receita <-  read_delim("receita.csv", 
                         delim = ";", 
                         locale = locale(decimal_mark=",",grouping_mark="."))
  
receitacsv <-  read_delim("receitacsv.csv", 
                         delim = ";", 
                         locale = locale(decimal_mark=",",grouping_mark="."))
  
  
  
colnames(receita)
receita <- receita %>% 
rename("NUMERO_CANDIDATO"  = "Numero.candidato")
  
# juntar data frames
  
dadosfgv <- candidatos %>% inner_join(votos, by = "NUMERO_CANDIDATO")
  
# filtrar dados
dadosfgv <- dadosfgv %>% 
    filter(SIGLA_UE == "SP")
  
# juntar data frame =
dados <- dadosfgv %>% inner_join(receita, by = "NUMERO_CANDIDATO")
  
  
#Remover linhas duplicadas
dados <- dados[!duplicated(dados),]
  
colnames(dados)
  
  
#para saber o numero total de candidatos
  
dim(candidatos)[1]
 
  
# para saber a media da receita
  
colMeans(receita["Valor.receita"])

  
# para saber o desvio padrao da receita
  
sd(receita$Valor.receita)

  
# para saber a media da quantidade de votos
  
colMeans(votos["QTDE_VOTOS"])

  
# para saber o desvio padrao da quantidade de votos
  
sd(votos$QTDE_VOTOS)

  
# loop para calcular o total de gastos do cadidato Carlos Alberto Rolim Zarattini
  
contador <- 0
  for (contador in 0:dim(receita)[1]){
  if (receita[Nome.candidato[contador]] == "CARLOS ALBERTO ROLIM ZARATTINI"){ 
    soma = soma + receita[Valor.receita[contador]]
    } 
  if (contador == (dim(receita)[1])) print (soma)
  }
  
# para obter os resultados das votações
  
summary(receitacsv["ColigaÃ.Ã.o"])
  
  
# Graficos
partido <- ("PCB, PCO, PRTB, PSOL/PSTU, PHS/PRP, PSDC, PSL/PTM/PMN/PTC/PT do B, PEN, PSC, PDT, PRB, PV, PTB, PSB, SD, PMDB/PROS/PP/PSD, PR, PPL, PSDB/DEM/PPS, PT/PC do B")
summary (receitacsv["Partido"])

dim(receitacsv["SituaÃ.Ã.o"]["Eleito por mÃ©di"])
  
summary(receita)
  
summary(votos)

#grafico de barra
ggplot(dados, aes(x= Sigla..Partido, y= QTDE_VOTOS))+
  geom_col()

#grafico de barra
ggplot(dados, aes(x= Sigla..Partido, y= Valor.receita))+
  geom_col()

ggplot(dados, aes(x= Sigla..Partido, y= QTDE_VOTOS))+
  geom_col()
  
#estatistica descritiva 
summary(dados)
  
table(dados$Sigla..Partido)
  
  
#boxplot distirbuição
  
boxplot(dados$Valor.receita, dados$QTDE_VOTOS, names=c('Valor','Votos'))
  
  
# correlacao das variveis votos e receita
cor(dados$QTDE_VOTOS, dados$Valor.receita)
  

  
# regressao primeiro modelo bivariado
modelo.1 <- lm(dados$QTDE_VOTOS ~ dados$Valor.receita)

summary(modelo.1)

# tranfomracao de variaveis, de nominal para dummy
dados <- dados %>%
mutate(GENERO = recode (DESCRICAO_SEXO,
                        "MASCULINO" = 1, 
                        "FEMININO"= 0))
  
# regressao segundo modelo multivariado 
modelo.2 <- lm(dados$QTDE_VOTOS ~ dados$Valor.receita + dados$GENERO)

summary(modelo.2)
  
  
#Graficos das relacao entre as duas variaveis
  
#Scatterplots

ggplot(dados, aes(x = Valor.receita, y = QTDE_VOTOS)) +
geom_point()
  
#relacao entre valor e votos por numero de patido
ggplot(dados, aes(x = Valor.receita, y = QTDE_VOTOS)) +
geom_point()+
facet_wrap(~NUMERO_PARTIDO)
  
#log de Valor.receita
ggplot(dados, aes(x = Valor.receita, y = QTDE_VOTOS)) +
geom_point()+
scale_x_log10()
  
#Histograms
dados %>% ggplot() + 
geom_histogram(aes(x = Valor.receita), binwidth = 4000)
  
dados %>% ggplot() + 
geom_histogram(aes(x = QTDE_VOTOS), binwidth = 4000)
  
dados %>% 
ggplot() + 
geom_histogram(aes(x = NUMERO_PARTIDO))
  
#Linegraphs

ggplot(dados, aes(x= Valor.receita, y= QTDE_VOTOS))+
geom_line()
  
#Bar plots
ggplot(dados, aes(x= Sigla..Partido, y= Valor.receita))+
geom_col()
  
ggplot(dados, aes(x= Sigla..Partido, y= QTDE_VOTOS))+
geom_col()
  
  
dados %>% ggplot() +
geom_bar(aes(x = Sigla..Partido))
