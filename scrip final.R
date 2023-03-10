#Instalando os pacotes
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages("lme4") 
install.packages("lmerTest") 
install.packages("data.table") 
install.packages("fields") 
install.packages("psych") 
install.packages("car") 
install.packages("MASS") 
install.packages("glmmTMB")
install.packages("glmer")
install.packages("lattice")
install.packages("DHARMa")
install.packages("vcd")
install.packages("Rmisc") 
install.packages("performance") 
install.packages("gllm")
install.packages('tidyverse')
install.packages('RVAideMemoire')
install.packages('MuMIn')
install.packages('piecewiseSEM')
install.packages('ggExtra')
install.packages('Rmisc')
install.packages('emmeans') 
install.packages('sjPlot')
install.packages('bbmle')
install.packages('ordinal')
install.packages('naniar')
install.packages("devtools")
install.packages("coda")
install.packages("ape")
install.packages("tidyr")
install.packages("esquisse")## faz uma interface grafica para produzir graficos
install.packages("ggridges")
install.packages("RColorBrewer")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("outliers")
install.packages('TMB', type = 'source')
install.packages("vegan")
# inicializando os pacotes
library(esquisse)
library(tidyr)
library(coda)     
library(reshape2) 
library(devtools)
library(flexplot)
library(Rtools)
library(lme4)
library(glmmTMB)
library(lattice)
library(ggplot2);theme_set(theme_bw())
require(Rmisc)
library(glmer)
library(performance)
library(gllm)
library(visdat)
library(tidyverse)
library(RVAideMemoire)
library(DHARMa)
library(performance)
library(MuMIn)
library(piecewiseSEM)
library(MASS)
library(Rmisc)
library(emmeans) 
library(sjPlot)
library(bbmle)
library(ordinal)
library(car)
library(ecolottery)
library(naniar)
library(vcd)
library(dplyr)
library(ggridges)
library(RColorBrewer)
library(lubridate)
library(FactoMineR)
library(factoextra)
library(outliers)
library(visdat)
library(vegan)

#Selecionando o diretório de trabalho
setwd("C:/Users/simon/Desktop/analises_mestrado")

#Lendo o banco de dados - poligono
dados <- readxl::read_excel("sem_out_area_grupao.xlsx")
dados$Est_reg <- as.factor(dados$Est_reg)
dados$Hora_bloco <- as.factor(dados$Hora_bloco)
dados$Varredura <- as.integer(dados$Varredura)
dados$Num_Ind <- as.integer(dados$Num_Ind)
dados$Mes <- as.factor(month(dados$Num_mes, label = TRUE))
dados$Grup_Ind <- as.factor(dados$Grup_Ind)
dados$Grupo_Area <- as.factor(dados$Grupo_Area)
dados$Grup_centr <- as.factor(dados$Grup_centr)

#Lendo o banco de dados - centroide
centroide <- readxl::read_excel("sem_out_Ind_centroide1.xlsx") 
centroide$Est_reg <- as.factor(centroide$Est_reg)
centroide$Estrato <- as.factor(centroide$Estrato)
centroide$Varredura <- as.integer(centroide$Varredura)
centroide$Individuo <- as.factor(centroide$Individuo)
centroide$Comportamento <- as.factor(centroide$Comportamento)
View(centroide)

#Lendo o banco de dados - proximidade
proximidade <- readxl::read_excel("sem_out_Proximidade.xlsx")
proximidade$Individuo_1 <- as.factor(proximidade$Individuo_1)
proximidade$Individuo_2 <- as.factor(proximidade$Individuo_2)
proximidade$Est_reg <- as.factor(proximidade$Est_reg)
proximidade$Estrato <- as.factor(proximidade$Estrato)
proximidade$Varredura <- as.integer(proximidade$Varredura)
proximidade$Comportamento <- as.factor(proximidade$Comportamento)
proximidade$Diade <- as.factor(proximidade$Diade)

#escolhendo a melhor distribuição de dados de contagem
Ord_plot(dados$AreaMPC) ## escolhe a melhor distribuição
Ord_plot(centroide$Dist_centr)
Ord_plot(proximidade$Distancia)

## melhor modelo MPC
Tamanho <- glmmTMB(Num_Ind ~  Soma_inseto +
                     Dist_cent_bord + Soma_FAI + 
                     Est_reg + Hora_bloco,data = dados, zi=~0, disp= ~1,
                   family = nbinom2(link = "log"), na.action = "na.fail")
summary(Tamanho)

## melhor modelo Centroide
Discent3B <- glmmTMB(Beta ~ Individuo + Soma_fruta + 
                       Soma_FAI + Pluviosidade + Soma_Insetos + 
                       Dist_borda +Dist_borda +
                       Est_reg + Comportamento + Estrato,zi=~1, disp= ~1,
                     data = centroide, family = nbinom2(link = "log"),na.action = "na.fail")
summary(Discent3B)

## melhor modelo distancia
Distind4 <- glmmTMB(Distancia ~ Diade + Dist_borda + Soma_FAI*Soma_Inseto +
                      Estrato + Comportamento + (1|Varredura), 
                    data = proximidade,
                    family = nbinom2(link = "log"), na.action = "na.fail")
summary(Distind4)

#escolhendo melhor modelo - repetir para cada análise
model1.set <- dredge(Tamanho, REML = FALSE)
model1.top <- get.models(model1.set, subset = delta < 2)
model1.avg <- model.avg(model1.top)
summary(model1.avg)

## Seleção de modelos - repetir para cada análise
## AICc = AIC corigido
ICtab(AreaPoliTBM1,AreaPoliTBM4,AreaPoliTBM3, type = c("AICc"), weights = TRUE)

## gráfico de densidade - repetir para cada análise

ggplot(dados, aes(x = Num_Ind, y = Hora_bloco )) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Número de indivíduos do grupo",
       y = "Período do dia", fill = "Nº Ind.", title = "Histogramas de Densidade")+ 
  theme_minimal()

## testes  PostHoc - repetir para cada análise
comparaD <- glht(Distind4, linfct = mcp(Diade = "Tukey")) ## compara os grupos
summary(comparaD)
contrast(emmeans(Distind4, specs="Comportamento"))
glmmTMB:::Anova.glmmTMB(Distind4) ## informa a variável de maior peso nos modelos

