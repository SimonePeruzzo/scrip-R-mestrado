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

### Área MPC - produtividade
ProdMPC1 <- glmmTMB(AreaMPC ~  Inseto_1 +  FAI_1,
                        data = dados, zi=~0, disp= ~1,
                        family = nbinom2(link = "log"), na.action = "na.fail")
summary(ProdMPC1)
### Área MPC - área de risco potencial
RiscMPC <- glmmTMB(AreaMPC ~  Dist_cent_bord + Est_reg,
                    data = dados, zi=~0, disp= ~1,
                    family = nbinom2(link = "log"), na.action = "na.fail")
summary(RiscMPC)
comparaT <- glht(RiscMPC, linfct = mcp(Est_reg = "Tukey")) ## compara os grupos
summary(comparaT)

### Tamanho do grupo - produtividade
TamanhoProd <- glmmTMB(Num_Ind ~  Inseto_1 + FAI_1 
              ,data = dados, zi=~0, disp= ~1,
                   family = nbinom2(link = "log"), na.action = "na.fail")
summary(TamanhoProd)
### Tamanho do grupo - área de risco potencial
TamanhoRisc <- glmmTMB(Num_Ind ~  Dist_cent_bord + Est_reg  
                       ,data = dados, zi=~0, disp= ~1,
                       family = nbinom2(link = "log"), na.action = "na.fail")
summary(TamanhoRisc)
comparaTR <- glht(TamanhoRisc, linfct = mcp(Est_reg = "Tukey")) ## compara os grupos
summary(comparaTR)

## Proximidade - faixa sexo/etária
Distind4 <- glmmTMB(Distancia ~  Individuo_1 + (1|Individuo_2) + (1|Comportamento) + (1|Varredura), 
                    data = proximidade,
                    family = nbinom2(link = "log"), na.action = "na.fail")
summary(Distind4)
comparaDist <- glht(Distind4, linfct = mcp(Individuo_1 = "Tukey")) ## compara os grupos
summary(comparaDist)
## Proximidade - área de risco potencial
Distind5 <- glmmTMB(Distancia ~  Individuo_1 + (1|Individuo_2) + Dist_borda + Est_reg
                    + (1|Varredura) + Estrato, 
                    data = proximidade,
                    family = nbinom2(link = "log"), na.action = "na.fail")
summary(Distind5)
comparaDist1 <- glht(Distind5, linfct = mcp(Est_reg = "Tukey")) ## compara os grupos
summary(comparaDist1)
comparaDist2 <- glht(Distind5, linfct = mcp(Estrato = "Tukey")) ## compara os grupos
summary(comparaDist2)
## Centroide  - produtividade
Discent3 <- glmmTMB(Dist_centr ~ Individuo + 
                      FAI_1 +Insetos_1 + Fruta_1+(1|Comportamento)
                      ,zi=~1, disp= ~1,
                    data = centroide, family = nbinom2(link = "log"),na.action = "na.fail")
summary(Discent3)
## Centroide  - área de risco potencial
Discent4 <- glmmTMB(Dist_centr ~ Individuo + 
                      Dist_borda + Estrato + Est_reg+(1|Comportamento)
                    ,zi=~1, disp= ~1,
                    data = centroide, family = nbinom2(link = "log"),na.action = "na.fail")
summary(Discent4)
comparaDist3 <- glht(Discent4, linfct = mcp(Est_reg = "Tukey")) ## compara os grupos
summary(comparaDist3)
comparaDist4 <- glht(Discent4, linfct = mcp(Estrato = "Tukey")) ## compara os grupos
summary(comparaDist4)

### testes de Simpson, Shannon e Pleiou

## valores menores que 2 significam baixa diversidade
Shannon <- diversity(diversidade)
## indica a homogeniedade - valores mais proximos a 1 = população mais homogenea
Pielou <- Shannon/log(specnumber(diversidade)) 
## indice de dominância - quanto maior o indice menor a diversidade - proximo a 1 uma especie esta dominando
Simpson <- diversity(diversidade, "simpson")
Simpson

### Gráficos de densidade
## número de indivíduos no grupo
Num_Isn <- ggplot(dados, aes(x = Num_Ind, y = Inseto_1)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Número de indivíduos no grupo",
       y = "Produtividade de insetos (Kg/ha)",fill = "Nº Ind")+ 
  theme_minimal()
Num_Est <- ggplot(dados, aes(x = Num_Ind, y = Est_reg )) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Número de indivíduos no grupo",
       y = "Estágio de regeneração utilizado", fill = "Nº Ind")+ 
  theme_minimal()
Num_Isn + Num_Est  ## imprime os dois gráficos um ao lado do outro
## área do MPC
Area_ins <- ggplot(dados, aes(x = AreaMPC, y = Inseto_1)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Área MPC Ponderada (m2)",
       y = "Produtividade de insetos (Kg/ha)",fill = "m2")+ 
  theme_minimal()
Area_est <- ggplot(dados, aes(x = AreaMPC, y = Est_reg)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Área MPC Ponderada (m2)",
       y = "Estágio de regeneração utilizado",fill = "m2")+ 
  theme_minimal()
Area_ins + Area_est
## proximidade interindividual
Dist_est <- ggplot(proximidade, aes(x = Distancia, y = Est_reg)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Distância interindividual (m)",
       y = "Estágio de regeneração utilizado",fill = "m")+ 
  theme_minimal()
Dist_estra <- ggplot(proximidade, aes(x = Distancia, y = Estrato)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Distância interindividual (m)",
       y = "Substrato utilizado (1=Chão)",fill = "m")+ 
  theme_minimal()

Dist_est + Dist_estra
## proximidade ao centroide
Cent_ind <- ggplot(centroide, aes(x = Dist_centr, y = Individuo)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Distância em relação ao centroide (m)",
       y = "Classe sexo/etária",fill = "m")+ 
  theme_minimal()
Cent_ins <- ggplot(centroide, aes(x = Dist_centr, y = Insetos_1)) + 
  geom_density_ridges_gradient(aes(fill = ..x..)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")))+
  labs(x = "Distância em relação ao centroide (m)",
       y = "Produtividade de Insetos (kg/ha)",fill = "m")+ 
  theme_minimal()

Cent_ind + Cent_ins

#escolhendo melhor modelo - repetir para cada análise quando há muitas variáveis
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

