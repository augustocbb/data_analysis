######### Cabeçalho ##################
#Data : 19/05/2020
#Autor : Augusto Bastos
# Descrição : Analise e modelagem inicial das informações de receita dividido em 
# 
# 1) Card operadoras nacionais móvel (tipo móvel, complemento não é "posts" nem "MC")
# 
# 2) Card operadoras nacionais residecial (tipo residencial, complemento não é "posts" nem "MC" nem "provedores regionais")
# 
# 3) Card provedores regionais (complemento é "provedores regionais")
# 
# 4) Conteúdo MP (complemento é "posts")
# 
# 5) Conteúdo MC (complemento é "MC")
# 
# 6) Ads mc (complemento é "ads mc")
# 
# OBS: Adsense (complemento é "adsense") - esse último faz parte da receita mas não precisa estudar

############ Bibliotecas ##################
library(openxlsx)
install.packages("RH2")

library(RH2)
library(gtools)
library(readxl)
# For obtaining GDP data
library(WDI)

library(plm)
library(sqldf)

# For manipulating data
library(magrittr)

library(rprojroot)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# For descriptive statistics and graphing
library(skimr)
library(ggplot2)
library(scales)
library(gridExtra)
library(forecast)
library(fpp2)
library(readxl)

library(FitAR)

#############Contrução das bases ############################################

base_historica_consolidade_2016_a_2020 <- read_excel("C:/Users/Augusto/Desktop/mp/analise estatistica/acompanhamento de resultados/base historica consolidade 2016 a 2020 deflacionada 03_06.xlsx", 
                                                     col_types = c("date", "numeric", "numeric", 
                                                                   "numeric", "text", "text", "text", 
                                                                   "text", "text", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "text", "numeric", "numeric"))
base_historica <- base_historica_consolidade_2016_a_2020

base_historica2 <- base_historica_consolidade_2016_a_2020

base_historica <- 

nomes        <-         c( "data", 
                           "semana", 
                           "mes", 
                           "ano", 
                           "parceiro", 
                           "operadora", 
                           "tipo", 
                           "subtipo", 
                           "complemento", 
                           "lead", 
                           "cpl", 
                           "vendas", 
                           "cpa", 
                           "receita", 
                           "clickoff", 
                           "provedor regional", 
                           "observacao",
                           "receita_deflacionada",
                           "trimestre")

names(base_historica) = nomes
names(base_historica2) = nomes

base_historica2$data2 <- difftime(base_historica2$data, min(base_historica2$data), units = "days")

parceiro <- sqldf("select min (data2) as a, data, parceiro from base_historica2 group by parceiro order by a")
cod_parceiro <- seq(from = 1, to = nrow(parceiro), by = 1)
parceiro <- cbind(parceiro, cod_parceiro)

write.csv2(parceiro, "parceiro.csv")

operadora <- sqldf("select min (data2) as a, data, operadora from base_historica2 group by operadora order by a")
cod_operadora <- seq(from = 1, to = nrow(operadora), by = 1)
operadora <- cbind(operadora, cod_operadora)

write.csv2(operadora, "operadora.csv")

tipo <- sqldf("select min (data2) as a, data, tipo from base_historica2 group by tipo order by a")
cod_tipo <- seq(from = 1, to = nrow(tipo), by = 1)
tipo <- cbind(tipo, cod_tipo)

write.csv2(tipo, "tipo.csv")

subtipo <- sqldf("select min (data2) as a, data, subtipo from base_historica2 group by subtipo order by a")
cod_subtipo <- seq(from = 1, to = nrow(subtipo), by = 1)
subtipo <- cbind(subtipo, cod_subtipo)

write.csv2(subtipo, "subtipo.csv")

complemento <- sqldf("select min (data2) as a, data, complemento from base_historica2 group by complemento order by a")
cod_complemento <- seq(from = 1, to = nrow(complemento), by = 1)
complemento <- cbind(complemento, cod_complemento)

write.csv2(complemento, "complemento.csv")

observacao <- sqldf("select min (data2) as a, data, observacao from base_historica2 group by observacao order by a")
cod_observacao <- seq(from = 1, to = nrow(observacao), by = 1)
observacao <- cbind(observacao, cod_observacao)

write.csv2(observacao, "observacao.csv")



