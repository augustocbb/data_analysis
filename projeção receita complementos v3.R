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
library(ggplot2)
# For descriptive statistics and graphing
library(skimr)
library(ggplot2)
library(scales)
library(gridExtra)
library(forecast)
library(fpp2)
library(readxl)

library(FitAR)

###### Funções de ajuste de previsão do semanal para o mensal #####
base_de_dias <- read_excel("C:/Users/Augusto/Desktop/mp/analise estatistica/acompanhamento de resultados/base de dias.xlsx", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric"))

ajuste_previsao <-  function(base_de_dias, base_mensal, base_semanal, fit_receita_mensal, fit_receita_semanal, tipo_ajuste){
   
   
   
   base_dist_semana <-  sqldf("select semana, mes, ano, count(data) as a  from base_de_dias group by semana, mes, ano")
   
   base_dist_semana$porcao_semana_mes <- base_dist_semana$a/7
   
   base_dist_mes <-  sqldf(" select b.*, c.num_dias_mes from base_dist_semana as b left join 
                           (select mes, ano, count(data) as num_dias_mes  from base_de_dias group by  mes, ano) as c on b.ano = c.ano and b.mes = c.mes")
   
  
 
   
   
      
      previsao_mensal <- forecast(fit_receita_mensal, h = 12)
      previsao_mensal <- previsao_mensal$mean
      
      previsao_mensal <- as.data.frame( as.matrix( previsao_mensal), date = time(previsao_mensal))
       
      
   
   
   previsao_mensal$data <- seq.Date( from = (as.Date ((last(base_mensal$data) ) )), length.out  = nrow(previsao_mensal), by = "month" )
   previsao_mensal$mes <- (month(previsao_mensal$data) )
   previsao_mensal$ano <- (year(previsao_mensal$data))
   
   names(previsao_mensal) <- c("previsao", "data", "mes", "ano")
      
      previsao_semanal <-forecast(fit_receita_semanal, h = 12)
      previsao_semanal <- previsao_semanal$mean
      
      previsao_semanal <- as.data.frame( as.matrix( previsao_semanal), date = time(previsao_semanal))
      
    
      
   
   
   previsao_semanal$data <- seq.Date( from = (as.Date ((last(base_semanal$data) ) )), length.out  = nrow(previsao_semanal), by = "week" )
   previsao_semanal$semana <- (week(previsao_semanal$data) )
   previsao_semanal$ano <- (year(previsao_semanal$data))
   
   names(previsao_semanal) <- c("previsao", "data", "semana", "ano")
   
   base_ajust_semana <- sqldf("select b.porcao_semana_mes, b.mes, c.* from   previsao_semanal as c
      left join base_dist_semana as b on b.semana = c.semana and b.mes = b.mes and b.ano = c.ano
                           group by b.semana, b.mes, b.ano")
   
   base_ajust_semana$forecast_semana_ajust <- base_ajust_semana$porcao_semana_mes*base_ajust_semana$previsao
   
   
   base_ajust_semana <- sqldf("select b.*, c.soma_mes  from base_ajust_semana as b left join 
                            (select sum(forecast_semana_ajust) as soma_mes, mes, ano 
                            from base_ajust_semana 
                            group by mes, ano) as c on b.mes = c.mes and b.ano = c.ano")
   
   base_ajust_semana <- sqldf("select x.*, y.num_dias_mes, y.num_dias_mes_previsao from  base_ajust_semana as x left join 
   (select b.*, c.num_dias_mes, c.a, sum(c.a) as num_dias_mes_previsao from base_ajust_semana as b
                              left join base_dist_mes as c on b.mes = c.mes and b.ano = c.ano and b.semana = c.semana
                              group by b.mes, b.ano) as y on x.mes = y.mes and x.ano = y.ano")
   
   base_ajust_semana <- sqldf("select a.*, b.previsao as prev_mes
from base_ajust_semana as a left join previsao_mensal  as b
                            on a.mes = b.mes and a.ano = b.ano")
   
   base_ajust_semana$previsao_ajustada_semana <-    base_ajust_semana$prev_mes*
      (base_ajust_semana$num_dias_mes_previsao/base_ajust_semana$num_dias_mes) /
      base_ajust_semana$soma_mes*base_ajust_semana$porcao_semana_mes*
      base_ajust_semana$previsao
   base_ajust_mes <- sqldf("select data, mes, ano, soma_mes from base_ajust_semana group by mes, ano")
   
   
   
   
   if(tipo_ajuste == "semanal"){
      x <- as.data.frame(cbind(base_ajust_semana$data,base_ajust_semana$semana, base_ajust_semana$mes,
            base_ajust_semana$ano, base_ajust_semana$previsao_ajustada_semana) )
      names(x)<- c("data", "semana", "mes", 'ano', "previsao")
      
      return( x )
   }
   else {
    x <-  as.data.frame( cbind(base_ajust_mes$data, 
                               base_ajust_mes$mes, base_ajust_mes$ano, base_ajust_mes$soma_mes))
    names(x)<- c("data", "mes", 'ano', "previsao")
      return( x )
   
   }
   
   
   
}

reparticao_semana <- function(base_semanal, base_de_dias,  fit_receita_semanal){
   
   previsao_semanal <-forecast(fit_receita_semanal, h = 12)
   previsao_semanal <- previsao_semanal$mean
   
   previsao_semanal <- as.data.frame( as.matrix( previsao_semanal), date = time(previsao_semanal))
   
   
   
   
   
   previsao_semanal$data <- seq.Date( from = (as.Date ((last(base_semanal$data) ) )), length.out  = nrow(previsao_semanal), by = "week" )
   previsao_semanal$semana <- (week(previsao_semanal$data) )
   previsao_semanal$ano <- (year(previsao_semanal$data))
   
   names(previsao_semanal) <- c("previsao", "data", "semana", "ano")
   
   base_ajust_semana <- sqldf("select b.porcao_semana_mes, b.mes, c.* from   previsao_semanal as c
      left join base_dist_semana as b on b.semana = c.semana and b.mes = b.mes and b.ano = c.ano
                           group by b.semana, b.mes, b.ano")
   
   base_ajust_semana$forecast_semana_ajust <- base_ajust_semana$porcao_semana_mes*base_ajust_semana$previsao
   
   
   
   x <- as.data.frame(cbind(base_ajust_semana$data,base_ajust_semana$semana, base_ajust_semana$mes,
                            base_ajust_semana$ano, base_ajust_semana$forecast_semana_ajust) )
   names(x)<- c("data", "semana", "mes", 'ano', "previsao")
   
   return(x)
}

reparticao_mes <- function(base_mensal, base_de_dias,  fit_receita_mensal){
   
   previsao_mensal <- forecast(fit_receita_mensal, h = 12)
   previsao_mensal <- previsao_mensal$mean
   
   previsao_mensal <- as.data.frame( as.matrix( previsao_mensal), date = time(previsao_mensal))
   
   
   
   
   previsao_mensal$data <- seq.Date( from = (as.Date ((last(base_mensal$data) ) )), length.out  = nrow(previsao_mensal), by = "month" )
   previsao_mensal$mes <- (month(previsao_mensal$data) )
   previsao_mensal$ano <- (year(previsao_mensal$data))
   
   names(previsao_mensal) <- c("previsao", "data", "mes", "ano")
   
   return(previsao_mensal)
   
}

#############Contrução das bases ############################################

base_historica_consolidade_2016_a_2020 <- read_excel("C:/Users/Augusto/Desktop/mp/analise estatistica/acompanhamento de resultados/base historica consolidade 2016 a 2020 deflacionada 01_07.xlsx", 
                                                     col_types = c("date", "numeric", "numeric", 
                                                                   "numeric", "text", "text", "text", 
                                                                   "text", "text", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "text", "numeric", "numeric"))
base_historica <- base_historica_consolidade_2016_a_2020

base_historica2 <- base_historica_consolidade_2016_a_2020



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


################# Analise Semanal ####################
##### 1) Card operadoras nacionais móvel #############


#ajuste do modelo para coeficientes de cada semana

base_semanal_on_movel <- sqldf("select trimestre,  mes, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento not in ('posts', 'mc')
      and tipo = 'mobile'
      and semana is not null 
           group by semana, ano
           order by data
")




receita_semanal_on_movel <- ts(data = base_semanal_on_movel$receita[1:(nrow(base_semanal_on_movel)-1)], start = c(2018, 22), end = c(2020, 27 ), frequency = 52)
receita_semanal_on_movel1 <- ts(data = base_semanal_on_movel$receita[1:(nrow(base_semanal_on_movel)-1)], start = c(1, 8), end = c(9, 13 ), frequency = 13)

plot(receita_semanal_on_movel)
plot(receita_semanal_on_movel1)


fit_receita_semanal_on_movel <-  auto.arima(receita_semanal_on_movel, stepwise=FALSE, 
                                             approximation=FALSE,  method = "ML", parallel = TRUE)

fit_receita_semanal_on_movel1 <-  auto.arima(receita_semanal_on_movel1, stepwise=FALSE, 
                                            approximation=FALSE,  method = "ML", parallel  =TRUE)

# 
 fit_receita_semanal_on_movel2 <-  hw(receita_semanal_on_movel1, seasonal = 'additive', PI=F)
 fit_receita_semanal_on_movel3 <-  hw(receita_semanal_on_movel1, seasonal = 'multiplicative', PI=F)
 
plot(forecast(fit_receita_semanal_on_movel, h = 12))
plot(forecast(fit_receita_semanal_on_movel1, h = 12))
plot(forecast(fit_receita_semanal_on_movel2, h = 12))
plot(forecast(fit_receita_semanal_on_movel3, h = 12))


checkresiduals(fit_receita_semanal_on_movel)
checkresiduals(fit_receita_semanal_on_movel1)
checkresiduals(fit_receita_semanal_on_movel2)
checkresiduals(fit_receita_semanal_on_movel3)



#residuoes melhores no autoarima porem o modelo resultante nao absorve bem a sazonalidade
# Conclusao usar o modelo 3

fit_receita_semanal_on_movel <- fit_receita_semanal_on_movel3

##### 2) Card operadoras nacionais residecial ###############
#(tipo residencial, complemento não é "posts" nem "MC" nem "provedores regionais")
base_semanal_on_residencial <- sqldf("select trimestre, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento not in ('posts', 'mc', 'provedor regional')
      and tipo = 'residencial'
      and semana is not null
           group by semana, ano
           order by data
")

View(base_semanal_on_residencial)


receita_semanal_on_residencial1 <- ts(data = base_semanal_on_residencial$receita[1:(nrow(base_semanal_on_residencial)-1)], start = c(1, 8), end = c(9, 13 ), frequency = 13)
receita_semanal_on_residencial <- ts(data = base_semanal_on_residencial$receita[1:(nrow(base_semanal_on_residencial)-1)], start = c( 2018, 22), end = c( 2020, 27 ), frequency = 52)

plot(receita_semanal_on_residencial)
plot(receita_semanal_on_residencial1)

fit_receita_semanal_on_residencial <-  auto.arima(receita_semanal_on_residencial, stepwise=FALSE, 
                                                  approximation=FALSE,  method = "ML", parallel = TRUE)

fit_receita_semanal_on_residencial1 <-  auto.arima(receita_semanal_on_residencial1, stepwise=FALSE, 
                                                   approximation=FALSE,  method = "ML", parallel  =TRUE)

# 
fit_receita_semanal_on_residencial2 <-  hw(receita_semanal_on_residencial1, seasonal = 'additive', PI=F)
fit_receita_semanal_on_residencial3 <-  hw(receita_semanal_on_residencial1, seasonal = 'multiplicative', PI=F)



plot(forecast(fit_receita_semanal_on_residencial, h = 12))
plot(forecast(fit_receita_semanal_on_residencial1, h = 12))
plot(forecast(fit_receita_semanal_on_residencial2, h = 12))
plot(forecast(fit_receita_semanal_on_residencial3, h = 12))


checkresiduals(fit_receita_semanal_on_residencial)
checkresiduals(fit_receita_semanal_on_residencial1)
checkresiduals(fit_receita_semanal_on_residencial2)
checkresiduals(fit_receita_semanal_on_residencial3)
fit_receita_semanal_on_residencial <- fit_receita_semanal_on_residencial3

# 
# o modelo 3 foi o unico que passou no pacf e tem melhor sazonalidade


##### 3) Card provedores regionais ##############
#(complemento é "provedores regionais")
base_semanal_provedor_regional <- sqldf("select trimestre, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'provedor regional'
      and semana is not null
           group by semana, ano
           order by data
")





receita_semanal_provedor_regional <- ts(data = base_semanal_provedor_regional$receita[1:(nrow(base_semanal_provedor_regional)-2)]
                                        , start = c(2019, 13), end = c(2020, 25 ), frequency = 52)

receita_semanal_provedor_regional1 <- ts(data = base_semanal_provedor_regional$receita[1:(nrow(base_semanal_provedor_regional)-2)], 
                                        start = c(1, 1), end = c(5, 12), frequency = 13)
plot(receita_semanal_provedor_regional)
plot(receita_semanal_provedor_regional1)

fit_receita_semanal_provedor_regional <-  auto.arima(receita_semanal_provedor_regional, stepwise=FALSE, 
                                                     approximation=FALSE,  method = "ML", parallel = TRUE)

fit_receita_semanal_provedor_regional1 <-  auto.arima(receita_semanal_provedor_regional1, stepwise=FALSE, 
                                                      approximation=FALSE,  method = "ML", parallel  =TRUE)

# 
fit_receita_semanal_provedor_regional2 <-  hw(receita_semanal_provedor_regional1, seasonal = 'additive', PI=F)
fit_receita_semanal_provedor_regional3 <-  hw(receita_semanal_provedor_regional1, seasonal = 'multiplicative', PI=F)



plot(forecast(fit_receita_semanal_provedor_regional, h = 12))
plot(forecast(fit_receita_semanal_provedor_regional1, h = 12))
plot(forecast(fit_receita_semanal_provedor_regional2, h = 12))
plot(forecast(fit_receita_semanal_provedor_regional3, h = 12))


checkresiduals(fit_receita_semanal_provedor_regional)
checkresiduals(fit_receita_semanal_provedor_regional1)
checkresiduals(fit_receita_semanal_provedor_regional2)
checkresiduals(fit_receita_semanal_provedor_regional3)

# O modelo multiplicativo continua sendo o maior devivo ao forte traço de sazonalidade do modelo
# Conclusão modelo 3

fit_receita_semanal_provedor_regional <- fit_receita_semanal_provedor_regional3

##### 4) Conteúdo MP (complemento é "posts")#####################

base_semanal_posts <- sqldf("select trimestre, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'posts'
      and semana is not null
           group by semana, ano
           order by data
")



receita_semanal_posts <- ts(
   data = base_semanal_posts$receita[1:(nrow(base_semanal_posts)-1)], 
   start = c(2019, 13), end = c(2020, 26), frequency = 52)

receita_semanal_posts1 <- ts(
   data = base_semanal_posts$receita[1:(nrow(base_semanal_posts)-1)], 
   start = c(1, 13), end = c(6, 13 ), frequency = 13)

fit_receita_semanal_posts <-  auto.arima(receita_semanal_posts, stepwise=FALSE, 
                                         approximation=FALSE,  method = "ML", parallel = TRUE)

fit_receita_semanal_posts1 <-  auto.arima(receita_semanal_posts1, stepwise=FALSE, 
                                          approximation=FALSE,  method = "ML", parallel  =TRUE)

# 
fit_receita_semanal_posts2 <-  hw(receita_semanal_posts1, seasonal = 'additive', PI=F)
fit_receita_semanal_posts3 <-  hw(receita_semanal_posts1, seasonal = 'multiplicative', PI=F)



plot(forecast(fit_receita_semanal_posts, h = 12))
plot(forecast(fit_receita_semanal_posts1, h = 12))
plot(forecast(fit_receita_semanal_posts2, h = 12))
plot(forecast(fit_receita_semanal_posts3, h = 12))


checkresiduals(fit_receita_semanal_posts)
checkresiduals(fit_receita_semanal_posts1)
checkresiduals(fit_receita_semanal_posts2)
checkresiduals(fit_receita_semanal_posts3)

# 
# modelo 3 adequou aos residuos e os auto arimas entraram em passeio aleatorio

fit_receita_semanal_posts <- fit_receita_semanal_posts3

##### 5) Conteúdo MC (complemento é "MC") ########################
base_semanal_mc <- sqldf("select trimestre, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'mc'
      
           group by semana, ano
           order by data
")




receita_semanal_mc <- ts(
   data = base_semanal_mc$receita[1:(nrow(base_semanal_mc)-1)], 
   start = c(2019, 13), end = c(2020, 26), frequency = 52)

receita_semanal_mc1 <- ts(
   data = base_semanal_mc$receita[1:(nrow(base_semanal_mc)-1)], 
   start = c(1, 13), end = c(6, 13 ), frequency = 13)


plot(receita_semanal_mc)
plot(receita_semanal_mc1)


fit_receita_semanal_mc <-  auto.arima(receita_semanal_mc, stepwise=FALSE, 
                                      approximation=FALSE,  method = "ML", parallel = TRUE)

fit_receita_semanal_mc1 <-  auto.arima(receita_semanal_mc1, stepwise=FALSE, 
                                       approximation=FALSE,  method = "ML", parallel  =TRUE)

# 
fit_receita_semanal_mc2 <-  hw(receita_semanal_mc1, seasonal = 'additive', PI=F)
fit_receita_semanal_mc3 <-  hw(receita_semanal_mc1, seasonal = 'multiplicative', PI=F)



plot(forecast(fit_receita_semanal_mc, h = 12))
plot(forecast(fit_receita_semanal_mc1, h = 12))
plot(forecast(fit_receita_semanal_mc2, h = 12))
plot(forecast(fit_receita_semanal_mc3, h = 12))


checkresiduals(fit_receita_semanal_mc)
checkresiduals(fit_receita_semanal_mc1)
checkresiduals(fit_receita_semanal_mc2)
checkresiduals(fit_receita_semanal_mc3)
# 
# Os modelos auto arima encontraram uma tendencia linear
# o modelo hw additive pareceu o mais coerente pois suaviza mais o efeito sazonal,
# porém não passou no Pacf dessa forma foi usado o multiplicativo

fit_receita_semanal_mc <- fit_receita_semanal_mc3

##### 6) Ads mc (complemento é "ads mc")##################

base_semanal_ads_mc <- sqldf("select trimestre, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'ads mc'
      
           group by semana, ano
           order by data
")

receita_semanal_ads_mc <- ts(
   data = base_semanal_ads_mc$receita[1:(nrow(base_semanal_ads_mc)-1)], 
   start = c(2019, 45), end = c(2020, 27), frequency = 52)

receita_semanal_ads_mc1 <- ts(
   data = base_semanal_ads_mc$receita[1:(nrow(base_semanal_ads_mc)-1)], 
   start = c(1, 6), end = c(3, 13 ), frequency = 13)

plot(receita_semanal_ads_mc)
plot(receita_semanal_ads_mc1)


fit_receita_semanal_ads_mc <-  auto.arima(receita_semanal_ads_mc, stepwise=FALSE, 
                                          approximation=FALSE,  method = "ML", parallel = TRUE)

fit_receita_semanal_ads_mc1 <-  auto.arima(receita_semanal_ads_mc1, stepwise=FALSE, 
                                           approximation=FALSE,  method = "ML", parallel  =TRUE)

# 
fit_receita_semanal_ads_mc2 <-  hw(receita_semanal_ads_mc1, seasonal = 'additive', PI=F)
fit_receita_semanal_ads_mc3 <-  hw(receita_semanal_ads_mc1, seasonal = 'multiplicative', PI=F)



plot(forecast(fit_receita_semanal_ads_mc, h = 12))
plot(forecast(fit_receita_semanal_ads_mc1, h = 12))
plot(forecast(fit_receita_semanal_ads_mc2, h = 12))
plot(forecast(fit_receita_semanal_ads_mc3, h = 12))


checkresiduals(fit_receita_semanal_ads_mc)
checkresiduals(fit_receita_semanal_ads_mc1)
checkresiduals(fit_receita_semanal_ads_mc2)
checkresiduals(fit_receita_semanal_ads_mc3)


##### 7) Adsense #########################


base_semanal_adsense <- sqldf("select trimestre, data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'adsense'
      
           group by semana, ano
           order by data
")

base_semanal_adsense$semana_mes <- base_semanal_adsense$semana%%4




receita_semanal_adsense <- ts(data = base_semanal_adsense$receita, start = c(1, 6), end = c(3, 13 ), frequency = 13)




fit_receita_semanal_adsense <- predict(lm( receita ~ semana_mes + semana + ano  , base_semanal_adsense))
 fit_receita_semanal_adsense <-  hw(receita_semanal_adsense, seasonal = 'multiplicative', PI=F)
 fit_receita_semanal_adsense <-  auto.arima(receita_semanal_adsense, max.order = 13, lambda = "auto")


plot(forecast(fit_receita_semanal_adsense, h = 13))
checkresiduals(fit_receita_semanal_adsense)



################# Analise Mensal ####################
##### 1) Card operadoras nacionais móvel #############


base_mensal_on_movel <- sqldf("select trimestre,  data, mes,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento not in ('posts', 'mc')
      and tipo = 'mobile'
      and semana is not null 
      
           group by mes, ano
           order by data
")
# 
# x <- lm(receita ~ mes + ano,base_mensal_on_movel)
# x2 <- predict.lm(x)
# y <- forecast(x2)
# 
# plot( base_mensal_on_movel$receita)
# lines(x2)
# plot(receita_mensal_on_movel)
# checkresiduals(x)

base_mensal_on_movel <- base_mensal_on_movel 
receita_mensal_on_movel <- ts(data = base_mensal_on_movel$receita, start = c(1, 1), end = c(10, 3 ), frequency = 3)

# fit_receita_mensal_on_movel <-  auto.arima(receita_mensal_on_movel, max.order = 12)
 fit_receita_mensal_on_movel <-  hw(receita_mensal_on_movel, seasonal = 'additive', stepwise=FALSE, 
                                    approximation=FALSE , method = "ML", PI=F)
fit_receita_mensal_on_movel1 <-  hw(receita_mensal_on_movel, seasonal = 'multiplicative', stepwise=FALSE, 
                                   approximation=FALSE , method = "ML", PI=F)

 k1 <- cbind(forecast(fit_receita_mensal_on_movel1)$mean, 
            forecast(fit_receita_mensal_on_movel)$meanl)
 autoplot(k1)
# k1
# plot(forecast(fit_receita_mensal_on_movel1), forecast(fit_receita_mensal_on_movel))
# usar hw o auto arima esta dando paseio aleatorio multiplicative ficou um pouquinho menor a previsao, mas quase igual
 # fit_receita_mensal_on_movel <- auto.arima(receita_mensal_on_movel,stepwise=FALSE, 
 #                                           approximation=FALSE,  method = "ML" )

plot(forecast(fit_receita_mensal_on_movel, h = 3))
plot(forecast(fit_receita_mensal_on_movel1, h = 3))
forecast(fit_receita_mensal_on_movel, h = 3)
forecast(fit_receita_mensal_on_movel1, h = 3)

base_mensal_on_residencial
checkresiduals(fit_receita_mensal_on_movel)



##### 2) Card operadoras nacionais residecial ###############
#(tipo residencial, complemento não é "posts" nem "MC" nem "provedores regionais")
base_mensal_on_residencial <- sqldf("select trimestre,  data, mes,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento not in ('posts', 'mc', 'provedor regional')
      and tipo = 'residencial'
      and semana is not null 
           group by mes, ano
           order by data
")



receita_mensal_on_residencial <- ts(data = base_mensal_on_residencial$receita[1:(nrow(base_mensal_on_residencial)-1)], start = c(1, 1), end = c(8, 3 ), frequency = 3)

#holt-winters

# fit_receita_mensal_on_residencial1 <-  hw(receita_mensal_on_residencial, seasonal = 'additive', stepwise=FALSE, 
#                                           approximation=FALSE , method = "ML",  PI=F)
#  plot(forecast(fit_receita_mensal_on_residencial1, h = 3))
# checkresiduals(fit_receita_mensal_on_residencial1)
# summary(fit_receita_mensal_on_residencial2)
# 
# #Arima residuos normais, projeção com acf e pacf bom
# receita_mensal_on_residencial

fit_receita_mensal_on_residencial <-  auto.arima(receita_mensal_on_residencial, stepwise=FALSE, 
                                                  approximation=FALSE,  method = "ML", parallel = TRUE)

acf(fit_receita_mensal_on_residencial$residuals)
plot(forecast(fit_receita_mensal_on_residencial, h = 3))
checkresiduals(fit_receita_mensal_on_residencial)
fit_receita_mensal_on_residencial <- fit_receita_mensal_on_residencial
shapiro.test(residuals(fit_receita_mensal_on_residencial))


##### 3) Card provedores regionais ##############
#(complemento é "provedores regionais")
base_mensal_provedor_regional <- sqldf("select trimestre, data, mes, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'provedor regional' 
      and semana is not null 
           group by mes, ano
           order by data
")


receita_mensal_provedor_regional <- ts(data = base_mensal_provedor_regional$receita[1:(nrow(base_mensal_provedor_regional)-1)], start = c(1,3), end = c(6, 3  ), frequency = 3)

fit_receita_mensal_provedor_regional <-  auto.arima(receita_mensal_provedor_regional,stepwise=FALSE, 
                                                    approximation=FALSE,  method = "ML")
fit_receita_mensal_provedor_regional1 <-  hw(receita_mensal_provedor_regional, stepwise=FALSE, 
                                            approximation=FALSE,  method = "ML", seasonal = 'additive')

# hw m ruim, modelo de passeio aleatorio
# fit_receita_mensal_provedor_regional2 <-  hw(receita_mensal_provedor_regional, stepwise=FALSE, 
#                                              approximation=FALSE,  method = "ML", seasonal = 'multiplicative')

g1 <- cbind(residuals(fit_receita_mensal_provedor_regional)
            , residuals(fit_receita_mensal_provedor_regional1)
            )
autoplot(g1)



plot(forecast(fit_receita_mensal_provedor_regional, h = 3))
checkresiduals(fit_receita_mensal_provedor_regional)
checkresiduals(fit_receita_mensal_provedor_regional1)


##### 5) Conteúdo MC (complemento é "MC") ########################
##### 4) Conteúdo MP (complemento é "posts")#####################


base_mensal_posts  <- sqldf("select trimestre, data, mes, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'posts'
      and semana is not null
           group by mes, ano
           order by data
")


receita_mensal_posts <- ts(data = base_mensal_posts$receita, start = c(1,3), end = c(6, 3 ), frequency = 3)
# hw aditivo ficou melhor, residuos ficaram reais porem quae nao ficaram normais no shapiro test


 fit_receita_mensal_posts <-  hw(receita_mensal_posts, seasonal = 'additive',method = "ML", stepwise = FALSE, 
                                 approximation = FALSE, PI=F)
  fit_receita_mensal_posts1 <-  hw(receita_mensal_posts, seasonal = 'multiplicative',method = "ML", stepwise = FALSE, 
                                 approximation = FALSE, PI=F)
 # 
 # modelo de tendencia linear e passeio aleatorio
#    fit_receita_mensal_posts <-  auto.arima(receita_mensal_posts, method = "ML", stepwise = FALSE, 
#                                           approximation = FALSE, max.order = 50)
# plot(receita_mensal_posts)


g1 <- cbind(residuals(fit_receita_mensal_posts)
            , residuals(fit_receita_mensal_posts1)
)
autoplot(g1)



plot(forecast(fit_receita_mensal_posts, h = 3))
plot(forecast(fit_receita_mensal_posts1, h = 3))

checkresiduals(fit_receita_mensal_posts1)
shapiro.test(residuals(fit_receita_mensal_posts))

base_mensal_mc <- sqldf("select trimestre, mes,  data, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'mc' 
      
           group by mes, ano
           order by data
")


receita_mensal_mc <- ts(data = base_mensal_mc$receita[1:(nrow(base_mensal_mc)-1)], start = c(1,3) , end = c(6, 3 ), frequency = 13)


# auto arima ficou um pouco melhor no acf, residupos ficaram melhores embora nao normais
fit_receita_mensal_mc <-  hw(receita_mensal_mc, seasonal = 'additive', PI=F,method = "ML", stepwise = FALSE, 
                             approximation = FALSE)
#  fit_receita_mensal_mc1 <-  hw(receita_mensal_mc, seasonal = 'additive', PI=F,method = "ML", stepwise = FALSE, 
#                               approximation = FALSE)
# # 
# 
fit_receita_mensal_mc1 <-  auto.arima(receita_mensal_mc,  method = "ML", stepwise = FALSE, 
                                      approximation = FALSE, max.order = 50)



g1 <- cbind(residuals(fit_receita_mensal_mc)
            , residuals(fit_receita_mensal_mc1)
)
autoplot(g1)


shapiro.test(residuals(fit_receita_mensal_mc))
plot(forecast(fit_receita_mensal_mc, h = 3))
plot(forecast(fit_receita_mensal_mc1, h = 3))
forecast(fit_receita_mensal_mc, h = 3)
forecast(fit_receita_mensal_mc1, h = 3)
checkresiduals(fit_receita_mensal_mc)
summary(fit_receita_mensal_mc)

fit_receita_mensal_mc <- fit_receita_mensal_mc1

##### 6) Ads mc (complemento é "ads mc")##################

base_mensal_ads_mc <- sqldf("select trimestre, data, mes, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'ads mc'
      
           group by mes, ano
           order by data
")







##### 7) Adsense (complemento é "adsense") #######




base_mensal_adsense <- sqldf("select trimestre, data, mes, semana,  ano, sum(receita) as receita,
sum(receita_deflacionada) as receita_deflacionada, sum(clickoff) as clickoff
      from base_historica2
      where complemento = 'adsense'
      
           group by mes, ano
           order by data
")


base_mensal_adsense$semana_mes <- base_mensal_adsense$semana%%4


base_mensal_adsense <- base_mensal_adsense[1:6,]
fit_receita_mensal_adsense <- predict(
   lm( receita ~ semana_mes + semana + ano  , base_mensal_adsense))


receita_mensal_adsense <- ts(data = base_mensal_adsense$receita, start = c(1,2), end = c(8, 3 ), frequency = 3)

 fit_receita_mensal_adsense <-  hw(receita_mensal_adsense, seasonal = 'multiplicative', PI=F, stepwise = false, approximation = FALSE)
 # fit_receita_mensal_adsense <-  auto.arima(receita_mensal_adsense, max.order = 13, lambda = "auto")


plot(forecast(fit_receita_mensal_adsense, h = 3))
checkresiduals(fit_receita_mensal_adsense)






################ Exportando projeções ##########

a <- ls(pattern="fit")


results <- list()
results[["fit_receita_mensal_ads_mc"]] <- forecast(fit_receita_mensal_ads_mc, h=12)
results[["fit_receita_mensal_adsense"]] <- forecast(fit_receita_mensal_adsense, h=12)
results[["fit_receita_mensal_mc"]] <- forecast(fit_receita_mensal_mc, h=12)
results[["fit_receita_mensal_on_movel"]] <- predict(fit_receita_mensal_on_movel, h=12)
results[["fit_receita_mensal_on_residencial"]] <- forecast(fit_receita_mensal_on_residencial1, h=12)
results[["fit_receita_mensal_posts"]] <- forecast(fit_receita_mensal_posts, h=12)
results[["fit_receita_mensal_provedor_regional"]] <- forecast(fit_receita_mensal_provedor_regional)
results[["fit_receita_semanal_ads_mc"]] <- forecast(fit_receita_semanal_ads_mc, h=12)
results[["fit_receita_semanal_adsense"]] <- forecast(fit_receita_semanal_adsense, h=12, level = c(20, 40, 80))
results[["fit_receita_semanal_mc"]] <- forecast(fit_receita_semanal_mc, h=12)
results[["fit_receita_semanal_on_movel"]] <- forecast(fit_receita_semanal_on_movel, h=12, level = c(20, 40, 80))
results[["fit_receita_semanal_on_residencial"]] <- forecast(fit_receita_semanal_on_residencial, h=12)
results[["fit_receita_semanal_posts"]] <- forecast(fit_receita_semanal_posts, h=12)
results[["fit_receita_semanal_provedor_regional"]] <- forecast(fit_receita_semanal_provedor_regional, h=12)


results_together <- do.call(smartbind,lapply(names(results),function(x){
   transform(as.data.frame(results[[x]]), Name = x)
}))

wb <- createWorkbook()

addWorksheet(wb, "Forecasts")
writeData(wb, "Forecasts", results_together, rowNames = TRUE)
saveWorkbook(wb, "Forcasts.xlsx", overwrite = TRUE)


# 


######## Ajustes para semanal e mensal #########


# 
# fit_receita_mensal_ads_mc	s

ajust_receita_mensal_ads_mc <- ajuste_previsao(base_de_dias = base_de_dias, base_mensal = base_mensal_ads_mc, base_semanal = base_semanal_ads_mc, fit_receita_semanal = fit_receita_semanal_ads_mc,
                                               fit_receita_mensal = fit_receita_mensal_ads_mc, tipo_ajuste = "semanal")
# fit_receita_mensal_adsense	s
ajust_receita_mensal_adsense <- ajuste_previsao(base_de_dias = base_de_dias,
                                                base_mensal = base_mensal_adsense, base_semanal = base_semanal_adsense,
                                                fit_receita_semanal = fit_receita_semanal_adsense,
                                                fit_receita_mensal = fit_receita_mensal_adsense, tipo_ajuste = "semanal")
# fit_receita_mensal_mc	s
ajust_receita_mensal_mc <- ajuste_previsao(base_de_dias = base_de_dias,
                                           base_mensal = base_mensal_mc, base_semanal = base_semanal_mc,
                                           fit_receita_semanal = fit_receita_semanal_mc,
                                           fit_receita_mensal = fit_receita_mensal_mc, tipo_ajuste = "semanal")


# fit_receita_semanal_on_residencial	m
ajust_receita_semanal_on_residencial <- ajuste_previsao(base_de_dias = base_de_dias, 
                                                        base_mensal = base_mensal_on_residencial, base_semanal = base_semanal_on_residencial,
                                                        fit_receita_semanal = fit_receita_semanal_on_residencial,
                                                        fit_receita_mensal = fit_receita_mensal_on_residencial, 
                                                        tipo_ajuste = "semanal")


# fit_receita_semanal_posts	m

ajust_receita_semanal_posts <- ajuste_previsao(base_de_dias = base_de_dias, 
                                               base_mensal = base_mensal_posts, base_semanal = base_semanal_posts,
                                               fit_receita_semanal = fit_receita_semanal_posts,
                                               fit_receita_mensal = fit_receita_mensal_posts, 
                                               tipo_ajuste = "semanal")
# fit_receita_semanal_provedor_regional	m
ajust_receita_semanal_provedor_regional <- ajuste_previsao(base_de_dias = base_de_dias, 
                                                           base_mensal = base_mensal_provedor_regional, base_semanal = base_semanal_provedor_regional,              
                                                           fit_receita_semanal = fit_receita_semanal_provedor_regional,
                                                           fit_receita_mensal = fit_receita_mensal_provedor_regional, 
                                                           tipo_ajuste = "semanal")


ajust_receita_mensal_posts<- reparticao_mes(base_de_dias = base_de_dias, 
                                      base_mensal = base_mensal_posts,   
                                      fit_receita_mensal =  fit_receita_mensal_posts )

ajust_receita_mensal_provedor_regional<- reparticao_mes(base_de_dias = base_de_dias, 
                                                        base_mensal = base_mensal_provedor_regional,   
                                                        fit_receita_mensal =  fit_receita_mensal_provedor_regional )


ajust_receita_semanal_ads_mc<- reparticao_semana(base_de_dias = base_de_dias, 
                                              base_semanal = base_semanal_ads_mc,   
                                              fit_receita_semanal =  fit_receita_semanal_ads_mc )

ajust_receita_semanal_adsense<- reparticao_semana(base_de_dias = base_de_dias, 
                                               base_semanal = base_semanal_adsense,   
                                               fit_receita_semanal =  fit_receita_semanal_adsense )

ajust_receita_semanal_mc<- reparticao_semana(base_de_dias = base_de_dias, 
                                          base_semanal = base_semanal_mc,   
                                          fit_receita_semanal =  fit_receita_semanal_mc )

ajust_receita_semanal_on_residencial<- reparticao_semana(base_de_dias = base_de_dias, 
                                                      base_semanal = base_semanal_on_residencial,   
                                                      fit_receita_semanal =  fit_receita_semanal_on_residencial )


################ Exportando projeções ##########

a <- ls(pattern="fit")


results <- list()
results[["fit_receita_mensal_ads_mc"]] <- ajust_receita_mensal_ads_mc
results[["fit_receita_mensal_adsense"]] <- ajust_receita_mensal_adsense
results[["fit_receita_mensal_mc"]] <- ajust_receita_mensal_mc
results[["fit_receita_mensal_on_movel"]] <- predict(fit_receita_mensal_on_movel, h=12)
results[["fit_receita_mensal_on_residencial"]] <- ajust_receita_semanal_on_residencial
results[["fit_receita_mensal_posts"]] <- ajust_receita_mensal_posts
results[["fit_receita_mensal_provedor_regional"]] <- ajust_receita_mensal_provedor_regional
results[["fit_receita_semanal_ads_mc"]] <- ajust_receita_semanal_ads_mc
results[["fit_receita_semanal_adsense"]] <- ajust_receita_semanal_adsense
results[["fit_receita_semanal_mc"]] <- ajust_receita_semanal_mc
results[["fit_receita_semanal_on_movel"]] <- forecast(fit_receita_semanal_on_movel, h=12, level = c(20, 40, 80))
results[["fit_receita_semanal_on_residencial"]] <- ajust_receita_semanal_on_residencial
results[["fit_receita_semanal_posts"]] <- ajust_receita_semanal_posts
results[["fit_receita_semanal_provedor_regional"]] <- ajust_receita_semanal_provedor_regional


results_together <- do.call(smartbind,lapply(names(results),function(x){
   transform(as.data.frame(results[[x]]), Name = x)
}))

wb <- createWorkbook()

addWorksheet(wb, "Forecasts")
writeData(wb, "Forecasts", results_together, rowNames = TRUE)
saveWorkbook(wb, "Forcasts.xlsx", overwrite = TRUE)


# 

############ Exportando bases ################
results <- list()
results[["base_mensal_ads_mc"]] <- cbind(base_mensal_ads_mc$mes, base_mensal_ads_mc$ano,  base_mensal_ads_mc$receita, base_mensal_ads_mc$clickoff)
results[["base_mensal_adsense"]] <- cbind(base_mensal_adsense$mes, base_mensal_adsense$ano,  base_mensal_adsense$receita, base_mensal_adsense$clickoff)
results[["base_mensal_clickoff"]] <- cbind(base_mensal_clickoff$mes, base_mensal_clickoff$ano,  base_mensal_clickoff$receita, base_mensal_clickoff$clickoff)
results[["base_mensal_mc"]] <- cbind(base_mensal_mc$mes, base_mensal_mc$ano,  base_mensal_mc$receita, base_mensal_mc$clickoff)
results[["base_mensal_on_movel"]] <- cbind(base_mensal_on_movel$mes, base_mensal_on_movel$ano,  base_mensal_on_movel$receita, base_mensal_on_movel$clickoff)
results[["base_mensal_on_residencial"]] <- cbind(base_mensal_on_residencial$mes, base_mensal_on_residencial$ano,  base_mensal_on_residencial$receita, base_mensal_on_residencial$clickoff)
results[["base_mensal_posts"]] <- cbind(base_mensal_posts$mes, base_mensal_posts$ano,  base_mensal_posts$receita, base_mensal_posts$clickoff)
results[["base_mensal_provedor_regional"]] <- cbind(base_mensal_provedor_regional$mes, base_mensal_provedor_regional$ano,  base_mensal_provedor_regional$receita, base_mensal_provedor_regional$clickoff)

results_together <- do.call(smartbind,lapply(names(results),function(x){
   transform(as.data.frame(results[[x]]), Name = x)
}))

wb <- createWorkbook()

addWorksheet(wb, "Forecasts")
writeData(wb, "Forecasts", results_together, rowNames = TRUE)
saveWorkbook(wb, "Forcasts.xlsx", overwrite = TRUE)


