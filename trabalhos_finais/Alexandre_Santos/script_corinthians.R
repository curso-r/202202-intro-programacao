# Instalando pacotes e selecionando bibliotecas ---------------------------
#install.packages("tidyverse")
#install.packages("prettydoc") #para a execução do Rmarkdown

library(prettydoc)
library(tidyverse)##ggplot2, read....
library(lubridate) ## manipulação de datas

# baixando banco brasileirão (transfermarkt_competicoes) ------------------

link <- 
  c("https://raw.githubusercontent.com/adaoduque/Brasileirao_Dataset/master/campeonato-brasileiro-full.csv")
bra_2003_2021 <- read_csv(link)

# manipulando os dados ----------------------------------------------------

# Filtrando somente os jogos com participação do Corinthians

jogos_corinthians <-
  bra_2003_2021 %>% 
filter(mandante == "Corinthians" | visitante == "Corinthians") 

# removendo colunas desnecessária e sem dados (selecionando)

jogos_corinthians<-
  jogos_corinthians %>%   
select(rodada, data, dia, hora, mandante, visitante, vencedor, arena, 
       mandante_placar, visitante_placar)

# Criando e modificando colunas -------------------------------------------

# cálculo de saldo de gols mandante

jogos_corinthians <-jogos_corinthians %>% 
mutate(saldo_mandante = mandante_placar-visitante_placar)

# cálculo de saldo de gols visitante

jogos_corinthians <-jogos_corinthians %>% 
  mutate(saldo_visitante = visitante_placar-mandante_placar)

# separação do ano, criação da coluna ano

jogos_corinthians <- jogos_corinthians %>% 
mutate(ano = year(ymd(data)))


# Criação do data frame de jogos do Corinthians como mandante 

corinthians_mandante <-jogos_corinthians %>% 
  filter (mandante == "Corinthians")


# Criação do data frame de jogos do Corinthians como visitante

corinthians_visitante <-jogos_corinthians %>% 
  filter (mandante != "Corinthians")

# habilitando/baixando as funções para cálculos de pontuação e resultado ------------------------

source("https://raw.githubusercontent.com/AlexandreStos/ef_functions/main/pontos_br")

# pontos_mandante()
# pontos_visitante()
# resultado_mandante()
# resultado_visitante()


# criando a coluna pontos no dataframe corinthians_mandante -------------------------------

corinthians_mandante  <- corinthians_mandante %>% 
  mutate(pontos = pontos_mandante(mandante_placar,visitante_placar))

# criando a coluna pontos no dataframe corinthians_visitante --------------

corinthians_visitante <- corinthians_visitante %>% 
  mutate(pontos = pontos_visitante(mandante_placar,visitante_placar))

# criando a coluna resultado no dataframe mandante ------------------------

corinthians_mandante  <- corinthians_mandante %>% 
  mutate(resultado = resultado_mandante(mandante_placar,visitante_placar))

# criando a coluna resultados no dataframe visitante ----------------------

corinthians_visitante  <- corinthians_visitante %>% 
  mutate(resultado = resultado_visitante(mandante_placar,visitante_placar))

# Realizando a analise descritiva dos dados -------------------------------

jogos_man <- count(corinthians_mandante, mandante =="Corinthians")

# total de pontos como mandante 

pontos_total_2003_2021<-sum (corinthians_mandante$pontos)
pontos_poss_2003_2021 <- (2021-2003)*(19*3)
porc_pontos_2003_2021 <- (pontos_total_2003_2021/pontos_poss_2003_2021)*100
porc_pontos_2003_2021 <-round(porc_pontos_2003_2021, 1)

pontos_total_vis_2003_2021<-sum (corinthians_visitante$pontos)
porc_pontos_vis_2003_2021 <- (pontos_total_vis_2003_2021/pontos_poss_2003_2021)*100
porc_pontos_vis_2003_2021 <- round(porc_pontos_vis_2003_2021, 1)

n_vitorias_man <- count(corinthians_mandante, resultado=="vitoria")
n_empates_man <- count(corinthians_mandante, resultado=="empate")
n_derrotas_man <- count(corinthians_mandante, resultado =="derrota")

n_vitorias_vis <- count(corinthians_visitante, resultado =="vitoria")
n_empates_vis <- count(corinthians_visitante, resultado =="empate")
n_derrotas_vis <- count(corinthians_visitante, resultado =="derrota")

# Total de pontos como mandante ou visitante por ano (exceto 2007, --------

#pontuação total possível por ano como mandante/visitante

pontos_poss_ano <- (19*3) # 19 partidas como mandante valendo 3 pontos
pontos_poss_ano_camp <- (19*3)*4 # 4 títulos (2003-2021)
#2003

mandante_2003 <- corinthians_mandante %>% filter(ano == "2003")
p_mandante_2003 <- sum(mandante_2003$pontos)
porc_pontos_2003 <- (p_mandante_2003/pontos_poss_ano)*100 # mult. por 100 para transformar em porcentagem
porc_pontos_2003 <- round(porc_pontos_2003, 0)

#2004

mandante_2004 <- corinthians_mandante %>% filter(ano == "2004")
p_mandante_2004 <- sum(mandante_2004$pontos)
porc_pontos_2004 <- (p_mandante_2004/pontos_poss_ano)*100
porc_pontos_2004 <- round(porc_pontos_2004, 0)

#2005

mandante_2005 <- corinthians_mandante %>% filter(ano == "2005")
p_mandante_2005 <- sum(mandante_2005$pontos)
porc_pontos_2005 <- (p_mandante_2005/pontos_poss_ano)*100 
porc_pontos_2005 <- round(porc_pontos_2005, 0)

#2006

mandante_2006 <- corinthians_mandante %>% filter(ano == "2006")
p_mandante_2006 <- sum(mandante_2006$pontos)
porc_pontos_2006 <- (p_mandante_2006/pontos_poss_ano)*100 
porc_pontos_2006 <- round(porc_pontos_2006, 0)

#2007

mandante_2007 <- corinthians_mandante %>% filter(ano == "2007")
p_mandante_2007 <- sum(mandante_2007$pontos)
porc_pontos_2007 <- (p_mandante_2007/pontos_poss_ano)*100 
porc_pontos_2007 <- round(porc_pontos_2007, 0)

#2008 Corinthians disputou a série B

#2009

mandante_2009 <- corinthians_mandante %>% filter(ano == "2009")
p_mandante_2009 <- sum(mandante_2009$pontos)
porc_pontos_2009 <- (p_mandante_2009/pontos_poss_ano)*100 
porc_pontos_2009 <- round(porc_pontos_2009, 0)

#2010

mandante_2010 <- corinthians_mandante %>% filter(ano == "2010")
p_mandante_2010 <- sum(mandante_2010$pontos)
porc_pontos_2010 <- (p_mandante_2010/pontos_poss_ano)*100 
porc_pontos_2010 <- round(porc_pontos_2010, 0)


#2011

mandante_2011 <- corinthians_mandante %>% filter(ano == "2011")
p_mandante_2011 <- sum(mandante_2011$pontos)
porc_pontos_2011 <- (p_mandante_2011/pontos_poss_ano)*100 
porc_pontos_2011 <- round(porc_pontos_2011, 0)

#2012

mandante_2012 <- corinthians_mandante %>% filter(ano == "2012")
p_mandante_2012 <- sum(mandante_2012$pontos)
porc_pontos_2012 <- (p_mandante_2012/pontos_poss_ano)*100 
porc_pontos_2012 <- round(porc_pontos_2012, 0)


#2013

mandante_2013 <- corinthians_mandante %>% filter(ano == "2013")
p_mandante_2013 <- sum(mandante_2013$pontos)
porc_pontos_2013 <- (p_mandante_2013/pontos_poss_ano)*100 
porc_pontos_2013 <- round(porc_pontos_2013, 0)


#2014

mandante_2014 <- corinthians_mandante %>% filter(ano == "2014")
p_mandante_2014 <- sum(mandante_2014$pontos)
porc_pontos_2014 <- (p_mandante_2014/pontos_poss_ano)*100 
porc_pontos_2014 <- round(porc_pontos_2014, 0)

#2015

mandante_2015 <- corinthians_mandante %>% filter(ano == "2015")
p_mandante_2015 <- sum(mandante_2015$pontos)
porc_pontos_2015 <- (p_mandante_2015/pontos_poss_ano)*100 
porc_pontos_2015 <- round(porc_pontos_2015, 0)


#2016

mandante_2016 <- corinthians_mandante %>% filter(ano == "2016")
p_mandante_2016 <- sum(mandante_2016$pontos)
porc_pontos_2016 <- (p_mandante_2016/pontos_poss_ano)*100 
porc_pontos_2016 <- round(porc_pontos_2016, 0)


#2017

mandante_2017 <- corinthians_mandante %>% filter(ano == "2017")
p_mandante_2017 <- sum(mandante_2017$pontos)
porc_pontos_2017 <- (p_mandante_2017/pontos_poss_ano)*100 
porc_pontos_2017 <- round(porc_pontos_2017, 0)


#2018

mandante_2018 <- corinthians_mandante %>% filter(ano == "2018")
p_mandante_2018 <- sum(mandante_2018$pontos)
porc_pontos_2018 <- (p_mandante_2018/pontos_poss_ano)*100 
porc_pontos_2018 <- round(porc_pontos_2018, 0)


#2019

mandante_2019 <- corinthians_mandante %>% filter(ano == "2019")
p_mandante_2019 <- sum(mandante_2019$pontos)
porc_pontos_2019 <- (p_mandante_2019/pontos_poss_ano)*100 
porc_pontos_2019 <- round(porc_pontos_2019, 0)


#2020

mandante_2020 <- corinthians_mandante %>% filter(ano == "2020")
p_mandante_2020 <- sum(mandante_2020$pontos)
porc_pontos_2020 <- (p_mandante_2020/pontos_poss_ano)*100 
porc_pontos_2020 <- round(porc_pontos_2020, 0)


#2021

mandante_2021 <- corinthians_mandante %>% filter(ano == "2021")
p_mandante_2021 <- sum(mandante_2021$pontos)
porc_pontos_2021 <- (p_mandante_2021/pontos_poss_ano)*100 
porc_pontos_2021 <- round(porc_pontos_2021, 0)


# Total de pontos por ano como visitante ----------------------------------

#2003

visitante_2003 <- corinthians_visitante %>% filter(ano == "2003")
p_visitante_2003 <- sum(visitante_2003$pontos)
porc_pontos_vis_2003 <- (p_visitante_2003/pontos_poss_ano)*100 # mult. por 100 para transformar em porcentagem
porc_pontos_vis_2003 <- round(porc_pontos_vis_2003, 0)

#2004

visitante_2004 <- corinthians_visitante %>% filter(ano == "2004")
p_visitante_2004 <- sum(visitante_2004$pontos)
porc_pontos_vis_2004 <- (p_visitante_2004/pontos_poss_ano)*100
porc_pontos_vis_2004 <- round(porc_pontos_vis_2004, 0)

#2005

visitante_2005 <- corinthians_visitante %>% filter(ano == "2005")
p_visitante_2005 <- sum(visitante_2005$pontos)
porc_pontos_vis_2005 <- (p_visitante_2005/pontos_poss_ano)*100 
porc_pontos_vis_2005 <- round(porc_pontos_vis_2005, 0)

#2006

visitante_2006 <- corinthians_visitante %>% filter(ano == "2006")
p_visitante_2006 <- sum(visitante_2006$pontos)
porc_pontos_vis_2006 <- (p_visitante_2006/pontos_poss_ano)*100 
porc_pontos_vis_2006 <- round(porc_pontos_vis_2006, 0)

#2007

visitante_2007 <- corinthians_visitante %>% filter(ano == "2007")
p_visitante_2007 <- sum(visitante_2007$pontos)
porc_pontos_vis_2007 <- (p_visitante_2007/pontos_poss_ano)*100 
porc_pontos_vis_2007 <- round(porc_pontos_vis_2007, 0)

#2008 Corinthians disputou a série B

#2009

visitante_2009 <- corinthians_visitante %>% filter(ano == "2009")
p_visitante_2009 <- sum(visitante_2009$pontos)
porc_pontos_vis_2009 <- (p_visitante_2009/pontos_poss_ano)*100 
porc_pontos_vis_2009 <- round(porc_pontos_vis_2009, 0)

#2010

visitante_2010 <- corinthians_visitante %>% filter(ano == "2010")
p_visitante_2010 <- sum(visitante_2010$pontos)
porc_pontos_vis_2010 <- (p_visitante_2010/pontos_poss_ano)*100 
porc_pontos_vis_2010 <- round(porc_pontos_vis_2010, 0)


#2011

visitante_2011 <- corinthians_visitante %>% filter(ano == "2011")
p_visitante_2011 <- sum(visitante_2011$pontos)
porc_pontos_vis_2011 <- (p_visitante_2011/pontos_poss_ano)*100 
porc_pontos_vis_2011 <- round(porc_pontos_vis_2011, 0)

#2012

visitante_2012 <- corinthians_visitante %>% filter(ano == "2012")
p_visitante_2012 <- sum(visitante_2012$pontos)
porc_pontos_vis_2012 <- (p_visitante_2012/pontos_poss_ano)*100 
porc_pontos_vis_2012 <- round(porc_pontos_vis_2012, 0)

#2013

visitante_2013 <- corinthians_visitante %>% filter(ano == "2013")
p_visitante_2013 <- sum(visitante_2013$pontos)
porc_pontos_vis_2013 <- (p_visitante_2013/pontos_poss_ano)*100 
porc_pontos_vis_2013 <- round(porc_pontos_vis_2013, 0)


#2014

visitante_2014 <- corinthians_visitante %>% filter(ano == "2014")
p_visitante_2014 <- sum(visitante_2014$pontos)
porc_pontos_vis_2014 <- (p_visitante_2014/pontos_poss_ano)*100 
porc_pontos_vis_2014 <- round(porc_pontos_vis_2014, 0)


#2015

visitante_2015 <- corinthians_visitante %>% filter(ano == "2015")
p_visitante_2015 <- sum(visitante_2015$pontos)
porc_pontos_vis_2015 <- (p_visitante_2015/pontos_poss_ano)*100 
porc_pontos_vis_2015 <- round(porc_pontos_vis_2015, 0)


#2016

visitante_2016 <- corinthians_visitante %>% filter(ano == "2016")
p_visitante_2016 <- sum(visitante_2016$pontos)
porc_pontos_vis_2016 <- (p_visitante_2016/pontos_poss_ano)*100 
porc_pontos_vis_2016 <- round(porc_pontos_vis_2016, 0)

#2017

visitante_2017 <- corinthians_visitante %>% filter(ano == "2017")
p_visitante_2017 <- sum(visitante_2017$pontos)
porc_pontos_vis_2017 <- (p_visitante_2017/pontos_poss_ano)*100 
porc_pontos_vis_2017 <- round(porc_pontos_vis_2017, 0)

#2018

visitante_2018 <- corinthians_visitante %>% filter(ano == "2018")
p_visitante_2018 <- sum(visitante_2018$pontos)
porc_pontos_vis_2018 <- (p_visitante_2018/pontos_poss_ano)*100 
porc_pontos_vis_2018 <- round(porc_pontos_vis_2018, 0)

#2019

visitante_2019 <- corinthians_visitante %>% filter(ano == "2019")
p_visitante_2019 <- sum(visitante_2019$pontos)
porc_pontos_vis_2019 <- (p_visitante_2019/pontos_poss_ano)*100 
porc_pontos_vis_2019 <- round(porc_pontos_vis_2019, 0)

#2020

visitante_2020 <- corinthians_visitante %>% filter(ano == "2020")
p_visitante_2020 <- sum(visitante_2020$pontos)
porc_pontos_vis_2020 <- (p_visitante_2020/pontos_poss_ano)*100 
porc_pontos_vis_2020 <- round(porc_pontos_vis_2020, 0)

#2021

visitante_2021 <- corinthians_visitante %>% filter(ano == "2021")
p_visitante_2021 <- sum(visitante_2021$pontos)
porc_pontos_vis_2021 <- (p_visitante_2021/pontos_poss_ano)*100 
porc_pontos_vis_2021 <- round(porc_pontos_vis_2021, 0)



# Criação de dataframe com resultados de pontos de mandante e visitante por ano

pontos_ano_man <- c(p_mandante_2003, p_mandante_2004, p_mandante_2005, 
                    p_mandante_2006, p_mandante_2007, p_mandante_2009,
                    p_mandante_2010, p_mandante_2011, p_mandante_2012,
                    p_mandante_2013, p_mandante_2014, p_mandante_2015,
                    p_mandante_2016, p_mandante_2017, p_mandante_2018,
                    p_mandante_2019, p_mandante_2020, p_mandante_2021)

porc_ano_man <-c(porc_pontos_2003, porc_pontos_2004, porc_pontos_2005,
                 porc_pontos_2006, porc_pontos_2007, porc_pontos_2009,
                 porc_pontos_2010, porc_pontos_2011, porc_pontos_2012,
                 porc_pontos_2013, porc_pontos_2014, porc_pontos_2015,
                 porc_pontos_2016, porc_pontos_2017, porc_pontos_2018,
                 porc_pontos_2019, porc_pontos_2020, porc_pontos_2021)


pontos_vis_ano <- c(p_visitante_2003, p_visitante_2004, p_visitante_2005, 
                p_visitante_2006, p_visitante_2007, p_visitante_2009,
                p_visitante_2010, p_visitante_2011, p_visitante_2012,
                p_visitante_2013, p_visitante_2014, p_visitante_2015,
                p_visitante_2016, p_visitante_2017, p_visitante_2018,
                p_visitante_2019, p_visitante_2020, p_visitante_2021)

porc_ano_vis <-c(porc_pontos_vis_2003, porc_pontos_vis_2004, porc_pontos_vis_2005,
                 porc_pontos_vis_2006, porc_pontos_vis_2007, porc_pontos_vis_2009,
                 porc_pontos_vis_2010, porc_pontos_vis_2011, porc_pontos_vis_2012,
                 porc_pontos_vis_2013, porc_pontos_vis_2014, porc_pontos_vis_2015,
                 porc_pontos_vis_2016, porc_pontos_vis_2017, porc_pontos_vis_2018,
                 porc_pontos_vis_2019, porc_pontos_vis_2020, porc_pontos_vis_2021)


pontos_camp_man <- c(p_mandante_2005,  p_mandante_2011,
                      p_mandante_2015, p_mandante_2017)

porc_camp_man <-c(porc_pontos_2005, porc_pontos_2011,
                  porc_pontos_2015, porc_pontos_2017)


pontos_camp_vis <- c(p_visitante_2005, p_visitante_2011, 
                      p_visitante_2015, p_visitante_2017)

porc_camp_vis <-c(porc_pontos_vis_2005, porc_pontos_vis_2011,
                  porc_pontos_vis_2015, porc_pontos_vis_2017)


# criando coluna ano sem 2008 (série B)

ano <- c(2003:2021)
ano <- ano [ano != 2008]

# criando coluna campeonato

campeonado <- c(2005,2011,2015,2017)

# calculando médias e desvios de pontos como mandante e visitante ---------

medias_pontos_mandante <- round(mean(pontos_ano_man), 2)
desv_pontos_mandante <- round(sd(pontos_ano_man), 2)
medias_porc_mandante <- round(mean(porc_ano_man), 2)
desv_porc_mandante <- round(sd(porc_ano_man), 2)

med_pontos_vis <- round(mean(pontos_vis_ano), 2)
desv_pontos_vis <- round(sd(pontos_vis_ano), 2)
medias_porc_vis <- round(mean(porc_ano_vis), 2)
desv_porc_vis <- round(sd(porc_ano_vis), 2)

# calculando médias e desvios de pontos dos anos campeões: mandante e visitante ---------

medias_camp_mandante <- round(mean(pontos_camp_man), 2)
desv_camp_mandante <- round(sd(pontos_camp_man), 2)
medias_porc_camp_mandante <- round(mean(porc_camp_man), 2)
desv_porc_camp_mandante <- round(sd(porc_camp_man), 2)

med_pontos_camp_vis <- round(mean(pontos_camp_vis), 2)
desv_pontoscamp_vis <- round(sd(pontos_camp_vis), 2)
medias_porc_camp_vis <- round(mean(porc_camp_vis), 2)
desv_porc_camp_vis <- round(sd(porc_camp_vis), 2)

# Todos resultados em tabela única

res_analises <- tibble (pontos_ano_man,pontos_vis_ano,porc_ano_man, porc_ano_vis, ano)

head(res_analises,18)# visualizando a tabela

res_analises_camp <- tibble(pontos_camp_man, porc_camp_man, pontos_camp_vis, porc_camp_vis, campeonado)

# Calculos de número de gols e saldo --------------------------------------

#2003

mandante_gols_2003 <-sum(mandante_2003$mandante_placar)
mandante_saldo_2003 <-sum(mandante_2003$saldo_mandante)
visitante_gols_2003 <-sum(visitante_2003$visitante_placar)
visitante_saldo_2003 <-sum(visitante_2003$saldo_visitante)

#2004

mandante_gols_2004 <-sum(mandante_2004$mandante_placar)
mandante_saldo_2004 <-sum(mandante_2004$saldo_mandante)
visitante_gols_2004 <-sum(visitante_2004$visitante_placar)
visitante_saldo_2004 <-sum(visitante_2004$saldo_visitante)

#2005

mandante_gols_2005 <-sum(mandante_2005$mandante_placar)
mandante_saldo_2005 <-sum(mandante_2005$saldo_mandante)
visitante_gols_2005 <-sum(visitante_2005$visitante_placar)
visitante_saldo_2005 <-sum(visitante_2005$saldo_visitante)

#2006

mandante_gols_2006 <-sum(mandante_2006$mandante_placar)
mandante_saldo_2006 <-sum(mandante_2006$saldo_mandante)
visitante_gols_2006 <-sum(visitante_2006$visitante_placar)
visitante_saldo_2006 <-sum(visitante_2006$saldo_visitante)

#2007

mandante_gols_2007 <-sum(mandante_2007$mandante_placar)
mandante_saldo_2007 <-sum(mandante_2007$saldo_mandante)
visitante_gols_2007 <-sum(visitante_2007$visitante_placar)
visitante_saldo_2007 <-sum(visitante_2007$saldo_visitante)

#2009

mandante_gols_2009 <-sum(mandante_2009$mandante_placar)
mandante_saldo_2009 <-sum(mandante_2009$saldo_mandante)
visitante_gols_2009 <-sum(visitante_2009$visitante_placar)
visitante_saldo_2009 <-sum(visitante_2009$saldo_visitante)

#2010

mandante_gols_2010 <-sum(mandante_2010$mandante_placar)
mandante_saldo_2010 <-sum(mandante_2010$saldo_mandante)
visitante_gols_2010 <-sum(visitante_2010$visitante_placar)
visitante_saldo_2010 <-sum(visitante_2010$saldo_visitante)

#2011

mandante_gols_2011 <-sum(mandante_2011$mandante_placar)
mandante_saldo_2011 <-sum(mandante_2011$saldo_mandante)
visitante_gols_2011 <-sum(visitante_2011$visitante_placar)
visitante_saldo_2011 <-sum(visitante_2011$saldo_visitante)

#2012

mandante_gols_2012 <-sum(mandante_2012$mandante_placar)
mandante_saldo_2012 <-sum(mandante_2012$saldo_mandante)
visitante_gols_2012 <-sum(visitante_2012$visitante_placar)
visitante_saldo_2012 <-sum(visitante_2012$saldo_visitante)

#2013

mandante_gols_2013 <-sum(mandante_2013$mandante_placar)
mandante_saldo_2013 <-sum(mandante_2013$saldo_mandante)
visitante_gols_2013 <-sum(visitante_2013$visitante_placar)
visitante_saldo_2013 <-sum(visitante_2013$saldo_visitante)

#2014

mandante_gols_2014 <-sum(mandante_2014$mandante_placar)
mandante_saldo_2014 <-sum(mandante_2014$saldo_mandante)
visitante_gols_2014 <-sum(visitante_2014$visitante_placar)
visitante_saldo_2014 <-sum(visitante_2014$saldo_visitante)

#2015

mandante_gols_2015 <-sum(mandante_2015$mandante_placar)
mandante_saldo_2015 <-sum(mandante_2015$saldo_mandante)
visitante_gols_2015 <-sum(visitante_2015$visitante_placar)
visitante_saldo_2015 <-sum(visitante_2015$saldo_visitante)

#2016

mandante_gols_2016 <-sum(mandante_2016$mandante_placar)
mandante_saldo_2016 <-sum(mandante_2016$saldo_mandante)
visitante_gols_2016 <-sum(visitante_2016$visitante_placar)
visitante_saldo_2016 <-sum(visitante_2016$saldo_visitante)

#2017

mandante_gols_2017 <-sum(mandante_2017$mandante_placar)
mandante_saldo_2017 <-sum(mandante_2017$saldo_mandante)
visitante_gols_2017 <-sum(visitante_2017$visitante_placar)
visitante_saldo_2017 <-sum(visitante_2017$saldo_visitante)

#2018

mandante_gols_2018 <-sum(mandante_2018$mandante_placar)
mandante_saldo_2018 <-sum(mandante_2018$saldo_mandante)
visitante_gols_2018 <-sum(visitante_2018$visitante_placar)
visitante_saldo_2018 <-sum(visitante_2018$saldo_visitante)

#2019

mandante_gols_2019 <-sum(mandante_2019$mandante_placar)
mandante_saldo_2019 <-sum(mandante_2019$saldo_mandante)
visitante_gols_2019 <-sum(visitante_2019$visitante_placar)
visitante_saldo_2019 <-sum(visitante_2019$saldo_visitante)

#2020

mandante_gols_2020 <-sum(mandante_2020$mandante_placar)
mandante_saldo_2020 <-sum(mandante_2020$saldo_mandante)
visitante_gols_2020 <-sum(visitante_2020$visitante_placar)
visitante_saldo_2020 <-sum(visitante_2020$saldo_visitante)

#2021

mandante_gols_2021 <-sum(mandante_2021$mandante_placar)
mandante_saldo_2021 <-sum(mandante_2021$saldo_mandante)
visitante_gols_2021 <-sum(visitante_2021$visitante_placar)
visitante_saldo_2021 <-sum(visitante_2021$saldo_visitante)


gols_m <- c(mandante_gols_2003,mandante_gols_2004,mandante_gols_2005,
             mandante_gols_2006,mandante_gols_2007,mandante_gols_2009,
             mandante_gols_2010,mandante_gols_2011,mandante_gols_2012,
             mandante_gols_2013, mandante_gols_2014, mandante_gols_2015,
             mandante_gols_2016,mandante_gols_2017,mandante_gols_2018,
             mandante_gols_2019,mandante_gols_2020,mandante_gols_2021)

saldo_m <- c(mandante_saldo_2003,mandante_saldo_2004,mandante_saldo_2005,
             mandante_saldo_2006,mandante_saldo_2007,mandante_saldo_2009,
             mandante_saldo_2010,mandante_saldo_2011,mandante_saldo_2012,
             mandante_saldo_2013, mandante_saldo_2014,mandante_saldo_2015,
             mandante_saldo_2016,mandante_saldo_2017,mandante_saldo_2018,
             mandante_saldo_2019,mandante_saldo_2020,mandante_saldo_2021)

gols_v <- c(visitante_gols_2003,visitante_gols_2004,visitante_gols_2005,
            visitante_gols_2006,visitante_gols_2007,visitante_gols_2009,
            visitante_gols_2010,visitante_gols_2011,visitante_gols_2012,
            visitante_gols_2013,visitante_gols_2014,visitante_gols_2015,
            visitante_gols_2016,visitante_gols_2017,visitante_gols_2018,
            visitante_gols_2019,visitante_gols_2020,visitante_gols_2021)

saldo_v <- c(visitante_saldo_2003,visitante_saldo_2004,visitante_saldo_2005,
            visitante_saldo_2006,visitante_saldo_2007,visitante_saldo_2009,
            visitante_saldo_2010,visitante_saldo_2011,visitante_saldo_2012,
            visitante_saldo_2013,visitante_saldo_2014,visitante_saldo_2015,
            visitante_saldo_2016,visitante_saldo_2017,visitante_saldo_2018,
            visitante_saldo_2019,visitante_saldo_2020,visitante_saldo_2021)


# anos com título

gols_m_camp <- c(mandante_gols_2005, mandante_gols_2011,mandante_gols_2015
            ,mandante_gols_2017)

saldo_m_camp <- c(mandante_saldo_2005, mandante_saldo_2011,mandante_saldo_2015,
                  mandante_saldo_2017)

gols_v_camp <- c(visitante_gols_2005,visitante_gols_2011,visitante_gols_2015,
                 visitante_gols_2017)

saldo_v_camp <- c(visitante_saldo_2005,visitante_saldo_2011,visitante_saldo_2015,
                  visitante_saldo_2017)


# dataframe dos 18 anos (gols e saldos)
gols_br <- data.frame(gols_m,saldo_m,gols_v,saldo_v,ano)
head(gols_br, 18)

#dataframe dos anos de título (gols e saldos)

gols_br_camp <- data.frame(gols_m_camp,saldo_m_camp,gols_v_camp,saldo_v_camp,campeonado)
head(gols_br_camp, 4)

# Analise descritiva  de gols e saldo para os 18 anos de campeonato brasileiro
gols_media_ano_man <-  round(mean(gols_br$gols_m),1)
desv_media_ano_man <- round(sd(gols_br$gols_m), 2)
gols_media_ano_vis <-  round(mean(gols_br$gols_v),1)
desv_media_ano_vis <- round(sd(gols_br$gols_v), 2)
saldo_media_ano_man <-  round(mean(gols_br$saldo_m),1)
desv_saldo_media_ano_man <- round(sd(gols_br$saldo_m), 2)
saldo_media_ano_vis <-  round(mean(gols_br$saldo_v),1)
desv_saldo_media_ano_vis <- round(sd(gols_br$saldo_v), 2)

# Analise descritiva para gols e saldo  nos anos de título campeonato brasileiro
gols_media_camp_man <-  round(mean(gols_br_camp$gols_m_camp),1)
desv_media_camp_man <- round(sd(gols_br_camp$gols_m_camp), 2)
gols_media_camp_vis <-  round(mean(gols_br_camp$gols_v_camp),2)
desv_media_camp_vis <- round(sd(gols_br_camp$gols_v_camp), 2)
saldo_media_camp_man <-  round(mean(gols_br_camp$saldo_m_camp),1)
desv_saldo_camp_man <- round(sd(gols_br_camp$saldo_m_camp), 2)
saldo_media_camp_vis <-  round(mean(gols_br_camp$saldo_v_camp),1)
desv_saldo_camp_vis <- round(sd(gols_br_camp$saldo_v_camp), 2)

              
# gerando gráficos (ggplot2) -------------------------------------------------------


# pontuação

gr1 <-
res_analises %>% 
ggplot(aes(x= ano, y= pontos_ano_man, label = pontos_ano_man))+
  geom_bar (stat="identity", fill= "black")+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y=" Nº de pontos", title = "Pontuação como mandante no Brasileirão 03-21")

gr2 <-
res_analises %>% 
  ggplot(aes(x= ano, y= pontos_vis_ano, label = pontos_vis_ano))+
  geom_bar (stat="identity", fill= "#4f017c", color= "black", alpha = 1)+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y=" Nº de pontos",title = "Pontuação como visitante no Brasileirão 03-21")

gr3 <-
  res_analises %>% 
  ggplot(aes(x= ano, y= porc_ano_man, label = porc_ano_man))+
  geom_bar (stat="identity", fill= "black")+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y=" % de pontos", title = "Porcentagem de pontos como mandante no Brasileirão 03-21")

gr4 <-
  res_analises %>% 
  ggplot(aes(x= ano, y= porc_ano_vis, label = porc_ano_vis))+
  geom_bar (stat="identity", fill= "#4f017c", color= "black", alpha = 1)+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y=" % de pontos",title = "Porcentagem de pontos como visitante no Brasileirão 03-21")

library(cowplot)# biblioteca usada para dispor gráficos em colunas

##### Figura 1. Pontuação Corinthians por ano 

plot_grid(gr1,gr2,nrow=2, ncol = 1)


##### Figura 2. Porcentagem de pontos do Corinthians

plot_grid(gr3,gr4,nrow=2, ncol=1)


# gols


 gols_br %>% 
  ggplot(aes(x= ano, y= gols_m, label = gols_m))+
  geom_line (stat="identity", color = "black")+
  scale_y_continuous(
    breaks = c(50,40,30,20,10,0,-10,-20))+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y=" Nº de gols", title = "Gols como mandante no Brasileirão 03-21")


  gols_br %>% 
  ggplot(aes(x= ano, y= gols_v, label = gols_v))+
  geom_line (stat="identity", color= "#4f017c")+
  scale_y_continuous(
    breaks = c(50,40,30,20,10,0,-10,-20))+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y="Nº de gols", title = "Gols como visitante no Brasileirão 03-21")


  gols_br %>% 
  ggplot(aes(x= ano, y= saldo_m, label = saldo_m))+
  geom_line (stat="identity", color= "black")+
  scale_y_continuous(
    breaks = c(50,40,30,20,10,0,-10,-20))+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y=" Saldo de gols", title = "Saldo de gols como mandante no Brasileirão 03-21")

  gols_br %>% 
  ggplot(aes(x= ano, y= saldo_v, label = saldo_v))+
  geom_line (stat="identity", color= "#4f017c")+
  scale_y_continuous(
    breaks = c(50,40,30,20,10,0,-10,-20))+
  geom_label(size=3)+
  theme_classic()+
  labs(x="Ano", y="Saldo de gols", title = "Saldo de gols como visitante no Brasileirão 03-21")




