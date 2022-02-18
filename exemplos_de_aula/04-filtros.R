# Carregando pacotes ------------------------------------------------------

library(readr)

# Carregando os dados -----------------------------------------------------

base_de_dados <- read_csv2("dados/voos_de_janeiro.csv")

# Filtrando linhas do data.frame  com vetores lógicos --------------

base_de_dados$atraso_chegada
base_de_dados$atraso_chegada == 4  # Retorna um vetor de VERDADEIRO ou FALSO

# Podemos filtrar linhas baseadas no retorno do vetor com TRUE e FALSE,
# sendo que: o que for TRUE ficará na base, e o que for FALSE será removido.
base_de_dados[base_de_dados$origem == "EWR",]

base_de_dados[base_de_dados$origem == "EWR", c("origem", "horario_saida")]

base_de_dados[base_de_dados$tempo_voo > 100, c("tempo_voo", "origem", "destino")]

voos_ewr <- base_de_dados[base_de_dados$origem == "EWR",]

#  Podemos combinar!
exemplo_ou <- base_de_dados[base_de_dados$origem == "EWR" | base_de_dados$tempo_voo > 100,]

exemplo_e <- base_de_dados[base_de_dados$origem == "EWR" & base_de_dados$tempo_voo > 100,]


## Exercícios --------------------------------------------------------------

library(dplyr)

# 1. Usando a base de voos, e considerando que as colunas "dia", "mes" e "ano"
# compõem a data de saída do voo, escreva um código que devolva apenas os voos
# que saíram no dia 15/01/2013:

# Considerando mes, dia e ano:
base_de_dados[base_de_dados$ano == 2013 & base_de_dados$mes == 1 & base_de_dados$dia == 15,]

# Considerando que a base já é de janeiro de 2013
base_de_dados[base_de_dados$dia == 15,]


# O mesmo filtro com dplyr: considerando o mes, dia e ano
base_de_dados %>%
  filter(ano == 2013, mes == 1, dia == 15)


# O mesmo filtro com dplyr: Considerando que a base já é de janeiro de 2013
base_de_dados %>%
  filter(dia == 15)


# 2. Usando a base de voos, escreva um código que devolva apenas os voos
# que NÃO sairam do aeroporto JFK:

base_de_dados[base_de_dados$origem != "JFK",]

# com dplyr
base_de_dados %>%
  filter(origem != "JFK")

# 3. Usando a base de voos, escreva um código que devolva apenas os voos
# que sairam do aeroporto JFK, e foram para Atlanta ("ATL"),
# e salve em um objeto chamado voos_jfk_atlanta:

voos_jfk_atlanta <- base_de_dados[base_de_dados$origem == "JFK" & base_de_dados$destino == "ATL",]

# com dplyr
voos_jfk_atlanta <- base_de_dados %>%
  filter(origem == "JFK", destino == "ATL")

# 4. Usando a base de voos, escreva um código que devolva apenas os voos
# que saíram nos dias 15/01/2013 ou 16/01/2013:

# base R - com %in% e considerando que a base é de janeiro de 2013
base_de_dados[base_de_dados$dia %in% c(15, 16),]
# base R - com | e considerando que a base é de janeiro de 2013
base_de_dados[base_de_dados$dia == 15 | base_de_dados$dia == 16,]


# base R - com %in% e considerando dia mes e ano
base_de_dados[base_de_dados$dia %in% c(15, 16) & base_de_dados$mes == 1 & base_de_dados$ano == 2013,]

# base R - com | e considerando dia mes e ano
base_de_dados[(base_de_dados$dia == 15 | base_de_dados$dia == 16) & base_de_dados$mes == 1 & base_de_dados$ano == 2013,]


# com dplyr e %in% (resolucao mais elegante!)
base_de_dados %>%
  filter(dia %in% c(15,16), mes == 1, ano == 2013)

# com dplyr e |
base_de_dados %>%
  filter((dia == 15 | dia == 16) & mes == 1 & ano == 2013)


# filter com dplyr -----------------

# Existe um outro jeito de fazer esse tipo de filtro

# vamos instalar um pacote novo!
# install.packages("dplyr")
library(dplyr)

# podemos escrever de uma forma que não repete o nome "base_de_dados"
exemplo_com_base <- base_de_dados[base_de_dados$tempo_voo > 100,]
# Com o base, os NAs estao por aqui ainda

# filter é usado para filtrar as linhas
#filter(seu_data_frame, CONDICOES)
exemplo_com_dplyr <- filter(base_de_dados, tempo_voo > 100)
# Com o dplyr, os NAs foram removidos


# select é para colunas
select(base_de_dados, ano, mes, dia)

# outros exemplos:

#base_de_dados[base_de_dados$origem == "EWR",]
filter(base_de_dados, origem == "EWR")

#base_de_dados[base_de_dados$origem == "EWR" | base_de_dados$tempo_voo > 100,]
filter(base_de_dados, origem == "EWR" | tempo_voo > 100)

#base_de_dados[base_de_dados$origem == "EWR" & base_de_dados$tempo_voo > 100,]
filter(base_de_dados, origem == "EWR" & tempo_voo > 100)

# opcional: dentro do filter, podemos usar "," ao invés de "&"
# essa é uma comodidade ESPECÍFICA do filter
View(filter(base_de_dados, origem == "EWR", tempo_voo > 100, dia == 1, horario_chegada > 900))

## Exercícios --------------------------------------------------------------

avaliacao_do_cliente <- c(1, 3, 0, 10, 2, 5, 20)
estado_de_nascimento <- c("SP", "PB", "PB", "RJ", "MT", "MT", "PA")

avaliacoes <- data.frame(avaliacao_do_cliente, estado_de_nascimento)

# usando o data.frame "avaliacoes", escreva códigos que atendam os pontos abaixo.
# Se possível, escreva duas versões de cada código: uma com filter e outra usando
# subsetting (os colchetes [])

# 1. Filtre as avaliações superiores a 3.
# base R:
avaliacoes[avaliacoes$avaliacao_do_cliente > 3, ]

# dplyr:
avaliacoes %>%
  filter(avaliacao_do_cliente > 3)

# 2. Filtre as avaliações de SP ou MT.
# base R
avaliacoes[avaliacoes$estado_de_nascimento %in% c("SP", "MT"),]

# dplyr
avaliacoes %>%
  filter(estado_de_nascimento %in% c("SP", "MT"))

# 3. Filtre as avaliações de PB ou MT com nota inferior a 4.
# base R
avaliacoes[avaliacoes$estado_de_nascimento %in% c("PB", "MT") & avaliacoes$avaliacao_do_cliente < 4,]

# dplyr
avaliacoes %>%
  filter(estado_de_nascimento %in% c("PB", "MT"), avaliacao_do_cliente < 4)
