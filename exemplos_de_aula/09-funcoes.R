# Funções -----------------------------------------------------------------

# Relembrando: o que são funcões?
# Funções são nomes que guardam um código de R. Esse código é
# avaliado quando rodamos uma função.

# As funções permitem automatizar algumas tarefas comuns de uma forma mais 
# poderosa e geral do que copiar e colar. 

## Argumentos --------------------------

# Argumentos são sempre separados por vírgulas

c(1, 3, 5)


# 2 conjuntos mais amplos de argumentos:

# 1) argumentos que oferecem os dados para a função

# 2) argumentos que controlam os detalhes do que deve ser executado.

library(dplyr)
# filter(base_de_dados, variavel > 4)

# Geralmente os argumentos relativos aos dados são os primeiros. 



# Alguns argumentos tem valores por padrão, que costumam ser os valores mais
# comuns a serem utilizados.

# EX. função round() é utilizada para arredondar valores.

round(3.55555)

# Ela recebe os argumentos: 
# x - o número para arredondar, e
# digits - o número de casas decimais para serem mantidas.

# Caso não seja informada quantas casas decimais devem ser mantidas,
# utilizando o argumento digits, o valor padrão utilizado será 0, e o
# número será arredondado para não ter nenhuma casa decimal.

round(10.555) # argumento digits não informado, usará valor padrão 0

round(10.555, 1) # argumento digits informado, usará 1 casa decimal

round(10.555, 2) # argumento digits informado, usará 2 casas decimais

round(digits = 2, x = 10.555)

# Caso você não nomeie os argumentos, a ordem deles será  importante! 

round(x = 10.55, digits = 1) # O resultado será o que queremos!

round(digits = 1, x = 10.55)  # O resultado será o que queremos! Está fora de
# ordem, porém os argumentos foram nomeados. 

round(10.55, 1) # O resultado será o que queremos! Os argumentos não foram 
# nomeados, porém estão na ordem em que aparecem na função.

round(1, 10.55) # O resultado não será o que queremos :( trocamos a ordem dos 
# argumentos e não passamos os nomes, portanto o R entenderá que 1 é o numero 
# que queremos arredondar, e 10.55 é o número de casas decimais para arredondar


# Você pode descobrir quais são os argumentos de uma função lendo a documentação,
# na aba help:
?round
help(round)

# dúvida -em que direcao a funcao arredonda?

round(x = 10.55, digits = 0)
round(x = 10.45, digits = 0)

# arredonda pra baixo
floor(10.55)

# arredonda pra cima
ceiling(10.40)

# Construindo funções no R ------------------------


# Estrutura de uma função

# nome_da_funcao <- function(argumentos){
#   corpo_da_funcao # o codigo que deverá ser executado
# }

# Snippet do RStudio: fun 
name <- function(variables) {
  # codigo que a funcao executa
}

# As funções devem ter nomes! 
# Listar os argumentos da função que você quer criar dentro da funcão function() .
# Depois dos argumentos, dentro das chaves {}, escrever o corpo da função,
# ou seja, o que deve ser executado quando a função for utilizada. 

# Dica: É mais fácil começar com um código que funciona e converte-lo em uma função.

# Exemplo: Converter real em dólar
# Imagine que queremos comprar algo e o valor está em dólar, e queremos fazer
# uma conversão para saber o valor aproximado em reais
# Primeiro vamos fazer um código simples, e depois transformaremos em uma função.

valor_em_dolar <- 99 # pode virar um arg!
cotacao_dolar_em_real <- 5.12 # em 24 de fev de 2022 # pode virar um arg!
# podemos consultar a taxa no site do Banco central: https://www.bcb.gov.br/
valor_em_real <- valor_em_dolar * cotacao_dolar_em_real
valor_em_real



# Agora vamos transformar em uma função: primeiro, quais são os argumentos?
# Ou seja, o que precisamos informar para que o cálculo seja realizado?
# O valor que desejamos em dolar que desejamos converter em real,
# e a cotação do dólar em real! Portanto serão nossos argumentos.
# E o que colocaremos no corpo da função? 



converter_dolar_para_real <- function(valor_em_dolar, cotacao_dolar_em_real = 5.12) {
  
  valor_em_real <- valor_em_dolar * cotacao_dolar_em_real
  valor_em_real # valor que retorna!

}

converter_dolar_para_real(99, 5.12)

converter_dolar_para_real(99)

converter_dolar_para_real(100, 3.15)

# duvidas , e com um df?

base_de_exemplo <- tibble::tibble(
  dolar = c(100, 200, 300),
  cotacao = c(5.12, 5.20, 5.30)
)

# criar uma coluna na base, usando o resultado da funcao que criamos!
base_final <- base_de_exemplo |> 
  mutate(real = converter_dolar_para_real(dolar, cotacao))


# converter_dolar_para_real <- function(valor_em_dolar, cotacao_dolar_em_real = 5.16){
#   valor_em_real <- valor_em_dolar * cotacao_dolar_em_real
#   valor_em_real
# }

# Agora que a função está criada, podemos testar com diferentes valores!
converter_dolar_para_real(799)

# e qual seria o valor se o dolar estivesse com outra cotação?
# usando a cotacao de 2016 - 02/09/2016
converter_dolar_para_real(799, 3.24)


# Função source ------------------------------------------------------

# Para organizar  o projeto e o código, podemos deixar as funções em outro(s) 
# arquivo(s) .R,  e carregá-las no script que estamos utilizando com a função 
# source():

# source("caminho_para_o_arquivo.R")

# Atenção: a função source() vai executar todo o código que está no arquivo! 
# Caso no arquivo tenha variáveis sendo criadas, elas serão carregadas também.

variavel_exemplo <- "Amanha é carnaval"


# Ex:
listar_arquivos("exemplos_de_aula/")

source("exemplos_de_aula/11-exemplo-source.R")

listar_arquivos("exemplos_de_aula/")
