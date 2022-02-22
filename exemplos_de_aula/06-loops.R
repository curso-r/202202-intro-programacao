# Loops de repetição -------------------------------------------------------

# Uma das maiores vantagens de usar linguagens de programação para análise
# de dados é que podemos fazer o computador resolver em segundos
# coisas que nós demoraríamos muito tempo para fazer.

# Temos muitos exemplos de quando isso acontece, mas o mais direto
# é quando precisamos repetir uma mesma tarefa várias vezes

# for: repete uma mesma ação várias vezes, dentro de um determinado escopo

# escopo = contexto em que a ação será repetida

# estrutura:
# for(ESCOPO) {
# ACAO
# }

# Exemplos de ACAO

# ideia de programa: criar um programa imprima no nosso console os números de
# 1 a 10

# escopo ou contexto: os números de 1 a 10. Vamos repetir uma ação (imprimir) 
# para cada um dos pedaços
# do nosso escopo

for (i in 1:10) {
# escopo: números de 1 a 10, representados pela letra "i"
  
  print(i)
  # ação, que faz menção à letra i
  Sys.sleep(1)
}

# a AÇÃO sempre deve se aplicar a um elemento nosso contexto. 
# vamos a mais elementos de ACAO, sem mudar o contexto:

for (i in 1:10) {
  # escopo: números de 1 a 10, representados pela letra "i"
  
  # DAQUI PRA BAIXO VEM A AÇÃO:
  
  quadrado_de_i <- i^2
  # a ação pode ter quantas linhas você quiser, é um trecho de código em que 
  # aplicam as mesmas regras
  # que estamos estudando até agora
  
  # podemos criar uma variável, por exemplo!
  
  print(quadrado_de_i)
  # terminamos igual fizemos na última vez: imprimindo algo no console
  
}

# outro exemplo de ação

for (i in 1:10) {
  # escopo: números de 1 a 10, representados pela letra "i"
  
  # DAQUI PRA BAIXO VEM A AÇÃO:
  
  textinho_a_ser_impresso <- paste0("Esse é o passo: ", i)
  # a ação pode ter quantas linhas você quiser, é um trecho de código em
  # que aplicam as mesmas regras
  # que estamos estudando até agora
  
  # podemos criar uma variável, por exemplo!
  
  print(textinho_a_ser_impresso)
  # terminamos igual fizemos na última vez: imprimindo algo no console
  
}

# Exemplos de ESCOPO

# o ESCOPO segue sempre um formato específico:

# for(VARIAVEL in VETOR_TOTAL)

# nossa ACAO será executada dentro dos elementos do VETOR_TOTAL, um por um, em ordem.

# descrevemos a ACAO usando a VARIAVEL

# VARIAVEL e VETOR_TOTAL podem ser quaisquer nomes que você quiser, só precisa se lembrar
# das regras de nomes que temos no R!

pessoas_da_sala <- c("Bea", "Nicole", "Raquel", "Ale", "Laila", "Daniel")

for (pessoa in pessoas_da_sala) {
  frase <- paste0("Olá ", pessoa,"! Boas vindas!")
  Sys.sleep(0.5)
  print(frase) 
} 

for (i in pessoas_da_sala) {
  frase <- paste0("Olá ", i,"! Boas vindas!")
  Sys.sleep(0.5)
  print(frase) 
} 

for (pessoa in pessoas_da_sala) {
  Sys.sleep(0.5)
  print(paste0("Olá ", pessoa,"! Boas vindas!"))
} 




for (pessoa in c("Bea", "Nicole", "Raquel", "Ale", "Laila", "Daniel")) {
  frase <- paste0("Olá ", pessoa,"! Boas vindas!")
  Sys.sleep(0.5)
  print(frase) 
} 


# voltando ao primeiro exemplo:

# for (PEDACO in VETOR_TOTAL){
for (i in 1:10) {
# VARIAVEL, aqui chamamos de i
# o 1:10 representa o VETOR_TOTAL
  
  print(i)
  # nossa ação. precissa mencionar a letra i
}

# você pode inclusive criar o VETOR_TOTAL fora do loop:

aonde_vou_iterar <- 1:10

for (i in aonde_vou_iterar) {
  # VARIAVEL, aqui chamamos de i
  # o 1:10 representa o VETOR_TOTAL
  
  print(i)
  # nossa ação. precisa mencionar a letra i
}

# VETOR_TOTAL pode até não ter números

aonde_vou_iterar <- c("a", "b", "c", "d", "e", "f")

for (i in aonde_vou_iterar) {
  # VARIAVEL, aqui chamamos de i
  # o 1:10 representa o VETOR_TOTAL
  
  print(i)
  # nossa ação. precisa mencionar a letra i
}

# PRESTE ATENÇÃO! A ORDEM IMPORTA!

aonde_vou_iterar <- c("c", "a", "d", "b", "e", "f")
# embaralhamos a ordem das letras

for (i in aonde_vou_iterar) {
  # VARIAVEL, aqui chamamos de i
  # o 1:10 representa o VETOR_TOTAL
  
  print(i)
  # nossa ação. precisa mencionar a letra i
}

# o VARIAVEL, pode ter qualquer nome também! você só precisa se lembrar de fazer menção a ela
# na ação

aonde_vou_iterar <- c("c", "a", "d", "b", "e", "f")
# embaralhamos a ordem das letras

for (variavel in aonde_vou_iterar) {
  # VARIAVEL, aqui chamamos de i
  # o 1:10 representa o VETOR_TOTAL
  
  print(variavel)
  # nossa ação. precisa mencionar a letra i
}

# Exemplo: calcular o atraso de saída dos voos para cada aeroporto de origem
library(readr)
library(magrittr)
library(dplyr)

base_de_dados <- read_csv2("dados/voos_de_janeiro.csv")

aeroportos_de_origem <- unique(base_de_dados$origem)

for (aero in aeroportos_de_origem) {
  #aero <- aeroportos_de_origem[1]
  dados_aero <- base_de_dados %>% filter(origem == aero)
  
  media <- mean(dados_aero$atraso_saida, na.rm = TRUE)
  
  media_arredondada <- round(media, 1)
  
  # print(paste0("Analisando os dados do aeroporto ", aero, "...."))
  # print(paste0("Para o aeroporto de ",
  #              aero,
  #              ", a média de atraso de saída de voos (em minutos) é de ",
  #              media_arredondada, "."))
  
  cat(paste0("Analisando os dados do aeroporto ", aero, ".... \n Para o aeroporto de ",
               aero,
               ", a média de atraso de saída de voos (em minutos) é de ",
               media_arredondada, "."))
  
}


# Exercícios --------------------------------------------------------------

# 1. Crie um script que imprima o texto "Esse aqui é o número XX", 
# onde XX varia de 1 a 50.

for (numero in 1:50) {
  
  textinho_a_ser_impresso <- paste0("Esse aqui é o número ", numero)
  
  print(textinho_a_ser_impresso)
  Sys.sleep(0.1)
}


for (variable in 1:50) {
  texto1 <- paste0("Esse aqui é o número ", variable)
  print(texto1) 
}

# 2. Crie um script que calcule o quadrado dos números de 7 a 31, calcule o 
# quadrado desses números e imprima na tela
# Cada vez que a nossa AÇÃO for executada (calcular o quadrado), 
# escreva seu programa de tal maneira que
# o computador indique o que está fazendo, conforme o esquema abaixo:

# "Iniciando ação",
# "Calculando o quadrado de XX" (essa frase será repetida para XX indo de 7 a 31)
# "O quadrado de XX vale YY"

# Uma das repetições do seu código deve imprimir as três frases:

# "Iniciando ação"
# "Calculando o quadrado de 31"
# "O quadrado de 31 vale 961"

for (i in 7:31) {
  one <- paste0("Iniciando ação")
  print(one)
  two <- paste0("Calculando o quadrado de ", i)
  print(two)
  three <- paste0("O quadrado de ", i, " vale ", i^2)
  print(three)
  
}



for (numero in 7:31) {
  
  print(paste0("Iniciando a ação..."))
  
  Sys.sleep(0.5)
  
  print(paste0("Calculando o quadrado de ", numero))
  
  Sys.sleep(0.5)
  
  quadrado_do_numero <- numero^2
  
  print(paste0("O quadrado de ", numero, " é ", quadrado_do_numero))
  
}

for (numero in 7:31) {
  
  quadrado <- numero^2
  cat("Iniciando a ação... \n Calculando o quadrado de ", numero,
           "... \n O quadrado de ", numero, " é ", quadrado)
}



for (quadrados in 7:31) {
  primeiro<-paste0("iniciando a ação")
  segundo<-paste0("calculando o quadrado de ", quadrados)
  quadrado_numeros<-quadrados^2
  terceiro<-paste0("esse é o resultado dos quadrados:", quadrado_numeros)
 # print(quadrado_numeros)
  print(primeiro)
  print(segundo)
  print(terceiro)
}
  
    
  
# Mais utilidades do for -------------

# abrir arquivos manualmente e procurar por informações neles pode tomar muito
# do nosso tempo...

# podemos usar o for para facilitar a nossa vida!

library(readr)

arquivos_de_dados <- c("dados/voos_de_janeiro.csv",
                       "dados/voos_de_fevereiro.csv",
                       "dados/voos_de_marco.csv")
# vetor de textos com o caminhos dos nossos arquivos

for(arquivo in arquivos_de_dados){
# como ficou o nosso ESCOPO?
# VARIAVEL aqui levou esse nome "arquivo"
# VETOR_TOTAL aqui levou o nome "arquivos_de_dados"
  
  dados <- read_csv2(arquivo)
  
  maior_atraso <- max(dados$atraso_saida, na.rm = TRUE)
  
  texto_de_saida <- paste0("O maior atraso de saída no arquivo ", arquivo, " é ", maior_atraso)
  
  print(texto_de_saida)
  
}

# o que não está muito legal nesse código?





















# vamos analisar o que ele já está cuspindo






















# 1. ele imprime varias informações que poluem a tela, mas isso naõ é um erro!

# para cada arquivo, o código de leitura devolve os tipos das colunas lidas
# Isso é bom para sabermos como que fica!

# 2. o máximo está dando NAs

arquivos <- c("dados/voos_de_janeiro.csv", "dados/voos_de_fevereiro.csv", "dados/voos_de_marco.csv")

for(arquivo in arquivos){
  
  
  dados <- suppressMessages(read_csv2(arquivo))
   
  
  maior_atraso <- max(dados$atraso_saida, na.rm = TRUE)
  
  texto_de_saida <- paste0("O maior atraso no arquivo ", arquivo, " é ", maior_atraso)
  
  print(texto_de_saida)
  
}

for(arquivo in arquivos){
  
  
 # dados <- read_csv2(arquivo, show_col_types = FALSE)
  
  dados <- read_delim("dados/voos_de_janeiro.csv", 
             delim = ";", escape_double = FALSE, 
             trim_ws = TRUE,
             show_col_types = FALSE)
  
  
  maior_atraso <- max(dados$atraso_saida, na.rm = TRUE)
  
  texto_de_saida <- paste0("O maior atraso no arquivo ", arquivo, " é ", maior_atraso)
  
  print(texto_de_saida)
  
}

# exemplo: abrindo tudo de uma vez!!!
vetor_com_meses <- list.files(path = "dados",
                              pattern = ".csv",
                              full.names = TRUE)

# lendo um dos meses
read_csv2(vetor_com_meses[1])

# for (mes in vetor_com_meses) {
#   ...
# }

library(purrr)

voos_2013 <- map_dfr(vetor_com_meses, read_csv2)

unique(voos_2013$mes)

# Exercícios --------------------------------------------------------------

# 1. Inclua no nosso loop os arquivos que indo até junho

arquivos <- c("dados/voos_de_janeiro.csv",
              "dados/voos_de_fevereiro.csv",
              "dados/voos_de_marco.csv",
              "dados/voos_de_abril.csv",
              "dados/voos_de_maio.csv",
              "dados/voos_de_junho.csv")

# 2. Adapte o script anterior para que ele imprima os maiores atrasos e também
# os voos que saíram mais adiantados.

library(readr)
library(dplyr)

for(arquivo in arquivos){


  dados <- suppressMessages(read_csv2(arquivo))


  maior_atraso <- max(dados$atraso_saida, na.rm = TRUE)

  mais_adiantado <- min(dados$atraso_saida, na.rm = TRUE)

  texto_de_saida <- paste0("O maior atraso no arquivo ",
                           arquivo,
                           " foi de ",
                           maior_atraso,
                           " minutos. O tempo que o voo mais adiantado saiu foi ",
                           abs(mais_adiantado),
                           " minutos antes do esperado." )

  print(texto_de_saida)
}

# Dica: Na nossa base de dados um valor negativo na coluna "atraso_saida" indica
# que o voo saiu adiantado




# resposta daniel -----

library(readr)
library(dplyr)

arquivos <- c("dados/voos_de_janeiro.csv", "dados/voos_de_fevereiro.csv", "dados/voos_de_marco.csv",
              "dados/voos_de_abril.csv", "dados/voos_de_maio.csv", "dados/voos_de_junho.csv")

for (arquivo in arquivos) {
  dados <- read_csv2(arquivo)
  maiores_atrasos <- max(dados$atraso_saida, na.rm = TRUE)
  print(paste0("o maior atraso observado foi de ", maiores_atrasos))
  maior_adiantamento <- min(dados$atraso_saida, na.rm = TRUE)
  print(paste0("o voo que saiu mais adiantado saiu ", maior_adiantamento, " minutos"))
  print(paste0("ou seja, o voo mais adiantado saiu ", abs(maior_adiantamento), " minutos antes do previsto"))
}

for (x in 1:10) {
  print(x)
  Sys.sleep(0.5)
}




# While ------
# O while é outra estrutura de repetição muito comum em
# linguagens de programação.

# Leia como: Enquanto uma condição for verdadeira, faça XYZ.

# while (CONDICAO) {
#   O QUE FARÁ
# }

# Exemplo:
dia <- 1
while (dia < 30) {
  print(paste0("O mês ainda não acabou! Hoje é dia ", dia, "!"))
  dia <- dia + 1 # se comentar essa linha, o loop nao acabará!
  Sys.sleep(0.1)
}

