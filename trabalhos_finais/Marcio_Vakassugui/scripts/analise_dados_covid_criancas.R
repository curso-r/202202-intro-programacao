# CURSO-R
# CURSO: INTRODUÇÃO À LINGUAGEM R
# PROFESSORES: BEATRIZ MILZ, NICOLE LUDUVICE E AMANDA AMORIM
# ALUNO: MARCIO VAKASSUGUI

#___________________________________________________________________________________________________________________________________________
#________________________ASPECTOS DA TAXA DE LETALIDADE DO SARS-COV2 EM CRIANÇAS ENTRE 0 E 11 ANOS DE IDADE_________________________________
#__________________________________________________PŔE-PROCESSAMENTO DOS DADOS______________________________________________________________


# 1) Carregamento de pacotes ---------------------------------------------------Conteúdo da aula sobre pacotes (função install.packages)----
library(dplyr)                                          # para operações de subsettings (select, filter, etc)
library(readr)                                          # para carregamento da base de dados
library(magrittr)                                       # para uso do operador pipe
library(esquisse)                                       # para o uso de interface gráfica para elaboração de gráficos com ggplot2
if(!require(ggplot2)) install.packages("ggplot2")       # para elaboração e gráficos
if(!require(skimr)) install.packages("skimr")           # para análise de valores missing
if(!require(lubridate)) install.packages("lubridate")   # para manipulação de datas
if(!require(gt)) install.packages("gt")                 # para elaboração de tabelas

# 2) Leitura da base de dados ---------------------------------------Conteúdo da aula sobre importação de bases de dados (lendo tabelas)----
base_dados_sars2 <- read_delim("dados/base_dados_sars2.csv",show_col_types = FALSE, id = NULL)
                                      #---------------> o dataset possui 711794 linhas e 13 variáveis

# 3) Visualização dos dados --------------------------------------------------------Conteúdo da aula sobre funções (função nativas do R)----
head(base_dados_sars2)

# 4) verificar tipos das variáveis ---------------------------------------------------------------Conteúdo da aula sobre funções (dplyr)----
glimpse(base_dados_sars2)                              # indica os tipos das variáveis identificados pelo Rstudio

# 5) Alteração dos tipos das variáveis------------------------------------------------------------------------------------------------------
base_dados_sars2$sexo <- as.factor(base_dados_sars2$sexo)
base_dados_sars2$data_nascimento <-  dmy(base_dados_sars2$data_nascimento) #Conteúdo live do curso-r 18/02/2022 (lubridate/youtube)
base_dados_sars2$data_1_sintomas <-  dmy(base_dados_sars2$data_1_sintomas) #Conteúdo live do curso-r 18/02/2022 (lubridate/youtube)
base_dados_sars2$fator_risco <- as.factor(base_dados_sars2$fator_risco)
base_dados_sars2$diagnostico <- as.factor(base_dados_sars2$diagnostico)
base_dados_sars2$resultado <- as.factor(base_dados_sars2$resultado)
base_dados_sars2$uti <- as.factor(base_dados_sars2$uti)
base_dados_sars2$internado <- as.factor(base_dados_sars2$internado)
base_dados_sars2$data_resultado <-  dmy(base_dados_sars2$data_resultado)  #Conteúdo live do curso-r 18/02/2022 (lubridate/youtube)

# 6) recodificar das variáveis categóricas  ------------------------------------------------------------------------------------------------
base_dados_sars2$sexo <- recode_factor(base_dados_sars2$sexo, "M" = "masculino", "F" = "feminino", "I" = "ignorado")
base_dados_sars2$raca <- recode_factor(base_dados_sars2$raca, "1" = "branco", "2" = "preto", "3" = "amarela", "4" = "pardo",
                                        "5" = "indigena", "6" = "não_se_aplica", "9" = "ignorado")
base_dados_sars2$fator_risco <- recode_factor(base_dados_sars2$fator_risco,"S" = "sim", "N" = "não", "I" = "ignorado")
base_dados_sars2$diagnostico      <- recode_factor(base_dados_sars2$diagnostico,"1" = "influenza", "2" = "outro_virus",
                                                   "3" = "outro_agente", "4" = "nao_especificado", "5" = "covid")
base_dados_sars2$resultado <- recode_factor(base_dados_sars2$resultado,"1" = "cura", "2" = "obito_por_covid", "3" = "obito_outra_causa",
                                            "9" = "ignorado")
base_dados_sars2$uti <-  recode_factor(base_dados_sars2$uti, "1" = "sim", "2" = "não", "9" = "ignorado")
base_dados_sars2$internado <- recode_factor(base_dados_sars2$internado, "1" = "sim", "2" = "não", "9" = "ignorado")

# 7) Verificar a existência de valores missing usando o pacote skimr------------------------------------------------------------------------
skim(base_dados_sars2)                                  # da analise podemos observar que 99.8% dos dados de data de nascimento foram 
                                                        # fornecidos, 1098 campos missing, portanto podemos calcular a idade dos paciente.

# 8) Calcular e criar a coluna idade_calculada----------------------------------------Conteúdo da aula sobre controle de fluxo e funções----
# obs. a base de dados fornece as datas de nascimentos e quando não informado pelo paciente a idade. 
# PARA FINS DIDÁTICOS, com o objetivo de aplicarmos o conteúdo sobre estruturas de repeticão (laços for e while) e estruturas de seleção 
# (if else) e funções, criaremos uma coluna "idade_calculada", preenchida pela valor obtido pela diferença entre a data de nascimento e 
# a data de 1ºs sintomas (conforme dicionário.pdf em anexo). Quando a data de nascimento não é fornecida, deve-se usar a idade declarada.

base_dados_sars2["idade_calculada"] <- 0                # criamos a coluna "idade_calculada" e a inicializamos com zeros

base_dados_sars2_0a30 <- base_dados_sars2 |>            # Reduzimos a base de dados considerando apenas as pessoas entre 0 e 30 anos,
                filter(idade <= 30)                     # pois nossa análise será feita entre as crianas de 0 a 11 anos.

col_idade <- base_dados_sars2_0a30 |>                   # selecionamos os dados da base para o cálculo das idades
  select(idade, data_nascimento, data_1_sintomas)

calculo_idade <- function(data01, data02) {             # criamos a função para o cálculo da variável local idade
  idade <-  floor(as.numeric((data01 - data02)/365.25)) # por causa de anos bissextos entre as datas, deve-se dividir o resultado por 365.25
  return(idade)
}

idade_calc <- c()                                       # criamos o vetor que armazenará os dados das idades calculadas

for (i in 1:nrow(col_idade)) {                          # loop for que  calcula as idades com a função "calculo_idade" quando houver data de  
  if(!is.na(col_idade$data_nascimento[i])){             # nascimentos e que considera a idade fornecida quando não houver data de nascimento
    idade_calc <- append(idade_calc,calculo_idade(col_idade$data_1_sintomas[i], col_idade$data_nascimento[i]))
  }
  else{
    idade_calc <- append(idade_calc,col_idade$data_nascimento[i])
  }
}

base_dados_sars2_0a30$idade_calculada <- idade_calc # atribuímos os valores para o registros da variável idade_calculada


# Observação:
# O dicionário de dados, anexo "dicionário.pdf", informa que "se digitado a data de nascimento, a idade é calculada e preenchida
# automaticamente pelo sistema: considerando o intervalo entre a data de nascimento e A DATA DOS PRIMEIROS SINTOMAS". 
# Ao realizarmos os cálculos da idade dos pacientes, constatamos que os dados fornecidos pelo Ministério da Saúde informam as idades 
# das crianças com menos de 1 ano de idade com problema de escala (dias e meses são informados como anos)
# Ex1. Um paciente nascido em 27/03/2020 que apresentou os primeiros sintomas em 20/04/2020, com fator de risco e que evoluiu para óbito por 
# covid teve sua idade informada como 1 ano ao invés de 1 mês.
# Ex2. Um paciente nascido em 15/08/2019 que apresentou os primeiros sintomas em 28/04/2020, sem fator de risco e que evoluiu para óbito por
# covid teve sua idade informada como 8 anos ao invés de 8 meses. 
# Assim, na coluna "idade_calculada" o valor 0 (zero) representa crianças com menos de um ano de idade.

# 9) Exclusão da coluna idade da base original com problema de escala para as crianças com menos de 1 ano-----------------------------------
base_dados_sars2_0a30$idade <- NULL

# 10) Salvar o dataset ---------------------------------------------------------------------------------------------------------------------
write_csv2(base_dados_sars2_0a30, file = "dados/base_dados_sars2_0a30.csv")

#___________________________________________________________________________________________________________________________________________
#__________________________________________________________ANÁLISE DOS DADOS________________________________________________________________
#___________________________________________________________________________________________________________________________________________

# 11) Total de crianças até 11 anos hospitalizadas entre 2020 e 2022 com SRAG por COVID ----------------------------------------------------
total_criancas_hospitalizadas <- base_dados_sars2_0a30 |>  
                                  filter(idade_calculada <=11)            #------------> 11114 crianças

# 12) Total de crianças até 11 anos hospitalizadas entre 2020 e 2022 com SRAG por COVID por faixa de idades---------------------------------
intervalo_classes_idade <- round(seq(0,12, 2),1)

total_criancas_hospitalizadas_idade <- table(cut(total_criancas_hospitalizadas$idade_calculada,
                                  breaks = intervalo_classes_idade,
                                  right = FALSE))

                                                                                          # (0  -  2]   =  5442
                                                                                          # (2  -  4]   =  1728
                                                                                          # (4  -  6]   =  1211
                                #-------------------------------------------------->      # (6  -  8]   =  980
                                                                                          # (8  -  10]  =  920
                                                                                          # (10 -  12]  =  833
total_criancas_hospitalizadas_idade

# 13) Gráfico 01- "Casos Totais de Covid de Crianças por Idade"-----------------------------------------------------------------------------
tabela03 <- total_criancas_hospitalizadas_idade |> 
  data.frame(Idade = c("(00 - 02]", "(02 - 04]", "(04 - 06]", "(06 - 08]", "(08 - 10]", "(10 - 12]")) |> 
  rename("Casos" = "Freq") |> 
  select(Idade, Casos)

#esquisser(tabela03)
grafico01 <- ggplot(tabela03) +
 aes(x = Idade, fill = Casos, weight = Casos) +
 geom_bar(width = 1) +
 scale_fill_distiller(palette = "RdYlBu", 
 direction = -1) +
 labs(x = "Idades", y = "Quantidades", title = "Casos Totais de Covid de Crianças por Idade") +
 coord_flip() +
 theme_classic()

grafico01

# 14) Salvar a figura em formato png com o gráfico de casos totais por idade----------------------------------------------------------------
res <-  300
fig_size <-  (res * 230/72)
png("imagens/grafico01.png",
    res = res,
    width = 2 * fig_size,
    height = fig_size)
grafico01
dev.off()
                                                                                
# 15) Total de crianças hospitalizadas com SRAG por COVID e raça ---------------------------------------------------------------------------
base_dados_sars2_0a30 |>
  group_by(raca) |> 
  filter(idade_calculada <= 11 & !is.na(raca)) |> 
  summarize( qtd = n())                                                                   # 2751  brancas
                                                                                          # 323   pretas
                                #-------------------------------------------------->      # 54    amarelas
                                                                                          # 5615  pardos
                                                                                          # 180   indígenas
                                                                                          # 1932  não declarado
                                                                                          
# Mesma análise acima utilizando table()
total_criancas_hospitalizadas_raca <- table(total_criancas_hospitalizadas$raca)
total_criancas_hospitalizadas_raca

# 16) Tabela 02 -Caso SARS-COV2 por raça para o manuscrito ---------------------------------------------------------------------------------

tabela02 <- total_criancas_hospitalizadas_raca |> 
  data.frame(Raça= c("branco", "preto","amarelo","pardo","indígena", "ignorado")) |> 
  rename("Quantidade" = "Freq") |> 
  select( Raça, Quantidade) |>
  gt() |> 
  tab_header(title = md("**Tabela 02: Casos de SARS-COV2 por raça**"),
             subtitle = "Crianças de 0 a 11 anos") |> 
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3),
              column_labels.background.color = "#A4B8C4") |> 
  tab_footnote(footnote = "Fonte: DataSus - Ministério da Saúde",
               locations = cells_title())  
  
tabela02

# 17) Salvar a tabela SARS-COV2 por raça e para o manuscrito -------------------------------------------------------------------------------
gtsave(tabela02,"tab02.png", expand = 10, path = "imagens")

# 18) Gráfico 02 SARS-COV2 por raca para o manuscrito---------------------------------------------------------------------------------------
dados_grafico_02 <- base_dados_sars2_0a30 |> 
                      group_by(raca) |> 
                      filter(idade_calculada <=11 & !is.na(raca)) |> 
                      summarise(freq = n()) |> 
                      rename("Quantidade" = "freq", "Raça" = "raca") |> 
                      data.frame() 

grafico02 <- ggplot(dados_grafico_02) +
               aes(x = Raça, fill = Raça, weight = Quantidade) +
               geom_bar() +
               scale_fill_viridis_d(option = "magma", 
               direction = 1) +
               labs(x = "Raça", y = "Quantidade", fill = "Raça") +
               scale_fill_manual(values = c("#BEE9E8", "#4DAA57", "#D90368", "#111D4A", "#0075F2", "#BC8DA7"))+
               coord_flip() +
               theme_classic()

# 19) Salvar gráfico 02 SARS-COV2 por raça--------------------------------------------------------------------------------------------------
res <-  300
fig_size <-  (res * 230/72)
png("imagens/grafico02.png",
    res = res,
    width = 2 * fig_size,
    height = fig_size)
grafico02
dev.off()


# 18) Gráfico 03 -Óbitos por SARS-COV2 por raça para o manuscrito --------------------------------------------------------------------------
grafico03 <- base_dados_sars2_0a30 |> 
  group_by(raca) |> 
  filter(idade_calculada <=11 & !is.na(raca)) |> 
  summarise(freq = n(),
            freqac = cumsum(freq)) |> 
  ggplot() +
  aes(x = "", y = freq, fill = raca) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y")+
  scale_fill_manual(values = c("#5B6086", "#B0829A", "#DA4167", "#80A4ED", "#FFD400", "#4C3957"))+
  labs(title = "Óbitos por Covid x Raça", x = "", y = "")+
  theme_minimal()

grafico03

# 19) Salvar O gráfico 03- Óbitos por SARS-COV2 por raça para o manuscrito------------------------------------------------------------------
res <-  300
fig_size <-  (res * 230/72)
png("imagens/grafico03.png",
    res = res,
    width = 2 * fig_size,
    height = fig_size)
grafico03
dev.off()


# 20) Total de crianças até 11 anos hospitalizadas entre 2020 e 2022 com SRAG por COVID com evolução para óbito por covid-------------------
total_criancas_obito_por_covid <- base_dados_sars2_0a30  |>  
  filter(idade_calculada <= 11 & resultado == "obito_por_covid")  #--------------------------------->      # 709 crianças 

# 21) Total de crianças até 11 anos hospitalizadasa entre 2020 e 2022 com SRAG por COVID com evolução par óbito por outra causa ------------
total_criancas_obito_covid_outras <- base_dados_sars2_0a30 |>  
  filter((idade_calculada <=11) &(resultado == "obito_por_covid" | resultado == "obito_outra_causa" ))  #-------------->      # 766 crianças 

# 22) Tabela intervalo de classes por óbito-------------------------------------------------------------------------------------------------
intervalo_classes_idade_obito <- round(seq(0,12, 2),1)
tabela_classes_idade_obito <- table(cut(total_criancas_obito_por_covid$idade_calculada,
                                  breaks = intervalo_classes_idade_obito,
                                  right = FALSE))
View(tabela_classes_idade_obito)                                                          # (0  -  2 ]   =  466
                                                                                          # (2  -  4 ]   =  61
                                                                                          # (4  -  6 ]   =  54
                                #-------------------------------------------------->      # (6  -  8 ]   =  38
                                                                                          # (8  -  10]   =  41
                                                                                          # (10 -  12]   =  49

  # 23) Gráfico 04 - Casos de óbitos por Covid em crianças por raça --------------------------------------------------------------------------
grafico04 <- ggplot(as.data.frame(tabela_classes_idade_obito, colnames(names)))+
  aes(x = Var1,
      fill = Var1,
      weight = Freq) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("#BC96E6", "#0D3B66", "#4D5359", "#B4DC7F", "#A3C3D9", "#387780"))+
  labs(x = "idades",
       y = "Quantidade de Casos",
       title = "Casos de Óbitos por Covid em Crianças por Idade",
       fill = "Idades") +
  theme_classic()

grafico04

# 24) Salvar a figura em formato png com o gráfico de casos de SARS-COV3 em crianças por classes de idade ----------------------------------
res <-  300
fig_size <-  (res * 230/72)
png("imagens/grafico04.png",
    res = res,
    width = 2 * fig_size,
    height = fig_size)
grafico04
dev.off()

# 25) Tabela de frequências absolutas de óbitos de crianças por raça -----------------------------------------------------------------------
total_criancas_obito <- base_dados_sars2_0a30  |>  
  group_by(raca)  |>  
  filter(idade_calculada <=11 & resultado == "obito_por_covid")         

f_abs_criancas_obitos <- table(total_criancas_obito$raca)
f_abs_criancas_obitos                                                                     # brancas     =   136
                                                                                          # pretos      =   20 
                                                                                          # amarelos    =   6 
                                #-------------------------------------------------->      # pardos      =   394
                                                                                          # indígenas   =   39 
                                                                                          # ignorado    =   92

f_r_criancas_obitos <- round(prop.table(f_abs_criancas_obitos)*100,3)
f_r_criancas_obitos                                                                       # brancas     =   19,796% 
                                                                                          # pretos      =   2,911%
                                                                                          # amarelos    =   0,873%
                                #-------------------------------------------------->      # pardos      =   57,351%
                                                                                          # indígenas   =   5,677% 
                                                                                          # ignorado    =   13,392%

# 26) Cálculo da taxa de letalidade --------------------------------------------------------------------------------------------------------
# Entende-se como taxa de variação de letalidade a relação entre o número total de óbitos em um determinado período dividido pelo total 
# de casos em que o paciente obteve alta do tratamento ou veio a óbito.
# A taxa de letalidade

# total de óbitos entre crianças de 0 a 11 anos
total_criancas_obito_tl <- base_dados_sars2_0a30  |>  
  filter(idade_calculada <=11 & resultado == "obito_por_covid")  |> 
  nrow() #--------------------------------------------------------------------------->    # total de óbitos por covid = 709

# total de casos alta ou óbito entre as crianças de 0 a 11 anos
# não consideraremos o total de óbitos por outra causa, de ignorados, mas tão somente o total de cura e o total de óbitos obtidos a partir
# da variável resultado;
# lembrando que 1- cura, 2- óbito por covid, 3- óbito por outra causa e 9-ignorado.
total_vitimas_com_alta_obito_tl <- base_dados_sars2 |> 
  filter(resultado == "obito_por_covid" | resultado == "cura") |> 
  nrow() #--------------------------------------------------------------------------->    # total de vítimas considerando todas as idades
                                                                                          # igual a 664193
  
letalidade_criancas_0a11 <- round(((total_criancas_obito_tl)/(total_vitimas_com_alta_obito_tl))*100,3)
letalidade_criancas_0a11                                                                  


                                                                                         # taxa de letalidade de 0,107%
                                                                                         # embora a taxa de letalidade de crianças ser baixo
                                #-------------------------------------------------->     # em relação ao número total de vítimas, em termos 
                                                                                         # absolutos é elevado.

# 27) Tabela01 campos utilizados na análise para o manuscrito ------------------------------------------------------------------------------
cabecalho <- c("Campo", "Descrição", "Preenchimento")
campo <- c("CS_SEXO", "DT_NASC", "NU_IDADE_N", "CS_RACA", "DT_SIN_PRI","SG_UF","ID_MN_RESI","FATOR_RISC", "CLASSI_FIN", 
           "EVOLUCAO", "UTI", "HOSPITAL", "DT_EVOLUCA")
nome <- c("sexo", "idade", "data_nascimento", "raca", "data_1_sintomas", "uf", "municipio", "fator_risco","diagnostico",
          "resultado", "uti", "internado", "data_resultado")
preenchimento <- c("M- masculino, F- feminino, I- ignorado", "data de nascimento", "numérico",  "1-branca, 2-preta, 3-amarela, 4-parda, 
                   5-indígena,6-não se aplica,9-ignorado", "data do primeiro sintoma", "sigla da UF ",
                   "código do município estabelecido pelo IBGE ", "S-sim, N-não, I-ignorado", "1) influenza, 2) outro vírus,
                   3) outro agente, 4) não especificado, 5) covid-19","1-cura, 2-óbito por covid, 3-óbito por outra causa, 9-ignorado",
                   "1-sim, 2-não, 9-ignorado", "1-sim, 2-não, 9-ignorado", "data da alta ou óbito")

tabela01 <- data.frame(campo, nome, preenchimento)
colnames(tabela01) <- c("Campo", "Descrição", "Preenchimento")

tabela01 |> 
  gt() |> 
  tab_header(title = md("**Tabela 01: Campos Utilizados na Análise**")) |> 
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3),
              column_labels.background.color = "#A4B8C4") |> 
  tab_style( style = list(cell_borders(sides = c("left", "right", "top", "bottom"), color = "black", weight = px(1))), 
             locations = list(cells_body(columns = c(Campo, Descrição, Preenchimento)))) |> 
  tab_footnote(footnote = "Fonte: DataSus - Ministério da Saúde",locations = cells_title()) |> 
  gtsave("tab01.png", path = "imagens")


