# CURSO-R
# CURSO: INTRODUÇÃO À LINGUAGEM R
# PROFESSORES: BEATRIZ MILZ, NICOLE LUDUVICE E AMANDA AMORIM
# ALUNO: MARCIO VAKASSUGUI

#___________________________________________________________________________________________________________________________________________
#________________________ASPECTOS DA TAXA DE LETALIDADE DO SARS-COV2 EM CRIANÇAS ENTRE 0 E 11 ANOS DE IDADE_________________________________
#______________________________________________________IMPORTAÇÃO DOS DADOS_________________________________________________________________
#___________________________________________________________________________________________________________________________________________


# 1) Instalação  e carregamento de pacotes ----------------Conteúdo da aula sobre pacotes (função install.packages)-------------------------
if(!require(dplyr))install.packages("dplyr")               # para operações de subsettings (select, filter, etc)
if(!require(readr))install.packages("readr")               # para carregamento da base de dados
if(!require(magrittr))install.packages("magrittr")         # para uso do operador pipe
if(!require(gt)) install.packages("gt")                    # para elaboração de tabelas

# 2) Leitura da base de dados -----------------------------Conteúdo da aula sobre importação de bases de dados (lendo tabelas)--------------
base_dados <- read_csv2("dados/INFLUD20-21-02-2022.csv")   # a base possui 1.199.928 observações e 153 variáveis 


# 3) Visualização dos dados -------------------------------Conteúdo da aula sobre funções (função nativas do R)-----------------------------
head(base_dados)

# 4) Definição das colunas a partir da análise do dicionário.pdf ------Conteúdo da aula sobre vetores---------------------------------------
colunas <- c("CS_SEXO", "DT_NASC", "NU_IDADE_N", "CS_RACA", "DT_SIN_PRI","SG_UF","ID_MN_RESI","FATOR_RISC", "CLASSI_FIN", "EVOLUCAO",
             "UTI", "HOSPITAL", "DT_EVOLUCA")

# CS_SEXO     =     Sexo do paciente (M- masculino, F- feminino, I- ignorado)
# DT_NASC     =     Data de nascimento do paciente
# NU_IDADE_N  =     Idade informada pelo paciente quando não se sabe a data de nascimento
# CS_RACA     =     Cor ou raça declarada (1-branca, 2-preta, 3-amarela, 4-parda, 5-indígena, 6-não se aplica, 9-ignorado)
# DT_SIN_PRI  =     Data de 1ºs sintomas do caso
# SG_UF       =     Unidade Federativa de residência do paciente
# ID_MN_RESI  =     Município de residência do pacient
# FATOR_RISC  =     Paciente apresenta algum fator de risco (S-sim, N-não, I-ignorado)
# CLASSI_FIN  =     Diagnóstico final do caso (SRAG por 1) influenza, 2) outro vírus, 3) outro agente, 4) não especificado, 5) covid-19)
# EVOLUCAO    =     Evolução do caso (1-cura, 2-óbito por covid, 3-óbito por outra causa, 9-ignorado)
# UTI         =     O paciente foi internado em uti (1-sim, 2-não, 9-ignorado)
# HOSPITAL    =     O paciente foi internado (1-sim, 2-não, 9-ignorado)
# DT_EVOLUCA  =     Data da alta ou óbito

# 5) Selecionar as colunas de interesse para a análise ----Conteúdo da aula sobre filtros (seleção de colunas - select()--------------------

base_dados_srag <- base_dados |>  
  select(all_of(colunas))                   #------------> a base passou a ter 1.199.928 observações e 13 variáveis

# base_dados_srag <- base_dados[colunas]    #------------> outra maneira vista na aula sobre filter e select

base_dados_srag 

# 6) Renomear as colunas -------------------------------------------------------------------------------------------------------------------
base_dados_srag <- rename(base_dados_srag, sexo = CS_SEXO, idade = NU_IDADE_N, data_nascimento = DT_NASC, raca = CS_RACA, 
                          data_1_sintomas = DT_SIN_PRI, uf = SG_UF, municipio = ID_MN_RESI,
                          fator_risco = FATOR_RISC, resultado = EVOLUCAO, uti = UTI, internado = HOSPITAL,
                          diagnostico = CLASSI_FIN, data_resultado = DT_EVOLUCA)


# 7) Visualizar os dados ----------------------------------Conteúdo da aula sobre funções (função nativas do R)-----------------------------
View(base_dados_srag)

# 8) Verificação dos tipos das variáveis ------------------Conteúdo da aula sobre funções (dplyr)-------------------------------------------
glimpse(base_dados_srag)

# 9) Filtrar o dataset de modo que contenha apenas os casos confirmados de srag - sars-cov2 --Conteúdo da aula sobre filtros----------------
base_dados_srag_sars2 <- base_dados_srag |>  
  filter(diagnostico == 5)    # diagnóstico = 5 significa que o caso do paciente foi classificado como SRAG por covid

                              # -------------------------> a base possui 711794 linhas e 13 variáveis

# 10) Salvar o dataset para posterior análise dos dados ------------------------------------------------------------------------------------
write_csv2(base_dados_srag_sars2, file = "dados/base_dados_sars2.csv")
 


