# base de voos!

# objetivos!
# ler dados e exportar
# conta de estatistica descritiva: media, ...
# gerar base!
# criar funcao
# if/else
# for!

# carregar pacotes ------
library(readr)
library(fs)
library(dplyr)

# criar objetos ------
# que depois eu quero transformar em um argumento
# arquivo <- "dados/voos_de_janeiro.csv"
# coluna <- "distancia"
# meta <- 1000
# pasta <- "exemplo_final"

gerar_tabela_descritiva <- function(arquivo, 
                                    coluna, 
                                    meta,
                                    pasta = "exemplo_final"){
  # leitura -----
  base_de_dados <- suppressMessages(read_csv2(arquivo)) 
  
  # filtro -----
  coluna_desejada <- base_de_dados[[coluna]]
  
  # calcular estatisticas desc.
  
  media <- round(mean(coluna_desejada, na.rm = TRUE), 2)
  
  desvio_padrao <-  round(sd(coluna_desejada, na.rm = TRUE), 2)
  
  minimo <- min(coluna_desejada, na.rm = TRUE)
  
  maximo <- max(coluna_desejada, na.rm = TRUE)
  
  nome_arquivo <- tools::file_path_sans_ext(basename(arquivo))
  
  if(media >= meta){
    media_maior_que_valor_meta <- TRUE
  } else {
    media_maior_que_valor_meta <- FALSE
  }
  
  
  
  # cria tabela
  tab_descritiva <- tibble::tibble(nome_arquivo , coluna, media,
                                   desvio_padrao, minimo, maximo,
                                   media_maior_que_valor_meta)
  
  
  # criar pasta
  fs::dir_create(pasta)
  
  # criar nome do arquivo para salvar
  nome_arquivo_salvar <- paste0(pasta, "/tabela_descritiva_", 
                                nome_arquivo, "_coluna_", 
                                coluna, ".xlsx")
  
  # salvar a tabela no computador
    writexl::write_xlsx(tab_descritiva, nome_arquivo_salvar)
  
  
  tab_descritiva
  
}

# exemplo de como usar a funcao!
gerar_tabela_descritiva(arquivo = "dados/voos_de_janeiro.csv",
                        coluna = "tempo_voo",
                        meta = 160,
                        pasta = "analises")

# vamos fazer para varios arquivos!

arquivos <- list.files(path = "dados", pattern = ".csv", full.names = TRUE)

base_completa <- NULL
for (arq in arquivos) {
  
  base_parcial_tempo_voo <- gerar_tabela_descritiva(arquivo = arq,
                          coluna = "tempo_voo",
                          meta = 160,
                          pasta = "exemplo_final")
  
  base_parcial_distancia <- gerar_tabela_descritiva(arquivo = arq,
                                                    coluna = "distancia",
                                                    meta = 1000,
                                                    pasta = "exemplo_final")
  
  
  base_completa <- bind_rows(base_completa,
                             base_parcial_tempo_voo ,
                             base_parcial_distancia )
  
  base_completa
}

write_csv2(base_completa, "base_completa.csv")

# ideia do ale - criar uma coluna que tem um valor
# calculaado baseado nas outras colunas

voos <- read_csv2("dados/voos_de_agosto.csv")

voos |> 
  mutate(distancia_pelo_tempo = round(distancia/tempo_voo, 2)) |> View()
