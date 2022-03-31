### TCC parte 1

# instalando pacotes e carregando bibliotecas -----------------------------
install.packages("spotifyr")
library(spotifyr)

# criando acesso ao meu spotify -------------------------------------------

# entrar no site https://developer.spotify.com/dashboard/login
# realizar o seu login
# clicar em create an app
# escolher um app name e uma description e clicar em create
# uma nova pagina e aberta e fornece o seu client id e client secret
# que devera ser utilizada no passo seguinte para voce ter acesso ao 
# seu spotify

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxx')
access_token <- spotifyr::get_spotify_access_token()

# recuperando os 50 artistas mais ouvidos e salvando num para posterior analise ----------------------------------

# o proximo passo sera obter os top 50 artistas mais ouvidos no 
# longo prazo utilizando a funçao abaixo. ha possibilidade de alterar
# o time range e o limit (numero de artistas recuperados)

top_artists_all_time <- get_my_top_artists_or_tracks(type = 'artists', 
                                                     time_range = 'long_term', 
                                                     limit = 50)

# por fim, irei salvar o dataframe que foi recuperado. como dentro de
# algumas colunas haviam listas, nao foi possivel salvar em csv. Dessa
# maneira, uma alternativa foi salvar utilizando o saveRDS. o arquivo
# gerado sera salvo no computador e permite ser aberto para posterior
# manipulacao no r

saveRDS(top_artists_all_time, file="top_artists_laila.RData")

# recuperando as 50 faixas mais ouvidas e salvando para posterior analise -----------------------------------

# obtencao das top 50 musicas mais ouvidas no longo prazo utilizando 
# a funcao 

top_tracks_all_time <- get_my_top_artists_or_tracks(
  type = "tracks",
  limit = 50,
  offset = 0,
  time_range = "long_term",
  authorization = get_spotify_authorization_code(),
  include_meta_info = FALSE)

# salvar o dataframe que foi recuperado utilizando o saveRDS. o arquivo
# gerado sera salvo no computador e permite ser aberto para posterior
# manipulacao no r
saveRDS(top_tracks_all_time, file="top_tracks_laila.RData")
