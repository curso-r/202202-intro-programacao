### TCC parte 2

# instalando pacotes e carregando bibliotecas -----------------------------
install.packages("tidyverse")
install.packages("lubridate")

library(tidyverse)
library(lubridate)

# carregando os dataframes gerados pelo spotifyr --------------------------
top_artists <- readRDS("top_artists_laila.RData")
top_tracks <- readRDS("top_tracks_laila.RData")
  #salvando o arquivo RDS como objeto no R para posterior manipulacao

# descobrindo os top 10 generos mais ouvidos de acordo com os artistas --------
genres <- pull(top_artists, genres) %>% unlist()
genres_freq <- as.data.frame(table(genres))
genres_freq %>% arrange(desc(Freq)) %>% head(10)
  #para recuperar os top10 generos, eu tive que desfazer a lista em que os generos
  #estavam salvos, dei um table para contabilizar, ordenei de forma descendente e
  #por fim mostrei apenas os primeiros 10 resultados com o head. A maioria dos
  #generos musicais referem-se a musica brasileira

# descobrindo qual o artista com mais e menos seguidores que escuto -------
followers <- top_artists %>% select(name, followers.total)
followers %>% arrange(desc(followers.total)) %>% head(1)
followers %>% arrange(desc(followers.total)) %>% tail(1)
  #para descobrir o artista com mais e menos seguidores eu recuperei o nome e a
  #quantidade total de seguidores no spotify e ordenei de maneira decrescente. 
  #o com maior numero de seguidores foi acessado com o head e o com menor com o tail.
  #observa-se que o artista com maior numero de seguidores foi a ariana grande e o
  #com menor numero foi a outra banda da lua, banda do estado de minas gerais.

# descobrindo qual o artista mais popular que escuto -------
popularity <- top_artists %>% select(name, popularity)
popularity %>% arrange(desc(popularity)) %>% head(1)
popularity %>% arrange(desc(popularity)) %>% tail(1)
  #para descobrir a popularidade foi realizado um processo semelhante ao executado
  #no topico anterior. novamente, o mais popular foi a ariana grande e o menos
  #popular foi a lamparina e a primavera.

#calculando a media e mediana de popularidade dos artistas que escuto -----
mean(top_artists$popularity)
median(top_artists$popularity)
  #foi possivel observar que nao ha grande diferença entre a media e a mediana
  #da popularidade dos artistas

#calculando a media e mediana da duração em minutos das top 50 musicas -----
duration_tracks <- top_tracks$duration_ms / 1000 / 60
mean(duration_tracks)
median(duration_tracks)
  #foi possivel observar que nao ha grande diferença entre a media e a mediana
  # da duração das musicas

#recuperando os artistas que fazem parte das top50 musicas ----------
artistas_top50_musicas <- NULL
for (i in 1:50) {
  artistas_top50_musicas <- c(artistas_top50_musicas, top_tracks$artists[[c(i, 3)]])
}
artistas_top50_musicas %>% table() %>% sort(decreasing = TRUE)
  #os artistas que mais apareceram foram carne doce, letrux, lamparina e a primavera
  #biltre e boogarins, todos brasileiros.

#recuperando o numero de musicas que o album de onde a faixa mais escutada veio -----
album_faixas <- top_tracks %>% select(album.name, album.total_tracks)
album_faixas %>% filter(album.total_tracks > 1)
album_faixas %>% filter(album.total_tracks > 1) %>% count()
  #para recuperar o numero de musicas em cada album, filtrei a quantidade para > 1
  #para excluir os singles e por fim, contabilizei o numero de albuns, que foi 34

#recuperando a data de lancamento de cada album e ha quanto tempo foi lancado --------------------
album_lancamento <- top_tracks %>% select(album.name, album.release_date)
class(album_lancamento$album.release_date)
as_datetime(album_lancamento$album.release_date) %>% year() %>% 
  as.data.frame() %>% table()
  #nesse passo, recuperei a data de lancamento de cada album, porem elas estavam 
  #como caractere. para conseguir trabalhar com datas, transformei a coluna com
  #lubridate e recuperei o ano de lancamento de cada um. observei que a maioria das
  #faixas que ouvi foram lançadas no ano de 2019. ouve um warning message,
  #porque uma das faixas só tinha o ano de lancamento e o lubridate nao reconheceu
