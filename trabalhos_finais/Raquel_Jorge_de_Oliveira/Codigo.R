# Vamos carregar os pacotes?
library(dados)
library(dplyr)
library(tibble)

# Carregando todas as informações.

starwars <- dados_starwars

# Salvando a base de dados em um objeto com nome mais curto.
# Vamos dar uma olhada na base? A minha alternativa preferida é...

view(starwars)

# ... mas aí abre uma outra aba e tem que ficar indo e voltando do nosso código.
# Vamos pra uma alternativa? Que tal executar cada um desses comandos por vez e
# ver o que aparece?

head(starwars)
tail(starwars)
dim(starwars)
colnames(starwars)

# Opa, já sabemos por alto o que tem aqui. Tô curiosa pra saber mais sobre as
# espécies dos personagens. Vamos lá.

starwars$especie

# Tem de tudo um pouco... será que podemos agregar esses dados pra ter uma noção
# sobre quantidades e representatividade de cada grupo?

starwars %>% count(especie, sort = TRUE)

# Bom, então temos que humanos, droides e NAs são os grupos mais representados 
# no universo Star Wars! Mas e cor da pele?

starwars %>% count(especie, cor_da_pele, sort = TRUE)

# Olha só... a maioria dos personagens pertence à espécie humana e tem a cor da
# pele branca ou clara. Como será se adicionarmos cor dos olhos e sexo biologico?

starwars %>% count(especie, cor_da_pele, cor_dos_olhos, sexo_biologico, sort = TRUE)

# Parece que temos um padrão aqui... vamos só confirmar uma coisa.

starwars %>% count(sexo_biologico, cor_da_pele, sort = TRUE)

# Viram? A categoria mais representada em um universo de fantasia — ou seja, onde
# tudo é teoricamente possível — é a de homens brancos. Não sou especialista em
# estudos de gênero, mas acho que podemos especular que isso seja uma escolha.
# Possivelmente essa escolha reflita a visão de mundo de quem escreveu os roteiros.
# O que você acha?
#
# Agora quero fazer uma graça.

starwars_nomes <- starwars$nome

for (nome in starwars_nomes) {
  Sys.sleep(0.5)
  print(paste0("Olá ", nome,"! Qual será seu sexo biológico?"))
}

# Meu objetivo aqui é ver de forma automatizada qual o sexo biológico dos
# personagens. Bora.

starwars_sexo_biologico <- unique(starwars$sexo_biologico)

for (nominho in starwars_nomes) {
  dados_nominho <- starwars %>% filter(nome == nominho)
  sexo_biologico_nominho <- dados_nominho$sexo_biologico
  
  print(paste0("Analisando as informações do personagem ", nominho, ", guenta aí!"))
  Sys.sleep(1)
  print(paste0("Tã-dã! O personagem " , nominho, " é " , sexo_biologico_nominho, "."))
  Sys.sleep(0.5)
  
}

# Então é isso, gente. Star Wars tem gender bias e race bias. Não acho que devamos
# deixar de gostar da série por isso, mas podemos agora a apreciar pelo que ela é,
# "warts and all". Para não acabar esse código numa vibe ruim, vou criar uma função
# pra saber o que cada personagem pilota :)
# Ah, quando for colocar o nome, não esquece de botar entre aspas, hein!
#
# Eu sou uma pessoa básica e queria mesmo era pilotar a Millenium Falcon, vroom vroom.

possante_nominho <- dados_nominho$naves_espaciais

bibifomfom <- function(starwars_nomes) {
  
  dados_nominho <- starwars %>% filter(nome == starwars_nomes)
  possante_nominho <- dados_nominho$naves_espaciais
  possante_nominho
  
}

# Muito obrigada por me acompanhar até aqui! Espero que tenha sido divertido.
