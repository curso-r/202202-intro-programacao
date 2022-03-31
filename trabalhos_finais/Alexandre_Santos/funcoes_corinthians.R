
# Criação de funções para calcular pontuação do corinthians como mandante e visitante -----------------------------

# criando a função pontos do mandante --------------------------------------------------------

pontos_mandante <- function(gols_mandante, gols_visitante){
  
  
  # objeto para receber os pontos de cada placar
  vetor_resultados <- c()
  
  # looping para executar a função para cada linha
  
  for (i in seq_along(gols_mandante)) {
    
    # Aumentar o vetor de resultados com a pontuação do placar atual
    if (gols_mandante[i] > gols_visitante[i]) {
      vetor_resultados <- append(vetor_resultados, 3)
    } else if (gols_mandante[i] < gols_visitante[i]) {
      vetor_resultados <- append(vetor_resultados, 0)
    } else {
      vetor_resultados <- append(vetor_resultados, 1)
    }
  }
  
  # Retornar
  return(vetor_resultados)
}


# criando a função de pontos de visitantes --------------------------------

pontos_visitante <- function(gols_mandante, gols_visitante){
  
  
  # objeto para receber os pontos de cada placar
  vetor_resultados <- c()
  
  # looping para executar a função para cada linha
  
  for (i in seq_along(gols_mandante)) {
    
    # Aumentar o vetor de resultados com a pontuação do placar atual
    if (gols_mandante[i] > gols_visitante[i]) {
      vetor_resultados <- append(vetor_resultados, 0)
    } else if (gols_mandante[i] < gols_visitante[i]) {
      vetor_resultados <- append(vetor_resultados, 3)
    } else {
      vetor_resultados <- append(vetor_resultados, 1)
    }
  }
  
  # Retornar
  return(vetor_resultados)
}

# Criação de funções para retornar resultado do corinthians como mandante e visitante -----------------------------

# criando a função resultado do mandante --------------------------------------------------------

resultado_mandante <- function(gols_mandante, gols_visitante){
  
  
  # objeto para receber os pontos de cada placar
  vetor_resultados1 <- c()
  
  # looping para executar a função para cada linha
  
  for (i in seq_along(gols_mandante)) {
    
    # Aumentar o vetor de resultados com a pontuação do placar atual
    if (gols_mandante[i] > gols_visitante[i]) {
      vetor_resultados1 <- append(vetor_resultados1, "vitoria")
    } else if (gols_mandante[i] < gols_visitante[i]) {
      vetor_resultados1 <- append(vetor_resultados1, "derrota")
    } else {
      vetor_resultados1 <- append(vetor_resultados1, "empate")
    }
  }
  
  # Retornar
  return(vetor_resultados1)
}


# criando a função resultado corinthians visitantes --------------------------------

resultado_visitante <- function(gols_mandante, gols_visitante){
  
  
  # objeto para receber os pontos de cada placar
  vetor_resultados2 <- c()
  
  # looping para executar a função para cada linha
  
  for (i in seq_along(gols_mandante)) {
    
    # Aumentar o vetor de resultados com a pontuação do placar atual
    if (gols_mandante[i] > gols_visitante[i]) {
      vetor_resultados2 <- append(vetor_resultados2, "derrota")
    } else if (gols_mandante[i] < gols_visitante[i]) {
      vetor_resultados2 <- append(vetor_resultados2, "vitoria")
    } else {
      vetor_resultados2 <- append(vetor_resultados2, "empate")
    }
  }
  
  # Retornar
  return(vetor_resultados2)
}

