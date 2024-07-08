calcNumFunc <- function(capacidadeMaxFunc, numVendasDep){
  number_max_func <- ceiling(numVendasDep / capacidadeMaxFunc)
  return(number_max_func)
}

calcUpper <- function(vendasPrevistas){
  capacidadeMaxFunc <- c(4000, 7000, 9500)
  uppers <- rep(0, 28)
  
  numVendasDep <- apply(vendasPrevistas, 2, max)
  
  # Elementos dos funcionários
  for (j in 1:4) { # Departamento
    for (i in 1:3) { # Tipo de funcionário
      index <- (j - 1) * 3 + i
      uppers[index] <- calcNumFunc(capacidadeMaxFunc[i], numVendasDep[j])
    }
  }
  
  # Elementos das encomendas
  for (i in 1:4) {
    index <- 12 + (i - 1) * 4
    for (week in 0:3) { # Para cada semana
      uppers[index + week + 1] <- sum(vendasPrevistas[week + 1:(4 - week), i])
    }
  }
  
  # Retornar o vetor uppers
  return(uppers)
  
}

#up <- calcUpper(vendasPrevistas)
#cat(up)