#Calculo custo total empregados
custoEmpregados <- c(6000, 8000, 9750)

calcCustoEmpregados <- function(empregados){
  mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
  resultados <- vector("numeric", length = 3)
  
  for (i in 1:nrow(mEmpregados)) {
    soma_linha <- sum(mEmpregados[i, ])
    resultados[i] <- soma_linha * custoEmpregados[i]
  }
  return(sum(resultados))
}

#Calculo nº produtos que funcionarios suportam p//Dep

capacidadeEmpregados <- c(4000, 7000, 9500)

calcEmpregadosMaxSup <- function(empregados, capacidadeEmpregados) {
  mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
  resultado <- numeric(ncol(mEmpregados))
  for (j in 1:ncol(mEmpregados)) {
    soma <- 0
    for (i in 1:nrow(mEmpregados)) {
      soma <- soma + mEmpregados[i, j] * capacidadeEmpregados[i]
    }
    resultado[j] <- soma
  }
  
  return(resultado)
}

#Calculo custo total encomendas
custoEncomendas <- c(6, 8, 9, 11)

calcCustoEncomendas <- function(encomendas){
  mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
  resultados <- vector("numeric", length = 4)
  
  for (i in 1:nrow(mEncomendas)) {
    soma_col <- sum(mEncomendas[, i])
    resultados[i] <- soma_col * custoEncomendas[i]
  }
  return(sum(resultados))
}


#Calculo matriz de vendas efetivas
#Vendas efetivas = previstas SE previstas<=encomenda E previstas<=sup funcionarios
#Vendas efetivas = min(Encomendas, Funcionarios) SE previstas>encomendas OU previstas>sup func

calcVendasEfetivas <- function(vendasPrevistas, mEncomendas, capacidadeEmpregados) {
  vendasEfetivas <- matrix(0, nrow = nrow(vendasPrevistas), ncol = ncol(vendasPrevistas))
  for (i in 1:nrow(vendasPrevistas)) {
    for (j in 1:ncol(vendasPrevistas)) {
      if (vendasPrevistas[i, j] <= mEncomendas[i, j] & vendasPrevistas[i, j] <= capacidadeEmpregados[j]) {
        vendasEfetivas[i, j] <- vendasPrevistas[i, j]
      } else if (i > 1 && vendasPrevistas[i - 1, j] <= mEncomendas[i - 1, j]) {
        vendasEfetivas[i, j] <- mEncomendas[i-1, j] - vendasPrevistas[i - 1, j]
      } else {
        vendasEfetivas[i, j] <- min(mEncomendas[i, j], capacidadeEmpregados[j])
      }
    }
  }
  return(vendasEfetivas)
}

#Calculo matriz de ganhos
valorProdutos <- c(8, 10, 12, 16)
calcVendasUSD <- function(vendasEfetivas, valorProdutos) {
  vendasUSD <- vendasEfetivas
  for (i in 1:ncol(vendasEfetivas)) {
    vendasUSD[, i] <- vendasEfetivas[, i] * valorProdutos[i]
  }
  return(vendasUSD)
}

#Calculo matriz stock 
calcStock <- function(vendasEfetivas, mEncomendas) {
  stock <- matrix(0, nrow = 4, ncol = 4)
  for (s in 1:4) {
    for (d in 1:4) {
      if (s == 1) {
        stock[s, d] <- mEncomendas[s, d] - vendasEfetivas[s, d]
      } else {
        stock[s, d] = stock[s - 1, d] + mEncomendas[s, d] - vendasEfetivas[s, d]
      }
    }
  }
  return(stock)
}


#Calculo matriz custo stock
custoProdutosStock <- c(3, 5, 6, 8)
calcCustoStock <- function(stock, custoProdutosStock) {
  custoStock <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    custoStock[, i] <- stock[, i] * custoProdutosStock[i]
  }
  return(custoStock)
}


#Funçoes de avaliação
eval <- function(s){
  #Divide s em empregados e encomendas 
  empregados = s[1:12]
  encomendas = s[13:28]
  mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
  mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
  
  #Usa cada uma das funçoes para calcular tudo
  custoEmpregados <- calcCustoEmpregados(empregados)
  custoEncomendas <- calcCustoEncomendas(encomendas)
  capacidadeEmpregados <- calcEmpregadosMaxSup(empregados, capacidadeEmpregados)
  numEncomendas <- sum(encomendas != 0)
  vendasEfetivas <- calcVendasEfetivas(vendasPrevistas, mEncomendas, capacidadeEmpregados)
  vendasUSD <- calcVendasUSD(vendasEfetivas, valorProdutos)
  stock <- calcStock(vendasEfetivas, mEncomendas)
  custoStock <- calcCustoStock(stock, custoProdutosStock)
  
  f1 <- sum(vendasUSD) - sum(custoEmpregados,custoEncomendas, custoStock)
  f2 <- sum(numEncomendas, sum(empregados))
  
  # Arredonda lucro e esforço
  f1 <- round(f1, digits = 2)
  f2 <- round(f2, digits = 2)
  
  
  # Retorna lucro e esforço
  return(c(lucro = f1, esforco = f2))
}

#funções auxiliares para calcular 

eval1 <- function(s) {
  resultado <- eval(s)
  return(unname(resultado["lucro"])) # retorna o lucro arredondado
}


eval2 <- function(s) {
  resultado <- eval(s)
  return(unname(resultado["esforco"])) # retorna o esforço arredondado
}


