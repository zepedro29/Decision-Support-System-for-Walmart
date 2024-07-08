setwd("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/OtimizacaoAtualizada")
library(genalg)
source("calcUpper.R")
library(rminer)

# Read data
cat("read Walmart Sales\n")
TS <- read.csv("walmart.csv")
TS$Date = as.Date(TS$Date, format="%Y-%m-%d")
d1 <- TS[, 4:7] # apenas dados das vendas

# Variáveis Growing Window
L <- nrow(d1)
K <- 4
Test <- 4
S <- 4
Runs <- 10
W <- L - (Runs - 1) * S # initial training window size for the ts space

for (b in 1:Runs) { 
  resultados <- matrix(NA, nrow = L, ncol = 4) # supondo que há 4 colunas em d1
  for(i in 1:4){
    H <- holdout(d1[, i], ratio = Test, mode = "incremental", iter = b, window = W, increment = S) 
    resultados[H$tr, i] <- d1[H$tr, i]
  }
  
  vendasPrevistas <- resultados[(length(H$tr)-3):length(H$tr), ]
  print("Vendas Previstas: \n")
  print(vendasPrevistas)
  cat("iter:", b, "Semana ", length(H$tr)-3, " ate:", length(H$tr))
  
  # definition of the famous rastrigin function
  # x is a vector with D real values.
  rastrigin=function(x=c()) { return (sum(x^2-10*cos(2*pi*x)+10))}
  
  # Definição dos limites inferior e superior
  D = 28
  
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
    
    # Retorna um vetor com -f1 e f2
    return(c(-f1, f2))
  }

  
  # Função wrapper para otimização com rbga
  eval_wrapper <- function(s) {
    return(eval(s)[1])
  }
  
  # Parâmetros para rbga
  popSize = 100 # population size
  iter = 10 # maximum number of iterations
  report = 10 # report progress every 10 iterations
  lower = rep(0, D)
  upper = calcUpper(vendasPrevistas)
  
  # Função para mostrar o melhor resultado
  showbest = function(method, par, eval) {
    cat("method:", method, "\n > par:", par, "\n > lucro:", -eval[1], "\n > esforço:", eval[2], "\n")
  }
  
  # Otimização com Algoritmo Genético
  ITER <<- 1 # variável global com o número de iterações do rbga
  traceGA = function(obj) {
    if ((ITER %% report) == 0) {
      PMIN = which.min(obj$evaluations)
      cat("iter:", ITER, " eval (lucro):", -obj$evaluations[PMIN], "\n")
    }
    ITER <<- ITER + 1
  }
  
  rga = rbga(lower, upper, popSize = popSize, evalFunc = eval_wrapper, iter = iter, monitor = traceGA)
  
  # Obtém a melhor solução
  PMIN = which.min(rga$evaluations)
  best_solution = rga$population[PMIN, ]
  
  # Calcula f1 e f2 para a melhor solução
  eval_values = eval(best_solution)
  best_f1 = -eval_values[1]  # Inverter o sinal para obter o lucro
  best_f2 = eval_values[2]   # Esforço
  
  # Exibe os resultados
  cat("Lucro (f1):", best_f1, "\n")
  cat("Esforço (f2):", best_f2, "\n")
}
