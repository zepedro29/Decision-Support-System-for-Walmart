setwd("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/OtimizacaoAtualizada")
library(DEoptim)
source("calcUpper.R")

# Read data
cat("read Walmart Sales\n")
TS <- read.csv("walmart.csv")
TS$Date = as.Date(TS$Date, format="%Y-%m-%d")
d1 <- TS[, 4:7] # apenas dados das vendas

# Variaveis Growing Window
L <- nrow(d1)
K <- 4
Test <- 4
S <- 4
Runs <- 10
W <- L - (Runs - 1) * S # initial training window size for the ts space

evaluations <- c()

for (b in 1:Runs) { 
  resultados <- matrix(NA, nrow = L, ncol = 4) # supondo que há 4 colunas em d1
  for(i in 1:4){
    H <- holdout(d1[, i], ratio = Test, mode = "incremental", iter = b, window = W, increment = S) 
    resultados[H$tr, i] <- d1[H$tr, i]
  }
  
  vendasPrevistas <- resultados[(length(H$tr)-3):length(H$tr), ]
  print("Vendas Previstas: \n")
  print(vendasPrevistas)
  cat("iter:", b, "Semana ", length(H$tr)-3, " ate:", length(H$tr), "\n")
  
  # Definition of the famous rastrigin function
  # x is a vector with D real values.
  rastrigin=function(x=c()) { return (sum(x^2-10*cos(2*pi*x)+10))}
  
  # Global variables, defined outside functions: ------------------------
  # lets set D to 30 (good benchmark, challenging):
  D=28
  # Calculo custo total empregados
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
  
  # Calculo nº produtos que funcionarios suportam p//Dep
  capacidadeEmpregados <- c(4000, 7000, 9500)
  
  calcEmpregadosMaxSup <- function(empregados, capacidadeEmpregados) {
    mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
    capacidade <- capacidadeEmpregados * rowSums(mEmpregados)
    return(capacidade)
  }
  
  # Calculo custo encomendas
  valorProdutos <- c(10, 15, 25, 30)
  custoProdutos <- c(6, 8, 12, 15)
  custoProdutosStock <- c(2, 3, 4, 5)
  
  calcCustoEncomendas <- function(encomendas) {
    mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
    custoEncomendas <- rowSums(mEncomendas * custoProdutos)
    return(sum(custoEncomendas))
  }
  
  # Calculo vendas efetivas
  calcVendasEfetivas <- function(vendasPrevistas, encomendas, capacidadeEmpregados) {
    vendasEfetivas <- pmin(vendasPrevistas, encomendas, capacidadeEmpregados)
    return(vendasEfetivas)
  }
  
  # Calculo vendas em USD
  calcVendasUSD <- function(vendasEfetivas, valorProdutos) {
    vendasUSD <- vendasEfetivas * valorProdutos
    return(rowSums(vendasUSD))
  }
  
  # Calculo estoque
  calcStock <- function(vendasEfetivas, encomendas) {
    stock <- encomendas - vendasEfetivas
    return(stock)
  }
  
  # Calculo custo do estoque
  calcCustoStock <- function(stock, custoProdutosStock) {
    custoStock <- rowSums(stock * custoProdutosStock)
    return(sum(custoStock))
  }
  
  # Função de avaliação
  eval <- function(s) {
    # Arredondar s para inteiros
    s <- round(s)
    # Divide s em empregados e encomendas 
    empregados = s[1:12]
    encomendas = s[13:28]
    mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
    mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
    
    # Usa cada uma das funções para calcular tudo
    custoEmpregados <- calcCustoEmpregados(empregados)
    custoEncomendas <- calcCustoEncomendas(encomendas)
    capacidadeEmpregados <- calcEmpregadosMaxSup(empregados, capacidadeEmpregados)
    numEncomendas <- sum(encomendas != 0)
    vendasEfetivas <- calcVendasEfetivas(vendasPrevistas, mEncomendas, capacidadeEmpregados)
    vendasUSD <- calcVendasUSD(vendasEfetivas, valorProdutos)
    stock <- calcStock(vendasEfetivas, mEncomendas)
    custoStock <- calcCustoStock(stock, custoProdutosStock)
    
    # Calcular f1 e f2
    f1 <- sum(vendasUSD) - sum(custoEmpregados, custoEncomendas, custoStock)
    f2 <- sum(numEncomendas, sum(empregados))
    
    # Retorna um vetor com -f1 e f2
    return(c(-f1, f2))
  }
  
  # Função wrapper para otimização com DEoptim
  eval_wrapper <- function(s) {
    # Arredondar s para inteiros
    s <- round(s)
    # Retorna apenas o primeiro valor (que é -f1) para DEoptim
    return(eval(s)[1])
  }
  
  # Parâmetros para DEoptim
  popSize=100 # population size
  iter=10 # maximum number of iterations
  report=10 # report progress every 10 iterations
  lower=rep(0,D)
  upper= calcUpper(vendasPrevistas)
  
  # Ajusta os limites superiores para serem inteiros
  upper <- round(upper)
  
  # Executa a otimização
  de <- DEoptim(fn = eval_wrapper, lower = lower, upper = upper, DEoptim.control(NP = popSize, itermax = iter, trace = report))
  
  # Obtém a melhor solução
  best_solution <- round(de$optim$bestmem)  # Arredonda a melhor solução
  
  # Calcula f1 e f2 para a melhor solução
  eval_values <- eval(best_solution)
  best_f1 <- -eval_values[1]  # Inverter o sinal para obter o lucro
  best_f2 <- eval_values[2]   # Esforço
  
  # Exibe os resultados
  cat("Lucro (f1):", round(best_f1), "\n")  # Arredonda o lucro para garantir que é um inteiro
  cat("Esforço (f2):", round(best_f2), "\n")  # Arredonda o esforço para garantir que é um inteiro
}
