setwd("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/OtimizacaoAtualizada")
source("blind.R") # fsearch is defined here
source("hill.R") # mcsearch is defined here
source("calcUpper.R")
source("funcao_eval.R")
library(rminer)

#Read data
cat("read Walmart Sales\n")
TS <- read.csv("walmart.csv")
TS$Date = as.Date(TS$Date, format="%Y-%m-%d")
d1 <- TS[, 4:7] #Conjunto de dados

evaluate_with_weights <- function(s, W1, W2) {
  resultado <- eval(s)
  
  lucro <- resultado["lucro"]
  esforco <- resultado["esforco"]
  
  E1 <- -W1 * lucro
  E2 <- W2 * esforco
  
  E <- E1 + E2
  
  return(list(E = E, E1 = E1, E2 = E2, lucro = lucro, esforco = esforco))
}

#Variaveis Growing Window
L <- nrow(d1)
K <- 4
Test <- 4
S <- 4
Runs <- 10
W <- L - (Runs - 1) * S #initial training window size for the ts space

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
  cat("iter:", b, "| Semana ", length(H$tr)-3, "ate:", length(H$tr), "\n")
  
  
  lower=rep(0,28) # lower bounds
  upper <- calcUpper(vendasPrevistas)
  
  # slight change of a real par under a normal u(0,0.5) function:
  rchange2=function(par) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }
  
  N=1000 # number of searches
  REPORT=N/10 # report results
  
  W1=0.5
  W2=0.5
  Z=500.000
  
  # Função de avaliação para Hill Climbing
  SA_eval <- function(s) {
    result <- evaluate_with_weights(s, W1, W2)
    return(result$E)  # Retorna apenas E para o otimizador
  }
  
  CSANN=list(maxit=N,temp=28,trace=TRUE)
  SA=optim(par=rep(0, 28),fn=SA_eval,method="SANN",gr=rchange2,control=CSANN)
  
  # Captura os valores da melhor solução
  best_result <- evaluate_with_weights(SA$par, W1, W2)
  
  cat("Lucro:", best_result$lucro, "\nEsforço:", best_result$esforco, "\n")
  cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
  
}