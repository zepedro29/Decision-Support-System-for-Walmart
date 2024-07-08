setwd("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/OtimizacaoAtualizada")
source("blind.R") # fsearch is defined here
source("hill.R") # hclimbing is defined here
source("calcUpper.R")
source("funcao_eval.R")
library(rminer)

# Read data
cat("read Walmart Sales\n")
TS <- read.csv("walmart.csv")
TS$Date = as.Date(TS$Date, format="%Y-%m-%d")
d1 <- TS[, 4:7] # Conjunto de dados

# Variáveis Growing Window
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
  cat("iter:", b, "| Semana ", length(H$tr)-3, "ate:", length(H$tr), "\n")
  
  lower = rep(0, 28) # lower bounds
  upper <- calcUpper(vendasPrevistas)
  
  # slight change of a real par under a normal u(0,0.5) function:
  rchange1=function(par,lower,upper) { 
    hchange(par, lower=lower, upper=upper, rnorm, mean=0, sd=0.25, round=FALSE) 
  }
  
  # Controle para Hill Climbing
  control <- list(maxit = 1000, REPORT = 100)
  
  HC = hclimbing(par = runif(28, lower, upper), fn = eval1, change = rchange1, lower = lower, upper = upper, control = control, type = "max")
  cat("best solution:", round(HC$sol), "evaluation function", -HC$eval, "\n")
  
  # Separar o vetor best solution em duas matrizes
  best_solution <- round(HC$sol) # Arredondar os valores para inteiros
  
  # Matriz de Funcionários (3 tipos de funcionários x 4 departamentos)
  funcionarios <- matrix(best_solution[1:12], nrow = 3, byrow = FALSE)
  colnames(funcionarios) <- c("departamento1", "departamento2", "departamento3", "departamento4")
  
  # Matriz de Encomendas (4 departamentos x 4 semanas)
  encomendas <- matrix(best_solution[13:28], nrow = 4, byrow = TRUE)
  colnames(encomendas) <- c("Semana1", "Semana2", "Semana3", "Semana4")
  
  cat("\nMatriz de Funcionários:\n")
  print(funcionarios)
  
  cat("\nMatriz de Encomendas:\n")
  print(encomendas)
}
