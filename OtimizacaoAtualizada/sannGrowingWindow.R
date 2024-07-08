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
  #print(H$tr)
  #print(resultados)
  #print(" ")
  
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

CSANN=list(maxit=N,temp=28,trace=TRUE)
SA=optim(par=rep(0, 28),fn=eval1,method="SANN",gr=rchange2,control=CSANN)
cat("best solution:",SA$par,"evaluation function",SA$value,"\n")

}


  