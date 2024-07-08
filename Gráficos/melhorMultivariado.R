library(vars)
library(rminer)
library(forecast)

# Definindo o diretório de trabalho
setwd("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Material/Rseries/Rseries")

# Carregar funções utilitárias, se necessário
source("multi-utils.R")

# Função para exibição do gráfico comparativo
fshow = function(Y, Pred_ARIMAX, Pred_VAR) {
  # Configura o layout dos gráficos
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2), oma = c(1, 1, 1, 1))
  
  # Calcula a média das previsões e das vendas reais
  media_real <- rowMeans(Y[, 3:6])
  media_pred_arimax <- rowMeans(do.call(cbind, Pred_ARIMAX[3:6]))
  media_pred_var <- rowMeans(do.call(cbind, Pred_VAR[3:6]))
  
  # Define os limites do gráfico
  ylim <- range(c(media_real, media_pred_arimax, media_pred_var))
  
  # Plotar valores reais e previstos
  plot(media_real, type = "l", col = "black", lwd = 2, ylim = ylim, xlab = "Time", ylab = "Values", main = "Comparação: Média dos Departamentos")
  lines(media_pred_arimax, col = "purple", lwd = 2)
  lines(media_pred_var, col = "orange", lwd = 2)
  
  # Adiciona a legenda
  legend("topright", legend = c("Real", "ARIMAX", "VAR"), col = c("black", "purple", "orange"), lty = 1, lwd = 2, cex = 0.8)
}

# Carregar dados
data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")

# Preparação dos dados
K = 4
LTS = K
fuel <- data[["Fuel_Price"]]
holiday <- data[["IsHoliday"]]
dep1 <- data[["WSdep1"]]
dep2 <- data[["WSdep2"]]
dep3 <- data[["WSdep3"]]
dep4 <- data[["WSdep4"]]
cdata = cbind(fuel, holiday, dep1, dep2, dep3, dep4)
hd = holdout(fuel, ratio = LTS, mode = "order")
mtr = ts(cdata[hd$tr,], frequency = K)
Y = cdata[hd$ts,]

# Execução do modelo ARIMAX
arimax = autoARIMAX(mtr, frequency = 4)
FA = forecastARIMAX(arimax, h = LTS)
Pred_ARIMAX = list(FA[[1]], FA[[2]], FA[[3]], FA[[4]], FA[[5]], FA[[6]])

# Execução do modelo VAR
mvar = autoVAR(mtr, LAGMAX = 2)
FV = forecastVAR(mvar, h = LTS)
Pred_VAR = list(FV[[1]], FV[[2]], FV[[3]], FV[[4]], FV[[5]], FV[[6]])

# Exibição dos resultados comparativos
fshow(Y, Pred_ARIMAX, Pred_VAR)

# Pausa para visualização
mpause()
