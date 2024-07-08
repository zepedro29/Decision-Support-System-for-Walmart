library(vars)
library(rminer)
library(forecast)

source("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Material/Rseries/Rseries/multi-utils.R") # Load multi-variate utility forecasting functions

cat("Read Walmart Sales\n")
data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv", header = TRUE)

K = 4
LTS = K
fuel <- data[["Fuel_Price"]] # Preço combustível
holiday <- data[["IsHoliday"]] # Feriado

# Departamentos
deps <- c("WSdep1", "WSdep2", "WSdep3", "WSdep4")

# Número de iterações para a janela crescente
n_iterations <- 10

# Matrizes para armazenar os valores das métricas em todas as iterações
mae_values <- array(NA, dim = c(length(deps), 3, n_iterations))
nmae_values <- array(NA, dim = c(length(deps), 3, n_iterations))

# Loop para prever cada departamento individualmente com o modelo ARIMAX
for (d in 1:length(deps)) {
  dep <- deps[d]
  
  # Dados do departamento atual
  dep_data <- data[[dep]]
  
  YR = diff(range(dep_data))
  
  for (iter in 1:n_iterations) {
    # Definição do tamanho da janela crescente
    train_size <- floor(nrow(data) * (0.6 + 0.04 * (iter - 1)))
    
    # Divisão dos dados em treino e teste
    hd <- holdout(fuel, ratio = (nrow(data) - train_size) / nrow(data), mode = "order")
    
    # Combinação dos dados de combustível e feriado com os dados do departamento
    cdata <- cbind(fuel, holiday, dep_data)
    
    # Objeto de série temporal para os dados de treino
    mtr <- ts(cdata[hd$tr, ], frequency = K)
    
    # Dados de teste (target)
    Y <- cdata[hd$ts, ]
    
    # Criação do modelo ARIMAX
    arimax <- autoARIMAX(mtr, frequency = 4)
    
    # Previsões multi-step ahead com o modelo ARIMAX
    FA <- forecastARIMAX(arimax, h = LTS)
    
    # Previsões para o departamento atual com o modelo ARIMAX
    Pred1_arimax <- FA[[1]]
    Pred2_arimax <- FA[[2]]
    Pred3_arimax <- FA[[3]]
    
    # Cálculo das métricas para cada previsão
    for (i in 1:3) {
      mae_values[d, i, iter] <- round(mmetric(Y[, i], get(paste0("Pred", i, "_arimax")), metric="MAE"), 1)
      nmae_values[d, i, iter] <- round(mmetric(Y[, i], get(paste0("Pred", i, "_arimax")), metric="NMAE", val=YR), 4)
    }
  }
}

# Cálculo da mediana para cada departamento ao longo das 10 iterações
median_mae <- apply(mae_values, c(1, 2), median, na.rm=TRUE)
median_nmae <- apply(nmae_values, c(1, 2), median, na.rm=TRUE)

# Criação da matriz de resultados
result_matrix <- matrix(NA, nrow=length(deps), ncol=2)
rownames(result_matrix) <- deps
colnames(result_matrix) <- c("Median MAE", "Median NMAE")

for (d in 1:length(deps)) {
  result_matrix[d, "Median MAE"] <- median(mae_values[d,,], na.rm = TRUE)
  result_matrix[d, "Median NMAE"] <- median(nmae_values[d,,], na.rm = TRUE)
}

print(result_matrix)
