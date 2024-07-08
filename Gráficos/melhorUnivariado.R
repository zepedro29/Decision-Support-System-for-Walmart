# Carregar as bibliotecas necessárias
library(forecast)
library(rminer)
library(ggplot2)
library(reshape2)

# Ler os dados do Walmart Sales
cat("A ler as vendas do Walmart\n")
TS <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")

# Data frame para armazenar todas as previsões
previsoes_df <- data.frame()

# Loop das colunas 4 a 7, que correspondem aos departamentos
for (i in 4:7) {
  cat("Departamento:", colnames(TS)[i], "\n")
  
  # Seleciona a coluna
  dep_data <- TS[, i]
  
  # Define os parâmetros
  K <- 4
  H <- 4
  L <- length(dep_data)
  Test <- K # number of multi-ahead steps
  S <- 4 # step jump
  Runs <- 10 # número de iteraçoes
  
  # forecast:
  W <- (L - Test) - (Runs - 1) * S # initial training window size for the ts space (forecast methods)
  
  # rminer:
  timelags <- c(1:4)
  D <- CasesSeries(dep_data, timelags) # note: nrow(D) is smaller by max timelags than length(dep_data)
  W2 <- W - max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
  YR <- diff(range(dep_data)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  # Loop de iterações
  for (b in 1:Runs) { # cycle of the incremental window training (growing window)
    
    # code for rminer package methods:
    H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)   
    
    # Fit Random Forest
    M_RF <- fit(y ~ ., D[H2$tr, ], model = "randomForest") # modelo randomForest
    Pred_RF <- lforecast(M_RF, D, start = (length(H2$tr) + 1), Test) # multi-step ahead forecasts
    
    # Fit Linear Regression
    M_LR <- fit(y ~ ., D[H2$tr, ], model = "lm") # modelo de regressão linear
    Pred_LR <- lforecast(M_LR, D, start = (length(H2$tr) + 1), Test) # multi-step ahead forecasts
    
    # Fit ARIMA model
    dtr <- ts(dep_data[H2$tr], frequency = K)
    M_ARIMA <- suppressWarnings(auto.arima(dtr))
    Pred_ARIMA <- forecast(M_ARIMA, h = length(H2$ts))$mean[1:Test]
    
    # Fit ETS model
    M_ETS <- suppressWarnings(ets(dep_data[H2$tr]))
    Pred_ETS <- forecast(M_ETS, h = length(H2$ts))$mean[1:Test]
    
   
    
    # Criar um data frame temporário para armazenar os resultados da iteração atual
    iteracao_df <- data.frame(
      Iteracao = b,
      Departamento = colnames(TS)[i],
      Real = dep_data[H2$ts],
      Pred_RF = Pred_RF,
      Pred_MLPE = Pred_LR,
      Pred_ARIMA = Pred_ARIMA,
      Pred_ETS = Pred_ETS
      
    )
    
    # Adicionar os resultados da iteração atual ao data frame principal
    previsoes_df <- rbind(previsoes_df, iteracao_df)
  }
}

# Calcular a média das previsões e das vendas reais por iteração
media_previsoes <- aggregate(. ~ Iteracao, data = previsoes_df[, -2], FUN = mean)

# Converter data frame para formato longo para ggplot
resultados_long <- melt(media_previsoes, id.vars = "Iteracao", variable.name = "Modelo", value.name = "Vendas")

# Separar tipo de previsão e vendas reais
resultados_long$Tipo <- ifelse(resultados_long$Modelo == "Real", "Real", "Previsto")

# Gerar o gráfico final
p <- ggplot(resultados_long, aes(x = Iteracao, y = Vendas, color = Modelo)) +
  geom_line(size = 1) +
  labs(title = "Média das Vendas Previstas vs Vendas Reais por Iteração",
       x = "Iteração",
       y = "Vendas",
       color = "Modelo") +
  theme_minimal()

# Exibir o gráfico
print(p)

