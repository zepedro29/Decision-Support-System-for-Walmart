library(forecast)
library(rminer)
library(ggplot2)

cat("read Walmart Sales\n")
data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv", header = TRUE)

# Convert Date column to Date format:
data$Date <- as.Date(data$Date)

# Vetor para armazenar os resultados das medianas por departamento
median_metrics <- matrix(NA, nrow = 4, ncol = 5, dimnames = list(paste("Departamento", 1:4), c("MAE", "NMAE", "RMSE", "RRSE", "R2")))

# Dataframe para armazenar todas as previsões e valores reais para o gráfico global
global_df <- data.frame(Time = integer(), Actual = numeric(), Predicted = numeric(), Department = character())

# Loop through columns 4 to 7 (Departamento 1 a Departamento 4)
for (i in 4:7) {
  cat("Departamento:", colnames(data)[i], "\n")
  
  # Selecionar a coluna do departamento atual
  d1 <- data[, i]
  
  L <- length(d1) # Tamanho da série temporal
  K <- 4 # Período sazonal (assumido)
  Test <- K # Número de passos à frente a prever
  S <- 4 # Incremento para o treinamento incremental
  Runs <- 10 # Número de iterações
  
  # Calculando o tamanho inicial da janela de treinamento
  W <- (L - Test) - (Runs - 1) * S
  
  # Preparando dados para rminer
  timelags <- c(1:4)
  D <- CasesSeries(d1, timelags)
  W2 <- W - max(timelags)
  YR <- diff(range(d1))
  
  # Vetor para armazenar os resultados das métricas de cada iteração
  ev2NMAE <- numeric(Runs)
  ev2MAE <- numeric(Runs)
  ev2RMSE <- numeric(Runs)
  ev2RRSE <- numeric(Runs)
  ev2R2 <- numeric(Runs)
  
  all_preds <- list() # Lista para armazenar todas as previsões
  
  # Loop das iterações
  for (b in 1:Runs) {
    H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)
    M2 <- fit(y ~ ., D[H2$tr, ], model = "randomForest")
    Pred2 <- lforecast(M2, D, start = (length(H2$tr) + 1), Test)
    all_preds[[b]] <- Pred2
    ev2NMAE[b] <- mmetric(y = d1[H2$ts], x = Pred2, metric = "NMAE", val = YR)
    ev2MAE[b] <- mmetric(y = d1[H2$ts], x = Pred2, metric = "MAE", val = YR)
    ev2RMSE[b] <- mmetric(y = d1[H2$ts], x = Pred2, metric = "RMSE", val = YR)
    ev2RRSE[b] <- mmetric(y = d1[H2$ts], x = Pred2, metric = "RRSE", val = YR)
    ev2R2[b] <- mmetric(y = d1[H2$ts], x = Pred2, metric = "R2", val = YR)
  }
  
  # Calculando as medianas das métricas
  median_metrics[i-3, 1] <- median(ev2MAE)
  median_metrics[i-3, 2] <- median(ev2NMAE)
  median_metrics[i-3, 3] <- median(ev2RMSE)
  median_metrics[i-3, 4] <- median(ev2RRSE)
  median_metrics[i-3, 5] <- median(ev2R2)
  
  cat("DEPARTAMENTO:", colnames(data)[i], "\n")
  cat("Median MAE:", median_metrics[i-3, 1], "\n")
  cat("Median NMAE:", median_metrics[i-3, 2], "\n")
  cat("Median RMSE:", median_metrics[i-3, 3], "\n")
  cat("Median RRSE:", median_metrics[i-3, 4], "\n")
  cat("Median R2:", median_metrics[i-3, 5], "\n \n")
  
  # Criar dataframe para o gráfico e adicionar ao dataframe global
  all_preds <- unlist(all_preds)
  Y <- d1[(L - length(all_preds) + 1):L]
  df <- data.frame(
    Time = (1:length(Y)) + (L - length(Y)),
    Actual = Y,
    Predicted = all_preds,
    Department = paste("WSdep", i-3)
  )
  global_df <- rbind(global_df, df)
}

# Exibindo as medianas por departamento em formato tabular
print(median_metrics)

# Salvar a matriz das medianas em um arquivo CSV
#write.csv(median_metrics, file = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/RandomForestmedian_metrics.csv", row.names = TRUE)


# Criar gráfico global de linha para vendas previstas X vendas reais
p <- ggplot(global_df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Previsto")) +
  facet_wrap(~ Department, scales = "free_y") +
  labs(title = "Vendas Reais vs Previstas - Todos os Departamentos",
       x = "Tempo",
       y = "Vendas",
       color = "Legenda") +
  theme_minimal()

# Mostrar gráfico
print(p)

