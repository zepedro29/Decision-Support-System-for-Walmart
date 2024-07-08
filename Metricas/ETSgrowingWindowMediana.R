library(forecast)
library(rminer)
library(ggplot2)

# Função para calcular métricas e retornar os resultados como vetor
calculate_median_metrics <- function(Y, Pred) {
  medians <- c(
    median(mmetric(Y, Pred, metric = "MAE")),
    median(mmetric(Y, Pred, metric = "NMAE")),
    median(mmetric(Y, Pred, metric = "RMSE")),
    median(mmetric(Y, Pred, metric = "RRSE")),
    median(mmetric(Y, Pred, metric = "R2"))
  )
  return(medians)
}

# read data:
cat("Lendo a série temporal de vendas:\n")
walmart_data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")
departamentos <- colnames(walmart_data)[grep("^WSdep", colnames(walmart_data))]

K <- 4 # Assumindo dados semanais, definimos K como 4 para representar as semanas em um mês
Test <- 5 # Indicamos que queremos prever as próximas 5 semanas
S <- 1 # Ajuste S conforme necessário, dependendo da frequência dos dados e da quantidade de previsões desejadas
Runs <- 4 # Número de iterações da janela crescente, ajuste conforme necessário

# Matriz para armazenar os resultados das medianas por departamento
median_metrics <- matrix(NA, nrow = length(departamentos), ncol = 5, 
                         dimnames = list(departamentos, c("MAE", "NMAE", "RMSE", "RRSE", "R2")))

# Dataframe para armazenar todas as previsões e valores reais para o gráfico global
global_df <- data.frame(Time = integer(), Actual = numeric(), Predicted = numeric(), Department = character())

# Growing window:
for (dep in departamentos) {
  cat("Previsão da janela crescente para o departamento:", dep, "\n")
  
  d1 <- walmart_data[, dep]
  L <- length(d1) # Tamanho da série temporal
  
  # Forecast:
  W <- (L - Test) - (Runs - 1) * S # Tamanho inicial da janela de treinamento para o espaço ts (métodos de previsão)
  
  all_preds <- list() # Lista para armazenar todas as previsões
  
  for (b in 1:Runs) {
    H <- holdout(d1, ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
    dtr <- ts(d1[H$tr], frequency = K) # Criar objeto ts
    M <- suppressWarnings(ets(dtr)) # Criar modelo ETS
    Pred <- forecast(M, h = length(H$ts))$mean[1:Test] # Previsões
    all_preds[[b]] <- Pred
  }
  
  # Concatenar todas as previsões em um único vetor
  all_preds <- unlist(all_preds)
  
  # Extrair os valores reais correspondentes às previsões
  Y <- d1[(L - length(all_preds) + 1):L]
  
  # Calcular as medianas das métricas para o departamento atual
  medians <- calculate_median_metrics(Y, all_preds)
  
  # Armazenar as medianas na matriz
  median_metrics[dep, ] <- medians
  
  # Imprimir as métricas finais para o departamento atual
  cat("DEPARTAMENTO:", dep, "\n")
  cat("Median MAE:", medians[1], "\n")
  cat("Median NMAE:", medians[2], "\n")
  cat("Median RMSE:", medians[3], "\n")
  cat("Median RRSE:", medians[4], "\n")
  cat("Median R2:", medians[5], "\n \n")
  
  # Criar dataframe para o gráfico e adicionar ao dataframe global
  df <- data.frame(
    Time = (1:length(Y)) + (L - length(Y)),
    Actual = Y,
    Predicted = all_preds,
    Department = dep
  )
  global_df <- rbind(global_df, df)
}

# Exibir as medianas por departamento em formato tabular
print(median_metrics)

# Salvar a matriz das medianas em um arquivo CSV
#write.csv(median_metrics, file = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/ETSmedian_metrics.csv", row.names = TRUE)


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
