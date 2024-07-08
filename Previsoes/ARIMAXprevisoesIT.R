library(vars)
library(rminer)
library(forecast)

source("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Material/Rseries/Rseries/multi-utils.R") # load multi-variate utility forecasting functions

# Carregar os dados de vendas do Walmart
cat("read Walmart Sales\n")
data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")

# Parâmetros
K <- 4  # Frequência (e.g., semanas)
LTS <- K  # Número de previsões
num_iteracoes <- 10

# Variáveis exógenas
fuel <- data[["Fuel_Price"]] # preço combustível
holiday <- data[["IsHoliday"]] # feriado

# Departamentos
departamentos <- c("WSdep1", "WSdep2", "WSdep3", "WSdep4")

# Data frame para armazenar todas as previsões com ID de iteração
previsoes_df <- data.frame()

# Função para prever com o modelo ARIMAX e armazenar resultados
prever_arimax <- function(dep, TS_dep_train, fuel, holiday, iteracao) {
  cdata <- cbind(fuel, holiday, TS_dep_train)
  mtr <- ts(cdata, frequency=K)
  arimax <- autoARIMAX(mtr, frequency = 4)
  FA <- forecastARIMAX(arimax, h = LTS)
  Pred <- FA[[3]]  # Supondo que o terceiro elemento seja a previsão do departamento
  
  # Criar um data frame temporário para armazenar os resultados da iteração atual
  iteracao_df <- data.frame(
    Iteracao = iteracao,
    Departamento = dep,
    Semana1 = Pred[1],
    Semana2 = Pred[2],
    Semana3 = Pred[3],
    Semana4 = Pred[4]
  )
  
  return(iteracao_df)
}

# Loop para prever cada departamento individualmente com o modelo ARIMAX
for (dep in departamentos) {
  cat("Fase 1: Previsão para o departamento:", dep, "\n")
  
  # Dados do departamento atual
  dep_data <- data[[dep]]
  
  # Inicializar os dados de treino com os dados originais
  TS_dep_train <- dep_data[1:(length(dep_data) - LTS)]
  
  for (j in 1:num_iteracoes) {
    iteracao_df <- prever_arimax(dep, TS_dep_train, fuel, holiday, j)
    
    # Adicionar os resultados da iteração atual ao data frame principal
    previsoes_df <- rbind(previsoes_df, iteracao_df)
    
    # Adicionar as previsões aos dados de treino para a próxima iteração
    TS_dep_train <- c(TS_dep_train, as.numeric(iteracao_df[3:6]))
  }
}

# Definir o caminho e o nome do arquivo de saída
output_file <- "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ARIMAXprevisoes_walmart.csv"

# Escrever o data frame em um arquivo CSV
write.csv(previsoes_df, file = output_file, row.names = FALSE)

# Mensagem de confirmação
cat("Dados previstos foram exportados para:", output_file, "\n")
