library(shiny)
library(dplyr)
library(rminer)
library(DEoptim)
library(DT)
#FINAL
# Interface do usuário
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      table.data.frame {
        width: 100%;
        border-collapse: collapse;
      }
      table.data.frame th, table.data.frame td {
        text-align: center;
        padding: 8px;
      }
    "))
  ),
  
  titlePanel("Previsões de Vendas Walmart"),
  
  tabsetPanel(
    tabPanel("Previsões",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Escolha o Modelo:",
                             choices = c("ARIMA", "ETS", "MLPE", "RandomForest", "VAR", "ARIMAX")),
                 selectInput("iteration", "Escolha a Iteração:",
                             choices = as.character(1:10)),
                 selectInput("option", "Escolha o Objetivo:", choices = c("O1", "O2")),
                 selectInput("optimization", "Escolha a Otimização:",
                             choices = c("Monte Carlo", "Hill Climbing", "Simulated Annealing", "Differential Evolution", "PSO", "RGA")),
                 actionButton("optimize", "Otimizar")
               ),
               mainPanel(
                 h4("Vendas Previstas:"),
                 DTOutput("results"),
                 h4("\nFuncionários:"),
                 DTOutput("funcionarios"),
                 h4("\nEncomendas:"),
                 DTOutput("encomendas"),
                 h4("\nLucro e Esforço:"),
                 textOutput("lucro"),
                 textOutput("esforco")
               )
             )
    ),
    tabPanel("Metricas",
             sidebarLayout(
               sidebarPanel(
                 selectInput("metrics_model", "Escolha o Modelo:",
                             choices = c("ARIMA", "ETS", "MLPE", "RandomForest", "VAR", "ARIMAX"))
               ),
               mainPanel(
                 DTOutput("metrics"),
                 uiOutput("metrics_image")
               )
             )
    ),
    tabPanel("Eval",
             fluidPage(
               h4("Custos Totais:"),
               textOutput("eval_custo_empregados"),
               textOutput("eval_custo_encomendas"),
               h4("\nCapacidade Máxima de Empregados:"),
               DTOutput("eval_capacidade_empregados"),
               h4("\nVendas Efetivas:"),
               DTOutput("eval_vendas_efetivas"),
               h4("\nVendas em USD:"),
               DTOutput("eval_vendas_usd"),
               h4("Stock:"),
               DTOutput("eval_stock"),
               h4("\nCusto de Stock:"),
               DTOutput("eval_custo_stock")
             )
    )
  )
)

server <- function(input, output, session) {
  # Variável reativa para armazenar a matriz de vendas previstas
  Matriz_Vendas_Previstas <- reactiveVal(NULL)
  
  get_model_file <- function(model) {
    switch(model,
           "ARIMA" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ARIMAprevisoes_walmart.csv",
           "ETS" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ETSprevisoes_walmart.csv",
           "MLPE" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/MLPEprevisoes_walmart.csv",
           "RandomForest" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/RandomForestprevisoes_walmart.csv",
           "VAR" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/VARprevisoes_walmart.csv",
           "ARIMAX" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ARIMAXprevisoes_walmart.csv")
  }
  
  get_metrics_file <- function(model) {
    switch(model,
           "ARIMA" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/ARIMAmedian_metrics.csv",
           "ETS" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/ETSmedian_metrics.csv",
           "MLPE" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/MLPEmedian_metrics.csv",
           "RandomForest" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/RandomForestmedian_metrics.csv",
           "VAR" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/var_updated.csv",
           "ARIMAX" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/arimax_updated.csv")
  }
  
  output$results <- renderDT({
    model_file <- get_model_file(input$model)
    
    if (!file.exists(model_file)) {
      return(datatable(data.frame(Mensagem = "Arquivo não encontrado.")))
    }
    
    data <- read.csv(model_file)
    iteration_data <- data %>% filter(Iteracao == as.numeric(input$iteration))
    
    if (nrow(iteration_data) == 0) {
      return(datatable(data.frame(Mensagem = "Nenhum dado encontrado para a iteração selecionada.")))
    }
    
    iteration_matrix <- as.matrix(iteration_data[, c("Semana1", "Semana2", "Semana3", "Semana4")])
    rownames(iteration_matrix) <- paste0("Dep", iteration_data$Departamento)
    
    # Converter os valores da matriz para inteiros
    iteration_matrix <- apply(iteration_matrix, 2, as.integer)
    
    # Atualizar a variável reativa e imprimir na consola
    Matriz_Vendas_Previstas(iteration_matrix)
    print(Matriz_Vendas_Previstas())
    
    datatable(as.data.frame(iteration_matrix), rownames = TRUE, options = list(dom = 't'))
  })
  
  output$metrics <- renderDT({
    metrics_file <- get_metrics_file(input$metrics_model)
    
    if (!file.exists(metrics_file)) {
      return(datatable(data.frame(Mensagem = "Arquivo não encontrado.")))
    }
    
    metrics_data <- read.csv(metrics_file, row.names = 1)
    
    datatable(metrics_data, options = list(dom = 't'))
  })
  
  observeEvent(input$optimize, {
    # Sempre obter a matriz original de vendas previstas
    vendasPrevistas <- Matriz_Vendas_Previstas()
    print(vendasPrevistas)
    
    if (is.null(vendasPrevistas)) {
      showNotification("Por favor, selecione um modelo e iteração para visualizar as previsões antes de otimizar.", type = "error")
      return(NULL)
    }
    
    # Escolha do conjunto de scripts com base na opção selecionada
    option_path <- "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/OtimizacaoAtualizada"
    
    source(paste0(option_path, "/blind.R"))
    source(paste0(option_path, "/hill.R"))
    source(paste0(option_path, "/montecarlo.R"))
    source(paste0(option_path, "/calcUpper.R"))
    #source(paste0(option_path, "/funcao_eval.R"))
    
    ##############EVAL#####################
    #Calculo custo total empregados
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
    
    #Calculo nº produtos que funcionarios suportam p//Dep
    
    capacidadeEmpregados <- c(4000, 7000, 9500)
    
    calcEmpregadosMaxSup <- function(empregados, capacidadeEmpregados) {
      mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
      resultado <- numeric(ncol(mEmpregados))
      for (j in 1:ncol(mEmpregados)) {
        soma <- 0
        for (i in 1:nrow(mEmpregados)) {
          soma <- soma + mEmpregados[i, j] * capacidadeEmpregados[i]
        }
        resultado[j] <- soma
      }
      
      return(resultado)
    }
    
    #Calculo custo total encomendas
    custoEncomendas <- c(6, 8, 9, 11)
    
    calcCustoEncomendas <- function(encomendas){
      mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
      resultados <- vector("numeric", length = 4)
      
      for (i in 1:nrow(mEncomendas)) {
        soma_col <- sum(mEncomendas[, i])
        resultados[i] <- soma_col * custoEncomendas[i]
      }
      return(sum(resultados))
    }
    
    
    #Calculo matriz de vendas efetivas
    #Vendas efetivas = previstas SE previstas<=encomenda E previstas<=sup funcionarios
    #Vendas efetivas = min(Encomendas, Funcionarios) SE previstas>encomendas OU previstas>sup func
    
    calcVendasEfetivas <- function(vendasPrevistas, mEncomendas, capacidadeEmpregados) {
      vendasEfetivas <- matrix(0, nrow = nrow(vendasPrevistas), ncol = ncol(vendasPrevistas))
      for (i in 1:nrow(vendasPrevistas)) {
        for (j in 1:ncol(vendasPrevistas)) {
          if (vendasPrevistas[i, j] <= mEncomendas[i, j] & vendasPrevistas[i, j] <= capacidadeEmpregados[j]) {
            vendasEfetivas[i, j] <- vendasPrevistas[i, j]
          } else if (i > 1 && vendasPrevistas[i - 1, j] <= mEncomendas[i - 1, j]) {
            vendasEfetivas[i, j] <- mEncomendas[i-1, j] - vendasPrevistas[i - 1, j]
          } else {
            vendasEfetivas[i, j] <- min(mEncomendas[i, j], capacidadeEmpregados[j])
          }
        }
      }
      return(vendasEfetivas)
    }
    
    #Calculo matriz de ganhos
    valorProdutos <- c(8, 10, 12, 16)
    calcVendasUSD <- function(vendasEfetivas, valorProdutos) {
      vendasUSD <- vendasEfetivas
      for (i in 1:ncol(vendasEfetivas)) {
        vendasUSD[, i] <- vendasEfetivas[, i] * valorProdutos[i]
      }
      return(vendasUSD)
    }
    
    #Calculo matriz stock 
    calcStock <- function(vendasEfetivas, mEncomendas) {
      stock <- matrix(0, nrow = 4, ncol = 4)
      for (s in 1:4) {
        for (d in 1:4) {
          if (s == 1) {
            stock[s, d] <- mEncomendas[s, d] - vendasEfetivas[s, d]
          } else {
            stock[s, d] = stock[s - 1, d] + mEncomendas[s, d] - vendasEfetivas[s, d]
          }
        }
      }
      return(stock)
    }
    
    
    #Calculo matriz custo stock
    custoProdutosStock <- c(3, 5, 6, 8)
    calcCustoStock <- function(stock, custoProdutosStock) {
      custoStock <- matrix(0, nrow = 4, ncol = 4)
      for (i in 1:4) {
        custoStock[, i] <- stock[, i] * custoProdutosStock[i]
      }
      return(custoStock)
    }
    
    
    #Funçoes de avaliação
    eval <- function(s){
      #Divide s em empregados e encomendas 
      empregados = s[1:12]
      encomendas = s[13:28]
      mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
      mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
      
      #Usa cada uma das funçoes para calcular tudo
      custoEmpregados <- calcCustoEmpregados(empregados)
      custoEncomendas <- calcCustoEncomendas(encomendas)
      capacidadeEmpregados <- calcEmpregadosMaxSup(empregados, capacidadeEmpregados)
      numEncomendas <- sum(encomendas != 0)
      vendasEfetivas <- calcVendasEfetivas(vendasPrevistas, mEncomendas, capacidadeEmpregados)
      vendasUSD <- calcVendasUSD(vendasEfetivas, valorProdutos)
      stock <- calcStock(vendasEfetivas, mEncomendas)
      custoStock <- calcCustoStock(stock, custoProdutosStock)
      
      f1 <- sum(vendasUSD) - sum(custoEmpregados,custoEncomendas, custoStock)
      f2 <- sum(numEncomendas, sum(empregados))
      
      # Arredonda lucro e esforço
      f1 <- round(f1, digits = 2)
      f2 <- round(f2, digits = 2)
      
      
      # Retorna lucro e esforço
      return(c(lucro = f1, esforco = f2))
    }
    
    #funções auxiliares para calcular 
    
    eval1 <- function(s) {
      resultado <- eval(s)
      return(unname(resultado["lucro"])) # retorna o lucro arredondado
    }
    
    
    eval2 <- function(s) {
      resultado <- eval(s)
      return(unname(resultado["esforco"])) # retorna o esforço arredondado
    }
    
    ################/EVAL##################
    # Variáveis de configuração
    lower = rep(0, 28)
    upper <- calcUpper(vendasPrevistas)
    
    if (input$optimization == "Monte Carlo") {               ####################OTIMIZAÇAO##########3
      if (input$option == "O1") {
        # Código para a opção O1 com Monte Carlo
        
        N <- 10000
        MC <- mcsearch(fn = eval1, lower = lower, upper = upper, N = N, type = "max")
        cat("\nbest solution:", round(MC$sol), " \n evaluation function", MC$eval, " \n (found at iteration:", MC$index, ")\n")
        
        lucro <- abs(round(MC$eval))
        bestSolution <- round(MC$sol)
        
      } else if (input$option == "O2") {
        evaluate_with_weights <- function(s, W1, W2) {
          resultado <- eval(s)
          
          lucro <- resultado["lucro"]
          esforco <- resultado["esforco"]
          
          E1 <- -W1 * lucro
          E2 <- W2 * esforco
          
          E <- E1 + E2
          
          return(list(E = E, E1 = E1, E2 = E2, lucro = lucro, esforco = esforco))
        }
        
        N=1000 # number of searches
        
        W1=0.5
        W2=0.5
        Z=500.000
        
        # Função de avaliação para Monte Carlo
        mc_eval <- function(s) {
          result <- evaluate_with_weights(s, W1, W2)
          return(result$E)  # Retorna apenas E para o otimizador
        }
        
        MC=mcsearch(fn=mc_eval,lower=lower,upper=upper,N=N,type="min")
        
        # Captura os valores da melhor solução
        best_result <- evaluate_with_weights(MC$sol, W1, W2)
        
        cat("best solution:",round(MC$sol)," \n evaluation function",MC$eval," \n (found at iteration:",MC$index,")\n")
        cat("Lucro:", best_result$lucro, "\nEsforço:", best_result$esforco, "\n")
        
        lucro <- abs(round(best_result$lucro))
        esforco <- abs(round(best_result$esforco))
        bestSolution <- round(MC$sol)
      }
    } else if (input$optimization == "Hill Climbing") {                              #########HILLCLIMBING###########
      if (input$option == "O1") {
        # slight change of a real par under a normal u(0,0.5) function:
        rchange1=function(par,lower,upper) { 
          hchange(par, lower=lower, upper=upper, rnorm, mean=0, sd=0.25, round=FALSE) 
        }
        
        # Controle para Hill Climbing
        control <- list(maxit = 1000, REPORT = 100)
        
        HC = hclimbing(par = runif(28, lower, upper), fn = eval1, change = rchange1, lower = lower, upper = upper, control = control, type = "max")
        cat("best solution:", round(HC$sol), "evaluation function", -HC$eval, "\n")
        
        lucro <- abs(round(-HC$eval))
        bestSolution <- round(HC$sol)
        
      } else if (input$option == "O2") {                                    ###########
        evaluate_with_weights <- function(s, W1, W2) {
          resultado <- eval(s)
          
          lucro <- resultado["lucro"]
          esforco <- resultado["esforco"]
          
          E1 <- -W1 * lucro
          E2 <- W2 * esforco
          
          E <- E1 + E2
          
          return(list(E = E, E1 = E1, E2 = E2, lucro = lucro, esforco = esforco))
        }
        
        # slight change of a real par under a normal u(0,0.5) function:
        rchange1=function(par,lower,upper) # change for hclimbing
        { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=TRUE) }
        
        # Controle para Hill Climbing
        control <- list(maxit = 1000, REPORT = 100)
        
        W1=0.5
        W2=0.5
        Z=500.000
        
        # Função de avaliação para Hill Climbing
        hc_eval <- function(s) {
          result <- evaluate_with_weights(s, W1, W2)
          return(result$E)  # Retorna apenas E para o otimizador
        }
        
        HC=hclimbing(par = runif(28, lower, upper),fn=hc_eval,change=rchange1,lower=lower,upper=upper,control=control,type="min")
        
        # Captura os valores da melhor solução
        best_result <- evaluate_with_weights(HC$sol, W1, W2)
        
        cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
        cat("Lucro:", -best_result$lucro, "\nEsforço:", best_result$esforco, "\n")
        
        lucro <- abs(round(-best_result$lucro))
        esforco <- abs(round(best_result$esforco))
        bestSolution <- round(MC$sol)
      }
      
    } else if (input$optimization == "Simulated Annealing") {    #############################SANN#################
      if (input$option == "O1") {
        
        # slight change of a real par under a normal u(0,0.5) function:
        rchange2=function(par) # change for hclimbing
        { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }
        
        N=1000 # number of searches
        REPORT=N/10 # report results
        
        CSANN=list(maxit=N,temp=28,trace=TRUE)
        SA=optim(par=rep(0, 28),fn=eval1,method="SANN",gr=rchange2,control=CSANN)
        cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
        
        lucro <- abs(round(SA$value))
        bestSolution <- round(SA$par)
        
      } else if (input$option == "O2") {                                 ##################
        
        lucro <- round(SA$value)
        bestSolution <- round(SA$par)
        
      }
    } else if (input$optimization == "Differential Evolution") {      ##########################################DE###########
      if (input$option == "O1") {
    
        # some parameters that will be equal for all methods:
        popSize=100 # population size
        iter=10 # maximum number of iterations
        report=10 # report progress every 10 iterations
        lower=rep(0, 28)
        upper= calcUpper(vendasPrevistas)
        
        ## show best:
        showbest=function(method,par,eval1)
        { cat("method:",method,"\n > par:",round(par),"\n > eval:",eval1,"\n") }
        
        # Differential Evolution Optimization: -------------------------
        de=DEoptim(fn=eval1,lower=lower,upper=upper,DEoptim.control(NP=popSize,itermax=iter,trace=report))
        # get the best solution:
        # note: the way to get the best solution and evaluation depends on the implementation of the method and thus
        # it can be different from method to method:
        showbest("DEoptim",de$optim$bestmem,-de$optim$bestval)
        
        lucro <- abs(round(-de$optim$bestval))
        bestSolution <- round(de$optim$bestmem)  # Corrigido aqui
        
      } else if (input$option == "O2") {                                                      ####################
        # Definition of the famous rastrigin function
        # x is a vector with D real values.
        rastrigin=function(x=c()) { return (sum(x^2-10*cos(2*pi*x)+10))}
        
        
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
        lower=rep(0, 28)
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
        
        lucro <- abs(round(best_f1))
        esforco <- abs(round(best_f2))
        bestSolution <- best_solution
        
      }
    } else if (input$optimization == "PSO") {   ##################################################################PSO########
      if (input$option == "O1") {
        
        # Configuração e execução do PSO
        popSize=100 # population size
        iter=10 # maximum number of iterations
        report=10 # report progress every 10 iterations
        lower=rep(0, 28)
        upper= calcUpper(vendasPrevistas)
        
        # Função para mostrar o melhor resultado
        showbest=function(method,par,eval1) {
          cat("method:", method, "\n > par:", par, "\n > eval:", eval1, "\n")
        }
        
        # Particle Swarm Optimization:
        ps = psoptim(par = lower, fn = eval1, lower = lower, upper = upper, control = list(trace = 1, REPORT = report, maxit = iter, s = popSize))
        showbest("psoptim", ps$par, -ps$value)
        
        
        lucro <- abs(round(-ps$value))
        bestSolution <- round(ps$par)
        
      } else if (input$option == "O2") {      
        
        # Função wrapper para otimização com psoptim
        eval_wrapper <- function(s) {
          return(eval(s)[1])
        }
        
        # Parâmetros para psoptim
        popSize = 100 # population size
        iter = 10 # maximum number of iterations
        report = 10 # report progress every 10 iterations
        lower = rep(0, 28)
        upper = calcUpper(vendasPrevistas)
        
        # Configuração e execução do PSO
        pso_control <- list(trace = 1, REPORT = report, maxit = iter, s = popSize)
        ps <- psoptim(par = lower, fn = eval_wrapper, lower = lower, upper = upper, control = pso_control)
        
        # Obtém a melhor solução
        best_solution <- round(ps$par)
        
        # Calcula f1 e f2 para a melhor solução
        eval_values <- eval(best_solution)
        best_f1 <- -eval_values[1]  # Inverter o sinal para obter o lucro
        best_f2 <- eval_values[2]   # Esforço
        
        # Exibe os resultados
        cat("best_solution:", best_solution, "\n")
        cat("Lucro (f1):", best_f1, "\n")
        cat("Esforço (f2):", best_f2, "\n")
        
        lucro <- abs(round(best_f1))
        esforco <- abs(round(best_f2))
        bestSolution <- best_solution
      }
    } else if (input$optimization == "RGA") {    ####################################################################RGA#########
      if (input$option == "O1") {
        rastrigin = function(x=c()) { return (sum(x^2 - 10 * cos(2 * pi * x) + 10)) }
        
        popSize = 100
        iter = 10
        report = 10
        lower = rep(0, 28)
        upper = calcUpper(vendasPrevistas)
        
        showbest <- function(method, par, eval1) {
          cat("method:", method, "\n > par:", par, "\n > eval:", -eval1, "\n")
        }
        
        ITER <<- 1
        traceGA <- function(obj) {
          if ((ITER %% report) == 0) {
            PMIN <- which.min(obj$evaluations)
            cat("iter:", ITER, " eval:", obj$evaluations[PMIN], "\n")
          }
          ITER <<- ITER + 1
        }
        
        rga <- rbga(lower, upper, popSize = popSize, evalFunc = eval1, iter = iter, monitor = traceGA)
        PMIN <- which.min(rga$evaluations)
        showbest("rbga", rga$population[PMIN,], rga$evaluations[PMIN])
        
        lucro <- abs(round(rga$evaluations[PMIN]))
        bestSolution <- rga$population[PMIN,]
        
      } else if (input$option == "O2") {                                                           #################
        
        eval_wrapper <- function(s) {
          resultado <- eval(round(s))
          return(-resultado["lucro"])  # Retorna apenas o lucro como valor negativo
        }
        
        popSize = 100
        iter = 10
        report = 10
        lower = rep(0, 28)
        upper = calcUpper(vendasPrevistas)
        
        showbest <- function(method, par, eval) {
          eval_values <- eval(round(par))
          cat("method:", method, "\n > par:", round(par), "\n > lucro:", -round(eval_values["lucro"]), "\n > esforço:", round(eval_values["esforco"]), "\n")
        }
        
        ITER <<- 1
        traceGA <- function(obj) {
          if ((ITER %% report) == 0) {
            PMIN <- which.min(obj$evaluations)
            cat("iter:", ITER, " eval (lucro):", -round(obj$evaluations[PMIN]), "\n")
          }
          ITER <<- ITER + 1
        }
        
        rga <- rbga(lower, upper, popSize = popSize, evalFunc = eval_wrapper, iter = iter, monitor = traceGA)
        PMIN <- which.min(rga$evaluations)
        best_solution <- round(rga$population[PMIN,])
        
        eval_values <- eval(best_solution)
        best_f1 <- -round(eval_values["lucro"])
        best_f2 <- round(eval_values["esforco"])
        
        cat("Lucro (f1):", best_f1, "\n")
        cat("Esforço (f2):", best_f2, "\n")
        
        lucro <- abs(best_f1)
        esforco <- abs(best_f2)
        bestSolution <- best_solution
      }
    }
    
    # Verificar se MC$sol existe e não é nulo antes de acessar seus elementos
    if (!is.null(bestSolution)) {
      # Matriz de Funcionários (3 tipos de funcionários x 4 departamentos)
      funcionarios <- matrix(bestSolution[1:12], nrow = 3, byrow = FALSE)
      colnames(funcionarios) <- c("Dep1", "Dep2", "Dep3", "Dep4")
      rownames(funcionarios) <- c("Junior", "Normal", "Senior")
      funcionarios <- apply(funcionarios, 2, as.integer)  # Convertendo para inteiros
      
      # Matriz de Encomendas (4 departamentos x 4 semanas)
      encomendas <- matrix(bestSolution[13:28], nrow = 4, byrow = TRUE)
      colnames(encomendas) <- c("Semana1", "Semana2", "Semana3", "Semana4")
      rownames(encomendas) <- c("Dep1", "Dep2", "Dep3", "Dep4")
      encomendas <- apply(encomendas, 2, as.integer)  # Convertendo para inteiros
      
      # Exibir as matrizes no painel principal
      output$funcionarios <- renderDT({
        datatable(as.data.frame(funcionarios), options = list(dom = 't'))
      })
      
      output$encomendas <- renderDT({
        datatable(as.data.frame(encomendas), options = list(dom = 't'))
      })
      
      output$lucro <- renderText({
        paste("Lucro:", lucro)
      })
      
      output$esforco <- renderText({
        if (input$option == "O2") {
          paste("Lucro:", lucro)
          paste("Esforço:", esforco)
        } else {
          NULL
        }
      })
      
      # Calcular e exibir as novas matrizes na aba "Eval"
      custo_empregados_val <- calcCustoEmpregados(bestSolution[1:12])
      output$eval_custo_empregados <- renderText({
        paste("Custo Total Empregados:", custo_empregados_val)
      })
      
      output$eval_capacidade_empregados <- renderDT({
        capacidade_empregados <- calcEmpregadosMaxSup(bestSolution[1:12], capacidadeEmpregados)
        capacidade_empregados_df <- data.frame(t(capacidade_empregados))
        colnames(capacidade_empregados_df) <- c("Capacidade_Dep1", "Capacidade_Dep2", "Capacidade_Dep3", "Capacidade_Dep4")
        rownames(capacidade_empregados_df) <- c("Capacidade")
        datatable(capacidade_empregados_df, options = list(dom = 't'))
      })
      
      custo_encomendas_val <- calcCustoEncomendas(bestSolution[13:28])
      output$eval_custo_encomendas <- renderText({
        paste("Custo Total Encomendas:", custo_encomendas_val)
      })
      
      output$eval_vendas_efetivas <- renderDT({
        vendas_efetivas <- calcVendasEfetivas(vendasPrevistas, encomendas, calcEmpregadosMaxSup(bestSolution[1:12], capacidadeEmpregados))
        vendas_efetivas_df <- as.data.frame(vendas_efetivas)
        colnames(vendas_efetivas_df) <- c("Semana1", "Semana2", "Semana3", "Semana4")
        rownames(vendas_efetivas_df) <- c("Dep1", "Dep2", "Dep3", "Dep4")
        datatable(vendas_efetivas_df, options = list(dom = 't'))
      })
      
      output$eval_vendas_usd <- renderDT({
        vendas_usd <- calcVendasUSD(calcVendasEfetivas(vendasPrevistas, encomendas, calcEmpregadosMaxSup(bestSolution[1:12], capacidadeEmpregados)), valorProdutos)
        vendas_usd_df <- as.data.frame(vendas_usd)
        colnames(vendas_usd_df) <- c("Semana1", "Semana2", "Semana3", "Semana4")
        rownames(vendas_usd_df) <- c("Dep1", "Dep2", "Dep3", "Dep4")
        datatable(vendas_usd_df, options = list(dom = 't'))
      })
      
      output$eval_stock <- renderDT({
        stock <- calcStock(calcVendasEfetivas(vendasPrevistas, encomendas, calcEmpregadosMaxSup(bestSolution[1:12], capacidadeEmpregados)), encomendas)
        stock_df <- as.data.frame(stock)
        colnames(stock_df) <- c("Semana1", "Semana2", "Semana3", "Semana4")
        rownames(stock_df) <- c("Dep1", "Dep2", "Dep3", "Dep4")
        datatable(stock_df, options = list(dom = 't'))
      })
      
      output$eval_custo_stock <- renderDT({
        custo_stock <- calcCustoStock(calcStock(calcVendasEfetivas(vendasPrevistas, encomendas, calcEmpregadosMaxSup(bestSolution[1:12], capacidadeEmpregados)), encomendas), custoProdutosStock)
        custo_stock_df <- as.data.frame(custo_stock)
        colnames(custo_stock_df) <- c("Semana1", "Semana2", "Semana3", "Semana4")
        rownames(custo_stock_df) <- c("Dep1", "Dep2", "Dep3", "Dep4")
        datatable(custo_stock_df, options = list(dom = 't'))
      })
      
    } else {
      showNotification("Erro: A solução do Monte Carlo está vazia.", type = "error")
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)

