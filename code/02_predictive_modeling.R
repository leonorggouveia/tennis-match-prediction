
# Projeto Aplicado em Ciência de Dados I #

# Grupo 1, Integrantes (CDB1):
# Amanda Viapiana, Inês Machado, Leonor Gouveia, Sílvia Gentil e Tiago Woodger

## PARTE 2 ##

# Bibliotecas a utilizar
rm(list=ls())                                     # Remover as variáveis do ambiente
x <- c("jsonlite", "caret",                       # Para ler os ficheiros em JSON e fazer validação cruzada
       "dplyr", "tidyr", "stringr", "lubridate",  # Para manipular dados, dataframes, strings e datas
       "xgboost", "class", "e1071",               # Para criar modelos de Gradient Boosting, KNN e Naive Bayes
       "ggplot2", "corrplot", "scales",           # Para criar gráficos e matrizes de correlação
       "lsr", "DescTools",                        # Para fazer medidas de associação entre variáveis
       "fastDummies", "UBL")                      # Para criar variáveis dummy e fazer Oversampling
                               
# install.packages(x)  # Instalar as bibliotecas
lapply(x, library, character.only = TRUE)  # Carregar as bibliotecas

# Leitura dos ficheiros de dados
dados <- stream_in(file("dados.json"))  # Ficheiro com os dados


##### DATA PREPARATION #####

# Transformar a variável Sets em fator
dados$Sets <- as.factor(dados$Sets)

# Mudar a ordem das colunas
ordem <- c("Winner", "Loser", "WBornCountry", "WDOB", "WBirthYear", "WAge", "WHand", "WBackHand", 
           "WHeight", "WGameRank", "WID", "LBornCountry", "LDOB", "LBirthYear", "LAge", "LHand", 
           "LBackHand", "LHeight", "LGameRank", "LID", "StartDate", "EndDate", "Year", "GameRound", 
           "Ground", "Tournament", "Location", "Prize", "Score", "Sets")
dados <- dados[, ordem]  # Aplicar a ordem estabelecida

# Imputação de dados
dados[is.na(dados$WHand), "WHand"] <- "Right-Handed"  # Com a moda
dados[is.na(dados$LHand), "LHand"] <- "Right-Handed"  # Com a moda
dados$WAge[is.na(dados$WAge)] <- mean(dados$WAge, na.rm = TRUE)  # Com a média
dados$LAge[is.na(dados$LAge)] <- mean(dados$LAge, na.rm = TRUE)  # Com a média
dados$WHeight[is.na(dados$WHeight)] <- mean(dados$WHeight, na.rm = TRUE)  # Com a média
dados$LHeight[is.na(dados$LHeight)] <- mean(dados$LHeight, na.rm = TRUE)  # Com a média

#--- Novas Variáveis ---#

# Criar variável para a diferença de idade entre os jogadores
dados$AgeDiff <- abs(dados$WAge - dados$LAge)

# Criar variável para a diferença de altura entre os jogadores
dados$HeightDiff <- abs(dados$WHeight - dados$LHeight)

# Criar variável para a taxa de vitórias de cada jogador no momento do jogo ("WWinRate", "LWinRate")
dados$WWinRate <- NA  # Criar uma nova coluna para a variável
dados$LWinRate <- NA  # Criar uma nova coluna para a variável
player_wins   <- list()  # Criar uma lista para o número de vitórias de cada jogador
player_losses <- list()  # Criar uma lista para o número de derrotas de cada jogador

# Iterar sobre cada linha (jogo) da base de dados
for (i in 1:nrow(dados)) {
  winner <- dados$WID[i]
  loser <- dados$LID[i]
  
  # Se o jogador ainda não tiver jogos atribuir 0 vitórias ou derrotas
  if (is.null(player_wins[[winner]]))  player_wins[[winner]]  <- 0
  if (is.null(player_losses[[winner]])) player_losses[[winner]] <- 0
  if (is.null(player_wins[[loser]]))   player_wins[[loser]]   <- 0
  if (is.null(player_losses[[loser]]))  player_losses[[loser]]  <- 0
  
  # Se o vencedor ainda não tem jogos, a taxa de vitória, por defeito, é de 0.5
  if (player_wins[[winner]] == 0 && player_losses[[winner]] == 0) {dados$WWinRate[i] <- 0.5}
  else {dados$WWinRate[i] <- player_wins[[winner]] / (player_wins[[winner]] + player_losses[[winner]])}
  
  # Se o perdedor ainda não tem jogos, a taxa de vitória, por defeito, é de 0.5
  if (player_wins[[loser]] == 0 && player_losses[[loser]] == 0) {dados$LWinRate[i] <- 0.5}
  else {dados$LWinRate[i] <- player_wins[[loser]] / (player_wins[[loser]] + player_losses[[loser]])}
  
  # Para o vencedor do jogo
  if (is.null(player_wins[[winner]])) {player_wins[[winner]] <- 0}    # Se não tem vitórias, é 0
  else {player_wins[[winner]] <- player_wins[[winner]] + 1}           # Caso contrário, adiciona mais uma
  
  # Para o perdedor do jogo
  if (is.null(player_losses[[loser]])) {player_losses[[loser]] <- 0}  # Se não tem vitórias, é 0 
  else {player_losses[[loser]] <- player_losses[[loser]] + 1}         # caso contrário, adiciona mais uma
  
  # Preenche os outros valores no caso de serem nulos(as vitórias do perdedor e derrotas do vencedor)
  if (is.null(player_losses[[winner]])) {player_losses[[winner]] <- 0} 
  if (is.null(player_wins[[loser]])) {player_wins[[loser]] <- 0}
}

# Criar uma variável para a diferença de rácio de vitórias entre os jogadores
dados$WinRateDiff <- abs(dados$WWinRate - dados$LWinRate)

# Função para calcular a média do ranking dos últimos 3 anos por jogador e criar uma nova coluna
calc_mean_rank <- function(data, id_col, rank_col, year_col, new_col_name) {
  data %>%
    group_by(!!sym(id_col)) %>%  # Agrupa os dados por jogador (ID)
    arrange(!!sym(id_col), !!sym(year_col)) %>%  # Ordena os dados por jogador e por ano
    mutate(
      !!sym(new_col_name) := sapply(1:n(), function(i) {  # Cria nova coluna com a média do "GameRank"
        current_year <- .data[[year_col]][i]  # Ano do jogo atual
        recent_years <- current_year - 3      # Limite inferior dos 3 anos anteriores
        player_id <- .data[[id_col]][i]       # ID do jogador atual
      
      past_ranks <- data %>% filter(  # Filtra o "GameRank" do mesmo jogador nos 3 anos anteriores
        .data[[id_col]] == player_id,
        .data[[year_col]] >= recent_years,
        .data[[year_col]] < current_year) %>%
        pull(!!sym(rank_col))
      
      mean_rank <- mean(past_ranks, na.rm = TRUE)  # Calcula a média ignorando NAs (converte NaN para NA)
      ifelse(is.nan(mean_rank), NA, mean_rank)})) %>% ungroup()}

dados <- calc_mean_rank(dados, id_col = "WID", rank_col = "WGameRank",  # Aplicar a função para Winners
                        year_col = "Year", new_col_name = "WGameRankMean")  # Nova coluna "WGameRankMean"

dados <- calc_mean_rank(dados, id_col = "LID", rank_col = "LGameRank",  # Aplicar a função para Losers
                        year_col = "Year", new_col_name = "LGameRankMean")  # Nova coluna "LGameRankMean"

# Imputar a média de 3 anos do ranking do jogador no ranking atual
dados[is.na(dados$WGameRank), "WGameRank"] <- round(dados[is.na(dados$WGameRank), "WGameRankMean"])  # Vencedor
dados[is.na(dados$LGameRank), "LGameRank"] <- round(dados[is.na(dados$LGameRank), "LGameRankMean"])  # Perdedor

# Imputar os rankings faltantes com a respetiva média
dados[is.na(dados$WGameRank), "WGameRank"] <- round(mean(dados$WGameRank, na.rm = TRUE))  # Para os vencedores
dados[is.na(dados$LGameRank), "LGameRank"] <- round(mean(dados$LGameRank, na.rm = TRUE))  # Para os perdedores
dados[is.na(dados$WGameRankMean), "WGameRankMean"] <- mean(dados$WGameRankMean, na.rm = TRUE)  # Para os vencedores
dados[is.na(dados$LGameRankMean), "LGameRankMean"] <- mean(dados$LGameRankMean, na.rm = TRUE)  # Para os perdedores

# Criar uma variável com a diferença de ranking entre os jogadores (com o ranking do jogo e a média de 3 anos)
dados$GameRankDiff <- abs(dados$WGameRank - dados$LGameRank)  # Ranking atual
dados$GameRankMeanDiff <- abs(dados$WGameRankMean - dados$LGameRankMean)  # Média de 3 anos

# Criar uma variável da média entre "WGameRank" e "LGameRank"
dados$GameRankMean <- (dados$WGameRank + dados$LGameRank)/2

# Criar uma variável da média entre "WGameRankMean" e "LGameRankMean"
dados$GameRankMeanMean <- (dados$WGameRankMean + dados$LGameRankMean)/2

# Criar uma nova variável com o número de alemães por jogo ("CountryDiff")
dados$CountryDiff <- ifelse(dados$WBornCountry == "Germany" & dados$LBornCountry == "Germany", 1,  # Condições
                            ifelse(dados$WBornCountry == "Germany" | dados$LBornCountry == "Germany", 2, 3))
dados$CountryDiff <- factor(dados$CountryDiff, levels = 1:3,  # Transformar a variável em fator
                            labels = c("Both Germany", "One Germany", "None Germany"))

table(dados$CountryDiff)  # Visualizar a quantidade de cada categoria

# Criar uma nova variável com o cruzamento entre as mãos do jogadores ("HandDiff")
dados$HandDiff <- ifelse(dados$WHand == "Right-Handed" & dados$LHand == "Right-Handed", 1,  # Condições
                         ifelse(dados$WHand == "Left-Handed" & dados$LHand == "Left-Handed", 2, 3))
dados$HandDiff <- factor(dados$HandDiff, levels = 1:3,  # Transformar a variável em fator
                         labels = c("Both Right-Handed", "Both Left-Handed", "Opposite Hands"))

table(dados$HandDiff)  # Visualizar a quantidade de cada categoria

# Transformar a variável "Ground" em fator
dados$Ground <- as.factor(dados$Ground)

# Agrupar as rondas dos torneios e criar uma variável
dados$GameRoundGroup <- dplyr::case_when(
  dados$GameRound %in% c("1st Round Qualifying", "2nd Round Qualifying") ~ "Qualifying",
  dados$GameRound %in% c("Round of 64", "Round Robin") ~ "Early Rounds",
  dados$GameRound %in% c("Round of 16", "Round of 32") ~ "Mid Rounds",
  dados$GameRound %in% c("Quarter-Finals", "Semi-Finals", "Finals", "3rd Round Qualifying") ~ "Final Rounds",
  TRUE ~ "Other")
ordem <- c("Qualifying", "Early Rounds", "Mid Rounds", "Final Rounds")  # Ordenar as rondas
dados$GameRoundGroup <- factor(dados$GameRoundGroup, levels = ordem)  # Transformar em fator

# Agrupar os nomes dos torneios
challenger <- c("Weiden", "Eisenach", "Oberstaufen", "Lubeck", "Neu Ulm", "Wolfsburg", "Braunschweig", "Heilbronn",
                "Meerbusch", "Marburg", "Pullach", "Ludwigshafen", "Aachen", "Eckental", "Karlsruhe", "Koblenz",
                "Furth", "Ismaning", "Dortmund", "Freudenstadt", "Bielefeld", "Cologne 1", "Munster", "Montabaur", 
                "Travemunde", "Waiblingen", "Neunkirchen", "Essen", "Luedenscheid", "Heilbronn II", "Dresden", 
                "Neumunster", "Celle", "Monchengladbach", "Sylt", "Magdeburg", "Alpirsbach", "Riemerling", "Emden",
                "Garmisch", "Nuembrecht", "Lippstadt", "Bochum", "Hamburen", "Aschaffenburg", "Bad Saarow", "Zell", 
                "Ettlingen", "Luebeck", "Cologne", "Augsburg", "Bad Lippspringe", "Wismar", "Dusseldorf")
tour <- c("Munich", "Hamburg", "Halle", "Stuttgart-1", "Stuttgart-2", "Frankfurt", "Berlin", "Cologne 2")
other <- c("Grand Slam Cup", "DAVIS CUP", "Nations Cup", "World Team Cup")

# Criar uma variável para os torneios agrupados
dados$TournamentGroup <- NA
dados[grepl("Germany F", dados$Tournament), "TournamentGroup"] <- "ATP Futures"
dados[grepl("M15", dados$Tournament), "TournamentGroup"]       <- "ATP Futures"
dados[grepl("M25", dados$Tournament), "TournamentGroup"]       <- "ATP Futures"
dados[grepl("WCT", dados$Tournament), "TournamentGroup"]       <- "Other"
dados[grepl("ATP", dados$Tournament), "TournamentGroup"]       <- "ATP Tour"
dados[dados$Tournament == "Buchholz", "TournamentGroup"]       <- "ATP Futures"
dados[dados$Tournament == "Germany 4B", "TournamentGroup"]     <- "ATP Futures"
dados[dados$Tournament %in% other, "TournamentGroup"]          <- "Other"
dados[dados$Tournament %in% tour, "TournamentGroup"]           <- "ATP Tour"
dados[dados$Tournament %in% challenger, "TournamentGroup"]     <- "ATP Challenger"
dados$TournamentGroup <- as.factor(dados$TournamentGroup)  # transformar em fator

# Correlação entre as variáveis numéricas 
dados_numericos <- dados[sapply(dados, is.numeric)]     # Selecionar as colunas numéricas
dados_numericos <- dados_numericos[c(9:12, 15, 18:21)]  # Selecionar as variáveis numéricas
matriz_cor <- cor(dados_numericos, use = "pairwise.complete.obs")  # Matriz de correlações
corrplot(matriz_cor, method = "number", type = "upper", tl.col = "black", tl.srt = 45)  # Visualizar a matriz

# Análise da associação entre as variáveis numéricas e "Sets"
anova_Year             <- aov(Year ~ Sets, data = dados)                # Associação com "Year"
eta_Year               <- sqrt(etaSquared(anova_Year)[,1])              # Eta Quadrado
anova_Prize            <- aov(Prize ~ Sets, data = dados)               # Associação com "Prize"
eta_Prize              <- sqrt(etaSquared(anova_Prize)[,1])             # Eta Quadrado
anova_AgeDiff          <- aov(AgeDiff ~ Sets, data = dados)             # Associação com "AgeDiff"
eta_AgeDiff            <- sqrt(etaSquared(anova_AgeDiff)[,1])           # Eta Quadrado
anova_HeightDiff       <- aov(HeightDiff ~ Sets, data = dados)          # Associação com "HeightDiff"
eta_HeightDiff         <- sqrt(etaSquared(anova_HeightDiff)[,1])        # Eta Quadrado
anova_WinRateDiff      <- aov(WinRateDiff ~ Sets, data = dados)         # Associação com "WinRateDiff"
eta_WinRateDiff        <- sqrt(etaSquared(anova_WinRateDiff)[,1])       # Eta Quadrado
anova_GameRankDiff     <- aov(GameRankDiff ~ Sets, data = dados)        # Associação com "GameRankDiff"
eta_GameRankDiff       <- sqrt(etaSquared(anova_GameRankDiff)[,1])      # Eta Quadrado
anova_GameRankMean     <- aov(GameRankMean ~ Sets, data = dados)        # Associação com "GameRankMean"
eta_GameRankMean       <- sqrt(etaSquared(anova_GameRankMean)[,1])      # Eta Quadrado
anova_GameRankMeanDiff <- aov(GameRankMeanDiff ~ Sets, data = dados)    # Associação com "GameRankMeanDiff"
eta_GameRankMeanDiff   <- sqrt(etaSquared(anova_GameRankMeanDiff)[,1])  # Eta Quadrado
anova_GameRankMeanMean <- aov(GameRankMeanMean ~ Sets, data = dados)    # Associação com "GameRankMeanMean"
eta_GameRankMeanMean   <- sqrt(etaSquared(anova_GameRankMeanMean)[,1])  # Eta Quadrado

eta_table <- data.frame(  # Criar tabela com os resultados
  Variável = c("Year", "AgeDiff", "HeightDiff", "GameRankDiff", "GameRankMean", 
               "Prize", "WinRateDiff", "GameRankMeanDiff", "GameRankMeanMean"),
  Eta_Squared = c(eta_Year, eta_AgeDiff, eta_HeightDiff, eta_GameRankDiff, eta_GameRankMean,
                  eta_Prize, eta_WinRateDiff, eta_GameRankMeanDiff, eta_GameRankMeanMean))

eta_table <- eta_table[order(-eta_table$Eta_Squared), ]  # Ordenar por maior associação
print(eta_table)  # Visualizar a tabela

# Calcular V de Cramér entre as variáveis categóricas e "Sets"
cramer_Ground          <- CramerV(table(dados$Ground, dados$Sets))           # "Ground"
cramer_HandDiff        <- CramerV(table(dados$HandDiff, dados$Sets))         # "HandDiff"
cramer_CountryDiff     <- CramerV(table(dados$CountryDiff, dados$Sets))      # "CountryDiff"
cramer_GameRoundGroup  <- CramerV(table(dados$GameRoundGroup, dados$Sets))   # "GameRoundGroup"
cramer_TournamentGroup <- CramerV(table(dados$TournamentGroup, dados$Sets))  # "TournamentGroup"

cramer_table <- data.frame(  # Criar tabela com os resultados
  Variável = c("Ground", "HandDiff", "CountryDiff", "TournamentGroup",  "GameRoundGroup"),
  Cramers_V = c(cramer_Ground, cramer_HandDiff, cramer_CountryDiff, cramer_TournamentGroup, cramer_GameRoundGroup))

cramer_table <- cramer_table[order(-cramer_table$Cramers_V), ]  # Ordenar por maior associação
print(cramer_table)  # Visualizar a tabela

#--- Gráficos entre a variável "Sets" e as variáveis independentes ---#

# "Ground"
ggplot(dados, aes(x = Ground, fill = Sets)) +
  geom_bar(position = "fill") + theme_minimal() +  # Tema de fundo
  scale_y_continuous(labels = label_percent()) +  # Escala
  scale_fill_manual(values = c("#FFE680", "#FF7F7F")) +  # Cores das colunas
  labs(x = "Tipo de Superfície", y = "Proporção", fill = "Sets") +  # Títulos
  geom_text(stat = "count", aes(label = after_stat(count)),  # Quantidade de jogos
            position = position_fill(vjust = 0.5), color = "black", size = 4)

# "HeightDiff"
ggplot(dados, aes(x = factor(Sets), y = HeightDiff)) +
  geom_boxplot(fill = "#FFE680") + theme_minimal() +  # Tema de fundo
  labs(title = "Boxplot da Diferença de Altura por Sets", x = "Sets", y = "Diferença de Altura") +  # Títulos
  geom_point(stat = "summary", fun = mean, shape = 20, size = 3, color = "#FF7F7F")  # Média
  
# "AgeDiff"
ggplot(dados, aes(x = factor(Sets), y = AgeDiff)) +
  geom_boxplot(fill = "#FFE680") + theme_minimal() +  # Tema de fundo
  labs(title = "Boxplot da Diferença de Idade por Sets", x = "Sets", y = "Diferença de Idade") +  # Títulos
  geom_point(stat = "summary", fun = mean, shape = 20, size = 3, color = "#FF7F7F")  # Média
  
# "CountryDiff"
ggplot(dados, aes(x = CountryDiff, fill = Sets)) +
  geom_bar(position = "fill") + theme_minimal() +  # Tema de fundo
  scale_y_continuous(labels = label_percent()) +  # Escala 
  scale_fill_manual(values = c("#FFE680", "#FF7F7F")) +  # Cores das colunas
  labs(x = "Nacionalidade", y = "Proporção", fill = "Sets") +  # Títulos
  geom_text(stat = "count", aes(label = after_stat(count)),  # Quantidade de jogos
            position = position_fill(vjust = 0.5), color = "black", size = 4)
  
# "GameRoundGroup"
ggplot(dados, aes(x = GameRoundGroup, fill = Sets)) +
  geom_bar(position = "fill") + theme_minimal() +  # Tema de fundo
  scale_y_continuous(labels = label_percent()) +  # Escala
  scale_fill_manual(values = c("#FFE680", "#FF7F7F")) +  # Cores das colunas
  labs(x = "Ronda", y = "Proporção", fill = "Sets") +  # Títulos
  geom_text(stat = "count", aes(label = after_stat(count)),  # Qauntidade de jogos
            position = position_fill(vjust = 0.5), color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Posição do texto

# "TournamentGroup"
ggplot(dados, aes(x = TournamentGroup, fill = Sets)) +
  geom_bar(position = "fill") + theme_minimal() +  # Tema de fundo
  scale_y_continuous(labels = label_percent()) +  # Escala
  scale_fill_manual(values = c("#FFE680", "#FF7F7F")) +  # Cores das colunas
  labs(x = "Torneio", y = "Proporção", fill = "Sets") +  # Títulos
  geom_text(stat = "count", aes(label = after_stat(count)),  # Quantidade de jogos
            position = position_fill(vjust = 0.5), color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Posição do texto

# "HandDiff"
ggplot(dados, aes(x = HandDiff, fill = Sets)) +
  geom_bar(position = "fill") + theme_minimal() +  # Tema de fundo
  scale_y_continuous(labels = label_percent()) +  # Escala
  scale_fill_manual(values = c("#FFE680", "#FF7F7F")) +  # Cores das colunas
  labs(x = "Mão Dominante dos Jogadores",y = "Proporção", fill = "Sets") +  # Títulos
  geom_text(stat = "count", aes(label = after_stat(count)),  # Quantidade de jogos
            position = position_fill(vjust = 0.5), color = "black", size = 4)

# "Prize"
ggplot(dados, aes(x = factor(Sets), y = Prize/1000)) +  # Contar o prémio em milhares
  stat_summary(fun = mean, geom = "point", size = 6, color = "#FFE680") + theme_minimal(base_size = 14) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "#FFE680", linewidth = 2) +
  labs(title = "Média do Prémio por Número de Sets", x = "Sets", y = "Prémio Monetário ($)") +  # Títulos
  scale_y_continuous(breaks = seq(280, 340, by = 10), labels = scales::comma_format(suffix = "K"))  # Escala


##### MODELING #####

# Separar somente os dados necessários
dadosM <- dados[c(28, 30, 35, 38:45)]  # Criar um dataset menor somente para os modelos
dadosM <- as.data.frame(dadosM)  # Transformar em um dataframe
dadosM$Sets <- droplevels(dadosM$Sets)  # Remover os níveis da variável Sets

# Criação de variáveis binárias para modelos numéricos
dadosM <- dummy_cols(dadosM, select_columns = "HandDiff", remove_first_dummy = TRUE)         # "HandDiff"
dadosM <- dummy_cols(dadosM, select_columns = "GameRoundGroup", remove_first_dummy = TRUE)   # "GameRoundGroup"
dadosM <- dummy_cols(dadosM, select_columns = "CountryDiff", remove_first_dummy = TRUE)      # "CountryDiff"
dadosM <- dummy_cols(dadosM, select_columns = "TournamentGroup", remove_first_dummy = TRUE)  # "TournamentGroup"
dadosM <- dadosM[c(1:4, 7, 12:21)]  # Selecionar as colunas necessárias

# Fazer diferentes técnicas de Oversampling
set.seed(1234)  # Criar uma semente
dadosD <- SmoteClassif(Sets ~ ., dadosM, C.perc = list("3" = 2, "2" = 1), k = 1)  # Oversampling (dobro de "3")
dadosB <- SmoteClassif(Sets ~ ., dadosM, C.perc = "balance", k = 1)               # Oversampling balanceado

table(dadosD$Sets)  # Número de observações no dataset com Oversampling com o dobro de observações de "3"
table(dadosB$Sets)  # Número de observações no dataset com Oversampling balanceado
table(dadosM$Sets)  # Número de observações no dataset original

# Criar para realizar a validação cruzada
folds1 <- createFolds(dadosM$Sets, k = 10, list = TRUE)  # Dataset original
folds2 <- createFolds(dadosD$Sets, k = 10, list = TRUE)  # Dataset com o dobro de observações de "3"
folds3 <- createFolds(dadosB$Sets, k = 10, list = TRUE)  # Dataset balanceado

result_rl   <- list()  # Lista para guardar os resultados da Regressão Logística balanceada
result_knnB <- list()  # Lista para guardar os resultados do KNN balanceado
result_knnD <- list()  # Lista para guardar os resultados do KNN com o dobro de "3"
result_xgbN <- list()  # Lista para guardar os resultados do XGBoost sem Oversampling
result_xgbB <- list()  # Lista para guardar os resultados do XGBoost balanceado
result_comb <- list()  # Lista para guardar os resultados do Combinado balanceado

# Correr os modelos
for(i in 1:10){
  # Criar folds para os diferentes datasets de treino e teste
  test_idx1 <- folds1[[i]]  # Dataset sem Oversampling
  test_idx2 <- folds2[[i]]  # Dataset com Oversampling
  test_idx3 <- folds3[[i]]  # Dataset com Oversampling e Undersampling (Oversampling Balanceado)
  
  # Organizar os dados para o modelo XGBoost sem Oversampling
  train_xgbN <- dadosM[-test_idx1,]  # Dados de treino para o XGBoost
  test_xgbN  <- dadosM[test_idx1,]   # Dados de teste para o XGBoost
  train_labelsN <- ifelse(train_xgbN$Sets == 3, 1, 0)  # Número de sets por jogo nos dados de treino
  test_labelsN  <- ifelse(test_xgbN$Sets == 3, 1, 0)   # Número de sets por jogo nos dados de teste
  train_xgbN <- data.matrix(train_xgbN[, -2])  # Remover a coluna Sets e transformar em matriz
  test_xgbN  <- data.matrix(test_xgbN[, -2])   # Remover a coluna Sets e transformar em matriz
  dtrainN <- xgb.DMatrix(data = train_xgbN, label = train_labelsN)  # Criar um objeto específico para o XGBoost
  dtestN  <- xgb.DMatrix(data = test_xgbN, label = test_labelsN)    # Criar um objeto específico para o XGBoost
  negative_cases <- sum(train_labelsN == FALSE)  # Número de jogos com 2 sets
  positive_cases <- sum(train_labelsN == TRUE)   # Número de jogos com 3 sets
  
  # Organizar os dados para o modelo XGBoost com Oversampling balanceado
  train_xgbB <- dadosB[-test_idx3,]  # Dados de treino para o XGBoost balanceado
  test_xgbB  <- dadosB[test_idx3,]   # Dados de teste para o XGBost balanceado
  train_labelsB <- ifelse(train_xgbB$Sets == 3, 1, 0)  # Número de sets por jogo nos dados de treino
  test_labelsB  <- ifelse(test_xgbB$Sets == 3, 1, 0)   # Número de sets por jogo nos dados de teste
  train_xgbB <- data.matrix(train_xgbB[, -2])  # Remover a coluna Sets e transformar em matriz
  test_xgbB  <- data.matrix(test_xgbB[, -2])   # Remover a coluna Sets e transformar em matriz
  dtrainB <- xgb.DMatrix(data = train_xgbB, label = train_labelsB)  # Criar um objeto específico para o XGBoost
  dtestB  <- xgb.DMatrix(data = test_xgbB, label = test_labelsB)    # Criar um objeto específico para o XGBoost
  
  # Organizar os dados para o modelo KNN com Oversampling com o dobro de "3"
  train_knnD <- dadosD[-test_idx2, ]  # Dados de treino para o KNN
  test_knnD  <- dadosD[test_idx2, ]   # Dados de teste para o KNN
  train_knnD_scaled <- scale(train_knnD[, -2])  # Normalizar os dados de treino
  means_knnD <- attr(train_knnD_scaled, "scaled:center")  # Média das variáveis nos dados de treino
  sds_knnD   <- attr(train_knnD_scaled, "scaled:scale")   # Desvio padrão das variáveis nos dados de treino
  test_knnD_scaled  <- scale(test_knnD[, -2], center = means_knnD, scale = sds_knnD)  # Normalizar os dados de teste
  
  # Organizar os dados para o modelo KNN com Oversampling balanceado
  train_knnB <- dadosB[-test_idx3, ]  # Dados de treino para o KNN balanceado
  test_knnB  <- dadosB[test_idx3, ]   # Dados de teste para o KNN balanceado
  train_knnB_scaled <- scale(train_knnB[, -2])  # Normalizar os dados de treino
  means_knnB <- attr(train_knnB_scaled, "scaled:center")  # Média das variáveis nos dados de treino
  sds_knnB   <- attr(train_knnB_scaled, "scaled:scale")   # Desvio padrão das variáveis nos dados de treino
  test_knnB_scaled  <- scale(test_knnB[, -2], center = means_knnB, scale = sds_knnB)  # Normalizar os dados de teste

  # Organizar os dados para o modelo de Regressão Logística com Oversampling balanceado
  train_rl <- data.frame(Sets = train_labelsB, train_xgbB)  # Transformar os dados de matriz para dataframe
  test_rl  <- data.frame(test_xgbB)                         # Transformar os dados de matriz para dataframe
  
  # Modelo KNN com Oversampling com o dobro de "3"
  knnD <- knn(train = train_knnD_scaled, test = test_knnD_scaled, cl = train_knnD$Sets, k = 5)  # Modelo
  accuracy <- mean(knnD == test_knnD$Sets)                 # Acurácia
  print(table(Predicted = knnD, Actual = test_knnD$Sets))  # Matriz de confusão
  print(accuracy)
  result_knnD[[i]] <- knnD           # Adicionar os resultados a uma lista
  
  # Modelo KNN com Oversampling balanceado
  knnB <- knn(train = train_knnB_scaled, test = test_knnB_scaled, cl = train_knnB$Sets, k = 5, prob = TRUE)  # Modelo
  accuracy <- mean(knnB == test_knnB$Sets)                 # Acurácia
  print(table(Predicted = knnB, Actual = test_knnB$Sets))  # Matriz de confusão
  print(accuracy)
  result_knnB[[i]] <- knnB           # Adicionar os resultados a uma lista
  
  # Modelo XGBoost sem Oversampling
  xgbN <- xgboost(data = dtrainN, max.depth = 6, nround = 10, objective = "binary:logistic",
                   scale_pos_weight = negative_cases*0.9/(positive_cases), verbose = 0)  # Modelo
  pred <- predict(xgbN, dtestN)            # Probabilidade do jogo ter 3 sets
  prediction <- ifelse(pred >= 0.5, 3, 2)  # Previsões
  tlN <- ifelse(test_labelsN == 1, 3, 2)
  print(table(Predicted = prediction, Actual = tlN))  # Matriz de confusão
  accuracy <- mean(prediction == tlN)                 # Acurácia
  print(accuracy)
  result_xgbN[[i]] <- prediction     # Adicionar os resultados a uma lista
  
  # Modelo XGBoost Com Oversampling balanceado
  xgbB <- xgboost(data = dtrainB, max.depth = 6, nround = 10, objective = "binary:logistic",
                       scale_pos_weight = 1, verbose = 0)  # Modelo
  predB <- predict(xgbB, dtestB)             # Probabilidade do jogo ter 3 sets
  predictionB <- ifelse(predB >= 0.5, 3, 2)  # Previsões
  tlB <- ifelse(test_labelsB == 1, 3, 2)
  print(table(Predicted = predictionB, Actual = tlB))  # Matriz de confusão
  accuracy <- mean(predictionB == tlB)                 # Acurácia
  print(accuracy)
  result_xgbB[[i]] <- predictionB    # Adicionar os resultados a uma lista
  
  # Modelo Combinado com Oversampling balanceado
  knn_probs <- attr(knnB, "prob")  # Probabilidade da classe vencedora
  classes_knn <- levels(train_knnB$Sets)  # Classes "2" e "3"
  probs_knn_mat <- matrix(NA, nrow = length(knnB), ncol = length(classes_knn))  # Matriz para as probabilidades
  colnames(probs_knn_mat) <- classes_knn  # Nomear as colunas da matriz com as classes
  
  for(j in 1:length(knnB)) {   # Para cada previsão:
    win_class <- knnB[j]       # Classe prevista
    win_prob <- knn_probs[j]   # Probabilidade da classe vencedora
    lose_prob <- 1 - win_prob  # Probabilidade da classe perdedora
    probs_knn_mat[j, win_class] <- win_prob  # Atualizar a matriz com a classe vencedora e sua probabilidade
    probs_knn_mat[j, setdiff(classes_knn, win_class)] <- lose_prob}  # Com a classe perdedora e sua probabilidade
  
  # Combina a probabilidade da classe 3 do KNN balanceado com XGB balanceado
  prob_comb <- (probs_knn_mat[, "3"] + predB) / 2  # Probabilidade do jogo ter 3 sets
  pred_comb <- ifelse(prob_comb >= 0.5, 3, 2)      # Previsões
  print(table(Predicted = pred_comb, Actual = test_knnB$Sets))  # Matriz de confusão
  accuracy <- mean(pred_comb == test_knnB$Sets)                 # Acurácia        
  print(accuracy)
  result_comb[[i]] <- pred_comb      # Adicionar os resultados a uma lista
  
  # Modelo de Regressão Logística com Oversampling balanceado
  model_rl <- glm(Sets ~ ., data = train_rl, family = "binomial")  # Modelo
  print(VIF(model_rl))  # Medir a multicolinearidade das variáveis independentes
  pred <- predict(model_rl, newdata = test_rl, type = "response")  # Probabilidade do jogo ter 3 sets
  pred_rl <- ifelse(pred >= 0.5, 3, 2)             # Previsões
  print(table(Predicted = pred_rl, Actual = tlB))  # Matriz de confusão
  accuracy <- mean(pred_rl == tlB)                 # Acurácia        
  print(accuracy)
  result_rl[[i]] <- pred_rl      # Adicionar os resultados a uma lista
}


##### Evaluation #####

#--- KNN com Oversampling com o dobro de "3" ---#

# Inicializar vetores para armazenar todas as previsões e valores reais
predicoes <- unlist(result_knnD)                                    # Previsões
reais <- unlist(lapply(folds2, function(idx) dadosD[idx, "Sets"]))  # Valores reais

# Criar uma matriz de confusão global
confusionMatrix(predicoes, reais)

#--- KNN com Oversampling balanceado ---#

# Inicializar vetores para armazenar todas as previsões e valores reais
predicoes <- unlist(result_knnB)                                    # Previsões
reais <- unlist(lapply(folds3, function(idx) dadosB[idx, "Sets"]))  # Valores reais

# Criar uma matriz de confusão global
confusionMatrix(predicoes, reais)

#--- XGBoost sem Oversampling ---#

# Inicializar vetores para armazenar todas as previsões e valores reais
predicoes <- unlist(result_xgbN)                                    # Previsões
predicoes <- as.factor(predicoes)                                   # Transformar em fator
reais <- unlist(lapply(folds1, function(idx) dadosM[idx, "Sets"]))  # Valores reais

# Criar uma matriz de confusão global
confusionMatrix(predicoes, reais)

#--- XGBoost com Oversampling balanceado ---#

# Inicializar vetores para armazenar todas as previsões e valores reais
predicoes <- unlist(result_xgbB)                                    # Previsões
predicoes <- as.factor(predicoes)                                   # Transformar em fator
reais <- unlist(lapply(folds3, function(idx) dadosB[idx, "Sets"]))  # Valores reais

# Criar uma matriz de confusão global
confusionMatrix(predicoes, reais)

#--- Combinado com Oversampling balanceado ---#

# Inicializar vetores para armazenar todas as previsões e valores reais
predicoes <- unlist(result_comb)                                    # Previsões
predicoes <- as.factor(predicoes)                                   # Transformar em fator
reais <- unlist(lapply(folds3, function(idx) dadosB[idx, "Sets"]))  # Valores reais

# Criar uma matriz de confusão global
confusionMatrix(predicoes, reais)

#--- Regressão Logística com Oversampling balanceado ---#

# Inicializar vetores para armazenar todas as previsões e valores reais
predicoes <- unlist(result_rl)                                      # Previsões
predicoes <- as.factor(predicoes)                                   # Transformar em fator
reais <- unlist(lapply(folds3, function(idx) dadosB[idx, "Sets"]))  # Valores reais

# Criar uma matriz de confusão global
confusionMatrix(predicoes, reais)
