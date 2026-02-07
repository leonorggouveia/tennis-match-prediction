
# Projeto Aplicado em Ciência de Dados I #

# Grupo 1, Integrantes (CDB1):
# Amanda Viapiana, Inês Machado, Leonor Gouveia, Sílvia Gentil e Tiago Woodger

## PARTE 1 ##

# Bibliotecas a utilizar
rm(list=ls())                                # Remover todas as variáveis do ambiente
x <- c("jsonlite", "ggplot2",                # Para ler os ficheiros em JSON e criar gráficos
       "dplyr", "tidyr",                     # Para manipular dados, dataframes
       "stringr", "lubridate", "tidyverse")  # Para manipular strings, datas e gráficos

# install.packages(x)  # Instalar as bibliotecas
lapply(x, library, character.only = TRUE)  # Carregar as bibliotecas

# Leitura dos ficheiros de dados
original <- stream_in(file("total.json"))  # Ficheiro com todos os dados
dados <- stream_in(file("games.json"))     # Ficheiro com os jogos na Alemanha


##### DATA UNDERSTANDING #####

# Lista de locais dos torneios na Alemanha
locations <- c("essen, germany", "luedenscheid, germany", "augsburg, germany", "dortmund, germany", 
               "nussloch, germany", "hambach, germany", "dresden, germany", "kamen, germany", "wetzlar, germany", 
               "kaltenkirchen, germany", "bad salzdetfurth, germany", "ludwigshafen, germany", "troisdorf, germany",
               "scheiwberdingen, germany", "friedberg, germany", "kenn, germany", "arnsberg-neheim, germany",
               "ueberlingen, germany", "kassel, germany", "weiden, germany", "lubeck, germany", "ulm, germany", 
               "oberhaching, germany", "aschaffenburg, germany", "monchengladbach, germany", "zell, germany", 
               "friesenheim, germany", "mannheim -neckarau, germany", "riemerling, germany", "villigen, germany", 
               "bad schussenried, germany", "saarlouis, germany", "stuggart, germany", "bamberg, germany",
               "esslingen, germany", "romerberg, germany", "ingolstadt, germany", "leun, germany", "sylt, germany", 
               "luebeck, germany", "lohr am main, germany", "forchheim, germany", "mettmann, germany", 
               "frankfurt am main, germany", "trie, germany", "magdeburg, germany", "neckarau, germany",
               "ettlingen, germany", "nurember, germany", "allershausen, germany", "neheim-husten, germany", 
               "eisenach, germany", "nuembrecht, germany", "oberweier, germany", "sundern, germany",
               "schwabisch-hall, germany", "alpirsbach, germany", "bochum, germany", "garmisch, germany",
               "furth, germany", "wetzlar, wetzlar", "karlsruhe, germany", "hong kong, germany", "trier, germany",
               "neumunster, germany", "wismar, germany", "lippstadt, germany", "bad saarow, germany",
               "leipzig, germany", "bad lippspringe, germany", "hamburen, germany", "celle, germany",
               "neu ulm, germany", "emden, germany", "montabaur, germany", "munster, germany", "bielefeld, germany", 
               "travemunde, germany", "waiblingen, germany", "duesseldorf, germany", "buchholz, germany",
               "neunkirchen, germany", "halle, germany", "hamburg, germany", "aachen, germany",
               "freudenstadt, germany", "braunchweig, germany", "koblenz, germany", "schwieberdingen, germany",
               "munich, germany", "stuttgart, germany", "eckental, germany", "ismaning, germany", "germany",
               "frankfurt, germany", "heilbronn, germany", "hannover, germany", "cologne, germany",
               "braunschweig, germany", "marburg, germany", "dusseldorf, germany", "meerbusch, germany",
               "pullach, germany", "leimen, germany", "berlin, germany", "kaarst, germany", "braunschweig",
               "wolfsburg, germany", "frankfurt, germany, germany", "nubloch, germany", "oberstaufen, germany")

locations  # Visualizar a lista dos locais

# Número de observações
nrow(original)                                  # Número total de observações
sum(tolower(original$Location) %in% locations)  # Número de observações na Alemanha
nrow(dados)                                     # Número de observações na Alemanha sem seeding
sum(tolower(original$Location) %in% locations) - nrow(dados)  # Número de jogos de seeding na Alemanha

# Nome de todos os jogadores no Top 500 da ATP
top500 <- distinct(original, original$PlayerName, original$ID)  # Pares distintos de Nome e Link do jogador 
top500 <- top500[-2]                                            # Eliminar a coluna "ID"
colnames(top500) <- "PlayerName"                                # Renomear a coluna de top500
nrow(top500)                                                    # Número de jogadores no Top 500 da ATP

# Quantidade e nome dos jogadores envolvidos em todos os jogos
outrosjog <- unique(original$Oponent)    # Lista com os nomes distintos dos oponentes
totaljog  <- top500$PlayerName           # Lista para todos os jogadores

for (name in outrosjog) {                # Fazer a travessia de todos os nomes do oponentes
  if (name %in% totaljog) {next}         # Se o nome já estiver na lista, passar para o próximo
  else {totaljog <- c(totaljog, name)}}  # Adicionar o nome que não foi encontrado à lista de nomes

totaljog          # Visualizar o nome dos jogadores
length(totaljog)  # Número total de jogadores

# Quantidade e nome dos jogadores envolvidos nos jogos da Alemanha
winners <- distinct(dados, dados$WID, dados$Winner)  # Pares distintos de Nome e ID dos vencedores
colnames(winners) <- c("ID", "Name")                 # Renomear as colunas de winners
losers  <- distinct(dados, dados$LID, dados$Loser)   # Pares distintos de Nome e ID dos perdedores
colnames(losers)  <- c("ID", "Name")                 # Renomear as colunas de losers

jogadores <- rbind(winners, losers)                     # Juntar os vencedores e perdedores
jogadores <- jogadores[-which(duplicated(jogadores)),]  # Remover os valores duplicados

jogadores        # Visualizar o nome dos jogadores na Alemanha
nrow(jogadores)  # Número de jogadores na Alemanha

# Quantidade e nome dos jogadores no Top 500 da ATP que jogaram na Alemanha
top500GER <- jogadores[jogadores$ID != "",]  # Lista dos jogadores no top 500 na Alemanha
seminfo   <- jogadores[jogadores$ID == "",]  # Lista dos jogadores que não estiveram no Top 500
seminfo   <- seminfo[-1]                     # Remover a coluna contendo os IDs

top500GER        # Visualizar o nome dos jogadores no Top 500
seminfo          # Visualizar o nome dos jogadores fora do Top 500
nrow(top500GER)  # Número de jogadores no Top 500
nrow(seminfo)    # Número de jogadores fora do Top 500


##### DATA PREPARATION #####

# Criar IDS para o restante dos jogadores
jogadores <- jogadores %>% mutate(IDNew = row_number())  # Criar uma nova coluna com o número da linha/observação
jogadores[jogadores$ID == "", "ID"] <- jogadores[jogadores$ID == "", "IDNew"]  # Substituir "" pelo número criado
jogadores <- jogadores[-3]   # Excluir a coluna "IDNew"

for (name in seminfo$Name){  # Travessia da lista com os jogadores sem "ID"
  dados[dados$Loser == name, "LID"] <- jogadores[jogadores$Name == name, "ID"]    # Substituir "" pela ID criada
  dados[dados$Winner == name, "WID"] <- jogadores[jogadores$Name == name, "ID"]}  # Substituir "" pela ID criada

# Filtrar os dados corretos para obter observações únicas
dados <- dados[-1]  # Remover as colunas com o id (padrão do mongodb)
dados <- dados[-which(duplicated(dados)), ]      # Remover duplicados
dados <- dados[-which(duplicated(dados[-15])),]  # Remover espelhos

# Alterações feitas à coluna "Date"
dados <- dados %>% separate(Date, into = c("StartDate", "EndDate"), sep = " - ")  # Separar a data em Início e Fim
dados$StartDate <- as.Date(dados$StartDate, format = "%Y.%m.%d")  # Mudar para o tipo Date
dados$EndDate   <- as.Date(dados$EndDate, format = "%Y.%m.%d")    # Mudar para o tipo Date
dados$Year      <- year(dados$StartDate)                          # Criar a variável "Year"

# Alterações feitas às colunas "WBirthYear" e "LBirthYear"
dados$WBirthYear <- as.numeric(dados$WBirthYear)      # Transformar os valores da coluna em numéricos
dados$LBirthYear <- as.numeric(dados$LBirthYear)      # Transformar os valores da coluna em numéricos
dados[is.na(dados$LBirthYear), "LBirthYear"] <- 1900  # Transformar NAs em 1900 (ano base para valores omissos)
dados[dados$LBirthYear == 1900, "LBirthYear"] <- NA   # Transformar os valores 1900 em NAs

# Padronizar os nomes dos países em "WBornCountry" e "LBornCountry"
dados[dados$WBornCountry == "USA", "WBornCountry"] <- "United States"  # Estados Unidos
dados[dados$LBornCountry == "USA", "LBornCountry"] <- "United States"  # Estados Unidos
dados[dados$WBornCountry == "Bosnia-Herzegovina", "WBornCountry"] <- "Bosnia and Herzegovina"  # Bósnia e Herzegovina
dados[dados$LBornCountry == "Bosnia-Herzegovina", "LBornCountry"] <- "Bosnia and Herzegovina"  # Bósnia e Herzegovina
dados[dados$WBornCountry == "Great Britain", "WBornCountry"] <- "United Kingdom"  # Reino Unido
dados[dados$LBornCountry == "Great Britain", "LBornCountry"] <- "United Kingdom"  # Reino unido
dados[dados$WBornCountry == "Netherlands", "WBornCountry"] <- "The Netherlands"   # Países-Baixos
dados[dados$LBornCountry == "Netherlands", "LBornCountry"] <- "The Netherlands"   # Países Baixos
dados[dados$WBornCountry == "Slovak Republic", "WBornCountry"] <- "Slovakia"      # Eslováquia
dados[dados$LBornCountry == "Slovak Republic", "LBornCountry"] <- "Slovakia"      # Eslováquia

# Alterações feitas às colunas "WDOB" e "LDOB"
dados$WDOB <- as.Date(dados$WDOB, format = "%d-%m-%Y")  # Mudar os valores da coluna para o tipo Date
dados$LDOB <- as.Date(dados$LDOB, format = "%d-%m-%Y")  # Mudar os valores da coluna para o tipo Date

# Alterações feitas às colunas "WHand" e "LHand"
dados[dados$WHand == "", "WHand"] <- NA  # Transformar os valores vazios em NAs
dados[dados$LHand == "", "LHand"] <- NA  # Transformar os valores vazios em NAs

# Alterações feitas às colunas "WBackHand" e "LBackHand"
dados[dados$WBackHand == "", "WBackHand"] <- NA  # Transformar os valores vazios em NAs
dados[dados$LBackHand == "", "LBackHand"] <- NA  # Transformar os valores vazios em NAs
dados$WBackHand <- gsub(" Backhand", "", dados$WBackHand)  # Remover " Backhand" das observações dos vencedores
dados$LBackHand <- gsub(" Backhand", "", dados$LBackHand)  # Remover " Backhand" das observações dos perdedores

# Alterações feitas às colunas "WHeight" e "LHeight"
dados$WHeight <- as.numeric(dados$WHeight)   # Transformar os valores da coluna em numéricos
dados$LHeight <- as.numeric(dados$LHeight)   # Transformar os valores da coluna em numéricos
dados[is.na(dados$WHeight), "WHeight"] <- 0  # Transformar NAs em valores nulos
dados[is.na(dados$LHeight), "LHeight"] <- 0  # Transformar NAs em valores nulos 
dados[dados$WHeight == 0, "WHeight"] <- NA   # Transformar os valores nulos em NAs
dados[dados$LHeight == 0, "LHeight"] <- NA   # Transformar os valores nulos em NAs

# Alterações feitas às colunas "WRank" e "LRank"
dados$WGameRank <- as.numeric(dados$WGameRank)  # Transformar em números
dados$LGameRank <- as.numeric(dados$LGameRank)  # Transformar em números

# Alterações feitas à coluna "Location"
dados[, "Location"] <- str_to_title(dados$Location)  # Capitalizar a primeira letra de todas as palavras

# Padronizar e renomear os locais dos jogos
dados[dados$Location == "Trie, Germany", "Location"] <- "Trier, Germany"                       # Nome errado
dados[dados$Location == "Braunchweig, Germany", "Location"] <- "Braunschweig, Germany"         # Nome errado
dados[dados$Location == "Hong Kong, Germany", "Location"] <- "Germany"                         # Nome errado
dados[dados$Location == "Stuggart, Germany", "Location"] <- "Stuttgart, Germany"               # Nome errado
dados[dados$Location == "Scheiwberdingen, Germany", "Location"] <- "Schwieberdingen, Germany"  # Nome errado
dados[dados$Location == "Villigen, Germany", "Location"] <- "Villingen, Germany"               # Nome errado
dados[dados$Location == "Nurember, Germany", "Location"] <- "Nuremberg, Germany"               # Nome errado
dados[dados$Location == "Mannheim -Neckarau", "Location"] <- "Mannheim"                    # Padronizar
dados[dados$Location == "Romerberg, Germany", "Location"] <- "Frankfurt Am Main, Germany"  # Padronizar
dados[dados$Location == "Riemerling, Germany", "Location"] <- "Munich, Germany"            # Padronizar
dados[dados$Location == "Wetzlar, Wetzlar", "Location"] <- "Wetzlar, Germany"              # Padronizar
dados[dados$Location == "Nubloch, Germany", "Location"] <- "Nussloch, Germany"             # Padronizar
dados[dados$Location == "Arnsberg-Neheim, Germany", "Location"] <- "Arnsberg, Germany"     # Padronizar
dados[dados$Location == "Neheim-Husten, Germany", "Location"] <- "Arnsberg, Germany"       # Padronizar
dados[dados$Location == "Oberweier, Germany", "Location"] <- "Friesenheim, Germany"        # Padronizar
dados[dados$Location == "Duesseldorf, Germany", "Location"] <- "Dusseldorf, Germany"       # Padronizar
dados[dados$Location == "Frankfurt, Germany", "Location"] <- "Frankfurt Am Main, Germany"  # Padronizar
dados[dados$Location == "Luebeck, Germany", "Location"] <- "Lubeck, Germany"               # Padronizar
dados[dados$Location == "Germany", "Location"] <- NA                                       # Padronizar
dados$Location <- gsub(", Germany", "", dados$Location)  # Remover ", Germany" do nome dos locais

# Alterações feitas à coluna "Tournament"
dados <- dados %>% mutate(Tournament = case_when(  # Alterar o nome dos torneios, juntar os jogos da Davis Cup
  Tournament == "FRG vs. ESP - INTER-ZONAL FINAL" ~ "DAVIS CUP", Tournament == "ARG V GER 1RD" ~ "DAVIS CUP",
  Tournament == "FRG vs. TCH - EUROPE ZONE B SF" ~ "DAVIS CUP", Tournament == "ROM V GER WGPO" ~ "DAVIS CUP",
  Tournament == "FRG vs. SWE - EUROPE ZONE A QF" ~ "DAVIS CUP", Tournament == "USA V GER QF" ~ "DAVIS CUP",
  Tournament == "	ESP V GER 1RD" ~ "DAVIS CUP", Tournament == "TCH V GER SF" ~ "DAVIS CUP",
  Tournament == "SWE V GER F" ~ "DAVIS CUP", Tournament == "ECU V GER WGPO" ~ "DAVIS CUP",
  Tournament == "DEN V GER QF" ~ "DAVIS CUP", Tournament == "YUG V GER SF" ~ "DAVIS CUP",
  Tournament == "BRA V GER 1RD" ~ "DAVIS CUP", Tournament == "USA V GER SF" ~ "DAVIS CUP",
  Tournament == "IND V GER 1RD" ~ "DAVIS CUP", Tournament == "NED V GER 1RD" ~ "DAVIS CUP",
  Tournament == "ARG V GER QF" ~ "DAVIS CUP", Tournament == "ITA V GER 1RD" ~ "DAVIS CUP",
  Tournament == "BEL V GER WGPO" ~ "DAVIS CUP", Tournament == "GER V NED 1RD" ~ "DAVIS CUP",
  Tournament == "TCH V GER QF" ~ "DAVIS CUP", Tournament == "GER v. CRO WG 1st RD" ~ "DAVIS CUP",
  Tournament == "AUS V GRM F" ~ "DAVIS CUP", Tournament == "GER v. FRA WG 1st RD" ~ "DAVIS CUP",
  Tournament == "ESP V GER QF" ~ "DAVIS CUP", Tournament == "GER v. THA WG PO" ~ "DAVIS CUP",
  Tournament == "RUS V GER SF" ~ "DAVIS CUP", Tournament == "GER v. ISR EAGI 2nd Rd." ~ "DAVIS CUP",
  Tournament == "CRO V GER 1RD" ~ "DAVIS CUP", Tournament == "GER v. VEN WG Q" ~ "DAVIS CUP",
  Tournament == "MEX V GER WGPO" ~ "DAVIS CUP", Tournament == "GER v ROM WG Rd 1" ~ "DAVIS CUP",
  Tournament == "SWE V GER QF" ~ "DAVIS CUP", Tournament == "GER v. ESP WG QF" ~ "DAVIS CUP",
  Tournament == "RSA V GER 1RD" ~ "DAVIS CUP", Tournament == "GER vs. KOR WG 1st RD" ~ "DAVIS CUP",
  Tournament == "RUS V GER 1RD" ~ "DAVIS CUP", Tournament == "GER vs. AUT WG 1st RD" ~ "DAVIS CUP",
  Tournament == "GER vs. RSA WG Play-off" ~ "DAVIS CUP", Tournament == "GER vs. BEL WG 1st RD" ~ "DAVIS CUP",
  Tournament == "GER vs. FRA WG QF" ~ "DAVIS CUP", Tournament == "GER vs. POL WG Play-Off" ~ "DAVIS CUP",
  Tournament == "GER vs. AUS WG Play-Off" ~ "DAVIS CUP", Tournament == "GER vs. CZE WG 1st RD" ~ "DAVIS CUP",
  Tournament == "GER vs. ARG 1st RD" ~ "DAVIS CUP", Tournament == "GER vs. FRA WG 1st RD" ~ "DAVIS CUP",
  Tournament == "GER vs. BRA WG Play-Off" ~ "DAVIS CUP", Tournament == "GER vs. ESP WG 1st RD" ~ "DAVIS CUP",
  Tournament == "ESP V GER 1RD" ~ "DAVIS CUP", Tournament == "GER v. BLR WG PO" ~ "DAVIS CUP",
  Tournament == "GER vs. HUN DC Qualifier 1st Round" ~ "DAVIS CUP", Tournament == "Ulm" ~ "Neu Ulm",
  Tournament == "GER vs BLR DC Qualifier 1st Round" ~ "DAVIS CUP", Tournament == "Munich-2" ~ "Munich",
  Tournament == "Dusseldorf-2" ~ "Dusseldorf", Tournament == "ATP Masters 1000 Stuttgart" ~ "Stuttgart-2",
  Tournament == "Stuttgart" ~ "Stuttgart-1", TRUE ~ Tournament))  # Manter o nome dos outros torneios

# Alterações feitas à coluna "Score"
dados$Score <- gsub(", ", "", dados$Score)  # Remover as vírgulas com espaço

# Consertar valores errados na coluna "Score"
dados[dados$Score == "8-1036 62 62 46", "Score"] <- "8-10 36 62 62 46"  # Acrescentar espaço
dados[dados$Score == "46 10-863 64", "Score"] <- "46 10-8 63 64"        # Acrescentar espaço
dados[dados$Score == "61 14-1286", "Score"] <- "61 14-12 86"            # Acrescentar espaço
dados[dados$Score == "46 8-1046", "Score"] <- "46 8-10 46"              # Acrescentar espaço
dados[dados$Score == "", "Score"] <- "46 64 63"             # Placar errado
dados[dados$Score == "66 36 40", "Score"] <- "36 63 64"     # Placar errado
dados[dados$Score == "67 64 06 20", "Score"] <- "67 64 62"  # Placar errado
dados[dados$Score == "76", "Score"] <- "76 63"              # Placar errado
dados[dados$Score == "76 33 63", "Score"] <- "76 36 63"     # Placar errado
dados[dados$Score == "06 04", "Score"] <- "06 04 (RET)"        # Jogo inacabado
dados[dados$Score == "76 20", "Score"] <- "76 20 (RET)"        # Jogo inacabado
dados[dados$Score == "67 12", "Score"] <- "67 12 (RET)"        # Jogo inacabado
dados[dados$Score == "67 75 40", "Score"] <- "67 75 40 (RET)"  # Jogo inacabado
dados[dados$Score == "3", "Score"] <- "30 (RET)"               # Jogo inacabado

# Criar uma coluna com o número de sets jogados ("Sets")
dados[, "Sets"] <- str_count(dados$Score, " ") + 1  # Contar o número de sets de cada jogo
dados[grepl(")", dados$Score), "Sets"] <- dados[grepl(")", dados$Score), "Sets"] - 1  # Retirar o set extra
dados$Sets <- factor(dados$Sets)  # Transformar a variável "Sets" em fator

# Jogos terminados por outras razões (que não vitória de um dos participantes)
sum(grepl(")", dados$Score))    # Total de jogos
sum(grepl("W/O", dados$Score))  # Por walkovers
sum(grepl("RET", dados$Score))  # Por retirement
sum(grepl("DEF", dados$Score))  # Por defaults

# Criar uma coluna para os jogos inacabados
dados[, "NF"] <- as.numeric(grepl(")", dados$Score))

# Alterações feitas à coluna "Prize"
cambio <- read.csv("media_anual_cambio.csv")  # Ler o ficheiro com as cotações
dados <- merge(dados, cambio, by = "Year")    # Juntar as duas bases de dados de acordo com "Year"

dados[grepl("�", dados$Prize), "Prize"] <- round(as.numeric(      # Selecionar os prémios em euros
  gsub("[^0-9]", "", dados[grepl("�", dados$Prize), "Prize"])) *  # Remover o caractér estranho
    dados[grepl("�", dados$Prize), "Value"])                      # Mudar os valores de euros para dólares

dados$Prize <- as.numeric(gsub("[^0-9]", "", dados$Prize))  # Remover o caractér $ e transformar em números
dados <- dados[-30]                                         # Remover a coluna "Value"

# Adicionar informações sobre os torneios em "Prize"
dados$Prize[dados$Tournament == "DAVIS CUP"] <- 0                                           # Valor omisso
dados$Prize[dados$Tournament == "Munich" & (dados$Year == 1968 | dados$Year == 1969)] <- 0  # Valor omisso
dados$Prize[dados$Tournament == "Wolfsburg" & dados$Year == 1993] <- 25000                  # Valor omisso

# Não foram feitas alterações nas colunas "GameRound", "Ground", "WID", "LID", "Winner" e "Loser"

# Criar uma coluna com a idade do jogador no momento do jogo
dados[, "WAge"] <- floor((dados$WDOB %--% dados$StartDate) / years(1))  # Idade do vencedor
dados[, "LAge"] <- floor((dados$LDOB %--% dados$StartDate) / years(1))  # Idade do perdedor

# Idade dos jogadores sem DOB mas com BirthYear
dados[is.na(dados$WDOB), "WAge"] <- floor(dados[is.na(dados$WDOB), "Year"] - dados[is.na(dados$WDOB), "WBirthYear"])
dados[is.na(dados$LDOB), "LAge"] <- floor(dados[is.na(dados$LDOB), "Year"] - dados[is.na(dados$LDOB), "LBirthYear"])

# Remover observações erradas
dados <- dados[-which(dados$Loser == "Francesco Scarpa"),]                     # Jogador não era nascido
dados <- dados[-which(dados$Loser == "Florian Merkel" & dados$Year == 1975),]  # Jogador não era nascido
dados <- dados[-which(dados$LID == "w449" & dados$Year %in% c(1995, 1998, 1999, 2004)),]  # Jogador errado
dados <- dados[-which(dados$WID == "w449" & dados$Year < 2000),]                          # Jogador errado

# Remover os jogos até 5 sets e jogos inacabados
dados <- dados[-which(dados$Tournament %in% c("ATP Tour World Championship", "Cologne WCT")
                      & dados$GameRound == "Finals"),]  
dados <- dados[-which(dados$Tournament == "ATP Masters 1000 Hamburg" & dados$GameRound == "Finals" 
                      & dados$Year %in% c(1990, 1991, 1993:2006)),]
dados <- dados[-which(dados$Tournament == "Dusseldorf" & dados$GameRound == "Finals" 
                      & dados$Year %in% c(1975, 1976)),]
dados <- dados[-which(dados$Tournament == "Stuttgart-2" & dados$GameRound == "Finals" 
                      & dados$Year %in% c(1981, 1990:1994, 1996:2001)),]
dados <- dados[-which(dados$Tournament == "Stuttgart-1" & dados$GameRound == "Finals"
                      & dados$Year %in% c(1978, 1979, 1981, 1982, 1984, 1991:1996, 1999:2006)),]
dados <- dados[-which(dados$Tournament == "Hamburg" & dados$GameRound == "Finals" 
                      & dados$Year %in% c(1975, 1978:1981, 1983, 1984, 1986:1989)),]
dados <- dados[-which(dados$Tournament == "Hamburg" & dados$GameRound == "Semi-Finals" 
                      & dados$Year %in% c(1974, 1975, 1979:1983, 1986:1989)),]
dados <- dados[-which(dados$Tournament == "Munich" & dados$GameRound == "Finals"
                      & dados$Year %in% c(1975, 1978, 1982, 1984)),]
dados <- dados[-which(dados$Tournament == "DAVIS CUP" & dados$Year < 2018),]
dados <- dados[-which(dados$Tournament == "Grand Slam Cup" & dados$GameRound %in% c("Finals", "Semi-Finals")),]
dados <- dados[-which(dados$Tournament == "Dusseldorf" & dados$GameRound == "Semi-Finals" & dados$Year == 1974),]
dados <- dados[-which(dados$Tournament == "Hamburg" & dados$Year %in% c(1968:1973)),] 
dados <- dados[-which(dados$Tournament == "Munich" & dados$GameRound == "Semi-Finals" & dados$Year == 1975),]
dados <- dados[-which(dados$Tournament == "Munich" & dados$Year == 1971),]
dados <- dados[-which(dados$Tournament == "Berlin" & dados$Year %in% c(1968, 1973)),]
dados <- dados[-which(dados$Tournament == "Berlin" & dados$GameRound == "Semi-Finals" & dados$Year == 1979),]
dados <- dados[-which(dados$Tournament == "Munich WCT" & dados$GameRound == "Finals" & dados$Year == 1982),]
dados <- dados[-which(dados$Tournament == "ATP Masters 1000 Essen" & dados$GameRound == "Finals"),]
dados <- dados[-which(dados$NF == 1),]
dados <- dados[-29]  # Eliminar a coluna NF

# Gravar a base de dados em um ficheiro JSON para a parte 2 do projeto
#write_json(dados, "dados.json")
