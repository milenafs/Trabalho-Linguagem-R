nomeColunas <- c("horario", "temp", "vento", "umid", "sensa")
arquivo <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(arquivo, header = FALSE, sep = ";", col.names = nomeColunas)

# ------------------------------------- limpeza dos dados da tabela ----------------------------------------- #
cepagri <- na.omit(cepagri) # Remove os NA's do dataframe
cepagri$temp <- as.numeric(cepagri$temp) # converte as temperaturas para numeric
cepagri$horario <- strptime(cepagri$horario, "%d/%m/%Y-%H:%M") # converte as strings para "datetime"
cepagri <- cepagri[cepagri$sensa < 40 && cepagri$umid > 15 && cepagri$umid < 95 && cepagri$vento < 75, ] # delimita os valores para algo mais aceitável e dentro do real
# ----------------------------------------------------------------------------------------------------------- #

summary(cepagri)
rm(cepagri)

# Ideias de tabelas
# 1. correlação vento-sensação térmica
# 2. correlação temperatura-umidade
# 3. 

# Ideias de gráficos
# 1. climatograma de um ano só
# 2.
# 3.
# 4.
# 5.


# ------------------------------------ Tabela médias por ano *** ------------------------------------------------ #
mediasPorAno <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  mediaTemp <- mean(cepagri$temp[cepagri$horario$year == ano])
  mediaVento <- mean(cepagri$vento[cepagri$horario$year == ano])
  mediaUmid <- mean(cepagri$umid[cepagri$horario$year == ano])
  mediaSensa <- mean(cepagri$sensa[cepagri$horario$year == ano])
  mediasPorAno <-rbind(mediasPorAno, data.frame(mediaTemp, mediaVento, mediaUmid, mediaSensa, row.names = ano + 1900))
}
rm(mediaTemp, mediaSensa, mediaUmid, mediaVento)
# ----------------------------------------------------------------------------------------------------------- #

# --------------------------- Gráfico média temperatura durante os anos ----------------------------------- #
grafTemp <- ggplot(mediasPorAno, aes(x = rownames(mediasPorAno), y = mediasPorAno$mediaTemp, group = 1))
grafTemp <- grafTemp + geom_point()
grafTemp <- grafTemp + geom_line()
grafTemp <- grafTemp + xlab("Anos") + ylab("Temperatura") + ggtitle("Temperatura média/Ano")
# --------------------------------------------------------------------------------------------------------- #

diaMaisQuente <- cepagri$horario[cepagri$temp == 38.1]
