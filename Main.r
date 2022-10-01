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

cor(cepagri[, 2:5]) # correlação dos valores da cepagri

# Ideias de tabelas
# 1. correlação temperatura-sensação térmica
# 2. correlação temperatura-umidade
# 3. umidades do ano de seca
# 4(bonus). tabela média por mês

# Ideias de gráficos
# 1. gráfico preenchimento inverno
# 2. grafico de ponto com uma reta normal dia por dia umidade ano de seca
# 3. grafico degradê de cor temperatura-sensação
# 4. linha + coluna temperatura-umidade por mês
# 5. gráfico de condições climáticas extremas

# ------------------------------------ Tabela médias por ano ------------------------------------------------ #
mediasPorAno <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  mediasPorAno <- rbind(mediasPorAno, apply(cepagri[cepagri$horario$year == ano, 2:5], 2, mean))
}
rownames(mediasPorAno) <- unique(cepagri$horario$year) + 1900; colnames(mediasPorAno) <- c("Temp", "Vento", "Umidade", "Sensacao")
# ----------------------------------------------------------------------------------------------------------- #

# ------------------------------------ Tabela médias por mês ------------------------------------------------ #
mediasPorMes <- data.frame()
nomeLinhas <- c()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri[cepagri$horario$year == ano,]$horario$mon)) {
    mediasPorMes <- rbind(mediasPorMes, apply(cepagri[cepagri$horario$year == ano & cepagri[cepagri$horario$year == ano,]$horario$mon == mes, 2:5], 2, mean))
    nomeLinhas <- c(nomeLinhas, paste(mes+1, "/", ano + 1900, sep = ""))
  }
}
colnames(mediasPorMes) <- c("Temp", "Vento", "Umidade", "Sensacao"); rownames(mediasPorMes) <- nomeLinhas
# ----------------------------------------------------------------------------------------------------------- #

# ------------------------------------------- tabela temperatura-sensação por mes ------------------------------------------- #
tempSensaMes <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri$horario$mon[cepagri$horario$year == ano])) {
    mediaTemperatura <- mean(cepagri$temp[cepagri$horario$mon == mes & cepagri$horario$year == ano])
    mediaSensacao <- mean(cepagri$sensa[cepagri$horario$mon == mes & cepagri$horario$year == ano])
    tempSensaMes <- rbind(tempSensaMes,data.frame(mediaTemperatura,mediaSensacao,row.names = paste(mes+1,"/",ano + 1900,sep="")))
  }
}
# -------------------------------------------- tabela ano 2014 ------------------------------------------------ #
mediasAnoSeca <- data.frame()
nomeLinhas <- c()
for (mes in unique(cepagri$horario$mon[cepagri$horario$year == 114])) {
  mediasAnoSeca <- rbind(mediasAnoSeca, quantile(cepagri[cepagri$horario$year == 114 & cepagri$horario$mon == mes,]$umid))
  nomeLinhas <- c(nomeLinhas, paste(mes+1, "/", 2014, sep = ""))
}
rownames(mediasAnoSeca) <- nomeLinhas; colnames(mediasAnoSeca) <- c("Minimo", "1 quartil", "Mediana", "3 quartil", "Máximo")

# --------------------------- Gráfico média temperatura durante os anos ----------------------------------- #
grafTemp <- ggplot(mediasPorMes, aes(x = rownames(mediasPorMes), group = 1,  weight = Umidade))
grafTemp <- grafTemp + geom_point(color=rgb(255, 0, 0, maxColorValue = 255), aes(y = Temp, group = 1))
grafTemp <- grafTemp + geom_line(color=rgb(255, 0, 0, maxColorValue = 255), aes(y = Temp, group = 1))
grafTemp <- grafTemp + theme_minimal()
grafTemp <- grafTemp + xlab("Meses") + ylab("Temperatura") + ggtitle("Temperatura média por mês")

grafUmidadeAnoSeca <- ggplot(mediasAnoSeca, aes(x = rownames(mediasAnoSeca), group=1))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_point(color=rgb(0, 0, 255, maxColorValue = 255), aes(y = mediasAnoSeca$Minimo))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_point(color=rgb(0, 0, 0, maxColorValue = 255), aes(y = mediasAnoSeca$`1 quartil`))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_point(color=rgb(0, 255, 0, maxColorValue = 255), aes(y = mediasAnoSeca$Mediana))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_point(color=rgb(0, 0, 0, maxColorValue = 255), aes(y = mediasAnoSeca$`3 quartil`))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_point(color=rgb(255, 0, 0, maxColorValue = 255), aes(y = mediasAnoSeca$Máximo))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_line(color=rgb(0, 0, 255, maxColorValue = 255), aes(y = mediasAnoSeca$Minimo))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_line(color=rgb(0, 0, 0, maxColorValue = 255), aes(y = mediasAnoSeca$`1 quartil`))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_line(color=rgb(0, 255, 0, maxColorValue = 255), aes(y = mediasAnoSeca$Mediana))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_line(color=rgb(0, 0, 0, maxColorValue = 255), aes(y = mediasAnoSeca$`3 quartil`))
grafUmidadeAnoSeca <- grafUmidadeAnoSeca + geom_line(color=rgb(255, 0, 0, maxColorValue = 255), aes(y = mediasAnoSeca$Máximo))
grafUmidadeAnoSeca
