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

# ------------------------------------------- Tabela temperatura-sensação por mes ------------------------------------------- #
tempSensaMes <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri$horario$mon[cepagri$horario$year == ano])) {
    mediaTemperatura <- mean(cepagri$temp[cepagri$horario$mon == mes & cepagri$horario$year == ano])
    mediaSensacao <- mean(cepagri$sensa[cepagri$horario$mon == mes & cepagri$horario$year == ano])
    tempSensaMes <- rbind(tempSensaMes,data.frame(mediaTemperatura,mediaSensacao,row.names = paste(mes+1,"/",ano + 1900,sep="")))
  }
}
# -------------------------------------------- Tabela ano 2017 ------------------------------------------------ #
mediasAnoSeca <- data.frame()
nomeLinhas <- c()
for (mes in unique(cepagri$horario$mon[cepagri$horario$year == 117])) {
  mediasAnoSeca <- rbind(mediasAnoSeca, cepagri[cepagri$horario$year == 117 & cepagri$horario$mon == mes,]$umid)
  nomeLinhas <- c(nomeLinhas, paste(mes+1, "/", 2017, sep = ""))
}
rownames(mediasAnoSeca) <- nomeLinhas; colnames(mediasAnoSeca) <- 1:ncol(mediasAnoSeca)

# ------------------------------------------- Tabela temperatura-umidade ------------------------------------------- # 

tempUmidadeMes <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri$horario$mon[cepagri$horario$year == ano])) {
    mediaTemperatura <- mean(cepagri$temp[cepagri$horario$mon == mes & cepagri$horario$year == ano])
    mediaUmidade <- mean(cepagri$umid[cepagri$horario$mon == mes & cepagri$horario$year == ano])
    tempUmidadeMes <- rbind(tempUmidadeMes,data.frame(mediaTemperatura,mediaUmidade,row.names = paste(mes+1,"/",ano + 1900,sep="")))
  }
}
