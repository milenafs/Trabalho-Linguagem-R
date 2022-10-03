# ------------------------------------ Tabela médias por ano ------------------------------------------------ #
mediasPorAno <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  mediasPorAno <- rbind(mediasPorAno, apply(cepagri[cepagri$horario$year == ano, 2:5], 2, mean)) # Faz a média das colunas de cada ano
}
rownames(mediasPorAno) <- unique(cepagri$horario$year) + 1900; colnames(mediasPorAno) <- c("Temp", "Vento", "Umidade", "Sensacao")
# ----------------------------------------------------------------------------------------------------------- #

# ------------------------------------ Tabela médias por mês ------------------------------------------------ #
mediasPorMes <- data.frame()
nomeLinhas <- c()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri[cepagri$horario$year == ano,]$horario$mon)) {
    mediasPorMes <- rbind(mediasPorMes, apply(cepagri[cepagri$horario$year == ano & cepagri[cepagri$horario$year == ano,]$horario$mon == mes, 2:5], 2, mean)) # Faz a média das colunas de cada mês
    nomeLinhas <- c(nomeLinhas, paste(mes+1, "/", ano + 1900, sep = ""))
  }
}
colnames(mediasPorMes) <- c("Temp", "Vento", "Umidade", "Sensacao"); rownames(mediasPorMes) <- nomeLinhas # Nomeando as colunas e as linhas do dataframe
# ----------------------------------------------------------------------------------------------------------- #

# ------------------------------------------- Tabela temperatura-sensação por mes ------------------------------------------- #
tempSensaMes <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri$horario$mon[cepagri$horario$year == ano])) {
    mediaTemperatura <- mean(cepagri$temp[cepagri$horario$mon == mes & cepagri$horario$year == ano]) # Faz a média das temperaturas por mês 
    mediaSensacao <- mean(cepagri$sensa[cepagri$horario$mon == mes & cepagri$horario$year == ano]) # Faz a média da sensação térmica por mês 
    tempSensaMes <- rbind(tempSensaMes,data.frame(mediaTemperatura,mediaSensacao,row.names = paste(mes+1,"/",ano + 1900,sep=""))) # Junta os dois valores em um dataframe
  }
}

# ------------------------------------------- Tabela temperatura-umidade ------------------------------------------- # 

tempUmidadeMes <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  for (mes in unique(cepagri$horario$mon[cepagri$horario$year == ano])) {
    mediaTemperatura <- mean(cepagri$temp[cepagri$horario$mon == mes & cepagri$horario$year == ano]) # Faz a média das temperaturas por mês 
    mediaUmidade <- mean(cepagri$umid[cepagri$horario$mon == mes & cepagri$horario$year == ano]) # Faz a média da umidade por mês 
    tempUmidadeMes <- rbind(tempUmidadeMes,data.frame(mediaTemperatura,mediaUmidade,row.names = paste(mes+1,"/",ano + 1900,sep=""))) # Junta os dois valores em um dataframe
  }
}
