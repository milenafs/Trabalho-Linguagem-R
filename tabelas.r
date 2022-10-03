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

# ------------------------------------------- Tabela de condições extremas de 2020 ------------------------------------------- # 
# 20 ou 21 de junho e acaba em 22 ou 23 de setembro.  inverno = julho agosto setembro
tabCondExtre <- data.frame(Temp=cepagri$temp[cepagri$temp > 35 & cepagri$horario$year == 120],Vento=cepagri$vento[cepagri$temp > 35 & cepagri$horario$year == 120],Umi=cepagri$umid[cepagri$temp > 35 & cepagri$horario$year == 120],Data=cepagri$horario[cepagri$temp > 35 & cepagri$horario$year == 120])
tabCondExtre <- rbind(data.frame(Temp=cepagri$temp[cepagri$temp < 5 & cepagri$horario$year == 120],Vento=cepagri$vento[cepagri$temp < 5 & cepagri$horario$year == 120],Umi=cepagri$umid[cepagri$temp < 5 & cepagri$horario$year == 120],Data=cepagri$horario[cepagri$temp < 5 & cepagri$horario$year == 120]))
tabCondExtre <- rbind(tabCondExtre ,data.frame(Temp=cepagri$temp[cepagri$vento>62 & cepagri$horario$year == 120],Vento=cepagri$vento[cepagri$vento>62 & cepagri$horario$year == 120],Umi=cepagri$umid[cepagri$vento>62 & cepagri$horario$year == 120],Data=cepagri$horario[cepagri$vento>62 & cepagri$horario$year == 120]))
tabCondExtre <- rbind(tabCondExtre ,data.frame(Temp=cepagri$temp[cepagri$umid<20 & cepagri$horario$year == 120],Vento=cepagri$vento[cepagri$umid<20 & cepagri$horario$year == 120],Umi=cepagri$umid[cepagri$umid<20 & cepagri$horario$year == 120],Data=cepagri$horario[cepagri$umid<20 & cepagri$horario$year == 120]))

# ------------------------------------------- Tabela com as temperaturas durante o inverno ------------------------------------------- # 

tabInverno <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  mediaTemperatura <- (mean(cepagri$temp[cepagri$horario$mon == 6 & cepagri$horario$year == ano])+      #
                         mean(cepagri$temp[cepagri$horario$mon == 7 & cepagri$horario$year == ano])+    # Média das temperaturas durante o inverno de cada ano
                         mean(cepagri$temp[cepagri$horario$mon == 8 & cepagri$horario$year == ano]))/3  #
  tabInverno <- rbind(tabInverno,data.frame(mediaTemperatura,ano=ano+1900))
}
tabInverno <- na.omit(tabInverno)
