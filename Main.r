nomeColunas <- c("horario", "temp", "vento", "umid", "sensa")
arquivo <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(arquivo, header = FALSE, sep = ";", col.names = nomeColunas)

cepagri <- na.omit(cepagri) # Remove os NA's do dataframe
summary(cepagri)

cepagri$temp <- as.numeric(cepagri$temp)
cepagri$horario <- strptime(cepagri$horario, "%d/%m/%Y-%H:%M")

mediasPorAno <- c()
for (ano in unique(cepagri$horario$year)) {
  mediaTemp <- mean(cepagri$temp[cepagri$horario$year == ano])
  mediaVento <- mean(cepagri$vento[cepagri$horario$year == ano])
  mediaUmid <- mean(cepagri$umid[cepagri$horario$year == ano])
  mediaSensa <- mean(cepagri$sensa[cepagri$horario$year == ano])
  mediasPorAno <- rbind(mediasPorAno, c(mediaTemp, mediaVento, mediaUmid, mediaSensa))
}

dimnames(mediasPorAno) <- list(unique(cepagri$horario$year) + 1900, c("Temp", "Vento", "Umidade", "Sensacao"))

