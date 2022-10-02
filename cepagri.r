nomeColunas <- c("horario", "temp", "vento", "umid", "sensa")
arquivo <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(arquivo, header = FALSE, sep = ";", col.names = nomeColunas)

# ------------------------------------- limpeza dos dados da tabela ----------------------------------------- #
cepagri <- na.omit(cepagri) # Remove os NA's do dataframe
cepagri$temp <- as.numeric(cepagri$temp) # converte as temperaturas para numeric
cepagri$horario <- strptime(cepagri$horario, "%d/%m/%Y-%H:%M") # converte as strings para "datetime"
cepagri <- cepagri[cepagri$sensa < 40 & cepagri$umid > 20 & cepagri$umid < 95 & cepagri$vento < 75 & cepagri$horario$year > 114 & cepagri$horario$year < 122, ] # delimita os valores para algo mais aceitável e dentro do real
# ----------------------------------------------------------------------------------------------------------- #

summary(cepagri)
rm(cepagri)

cor(cepagri[, 2:5]) # correlação dos valores da cepagri