nomeColunas <- c("horario", "temp", "vento", "umid", "sensa")
arquivo <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(arquivo, header = FALSE, sep = ";", col.names = nomeColunas)

cepagri <- na.omit(cepagri) # Remove os NA's do dataframe
summary(cepagri)

cepagri$temp <- as.double(cepagri$temp)
horarios[1]

plot(cepagri)
