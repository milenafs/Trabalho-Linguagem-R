# --------------------------- Gráfico das condições climáticas extremas de 2020 ----------------------------------- #

grafCondExtre <- ggplot(tabCondExtre, aes(x = Data, group = 1))
grafCondExtre <- grafCondExtre + geom_point(aes(y = Temp,colour="Temperatura"),size = 0.5, group=1)
grafCondExtre <- grafCondExtre + geom_point(aes(y = Vento,colour="Vento"),size = 0.5, group=1)
grafCondExtre <- grafCondExtre + geom_point(aes(y = Umi,colour="Umidade"),size = 0.5, group=1)

grafCondExtre <- grafCondExtre + geom_line(aes(y = Temp,colour="Temperatura"))
grafCondExtre <- grafCondExtre + geom_line(aes(y = Vento,colour="Vento"))
grafCondExtre <- grafCondExtre + geom_line(aes(y = Umi,colour="Umidade"))+theme_minimal();
grafCondExtre <- grafCondExtre + xlab("Meses") + ylab("Valores(ºC % km/h)") + labs(colour="Legenda",title="Análise de Temperatura, Vento e Umidade de 2020")
grafCondExtre

# --------------------------- Gráfico média temperatura durante os anos ----------------------------------- #

grafTemp <- ggplot(mediasPorMes, aes(x = 1:nrow(mediasPorMes), group = 1,  weight = Umidade))
grafTemp <- grafTemp + geom_point(color=rgb(255, 0, 0, maxColorValue = 255), aes(y = Temp, group = 1))
grafTemp <- grafTemp + geom_line(color=rgb(255, 0, 0, maxColorValue = 255), aes(y = Temp, group = 1))
grafTemp <- grafTemp + theme_minimal()
grafTemp <- grafTemp + xlab("Meses") + ylab("Temperatura") + ggtitle("Temperatura média por mês")

#----------------------------------Gráfico - correlação temperatura-sensação térmica mês--------------------------------------#

grafTempSensa <- ggplot(tempSensaMes, aes(x = 1:nrow(tempSensaMes), group = 1))
grafTempSensa <- grafTempSensa + geom_point(aes(y = mediaTemperatura, colour = mediaTemperatura), alpha = 1, group = 1)
grafTempSensa <- grafTempSensa + geom_point(aes(y = mediaSensacao, colour = mediaSensacao), alpha = 0.3, group = 1)

grafTempSensa <- grafTempSensa + scale_color_continuous(name =  "Média Temperatura", low = "blue", high = "red")
grafTempSensa <- grafTempSensa + xlab("Meses") + ylab("Temperatura") + ggtitle("Correlação Temperatura e Sensação Térmica")
grafTempSensa

#------------------------------Gráfico preenchimento inverno-----------------------------------
# 20 ou 21 de junho e acaba em 22 ou 23 de setembro.  inverno = julho agosto setembro

grafInverno <- ggplot(tabInverno, aes(x = ano)) + geom_density(aes(y = ..count..),color="darkblue", fill="darkblue",alpha=0.25) # crfia um gráfico de densidade
grafInverno <- grafInverno + xlab("Anos") + ylab("Nível da Média Témica") + ggtitle("Comparação da Média dos Invernos desde 2015 até 2021")
grafInverno

#------------------------------gráfico de condições climáticas extremas--------------------------------#
#condições extremas: temp > 35 < 5, vento > 62 umidade < 10%

grafCondExtre <- ggplot(tabCondExtre, aes(x = Data, group = 1))
grafCondExtre <- grafCondExtre + geom_point(aes(y = Temp,colour="Temperatura"),size = 0.5, group=1)
grafCondExtre <- grafCondExtre + geom_point(aes(y = Vento,colour="Vento"),size = 0.5, group=1)
grafCondExtre <- grafCondExtre + geom_point(aes(y = Umi,colour="Umidade"),size = 0.5, group=1)

grafCondExtre <- grafCondExtre + geom_line(aes(y = Temp,colour="Temperatura"))
grafCondExtre <- grafCondExtre + geom_line(aes(y = Vento,colour="Vento"))
grafCondExtre <- grafCondExtre + geom_line(aes(y = Umi,colour="Umidade"))+theme_minimal();
grafCondExtre <- grafCondExtre + xlab("Meses") + ylab("Valores(ºC % km/h)") + labs(colour="Legenda",title="Análise de Temperatura, Vento e Umidade de 2020")
grafCondExtre
  
#----------------------------------Gráfico - correlação temperatura-umidade mês--------------------------------------

grafTempUmid <- ggplot(tempUmidadeMes) +  
  scale_color_continuous(name = "Umidade",low = "blue", high = "red") + 
  geom_line(aes(x = 1:nrow(tempUmidadeMes), y = mediaUmidade, colour = mediaUmidade), alpha = 0.7) + 
  geom_col(aes(x = 1:nrow(tempUmidadeMes), y = mediaTemperatura, fill = mediaTemperatura)) +
  xlab("Meses") + ylab("Temperatura e Umidade") + ggtitle("Correlação Temperatura e Umidade") +
  guides(fill = guide_legend(title = "Temperatura"))
grafTempUmid

