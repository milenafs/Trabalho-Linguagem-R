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


#------------------------------gráfico preenchimento inverno-----------------------------------
# 20 ou 21 de junho e acaba em 22 ou 23 de setembro.  inverno = julho agosto setembro
tabInverno <- data.frame()
for (ano in unique(cepagri$horario$year)) {
  mediaTemperatura <- (mean(cepagri$temp[cepagri$horario$mon == 6 & cepagri$horario$year == ano])+
                         mean(cepagri$temp[cepagri$horario$mon == 7 & cepagri$horario$year == ano])+
                         mean(cepagri$temp[cepagri$horario$mon == 8 & cepagri$horario$year == ano]))/3
  tabInverno <- rbind(tabInverno,data.frame(mediaTemperatura,ano=ano+1900))
}
tabInverno <- na.omit(tabInverno)
grafInverno <- ggplot(tabInverno, aes(x = ano)) + geom_density(aes(y = ..count..),color="darkblue", fill="darkblue",alpha=0.25)
grafInverno <- grafInverno + xlab("Anos") + ylab("Nível da Média Témica") + ggtitle("Comparação da Média dos Invernos desde 2015 até 2021")
grafInverno

#------------------------------gráfico de condições climáticas extremas--------------------------------#
#condições extremas: temp > 35 < 5, vento > 62 umidade < 10%
rm(tabCondExtre)
tabCondExtre <- data.frame(Temp=cepagri$temp[cepagri$temp > 35 & cepagri$horario$year == 119],Vento=cepagri$vento[cepagri$temp > 35 & cepagri$horario$year == 119],Umi=cepagri$umid[cepagri$temp > 35 & cepagri$horario$year == 119],Data=cepagri$horario[cepagri$temp > 35 & cepagri$horario$year == 119])
tabCondExtre <- rbind(data.frame(Temp=cepagri$temp[cepagri$temp < 5 & cepagri$horario$year == 119],Vento=cepagri$vento[cepagri$temp < 5 & cepagri$horario$year == 119],Umi=cepagri$umid[cepagri$temp < 5 & cepagri$horario$year == 119],Data=cepagri$horario[cepagri$temp < 5 & cepagri$horario$year == 119]))
tabCondExtre <- rbind(tabCondExtre ,data.frame(Temp=cepagri$temp[cepagri$vento>62 & cepagri$horario$year == 119],Vento=cepagri$vento[cepagri$vento>62 & cepagri$horario$year == 119],Umi=cepagri$umid[cepagri$vento>62 & cepagri$horario$year == 119],Data=cepagri$horario[cepagri$vento>62 & cepagri$horario$year == 119]))
tabCondExtre <- rbind(tabCondExtre ,data.frame(Temp=cepagri$temp[cepagri$umid<20 & cepagri$horario$year == 119],Vento=cepagri$vento[cepagri$umid<20 & cepagri$horario$year == 119],Umi=cepagri$umid[cepagri$umid<20 & cepagri$horario$year == 119],Data=cepagri$horario[cepagri$umid<20 & cepagri$horario$year == 119]))

grafCondExtre <- ggplot(tabCondExtre, aes(x = Data, group = 1))
grafCondExtre <- grafCondExtre + geom_point(aes(y = Temp),size = 0.5, color= "darkgreen", group=1)
grafCondExtre <- grafCondExtre + geom_point(aes(y = Vento),size = 0.5, color= "darkblue", group=1)
grafCondExtre <- grafCondExtre + geom_point(aes(y = Umi),size = 0.5, color= "darkred", group=1)

grafCondExtre <- grafCondExtre + geom_line(aes(y = Temp), color= "darkgreen")
grafCondExtre <- grafCondExtre + geom_line(aes(y = Vento), color= "blue")
grafCondExtre <- grafCondExtre + geom_line(aes(y = Umi), color= "darkred")+theme_minimal();
#grafCondExtre <- grafCondExtre + scale_color_continuous(low = "blue", high = "red")
grafCondExtre <- grafCondExtre + xlab("Meses") + ylab("Valores(ºC % km/h)") + ggtitle("Análise de Temperatura, Vento e Umidade de 2019")
grafCondExtre
  

#----------------------------------Gráfico - correlação temperatura-umidade mês--------------------------------------

grafTempUmid <- ggplot(tempUmidadeMes) +  
  scale_color_continuous(name = "Umidade",low = "blue", high = "red") + 
  geom_line(aes(x = 1:nrow(tempUmidadeMes), y = mediaUmidade, colour = mediaUmidade), alpha = 0.7) + 
  geom_col(aes(x = 1:nrow(tempUmidadeMes), y = mediaTemperatura, fill = mediaTemperatura)) +
  xlab("Meses") + ylab("Temperatura e Umidade") + ggtitle("Correlação Temperatura e Umidade") +
  guides(fill = guide_legend(title = "Temperatura")) 


