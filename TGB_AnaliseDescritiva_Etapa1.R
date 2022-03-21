#############################################
# Pacotes
#############################################

if(!require(chron)){install.packages("chron")}
library(chron)

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

if(!require(GGally)){install.packages("GGally")}
library(GGally)

### arrumar o fundo do plot
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library(RColorBrewer)

if(!require(cowplot)){install.packages("cowplot")}
library(cowplot)

if(!require(ggcorrplot)){install.packages("ggcorrplot")}
library(ggcorrplot)

if(!require(reshape)){install.packages("reshape")}
library(reshape)

### para o latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)

#############################################
# Importando os dados e tratando-os
#############################################

library(readr)
etapa1f <- read_delim("D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/etapa1f.txt", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)

ETAPA1 = etapa1f

# Tornando a vari?vel como "times"
ETAPA1$TempoEsp <- chron(times=ETAPA1$TempoEsp)  # Mudando o vetor tempo para leitura no R como objeto
ETAPA1$TempoEsp <- minutes(ETAPA1$TempoEsp)+hours(ETAPA1$TempoEsp)*60
head(ETAPA1)


#Algumas modifica??es no tipo de dados...
ETAPA1$Estacao <- factor(ETAPA1$Estacao, levels=c("verao", "outono", "inverno", "primavera"))
ETAPA1$Turno <- factor(ETAPA1$Turno, levels=c("manha", "tarde", "noite"))
ETAPA1$Data <- chron(as.character(ETAPA1$Data),format=c(dates="d/m/y"))
attach(ETAPA1)


#ETAPA1$MortePerc = ETAPA1$Morte/100

ETAPA1$MortePerc = (ETAPA1$NumMort+1)/(ETAPA1$TotalAves+1)

#ETAPA1$Morte_Tran = log(0.01+ETAPA1$MortePerc)


ETAPA1$logit_Morte =
  log(ETAPA1$MortePerc/(1-ETAPA1$MortePerc))

ETAPA1$log_TotalAves = log(ETAPA1$TotalAves)


### Fazer com os dados todos.

# set.seed(1633)
# divisao = sample(c("Treino","Teste"),
#                  prob= c(0.8,0.2),
#                  size = nrow(ETAPA1),
#                  replace = T)
# 
# dados_teste = ETAPA1[divisao == "Teste",]
# dados_treino = ETAPA1[divisao == "Treino",]


#############################################
# Univariada - Quantitativas
#############################################


##### Quantitativas

fun_MedidasDesc = function(vetor){
  
  medidas = 
    c(min(vetor),
      quantile(vetor,0.25),
      median(vetor),
      mean(vetor),
      quantile(vetor,0.75),
      max(vetor),
      sd(vetor),
      (sd(vetor)/mean(vetor)) )
  
  names(medidas) = 
    c("Min","Q1","Med","Media",
      "Q3","Max","Dp","CV")
  
  return(medidas)
}


### Descritivas no geral

round(fun_MedidasDesc(ETAPA1$TempoEsp),digits=3)

round(fun_MedidasDesc(ETAPA1$TempExt),digits=3)

round(fun_MedidasDesc(ETAPA1$URExt),digits=3)

round(fun_MedidasDesc(ETAPA1$MortePerc),digits=3)



round(fun_MedidasDesc(ETAPA1$NumMort),digits=3)
xtable(
  matrix(c(names(fun_MedidasDesc(ETAPA1$NumMort)),
           round(fun_MedidasDesc(ETAPA1$NumMort),digits=3)),
         byrow=T,ncol=8))

round(fun_MedidasDesc(ETAPA1$TotalAves),digits=3)
xtable(
  matrix(c(names(fun_MedidasDesc(ETAPA1$TotalAves)),
           round(fun_MedidasDesc(ETAPA1$TotalAves),digits=3)),
         byrow=T,ncol=8))



round(fun_MedidasDesc(ETAPA1$logit_Morte),digits=3)
xtable(
  matrix(c(names(fun_MedidasDesc(ETAPA1$logit_Morte)),
           round(fun_MedidasDesc(ETAPA1$logit_Morte),digits=3)),
         byrow=T,ncol=8))


brewer.pal(n = 4, name = "Dark2")[1]

box_TempoEsp <- 
  ETAPA1 %>% select(TempoEsp) %>%
  ggplot(aes(x="", y = TempoEsp)) +
  geom_boxplot(fill = brewer.pal(n = 4, name = "Dark2")[1], 
               color = "black") + 
  coord_flip() +
  
  scale_y_continuous(limits = c(min(ETAPA1$TempoEsp),
                                max(ETAPA1$TempoEsp)))+
  theme_classic() +
  xlab("") +
  labs(y = "Tempo de espera (min)", x = "")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))

hist_TempoEsp <- 
  ETAPA1 %>% select(TempoEsp) %>%
  ggplot() +
  geom_histogram(aes(x = TempoEsp, y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 30, 
                 fill = brewer.pal(n = 4, name = "Dark2")[1], 
                 color = "black") +
  labs(x = "", y = "Frequência relativa")+
  
  scale_x_continuous(limits = c(min(ETAPA1$TempoEsp),
                                max(ETAPA1$TempoEsp)))+
  
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))


f1 = 
cowplot::plot_grid(hist_TempoEsp, 
                   box_TempoEsp, 
                   ncol = 1, rel_heights = c(2, 1),
                   align = 'v', axis = 'lr')  







box_TempExt <- 
  ETAPA1 %>% select(TempExt) %>%
  ggplot(aes(x="", y = TempExt)) +
  geom_boxplot(fill = brewer.pal(n = 4, name = "Dark2")[2], 
               color = "black") + 
  coord_flip() +
  
  scale_y_continuous(limits = c(min(ETAPA1$TempExt),
                                max(ETAPA1$TempExt)))+
  
  theme_classic() +
  xlab("") +
  labs(y = "Temperatura externa (°C)", x = "")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))

hist_TempExt <- 
  ETAPA1 %>% select(TempExt) %>%
  ggplot() +
  geom_histogram(aes(x = TempExt, y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 0.9, 
                 fill = brewer.pal(n = 4, name = "Dark2")[2], 
                 color = "black") +
  
  scale_x_continuous(limits = c(min(ETAPA1$TempExt),
                                max(ETAPA1$TempExt)))+
  
  labs(x = "", y = "Frequência relativa")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))


f2 = 
cowplot::plot_grid(hist_TempExt, 
                   box_TempExt, 
                   ncol = 1, rel_heights = c(2, 1),
                   align = 'v', axis = 'lr') 


### Temperatura com Estacao 


box_URExt <- 
  ETAPA1 %>% select(URExt) %>%
  ggplot(aes(x="", y = URExt)) +
  geom_boxplot(fill = brewer.pal(n = 4, name = "Dark2")[3], 
               color = "black") + 
  coord_flip() +
  
  scale_y_continuous(limits = c(min(ETAPA1$URExt),
                                max(ETAPA1$URExt)))+
  
  theme_classic() +
  xlab("") +
  labs(y = "Umidade externa (%)", x = "")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))


hist_URExt <- 
  ETAPA1 %>% select(URExt) %>%
  ggplot() +
  geom_histogram(aes(x = URExt, y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 3, 
                 fill = brewer.pal(n = 4, name = "Dark2")[3], 
                 color = "black") +
  
  scale_x_continuous(limits = c(min(ETAPA1$URExt),
                                max(ETAPA1$URExt)))+
  
  labs(x = "", y = "Frequência relativa")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))

f3 = 
cowplot::plot_grid(hist_URExt, 
                   box_URExt, 
                   ncol = 1, rel_heights = c(2, 1),
                   align = 'v', axis = 'lr') 









box_Morte_Tran <- 
  ETAPA1 %>% select(logit_Morte) %>%
  ggplot(aes(x="", y = logit_Morte)) +
  geom_boxplot(fill = brewer.pal(n = 4, name = "Dark2")[4], 
               color = "black") + 
  coord_flip() +
  
  scale_y_continuous(limits = c(min(ETAPA1$logit_Morte),
                                max(ETAPA1$logit_Morte)))+
  
  theme_classic() +
  xlab("") +
  labs(y = "Morte transformada", x = "")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))


hist_Morte_Tran <- 
  ETAPA1 %>% select(logit_Morte) %>%
  ggplot() +
  geom_histogram(aes(x = logit_Morte, y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 0.1, 
                 fill = brewer.pal(n = 4, name = "Dark2")[4], 
                 color = "black") +
  
  scale_x_continuous(limits = c(min(ETAPA1$logit_Morte),
                                max(ETAPA1$logit_Morte)))+
  
  labs(x = "", y = "Frequência relativa")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))


f4 = 
cowplot::plot_grid(hist_Morte_Tran, 
                   box_Morte_Tran, 
                   ncol = 1, rel_heights = c(2, 1),
                   align = 'v', axis = 'lr') 



ggarrange(f1,f2,f3,f4,ncol=2,nrow=2)


ggsave(filename = "Desc_Quant_Uni.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")



#############################################
# Univariada - Qualitativas
#############################################

round(table(ETAPA1$Estacao)/sum(nrow(ETAPA1)),
      digits=3)

round(table(ETAPA1$Turno)/sum(nrow(ETAPA1)),
      digits=3)


#############################################
# Bivariadada - Quantitativos
#############################################

names(ETAPA1)

### Pensar em fazer pela estacao do ano.
### Cores - Elipse

ggcorrplot(cor(ETAPA1[,c("TempoEsp","TempExt","URExt","logit_Morte")],
               method = "spearman"), 
           hc.order = TRUE, 
           type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_bw,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab_size = 10, digits = 2,
           lab=T)


### Entre os dados quantitativos

### Morte e Estacao



Morte_Estacao_boxplot <- 
  ggplot(ETAPA1, aes(x=Estacao, y=logit_Morte, fill = Estacao)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Estação") + 
  ylab("Morte transformada") +
  ggtitle("Morte transformada\n pelas estações do dano")+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

Morte_Estacao_boxplot 




### Morte e Turno

Morte_Turno_boxplot <- 
  ggplot(ETAPA1, aes(x=Turno, y=logit_Morte, fill = Turno)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Turno") + 
  ylab("Morte transformada") +
  ggtitle("Morte transformada\npelos turnos")+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

Morte_Turno_boxplot


ggarrange(Morte_Estacao_boxplot,
          Morte_Turno_boxplot,
          labels = LETTERS[1:2],
          ncol=2)


ggsave(filename = "Desc_MorteTr_Box_TurnoEstacao.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")


### Morte, Turno e Estacao

Morte_Estacao_Turno_int = 
ggplot(ETAPA1, aes(x = Estacao, y = logit_Morte, 
                   colour = Turno, group = Turno)) +
  stat_summary(fun.y = mean, geom = "point",size=2.5) +
  stat_summary(fun.y = mean, geom = "line",size=1.2) +
  labs(title = "Interação entre Estação e Turno com\na média da morte transformada",
       x = "\nEstações",
       y = "Média da morte transformada\n",
       colour = "Turnos") +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  scale_colour_brewer(palette="Set1") +
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

Morte_Estacao_Turno_int


ggsave(filename = "Desc_MorteTr_Int_TurnoEstacao.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")




vetor=c()

for(i in 1:length(ETAPA1$NumMort)){
  if(ETAPA1$NumMort[i]>800){
    vetor = c(vetor,i)
  }
}

ETAPA1[vetor,5:11]




#############################################
# Bivariadada -  Estacao com tudo
#############################################

### Umidade

dados_estacao_umidade = melt(ETAPA1[,c(2,6)])

box_umidade_estacao <- 
  ggplot(data = dados_estacao_umidade) +
  
  geom_boxplot(mapping = aes(y = value,x=Estacao,fill=Estacao), 
               color = 'black',
               show.legend = T) +
  
  labs(x = "", y = "Umidade", 
       title = "",
       fill= "") +
  
  scale_fill_brewer(palette="Set1")+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

box_umidade_estacao




### TempExt

dados_estacao_temp = melt(ETAPA1[,c(2,5)])

box_estacao_temp <- 
  ggplot(data = dados_estacao_temp) +
  
  geom_boxplot(mapping = aes(y = value,x=Estacao,fill=Estacao), 
               color = 'black',
               show.legend = T) +
  
  labs(x = "", y = "Temperatura", 
       title = "",
       fill= "") +
  
  scale_fill_brewer(palette="Set1")+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

box_estacao_temp

ggarrange(box_estacao_temp,
          box_umidade_estacao,
          labels = c("A","B"),
          ncol=2)

ggsave(filename = "Desc_Estacao_Box_TempUR.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")


