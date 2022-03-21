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
# Importando os dados
#############################################




library(readr)
ETAPA2GERAL <- read_delim("ETAPA2GERAL.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(ETAPA2,2,pMiss)



names(ETAPA2GERAL)

ETAPA2 = ETAPA2GERAL[,c(4:8,10,11,13,14,16:21)]


# Tornando a variavel como "times"
ETAPA2$Tempototal <- chron(times=ETAPA2$Tempototal)  # Mudando o vetor tempo para leitura no R como objeto
ETAPA2$Tempototal <- minutes(ETAPA2$Tempototal)+hours(ETAPA2$Tempototal)*60
head(ETAPA2)

ETAPA2$TempoEspera <- chron(times=ETAPA2$TempoEspera)  # Mudando o vetor tempo para leitura no R como objeto
ETAPA2$TempoEspera <- minutes(ETAPA2$TempoEspera)+hours(ETAPA2$TempoEspera)*60
head(ETAPA2)

# Qualitativas
ETAPA2$Turno <- factor(ETAPA2$Turno, levels=c("manha", "tarde", "noite"))

head(ETAPA2GERAL)[,1:5]
head(ETAPA2GERAL)[,21]


#Ajustes


ETAPA2$Dif_TR = abs(ETAPA2$TRInicial- ETAPA2$TRFinal)

hist(ETAPA2$Dif_TR)

ETAPA2$MortePerc = ETAPA2$NumMort/ETAPA2$TotalAves

ETAPA2$LogitMort2 =
  log(ETAPA2$MortePerc/(1-ETAPA2$MortePerc))



#############################################
# Univariada - Quantitativas Descritivas
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

names(ETAPA2)

round(fun_MedidasDesc(ETAPA2$Tempototal),digits=3)
round(fun_MedidasDesc(ETAPA2$TempoEspera),digits=3)



round(fun_MedidasDesc(ETAPA2$TInt),digits=3)
round(fun_MedidasDesc(ETAPA2$URInt),digits=3)

round(fun_MedidasDesc(ETAPA2$TExt),digits=3)
round(fun_MedidasDesc(ETAPA2$URExt),digits=3)


round(fun_MedidasDesc(ETAPA2$TRFinal),digits=3)

round(fun_MedidasDesc(ETAPA2$Morte),digits=3)
round(fun_MedidasDesc(ETAPA2$NumMort),digits=3)
round(fun_MedidasDesc(ETAPA2$LogitMort),digits=3)


head(ETAPA2$NumMort)

head(ETAPA2$NumMort / ETAPA2$TotalAves)

head(ETAPA2$LogitMort2)
head(ETAPA2$LogitMort)



round(fun_MedidasDesc(ETAPA2$NumMort),digits=3)
xtable(
  matrix(c(names(fun_MedidasDesc(ETAPA2$NumMort)),
           round(fun_MedidasDesc(ETAPA2$NumMort),digits=3)),
         byrow=T,ncol=8))

round(fun_MedidasDesc(ETAPA2$TotalAves),digits=3)
xtable(
  matrix(c(names(fun_MedidasDesc(ETAPA2$TotalAves)),
           round(fun_MedidasDesc(ETAPA2$TotalAves),digits=3)),
         byrow=T,ncol=8))





brewer.pal(n = 4, name = "Dark2")[1]


#############################################
# Univariada - Quantitativas Descritivas
#############################################


names(ETAPA2)

indice_col_quanti =
  c(match("Tempototal",names(ETAPA2)),
    match("TempoEspera",names(ETAPA2)),
    match("TInt",names(ETAPA2)),
    match("URInt",names(ETAPA2)),
    match("TExt",names(ETAPA2)),
    match("URExt",names(ETAPA2)),
    match("TotalAves",names(ETAPA2)),
    match("TempoViagem",names(ETAPA2)),
    match("Dif_TR",names(ETAPA2)),
    match("LogitMort2",names(ETAPA2)),
    match("Distancia",names(ETAPA2)))


legenda_quanti=
  c("Tempo Total (min)",
    "Tempo de Espera (min)",
    "Temperatura Interna (°C)",
    "Umidade Interna (%)",
    "Temperatura Externa (°C)",
    "Umidade Externa (%)",
    "Total de aves",
    "Tempo de Viagem (min)",
    "Diferença Temp. Retal (°C)",
    "Logit da Morte Percentual",
    "Distância em KM")

cores_quanti = 
  c(brewer.pal(n = 7, name = "Dark2"),
    brewer.pal(n = 4, name = "Accent"))

lista_fig_quanti=
  list()


for(i in 1:length(indice_col_quanti)){
  
  vetor = unlist(ETAPA2[,indice_col_quanti[i]])
  
  bw <- (2 * IQR(vetor))/length(vetor)^(1/3)
  
  box <- 
    ggplot(data=data.frame(vetor),
           aes(x="", y = vetor)) +
    geom_boxplot(fill = cores_quanti[i], 
                 color = "black") + 
    coord_flip() +
    scale_y_continuous(limits = c(min(vetor),
                                  max(vetor)))+
    theme_classic() +
    xlab("") +
    labs(y = legenda_quanti[i], x = "")+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14, 
                              family ="serif"))
  
  
  hist <- 
    ggplot(data=data.frame(vetor)) +
    geom_histogram(aes(x = vetor, y = (..count..)/sum(..count..)),
                   position = "identity", binwidth = bw, 
                   fill = cores_quanti[i], 
                   color = "black") +
    labs(x = "", y = "Frequência relativa")+
    
    scale_x_continuous(limits = c(min(vetor),
                                  max(vetor)))+
    
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14, 
                              family ="serif"))
  
  lista_fig_quanti[[i]]=
    cowplot::plot_grid(hist, 
                     box, 
                     ncol = 1, rel_heights = c(2, 1),
                     align = 'v', axis = 'lr')  
  
}

legenda_quanti

ggarrange(lista_fig_quanti[[2]],
          lista_fig_quanti[[1]],
          lista_fig_quanti[[3]],
          lista_fig_quanti[[4]],
          lista_fig_quanti[[5]],
          lista_fig_quanti[[6]],
          lista_fig_quanti[[9]],
          lista_fig_quanti[[10]],
          lista_fig_quanti[[11]])

ggsave(filename = "ETAPA2_Desc_Quant_Uni.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 25.5, 
       units = "cm")


# indice_col_quanti =
#   c(match("Tempototal",names(ETAPA2)),
#     match("TempoEspera",names(ETAPA2)),
#     match("TInt",names(ETAPA2)),
#     match("URInt",names(ETAPA2)),
#     match("TExt",names(ETAPA2)),
#     match("URExt",names(ETAPA2)),
#     match("TotalAves",names(ETAPA2)),
#     match("TempoViagem",names(ETAPA2)),
#     match("Dif_TR",names(ETAPA2)),
#     match("LogitMort2",names(ETAPA2)),
#     match("Distancia",names(ETAPA2)))


### Dois momentos de coleta
### Duas populacoes talvez?


### Distancia --> A __ B
### TempoViagem --> A TIME B







#############################################
# Univariada - Qualitativas
#############################################

round(table(ETAPA2$Turno))

round(table(ETAPA2$Turno)/sum(nrow(ETAPA2)),
      digits=3)

#############################################
# Bivariada - Quantitativas
############################################

indice_col_quanti

names(ETAPA2[,indice_col_quanti])

plot(ETAPA2$Distancia,
     ETAPA2$TempoViagem)

### Elipse


ggcorrplot(cor(ETAPA2[,names(ETAPA2[,indice_col_quanti[-8] ])],
               method = "spearman"), 
           hc.order = TRUE, 
           type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_bw,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab_size = 5, digits = 2,
           lab=T)



Morte_Turno_boxplot <- 
  ggplot(ETAPA2, aes(x=Turno, y=LogitMort2, fill = Turno)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Turno") + 
  ylab("Logit do percentual de mortes") +
  ggtitle("Logit da morte\npelos turnos")+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

Morte_Turno_boxplot

ggsave(filename = "ETAPA2_Desc_MorteTr_Box_Turno.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 19.75,
       height = 12.5,
       units = "cm")
