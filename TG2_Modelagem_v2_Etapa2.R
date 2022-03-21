#############################################
# Pacores
#############################################

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(GGally)){install.packages("GGally")}
library(GGally)
if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library(RColorBrewer)
if(!require(cowplot)){install.packages("cowplot")}
library(cowplot)
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
library(ggcorrplot)
if(!require("qqplotr")){install.packages("qqplotr")}
library(qqplotr)


if(!require(rpart.plot)){install.packages("rpart.plot")}
library(rpart.plot)
if(!require(ggparty)){install.packages("ggparty")}
library(ggparty)
if(!require(partykit)){install.packages("partykit")}
library(partykit)
### arrumar o fundo do plot
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

if(!require(chron)){install.packages("chron")}
library(chron)

if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

if(!require(rBayesianOptimization)){install.packages("rBayesianOptimization")}
library(rBayesianOptimization)


if(!require(rpart)){install.packages("rpart")}
library(rpart)
### Random Forest
if(!require(ranger)){install.packages("ranger")}
library(ranger)
### XGBoost
if(!require(xgboost)){install.packages("xgboost")}
library(xgboost)
### MASS
if(!require(MASS)){install.packages("MASS")}
library(MASS)
if(!require(statmod)){install.packages("statmod")}
library(statmod)

### To Latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)

#############################################
# Importando os dados e tratando-os
#############################################


library(readr)
ETAPA2GERAL <- read_delim("ETAPA2GERAL.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

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


#Ajustes
ETAPA2$Dif_TR = abs(ETAPA2$TRFinal - ETAPA2$TRInicial)

ETAPA2$MortePerc = ETAPA2$NumMort/ETAPA2$TotalAves

ETAPA2$LogitMort2 =
  log(ETAPA2$MortePerc/(1-ETAPA2$MortePerc))

ETAPA2$log_TotalAves = log(ETAPA2$TotalAves)
ETAPA2$log_NumMorte = log(ETAPA2$NumMort)


#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(ETAPA2,2,pMiss)




# set.seed(1633)
# divisao = sample(c("Treino","Teste"),
#                  prob= c(0.8,0.2),
#                  size = nrow(ETAPA2),
#                  replace = T)
# 
# dados_teste = ETAPA2[divisao == "Teste",]
# dados_treino = ETAPA2[divisao == "Treino",]


dados_treino = ETAPA2


#############################################
# Funcoes Gerais
#############################################

generate_newdata <- function(data) {
  z <- data.frame(Temp = rep(seq(from = min(data$Temp),
                                 to = max(data$Temp),
                                 length.out = length(data$Temp))))
  
  z$x <- model.matrix(~ ., data = z)
  z
}



add_splitvar_breaks_index_new <- function(party_object, plot_data, round_digits = NULL) {
  
  plot_data$breaks_label <- I(rep(list(NA), length(party_object)))
  
  for (i in plot_data$id) {
    party_split <- party_object[[i]]$node$split
    party_node <- party_object[[i]]$node
    split_index <- party_split$index
    split_breaks <- party_split$breaks
    
    # check if node has a splitvar
    if (!is.null(party_split$varid)) {
      kids <- which(plot_data$parent == i)
      split_var <- names(party_object[[i]]$data)[party_split$varid]
      plot_data[i, "splitvar"] <- split_var
      
      # index
      # if only index provided, splitvar categorical. assign children according
      # to factor levels
      if (!is.null(split_index) & is.null(split_breaks)) {
        var_levels <- levels(party_object$data[,split_var])
        # iterate through index
        for (j in 1:length(split_index)) {
          if (is.na(split_index[j])) next
          # get kid index is pointing to
          kid <- kids[split_index[j]]
          # if first index  for kid, just assign according factor level
          if (is.na(plot_data$breaks_label[kid])) {
            plot_data[kid, "breaks_label"] <- var_levels[j]
            # else add factor level to present level(s)
          } else {
            plot_data[kid, "breaks_label"][[1]] <- list(c(plot_data[kid, "breaks_label"][[1]],
                                                          var_levels[j]))
          }
        }
      }
      
      # check whether intervals of continuous variable defined by breaks
      if (!is.null(split_breaks)) {
        # check if breaks are supposed to be rounded and apply if so
        if(!is.null(round_digits)) split_breaks <- round(split_breaks, round_digits)
        # if no index provided, intervals are supposed to be assigned
        # consecutively to kids. assign index accordingly.
        if (is.null(split_index)) split_index <- 1:(length(split_breaks) + 1)
        # iterate through index
        for (j in 1:length(split_index)) {
          kid <- kids[split_index[j]]
          # for first interval use -inf as lower bound
          if (j == 1) {
            # check whether more intervals lead to this kid. If so, don't use inequality signs
            if (split_index[j] %in% split_index[-j]) {
              split_interval <- paste0("(-Inf, ",
                                       split_breaks[j],
                                       ifelse(party_split$right == TRUE,
                                              "]",")"))
            } else {
              split_interval <- paste(ifelse(party_split$right == TRUE,
                                             "NA <= NA*","NA <  NA*"),
                                      #"\u2264","<"),
                                      split_breaks[1])
            }
            # for last interval use inf as upper bound
          } else if (j == length(split_index)) {
            # check whether more intervals lead to this kid. If so, don't use inequality signs
            if (split_index[j] %in% split_index[-j]) {
              split_interval <- paste0(ifelse(party_split$right == TRUE,
                                              "(","["),
                                       split_breaks[j - 1],
                                       ", Inf)")
            } else {
              split_interval <- paste(ifelse(party_split$right == TRUE,
                                             "NA >  NA*","NA >= NA*"),
                                      split_breaks[j - 1])
            }
            # else use break[j-1] for lower interval bound
          } else {
            split_interval <- paste0(ifelse(party_split$right == TRUE,
                                            "(","["),
                                     split_breaks[j - 1],", ",
                                     split_breaks[j],
                                     ifelse(party_split$right == TRUE,
                                            "]",")"))
          }
          
          if (is.na(plot_data$breaks_label[kid])) {
            plot_data[kid, "breaks_label"] <- split_interval
          }
          else {
            # plot_data[kid, "breaks_label"][[1]] <- list(c(plot_data[kid, "breaks_label"][[1]],
            #                                               split_interval))
            plot_data[kid, "breaks_label"][[1]] <- paste(plot_data[kid, "breaks_label"][[1]],
                                                         split_interval, sep = " | ")
          }
        }
        
      }
    }
  }
  return(plot_data["breaks_label"])
}






#############################################
# MOB - Modelo 1
#############################################

##### Ajustando o modelo

mob1 = glmtree(NumMort ~ 
                 TempoEspera|
                 Turno+
                 Tempototal+
                 TExt+URExt+TInt+URInt+
                 Dif_TR+
                 Distancia,
               data = dados_treino,
               family = poisson(link = "log"),
               offset = log_TotalAves,
               alpha = 0.05)



#####
# Analise
#####


xtable(summary(mob1)[[1]])
xtable(summary(mob1)[[2]]) 

AIC(mob1[[2]])
AIC(mob1[[3]])

BIC(mob1[[2]])
BIC(mob1[[3]])


#####
# Arvore
#####

rounded_labels_MOB_1 <- 
  add_splitvar_breaks_index_new(party_object = mob1,
                                plot_data = ggparty:::get_plot_data(mob1))

g_MOB_1=
  ggparty(mob1,
          terminal_space = 0,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta1 = "$node$info$coefficients[2]")) +
  
  geom_edge(size = 1) +
  
  geom_edge_label(size=6)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 26, col = "black", fontface = "bold"),
                                   list(size = 24),
                                   list(size = 20, col="black")),
                  ids = "inner") +
  
  
  geom_node_label(# map content to label for each line
    line_list = list(
      aes(label = paste("beta[0] == ", round(intercept, 4))),
      aes(label = paste("beta[1] == ",round(beta1, 4)))
    ),
    # set graphical parameters for each line in same order
    line_gpar = list(list(size = 18, parse = T),
                     list(size = 18, parse = T)),
    ids = "terminal",
    # nudge labels towards bottom so that edge labels have enough space
    # alternatively use shift argument of edge_label
    nudge_y = -.05) +
  # don't show legend for splitvar mapping to color since self-explanatory
  theme(legend.position = "none") +
  # html_documents seem to cut off a bit too much at the edges so set limits manually
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 1.1))+
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 4,
                  nudge_y = 0.05,
                  nudge_x = 0.005)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 20, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")

g_MOB_1

ggarrange(g_MOB_1)


ggsave(filename = "ETAPA2_Mod_MOB1.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")





#####
# Resíduos
#####

autoplot(mob1)

g_MOB_1_res=
  ggparty(mob1) +
  geom_edge(size = 1) +
  
  geom_edge_label(size=4.5)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 18, col = "black", fontface = "bold"),
                                   list(size = 17),
                                   list(size = 15, col="black")),
                  ids = "inner")+
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = fitted_values, y = residuals,
                                       fill = factor(id)),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nValores ajustados"),ylab("Resíduos\n"),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none"),
                        geom_hline(yintercept = 0,
                                   col="red",
                                   alpha=0.5)),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 4,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")
  

ggarrange(g_MOB_1_res)

ggsave(filename = "ETAPA2_Mod_MOB1_res.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")


#####
# Xy
#####

autoplot(mob1)

g_MOB_1_x_y=
  ggparty(mob1) +
  geom_edge(size = 1) +
  
  geom_edge_label(size=5.5)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 18, col = "black", fontface = "bold"),
                                   list(size = 17),
                                   list(size = 15, col="black")),
                  ids = "inner")+
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = TempoEspera, y = NumMort),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nTempo de espera (min)"),ylab("Número de Aves mortas\n"),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 4,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(g_MOB_1_x_y)



ggsave(filename = "ETAPA2_Mod_MOB1_xy.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")




#####
# Curvas
#####

coef_mob1 = coef(mob1)
coef_mob1

fun_curvas_mob1 = function(i,x){
  return(exp(coef_mob1[i,1] + coef_mob1[i,2]*x)*
           mean(dados_treino$TotalAves))
}


summary(dados_treino$TempoEspera)

grafico_curvas_mob1 =
  ggplot(data.frame(x = c(1, 400 )),
         aes(x = x)) +
  stat_function(fun = fun_curvas_mob1, args = list(i=1),
                aes(colour = rownames(coef_mob1)[1]),
                size = 1.5) +
  stat_function(fun = fun_curvas_mob1, args = list(i=2),
                aes(colour = rownames(coef_mob1)[2]),
                size = 1.5) +
  scale_x_continuous(name = "Tempo de espera (min)",
                     breaks = scales::pretty_breaks(n = 7))+
  scale_y_continuous(name = "Aves mortas",
                     breaks = scales::pretty_breaks(n = 7)) +
  theme_bw()+
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14,
                            family ="serif"),
        legend.position = c(.2,.8),
        legend.text = element_text(size=14))+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Nó")

grafico_curvas_mob1

ggsave(filename = "ETAPA2_Mod_MOB1_curvas.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")






#############################################
# MOB - Modelo 2
#############################################

##### Ajustando o modelo

mob2 = glmtree(NumMort ~ 
                 TempoEspera+
                 TInt+URInt|
                 Turno+
                 Tempototal+
                 TExt+URExt+
                 Dif_TR+
                 Distancia,
               data = dados_treino,
               family = poisson(link = "log"),
               offset = log_TotalAves,
               alpha = 0.05)


plot(mob2)


#####
# Analise
#####

summary(mob2)


xtable(summary(mob2)[[1]])
xtable(summary(mob2)[[2]]) 

AIC(mob2[[2]])
AIC(mob2[[3]])

BIC(mob2[[2]])
BIC(mob2[[3]])


#####
# Arvore
#####

rounded_labels_MOB_2 <- 
  add_splitvar_breaks_index_new(party_object = mob2,
                                plot_data = ggparty:::get_plot_data(mob2))

g_MOB_2=
  ggparty(mob2,
          terminal_space = 0,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta1 = "$node$info$coefficients[2]",
                          beta2 = "$node$info$coefficients[3]",
                          beta3 = "$node$info$coefficients[4]")) +
  
  geom_edge(size = 1) +
  
  geom_edge_label(size=6)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 26, col = "black", fontface = "bold"),
                                   list(size = 24),
                                   list(size = 20, col="black")),
                  ids = "inner")+
  
  
  geom_node_label(# map content to label for each line
    line_list = list(
      aes(label = paste("beta[0] == ", round(intercept, 4))),
      aes(label = paste("beta[1] == ",round(beta1, 4))),
      aes(label = paste("beta[2] == ",round(beta2, 4))),
      aes(label = paste("beta[3] == ",round(beta3, 4)))
    ),
    # set graphical parameters for each line in same order
    line_gpar = list(list(size = 14, parse = T),
                     list(size = 14, parse = T),
                     list(size = 14, parse = T),
                     list(size = 14, parse = T)),
    ids = "terminal",
    # nudge labels towards bottom so that edge labels have enough space
    # alternatively use shift argument of edge_label
    nudge_y = -.05) +
  # don't show legend for splitvar mapping to color since self-explanatory
  theme(legend.position = "none") +
  # html_documents seem to cut off a bit too much at the edges so set limits manually
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.2, 1.2))+
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3.5,
                  nudge_y = 0.13,
                  nudge_x = 0.005)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 16, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")

g_MOB_2

ggarrange(g_MOB_2)

ggsave(filename = "ETAPA2_Mod_MOB2.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")





#####
# Resíduos
#####

autoplot(mob1)

g_MOB_2_res=
  ggparty(mob2) +
  geom_edge(size = 1) +
  
  geom_edge_label(size=5.5)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 18, col = "black", fontface = "bold"),
                                   list(size = 17),
                                   list(size = 15, col="black")),
                  ids = "inner")+
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = fitted_values, y = residuals,
                                       fill = factor(id)),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nValores ajustados"),ylab("Resíduos\n"),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none"),
                        geom_hline(yintercept = 0,
                                   col="red",
                                   alpha=0.5)),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 4,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(g_MOB_2_res)

ggsave(filename = "ETAPA2_Mod_MOB2_res.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")


#####
# Xy
#####

autoplot(mob1)

names(ETAPA2)





g_MOB_2_x_y=
  ggparty(mob2,terminal_space = 0.9) +
  geom_edge(size = 1) +
  
  geom_edge_label(size=4)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 10, col="black")),
                  ids = "inner")+
  
  geom_node_plot(gglist=list(geom_blank(),
                             ylab("Número de aves mortas"),
                             theme_minimal(base_size = 1),
                             theme(text = element_text(size = 20, 
                                                       family ="serif"),
                                   legend.position = "none")),
                 shared_axis_labels = T)+
  
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = TempoEspera, y = NumMort),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nTempo de espera (min)"),ylab(""),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 height = 0.33) +
  

  geom_node_plot(gglist = 
                   list(geom_point(aes(x = TInt, y = NumMort),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("Temp. Int. (°C)"),ylab(""),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 height = 0.33,
                 nudge_y =-0.3) +
  
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = URInt, y = NumMort),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("Umid. Int. (%)"),ylab(""),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 height = 0.33,
                 nudge_y =-0.6) +
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3.6,
                  nudge_y = 0.01,
                  nudge_x = 0.06)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")





g_MOB_2_x_y

ggarrange(g_MOB_2_x_y)

ggsave(filename = "ETAPA2_Mod_MOB2_xy.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")




###############################
# CART - Modelo 3
###############################

### 1.Incorporar o log_total como covariavel
### 2.Trabalhar com o percentual

matrix(c(TotalAves,NumMort),ncol=2,
       byrow=F)


CART_3 = rpart(matrix(c(TotalAves,NumMort),
                      ncol=2,
                      byrow=F) ~ 
                 TempoEspera+
                 Turno+
                 Tempototal+
                 TExt+URExt+TInt+URInt+
                 Dif_TR+
                 Distancia,
               data = dados_treino, method = "poisson",
               control = rpart.control(cp=0.01))



# CART_3 = rpart(NumMort ~ 
#                  TempoEsp+TempExt+URExt+Estacao+Turno+TotalAves,
#                data = dados_treino, method = "poisson",
#                control = rpart.control(cp=0.01))

plot(as.party(CART_3))
# nao rodar se cp = 0.0001

rpart.plot(CART_3)

melhorCp = CART_3$cptable[which.min(CART_3$cptable[,"xerror"]),"CP"]
melhorCp



plot(as.party(prune(CART_3,cp=melhorCp)))

##### Plotando 

# Separando o banco pelo noIs finais
CART_3_obs_por_no = 
  split(dados_treino,
        predict(as.party(prune(CART_3,cp=melhorCp)), 
                type = "node"))

names(CART_3_obs_por_no)

# calculando as descritivas nos noIs finais
CART_3_desc = list()

for(i in 1:length(names(CART_3_obs_por_no))){
  CART_3_desc[[i]] =
    summarise(CART_3_obs_por_no[[i]],
              Q1 = quantile(NumMort,0.25),
              Q2 = median(NumMort),
              Media = mean(NumMort),
              Q3 = quantile(NumMort,0.75),
              Variancia = sd(NumMort)) 
}

##guardando os valores das descritivas
#para entrar na arvore

names(CART_3_obs_por_no)

CART_3_desc_vetor = list()

for(i in 1:length(CART_3_desc[[1]]) ){
  
  CART_3_desc_vetor[[i]] =
    c(1,2, #nois 1,2
      CART_3_desc[[1]][1,i],
      4, #no 4
      CART_3_desc[[2]][1,i],
      CART_3_desc[[3]][1,i],
      7,
      CART_3_desc[[4]][1,i],
      CART_3_desc[[5]][1,i])
  
}


CART_3_Q1 = unlist(CART_3_desc_vetor[[1]])
CART_3_Q2 = unlist(CART_3_desc_vetor[[2]])
CART_3_Media = round(unlist(CART_3_desc_vetor[[3]]),digits=2)
CART_3_Q3 = unlist(CART_3_desc_vetor[[4]])
CART_3_Variancia = round(unlist(CART_3_desc_vetor[[5]]),digits=2)

dados_CART_3 = ggparty(as.party(prune(CART_3,cp=melhorCp)))
dados_CART_3$data = 
  cbind(dados_CART_3$data, 
        CART_3_Q1,
        CART_3_Q2,
        CART_3_Media,
        CART_3_Q3,
        CART_3_Variancia)


g_CART3 =
  dados_CART_3+
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 10, col = "black", fontface = "bold"),
                                   list(size = 12)),
                  ids = "inner") +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.1))+
  
  xlab("Número de aves mortas")+
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")\n \n",
                                     "Q1 = ", CART_3_Q1,"\n",
                                     "Q2 = ", CART_3_Q2,"\n",
                                     "Média = ", CART_3_Media,"\n",
                                     "Q3 = ", CART_3_Q3,"\n",
                                     "Sd = ", CART_3_Variancia)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 4,
                  nudge_y = -0.1,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


g_CART3

ggarrange(g_CART3)


ggsave(filename = "ETAPA2_Mod_CART3.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")





###############################
# CTREE - Modelo 1
###############################

### Minino 0.05 de alfa

CTREE_1 = ctree(NumMort ~ 
                  TempoEspera+
                  Turno+
                  Tempototal+
                  TExt+URExt+TInt+URInt+
                  Dif_TR+
                  Distancia+TotalAves,
                data= dados_treino,
                control = ctree_control(alpha=0.05))

plot(CTREE_1)


##### Plotando

# Separando o banco pelo noIs finais
CTREE_1_obs_por_no = 
  split(dados_treino,
        predict(CTREE_1, type = "node"))

names(CTREE_1_obs_por_no)

# calculando as descritivas nos noIs finais
CTREE_1_desc = list()

for(i in 1:length(names(CTREE_1_obs_por_no))){
  CTREE_1_desc[[i]] =
    summarise(CTREE_1_obs_por_no[[i]],
              Q1 = quantile(NumMort,0.25),
              Q2 = median(NumMort),
              Media = mean(NumMort),
              Q3 = quantile(NumMort,0.75),
              Variancia = sd(NumMort)) 
}

##guardando os valores das descritivas
#para entrar na arvore

names(CTREE_1_obs_por_no)

CTREE_1_desc_vetor = list()

for(i in 1:length(CTREE_1_desc[[1]]) ){
  
  CTREE_1_desc_vetor[[i]] =
    c(1, #nois 1
      CTREE_1_desc[[1]][1,i],
      CTREE_1_desc[[2]][1,i])
  
}


CTREE_1_Q1 = unlist(CTREE_1_desc_vetor[[1]])
CTREE_1_Q2 = unlist(CTREE_1_desc_vetor[[2]])
CTREE_1_Media = round(unlist(CTREE_1_desc_vetor[[3]]),digits=2)
CTREE_1_Q3 = unlist(CTREE_1_desc_vetor[[4]])
CTREE_1_Variancia = round(unlist(CTREE_1_desc_vetor[[5]]),digits=2)

dados_CTREE_1 = ggparty((CTREE_1))
dados_CTREE_1$data = 
  cbind(dados_CTREE_1$data, 
        CTREE_1_Q1,
        CTREE_1_Q2,
        CTREE_1_Media,
        CTREE_1_Q3,
        CTREE_1_Variancia)


g_CTREE_1 =
  dados_CTREE_1+
  geom_edge(size=1.2) +
  
  geom_edge_label(size=5.5)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 26, col = "black", fontface = "bold"),
                                   list(size = 24),
                                   list(size = 20, col="black")),
                  ids = "inner") +
  
  coord_cartesian(xlim = c(0.1, 0.9), ylim = c(0.55, 1.1))+
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")\n \n",
                                     "Q1 = ", CTREE_1_Q1,"\n",
                                     "Q2 = ", CTREE_1_Q2,"\n",
                                     "Média = ", CTREE_1_Media,"\n",
                                     "Q3 = ", CTREE_1_Q3,"\n",
                                     "Sd = ", CTREE_1_Variancia)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 5,
                  nudge_y = -0.05,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


g_CTREE_1

ggarrange(g_CTREE_1)


ggsave(filename = "ETAPA2_Mod_CTREE1.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")






###############################
# GLM 
###############################

glm_1 = glm(NumMort ~ TempoEspera+
              Turno+
              Tempototal+
              TExt+URExt+TInt+URInt+
              Dif_TR+
              Distancia,
            data= dados_treino,
            family = poisson(link = "log"),
            offset = log_TotalAves)


summary(glm_1)

xtable(summary(glm_1),digits=4)

diagnostico_linear = function(modelo){
  
  lista_retorno = list()
  
  predito = modelo$fitted.values
  residuo = resid(modelo,type="deviance")
  index = seq(1,length(predito))
  
  residuo_quantilico = qresiduals(modelo)
  
  # histograma =   
  #   ggplot(data = data.frame(residuo), aes(x=residuo)) + 
  #   theme_bw()+
  #   geom_histogram(aes(y=..density..),
  #                  color="black", 
  #                  fill= "SteelBlue")+
  #   xlab("") + 
  #   ylab("Contagem") +
  #   ggtitle("Histograma dos\nresíduos padronizados")+
  #   theme(text = element_text(size = 14, 
  #                             family ="serif"),
  #         plot.title = element_text(hjust = 0.5))+
  #   scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #   scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
  
  
  densidade =   
    ggplot(data = data.frame(residuo_quantilico), aes(x=residuo_quantilico)) + 
    theme_bw()+
    geom_density(size=1.5)+
    xlab("") + 
    ylab("Densidade") +
    ggtitle("Densidade dos\nresíduos quantílicos")+
    theme(text = element_text(size = 14, 
                              family ="serif"),
          plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
  
  
  graf_qqplot = 
    ggplot(data=data.frame(residuo_quantilico),
           aes(sample=residuo_quantilico))+
    stat_qq_band(alpha=0.5)+
    stat_qq_point(size=1.5,shape=1)+
    stat_qq_line(color="blue",linetype="dashed",
                 alpha=0.5)+
    theme_bw()+
    labs(title = "Normal QQPlot dos\nresíduos quantílicos",
         x = "Quantis teóricos",
         y = "Quantis amostrais") +
    theme( plot.title = element_text(hjust = 0.5),
           text = element_text(size = 14, 
                               family ="serif")) 
  
  disp_res_VS_pred = 
    ggplot(data = data.frame(residuo,
                             predito), 
           aes(x= predito, y= residuo)) + 
    geom_point(col = "black",size = 1.5) + theme_bw() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    xlab("Preditos") + 
    ylab("Resíduos deviance") +
    ggtitle("Valores preditos\ncontra os resíduos")+
    theme(text = element_text(size = 14, 
                              family ="serif"),
          plot.title = element_text(hjust = 0.5))
  
  
  disp_res_VS_index = 
    ggplot(data = data.frame(residuo,
                             index), 
           aes(x= index, y= residuo)) + 
    geom_point(col = "black",size = 1.5) + theme_bw() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    xlab("Índice") + 
    ylab("Resíduos deviance") +
    ggtitle("Valores preditos\ncontra índice")+
    theme(text = element_text(size = 14, 
                              family ="serif"),
          plot.title = element_text(hjust = 0.5))
  
  
  
  disp_res_VS_pred2 = 
    ggplot(data = data.frame(residuo,
                             predito), 
           aes(x= predito, y= residuo)) + 
    
    geom_smooth(method = "loess",
                size=1,se=T,
                level=0.7)+
    
    geom_point(col = "black",size = 1.5) + theme_bw() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    xlab("Preditos") + 
    ylab("Resíduos deviance") +
    ggtitle("Valores preditos\ncontra os resíduos")+
    theme(text = element_text(size = 14, 
                              family ="serif"),
          plot.title = element_text(hjust = 0.5))
  
  
  lista_retorno[[1]]=
    ggarrange(disp_res_VS_pred,disp_res_VS_index,
              graf_qqplot,labels = LETTERS[1:3])
  
  lista_retorno[[2]]=
    ggarrange(disp_res_VS_pred2)
  
  lista_retorno[[3]]=
    ggarrange(disp_res_VS_pred)
  
  return(lista_retorno)
  
}


graf_diag_glm1 = diagnostico_linear(glm_1)

graf_diag_glm1[[1]]

ggsave(filename = "ETAPA2_Mod_GLM1_Res.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")



glm_2=
step(glm_1, direction = "both", test = "F")

summary(glm_2)



glm_3 = glm(NumMort ~ TempoEspera+
              Turno+
              Tempototal+
              TExt+URExt+TInt+URInt+
              Dif_TR+
              Distancia,
            data= dados_treino,
            family = poisson(link = "log"),
            offset = log_TotalAves)
summary(glm_3)
