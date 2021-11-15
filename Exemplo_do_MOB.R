###############################
# Pacotes
###############################
### MOB
if(!require(partykit)){install.packages("partykit")}
library(partykit)
### GGplot para arvore
if(!require(ggparty)){install.packages("ggparty")}
library(ggparty)
### arrumar o fundo do plot
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
### banco de dados
if(!require(datasets)){install.packages("datasets")}
if(!require(AER)){install.packages("AER")}
library(AER)
### Manipulacao
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
### Chutar a distribuicao de Y
if(!require(gamlss)){install.packages("gamlss")}
library(gamlss)

###############################
# Airquality - 0.05
###############################

### Obtendo os dados e removando as linhas
## com NA
dados = datasets::airquality
dados2 = na.omit(dados)

### Ajustando o MOB com alpha de 0.05
mob_ozonio_005 = 
  glmtree(Ozone ~ Temp | Wind + Solar.R + Month + Day,
          data = dados2,
          family = Gamma(link="log"),
          alpha = 0.05)

plot(mob_ozonio_005)


###################################
# Grafico da arvore

### Obtendo os coeficientes
gg_mob_ozonio_005 = 
  ggparty(mob_ozonio_005,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta = "$node$info$coefficients[2]"))

### Obtendo o exponencial dos coeficientes
gg_mob_ozonio_005$data$beta_exp = 
  exp(gg_mob_ozonio_005$data$beta)

### Funcao para obter os valores preditos do
## conjunto de treino. Isso serve para fazer a curva
## do modelo parametrico.
generate_newdata <- function(data) {
  z <- data.frame(Temp = rep(seq(from = min(data$Temp),
                                 to = max(data$Temp),
                                 length.out = length(data$Temp))))
  
  z$x <- model.matrix(~ ., data = z)
  z
}

### Obtendo os valores preditos do proprio treino.
pred_df = 
  get_predictions(mob_ozonio_005,
                  ids="terminal",
                  newdata_fun = generate_newdata)

pred_df$prediction_exp = exp(pred_df$prediction)


### Coeficientes e seus exponenciais
gg_mob_ozonio_005$data$beta_exp
round(gg_mob_ozonio_005$data$intercept,digits=4)
round(gg_mob_ozonio_005$data$beta,digits=4)
round(gg_mob_ozonio_005$data$beta_exp,digits=4)

### grafico
graf_mob_ozonio_005=
  gg_mob_ozonio_005 +
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 10, col="black")),
                  ids = "inner") + 
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = Temp, y = Ozone,
                                       fill = factor(id)),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nTemperatura (?F)"),ylab("Oz?nio (ppb)\n"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none"),
                        geom_line(data=pred_df,
                                  aes(x=Temp,
                                      y=prediction_exp),
                                  size=1.2,
                                  col="red",
                                  alpha=0.5)),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("N? ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(graf_mob_ozonio_005)

ggsave(filename = "MOB_arvore_air005.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/Mob",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")


###################################
# Grafico da arvore dos residuos


graf_mob_ozonio_005_res=
  gg_mob_ozonio_005 +
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 10, col="black")),
                  ids = "inner") + 
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = fitted_values, y = residuals,
                                       fill = factor(id)),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nValores ajustados"),ylab("Res?duos\n"),
                        theme_bw(base_size = 15),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none"),
                        geom_hline(yintercept = 0,
                                   col="red",
                                   alpha=0.5)),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("N? ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(graf_mob_ozonio_005_res)

ggsave(filename = "MOB_arvore_air005_res.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/Mob",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")











###############################
# Airquality 1
###############################

### O mesmo que o anterior, mas agora com 
## alpha = 100%

mob_ozonio_100 = 
  glmtree(Ozone ~ Temp | Wind + Solar.R + Month + Day,
          data = dados2,
          family = Gamma(link="log"),
          alpha = 1)

plot(mob_ozonio_100)


###################################
# Grafico da arvore

gg_mob_ozonio_100 = 
  ggparty(mob_ozonio_100,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta = "$node$info$coefficients[2]"))


gg_mob_ozonio_100$data$beta_exp = 
  exp(gg_mob_ozonio_100$data$beta)

round(gg_mob_ozonio_100$data$intercept,digits=4)
round(gg_mob_ozonio_100$data$beta,digits=4)
round(gg_mob_ozonio_100$data$beta_exp,digits=4)


generate_newdata <- function(data) {
  z <- data.frame(Temp = rep(seq(from = min(data$Temp),
                                 to = max(data$Temp),
                                 length.out = length(data$Temp))))
  
  z$x <- model.matrix(~ ., data = z)
  z
}


pred_df = 
  get_predictions(mob_ozonio_100,
                  ids="terminal",
                  newdata_fun = generate_newdata)


pred_df$prediction_exp = exp(pred_df$prediction)

gg_mob_ozonio_100$data$beta_exp

graf_mob_ozonio_100=
  gg_mob_ozonio_100 +
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 10, col="black")),
                  ids = "inner") + 
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = Temp, y = Ozone,
                                       fill = factor(id)),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        xlab("\nTemperatura (?F)"),ylab("Oz?nio (ppb)\n"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none"),
                        geom_line(data=pred_df,
                                  aes(x=Temp,
                                      y=prediction_exp),
                                  size=1.2,
                                  col="red",
                                  alpha=0.5)),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("N? ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(graf_mob_ozonio_100)

ggsave(filename = "MOB_arvore_air100.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/Mob",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")


###################################
# Grafico da arvore dos residuos


graf_mob_ozonio_100_res=
  gg_mob_ozonio_100 +
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 10, col="black")),
                  ids = "inner") + 
  
  geom_node_plot(gglist = 
                   list(geom_point(aes(x = fitted_values, y = residuals,
                                       fill = factor(id)),
                                   show.legend = FALSE,
                                   color = "#0073C2FF", 
                                   alpha = 0.55, shape= 19),
                        ylab("Res?duo\n"),xlab("\nValor ajustado"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none"),
                        geom_hline(yintercept = 0,
                                   col="red",
                                   alpha=0.5)),
                 shared_axis_labels =T) +
  
  geom_node_label(aes(label = paste0("N? ", id," (N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = 0.01,
                  nudge_x = 0.015)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(graf_mob_ozonio_100_res)

ggsave(filename = "MOB_arvore_air100_res.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/Mob",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")




