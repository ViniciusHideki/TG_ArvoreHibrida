#############################
# Pacote
#############################
### Algoritmo do C45
if(!require(RWeka)){install.packages("RWeka")}
library(RWeka)
### auxilio para arvores
if(!require(partykit)){install.packages("partykit")}
library(partykit)
### ggplot para arvore
if(!require(ggparty)){install.packages("ggparty")}
library(ggparty)
### banco de dados
if(!require(MASS)){install.packages("MASS")}
#library(MASS)
### ggplot para arvore
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

#############################
# Exemplo do Quino
#############################

dados = MASS::quine

#dados$Teste = runif(length(dados$Lrn))

### Ajustando o modelo
c45_quine <- J48(Teste ~ Eth + Sex + Age + Days, 
                 data = dados,
                 control = Weka_control(U=F,
                                        M=10,
                                        B=F))

### Verifica-se os argumentos de controle
## do J48 (J48 eh o nome de C45, mas feito por Java)
WOW("J48")

c45_quine

plot(c45_quine)


#############################
# ggplot
#############################

g1 = 
  ggparty(c45_quine_party) +
  geom_edge(size = 1) +
  geom_edge_label() +
  
  geom_node_info(aes(col = factor(level)))+
  
  geom_node_plot(gglist = 
                   list(geom_bar(aes(x = "", fill = Lrn),
                                 position = position_fill()),
                        xlab(""),ylab("Percentual"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels = TRUE)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12)),
                  ids = "inner") +
  
  geom_node_label(aes(label = paste0("N? ", id,"\n(N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.39,
                  nudge_x = 0.026)+
  
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'))+
  scale_color_brewer(palette="Dark2")


g1

#jpeg("C45_arvore_min10.jpeg",quality = 100)
#g1
#dev.off()

ggarrange(g1)

ggsave(filename = "C45_arvore_min10.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/C45",  
       width = 19.75, 
       height = 15.5, 
       units = "cm")




#############################
# Exemplo do Quino2
#############################

dados = MASS::quine

c45_quine2 <- J48(Lrn ~ Eth + Sex + Age + Days, 
                  data = dados,
                  control = Weka_control(U=F,
                                         M=2))

c45_quine2

WOW("J48")

plot(c45_quine2)

#############################
# ggplot
#############################

c45_quine2_party = as.party(c45_quine2)

plot(c45_quine2_party)

g2 = 
  ggparty(c45_quine2_party) +
  geom_edge(size = 1) +
  geom_edge_label() +
  
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 9, col = "black", fontface = "bold"),
                                   list(size = 8)),
                  ids = "inner")+
  
  geom_node_plot(gglist = 
                   list(geom_bar(aes(x = "", fill = Lrn),
                                 position = position_fill()),
                        xlab(""),ylab("Frequ?ncia"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels = TRUE)+
  
  geom_node_label(aes(label = paste0("N? ", id,"\n(N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.2775,
                  nudge_x = 0.023)+
  
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggarrange(g2)



ggsave(filename = "C45_arvore_min2.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/C45",  
       width = 22.75, 
       height = 15.5, 
       units = "cm")
