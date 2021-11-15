###############################
# Pacotes
###############################

if(!require(partykit)){install.packages("partykit")}
library(partykit)

if(!require(rpart)){install.packages("rpart")}
library(rpart)

if(!require(ggparty)){install.packages("ggparty")}
library(ggparty)

if(!require(datasets)){install.packages("datasets")}
#library(datasets)
#data("USArrests")

### arrumar o fundo do plot
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

###############################
# Exemplo do Quakes
###############################

### Dados
dados2 = datasets::quakes
colnames(dados2)

### Ajustando a arvore
CART_quakes = rpart(mag ~ lat + long + depth + stations,
                    data = dados2, method = "anova",
                    control = rpart.control(minbucket = 50,
                                            cp=0.01))

#control = rpart.control(minbucket = 50)
CART_quakes

### Comando para obter o melhor parametro de poda,
## mas queremos fixar cp=0.01 mesmo.

#melhorCp = CART_quakes$cptable[which.min(CART_quakes$cptable[,"xerror"]),"CP"]
#melhorCp
#plot(as.party(prune(CART_quakes,cp=melhorCp)))

### Plot da arvore
plot(CART_quakes)

### ggplot
g_quakes =
  ggparty(as.party(CART_quakes))+
  geom_edge() +
  
  geom_edge_label(size=3)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12)),
                  ids = "inner") +
  
  geom_node_plot(gglist = 
                   list(geom_boxplot(aes(x = "", y = mag,
                                         fill = factor(id)),
                                     show.legend = FALSE),
                        xlab(""),ylab("Magnitude\n(escala Richter)\n"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels =T)+
  
  geom_node_label(aes(label = paste0("N? ", id,"\n(N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.325,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


g_quakes

ggarrange(g_quakes)

ggsave(filename = "CART_arvore_quakes.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/CART",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")



###############################
# Exemplo do Quakes - com cp=0.02 agora
###############################

g_quakes_002 =
  ggparty(as.party(prune(CART_quakes,cp=0.02)))+
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12)),
                  ids = "inner") +
  
  geom_node_plot(gglist = 
                   list(geom_boxplot(aes(x = "", y = mag,
                                         fill = factor(id)),
                                     show.legend = FALSE),
                        xlab(""),ylab("Magnitude\n(escala Richter)\n"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels =T)+
  
  geom_node_label(aes(label = paste0("N? ", id,"\n(N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.39,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")

g_quakes_002

ggarrange(g_quakes_002)

ggsave(filename = "CART_arvore_quakes_002.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/CART",  
       width = 22.75, 
       height = 15.5, 
       units = "cm")










