###############################
# Pacotes
###############################
if(!require(partykit)){install.packages("partykit")}
library(partykit)
if(!require(ggparty)){install.packages("ggparty")}
library(ggparty)

if(!require(datasets)){install.packages("datasets")}
#library(datasets)
#data("USArrests")

### arrumar o fundo do plot
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
### banco de dados
if(!require(AER)){install.packages("AER")}
library(AER)
### Manipulacao
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

###############################
# Exemplo do phd - alpha 0.05
###############################

### Obtendo os dados
data("PhDPublications")

### Ajustando a arvore
CTREE_phd = ctree(articles ~ .,data=PhDPublications,
                  control = ctree_control(alpha=0.05,
                                          minsplit = 21,
                                          minbucket = 7))


### Codigo para obter os quantis, separando cada
## observao por no.
CTREE_phd_obs_por_no = 
  split(PhDPublications,
        predict(CTREE_phd, type = "node"))



### Obtendo os quais por no.
CTREE_phd_quantis = list()
for(i in 1:5){
  CTREE_phd_quantis[[i]] =
    summarise(CTREE_phd_obs_por_no[[i]],
              Q1 = quantile(articles,0.25),
              Q2 = median(articles),
              Q3 = quantile(articles,0.75)) 
}

dados_CTREE_phd = ggparty(CTREE_phd)

Q1 = c(rep(0,3),CTREE_phd_quantis[[1]][1,1],
       CTREE_phd_quantis[[2]][1,1],0,CTREE_phd_quantis[[3]][1,1],
       CTREE_phd_quantis[[3]][1,1],CTREE_phd_quantis[[5]][1,1]) 

Q2 = c(rep(0,3),CTREE_phd_quantis[[1]][1,2],
       CTREE_phd_quantis[[2]][1,2],0,CTREE_phd_quantis[[3]][1,2],
       CTREE_phd_quantis[[3]][1,2],CTREE_phd_quantis[[5]][1,2])

Q3 = c(rep(0,3),CTREE_phd_quantis[[1]][1,3],
       CTREE_phd_quantis[[2]][1,3],0,CTREE_phd_quantis[[3]][1,3],
       CTREE_phd_quantis[[3]][1,3],CTREE_phd_quantis[[5]][1,3])


### Colocando junto a arvore os quantis obtidos.
dados_CTREE_phd$data = 
  cbind(dados_CTREE_phd$data, Q1,Q2,Q3 )

g_phd =
  dados_CTREE_phd+
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
                   list(geom_boxplot(aes(x = "", y = articles,
                                         fill = factor(id)),
                                     show.legend = FALSE),
                        xlab(""),ylab("Artigos\n"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels =T)+
  
  geom_node_label(aes(label = paste0("N? ", id," (N = ",nodesize,")\n",
                                     "Q1 = ", Q1,"\n",
                                     "Q2 = ", Q2,"\n",
                                     "Q3 = ", Q3)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.42,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


g_phd




ggarrange(g_phd)

ggsave(filename = "CTREE_arvore_phd005.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/Ctree",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")


###############################
# Exemplo do phd - alpha 0.2
###############################

data("PhDPublications")

names(PhDPublications)

CTREE_phd2 = ctree(articles ~ .,data=PhDPublications,
                   control = ctree_control(alpha= 0.15,
                                           minsplit = 21,
                                           minbucket = 7))

plot(CTREE_phd2)

CTREE_phd2_obs_por_no = 
  split(PhDPublications,
        predict(CTREE_phd2, type = "node"))

names(CTREE_phd2_obs_por_no)


CTREE_phd2_quantis = list()


CTREE_phd2_quantis

for(i in 1:7){
  CTREE_phd2_quantis[[i]] =
    summarise(CTREE_phd2_obs_por_no[[i]],
              Q1 = quantile(articles,0.25),
              Q2 = median(articles),
              Q3 = quantile(articles,0.75)) 
}

names(CTREE_phd2_obs_por_no)

dados_CTREE_phd2 = ggparty(CTREE_phd2)

Q1 = c(c(1,2,3),CTREE_phd2_quantis[[1]][1,1],
       CTREE_phd2_quantis[[2]][1,1],6,
       CTREE_phd2_quantis[[3]][1,1],
       CTREE_phd2_quantis[[4]][1,1],9,
       CTREE_phd2_quantis[[5]][1,1],11,
       CTREE_phd2_quantis[[6]][1,1],CTREE_phd2_quantis[[7]][1,1]) 

Q2 = c(c(1,2,3),CTREE_phd2_quantis[[1]][1,2],
       CTREE_phd2_quantis[[2]][1,2],6,
       CTREE_phd2_quantis[[3]][1,2],
       CTREE_phd2_quantis[[4]][1,2],9,
       CTREE_phd2_quantis[[5]][1,2],11,
       CTREE_phd2_quantis[[6]][1,2],CTREE_phd2_quantis[[7]][1,2]) 

Q3 = c(c(1,2,3),CTREE_phd2_quantis[[1]][1,3],
       CTREE_phd2_quantis[[2]][1,3],6,
       CTREE_phd2_quantis[[3]][1,3],
       CTREE_phd2_quantis[[4]][1,3],9,
       CTREE_phd2_quantis[[5]][1,3],11,
       CTREE_phd2_quantis[[6]][1,3],CTREE_phd2_quantis[[7]][1,3]) 


dados_CTREE_phd2$data = 
  cbind(dados_CTREE_phd2$data, Q1,Q2,Q3 )




g_phd2 =
  dados_CTREE_phd2+
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
                   list(geom_boxplot(aes(x = "", y = articles,
                                         fill = factor(id)),
                                     show.legend = FALSE),
                        xlab(""),ylab("Artigos\n"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels =T)+
  
  geom_node_label(aes(label = paste0("N? ", id,"\n(N = ",nodesize,")\n",
                                     "Q1 = ", Q1,"\n",
                                     "Q2 = ", Q2,"\n",
                                     "Q3 = ", Q3)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 2.7,
                  nudge_y = -0.43,
                  nudge_x = 0.015)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


#g_phd

ggarrange(g_phd2)

ggsave(filename = "CTREE_arvore_phd015.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/Ctree",  
       width = 19.75, 
       height = 17.5, 
       units = "cm")