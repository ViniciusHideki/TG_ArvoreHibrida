###############################
# Pacotes
###############################
### arvores
if(!require(partykit)){install.packages("partykit")}
library(partykit)
if(!require(ggparty)){install.packages("ggparty")}
library(ggparty)
### arrumar o fundo do plot
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
### Manipulacao
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
### Distribuicao
if(!require(gamlss)){install.packages("gamlss")}
library(gamlss)
### RPART
if(!require(rpart)){install.packages("rpart")}
library(rpart)
### testes de normalidade
if(!require(nortest)){install.packages("nortest")}
library(nortest)
### Random Forest
if(!require(ranger)){install.packages("ranger")}
library(ranger)
### XGBoost
if(!require(xgboost)){install.packages("xgboost")}
library(xgboost)


### Funcao para usar para arredondar
## os valores numericos das linhas
## da arvore.
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

###############################
# Gerando os dados
###############################

set.seed(123)
n <- 14000
X1 <- runif(n)
X2 <- runif(n)
X1_2 <- X1+X2
X3 = runif(n,3,4)
X4 = runif(n,10,12)
X5 = runif(n,5,6)
X3_4_5 = (X3+X4)/log(X5,base=5)


Y <- 100 + X1 - exp(1.5*X2) + 2*log(X1_2) + 
  0.7*X3 + 0.3*X4 + X5 + 0.3*(X3_4_5) + 
  rnorm(n, 0, 4)


dados = data.frame(Y,X1,X2,X3,X4,X5)

###############################
# Divisao
###############################

divisao = sample(c("Treino","Teste"),
                 prob= c(0.7,0.3),
                 size = nrow(dados),
                 replace = T)

dados_teste = dados[divisao == "Teste",]
dados_treino = dados[divisao == "Treino",]


### Riscos ira guardar o EQM de cada
## algoritmo
riscos = list()

### Calcula o EQM
calcula_EQM = function(preditos,y_teste){
  return(mean((y_teste - preditos)**2))
}

### Calcula a variancia do EQM
calcula_VAR_EQM = function(preditos,y_teste){
  EQM = calcula_EQM(preditos,y_teste)
  valor = mean(((y_teste - preditos)**2 - EQM)**2)
  return(valor)
}

### Calcula o IC
calcula_IC = function(Eqm,Var,n){
  c(Eqm - 1.96*sqrt( (1/n)*Var ),
    Eqm + 1.96*sqrt( (1/n)*Var ))
}

### Calcula tudo de uma vez
calcula_EQM_e_IC = function(preditos,y_teste,n){
  EQM = calcula_EQM(preditos,y_teste)
  Var = calcula_VAR_EQM(preditos,y_teste)
  vetor = c(EQM,calcula_IC(EQM,Var,n))
  return(vetor)
}

###############################
# CART
###############################

### Ajustando o cart
CART_1 = rpart(Y ~ X1+X2+X3+X4+X5,
               data = dados_treino, method = "anova",
               control = rpart.control(cp=0.01))


### Obtendo o melhor CP para a poda
melhorCp = CART_1$cptable[which.min(CART_1$cptable[,"xerror"]),"CP"]
melhorCp

### Podando
CART_1_poda = prune(CART_1,cp=melhorCp)

### Vendo a arvore no geral
plot(as.party(CART_1_poda))

### Arredondando as linhas da arvore
rounded_labels_cart <- 
  add_splitvar_breaks_index_new(party_object = as.party(CART_1_poda),
                                plot_data = ggparty:::get_plot_data(as.party(CART_1_poda)), 
                                round_digits = 3)

### ggplot
g_cart =
  ggparty(as.party(CART_1_poda))+
  geom_edge() +
  
  geom_edge_label(size=3,
                  mapping = aes(label = unlist(rounded_labels_cart)),
                  data = rounded_labels_cart)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12)),
                  ids = "inner") +
  
  geom_node_plot(gglist = 
                   list(geom_boxplot(aes(x = "", y = Y,
                                         fill = factor(id)),
                                     show.legend = FALSE),
                        xlab(""),ylab("Y"),
                        theme_bw(base_size = 8),
                        theme(text = element_text(size = 14, 
                                                  family ="serif"),
                              legend.position = "none")),
                 shared_axis_labels =T)+
  
  geom_node_label(aes(label = paste0("N? ", id,"\n(N = ",nodesize,")")),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.49,
                  nudge_x = 0.03)+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


g_cart

ggarrange(g_cart)


ggsave(filename = "SIM_CART_arvore.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/MOB",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")


### Calculado o predito
predito_CART_1 = predict(CART_1_poda,
                         dados_teste[,2:6])


riscos$CART_1 = calcula_EQM_e_IC(predito_CART_1,
                                 dados_teste$Y,length(dados_teste$Y))
riscos


###############################
# CTREE
###############################

CTREE_1 = ctree(Y ~ X1+X2+X3+X4+X5,
                data= dados_treino,
                control = ctree_control(alpha=0.01))


###Nao roda, pois a arvore ficou
## muito grande
#plot(CTREE_1)

### Calculado o predito
predito_CTREE_1 = predict(CTREE_1,
                          dados_teste[,2:6])


riscos$CTREE_1 = calcula_EQM_e_IC(predito_CTREE_1,
                                  dados_teste$Y,
                                  length(dados_teste$Y))
riscos


###############################
# MOB
###############################

### Fazendo um histograma para verificar
## o comportamento geral de Y
bw2 <- 2 * IQR(dados$Y)/ 
  length(dados$Y)^(1/3)

hist_Y = 
  ggplot(data = dados, 
         aes(x=Y)) + 
  theme_bw()+
  geom_histogram(aes(y=..density..), 
                 color="black", 
                 fill= "#0073C2FF",
                 binwidth = bw2,
                 alpha = 0.6)+
  xlab("Y") + 
  ylab("Densidade") +
  ggtitle("Histograma de Y")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))

hist_Y

ggsave(filename = "SIM_Hist_Y.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/MOB",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")


### Como Y eh positivo, verica-se
## possiveis distribuicoes para ele.
Possiveis_Dist = fitDist(dados_treino$Y,
                         type="realplus")

sort(Possiveis_Dist$fits)

### Execuando o MOB.
### Cuidado: Demorou um certo tempo
## para executa-lo. 
start_time <- Sys.time()
MOB_1 = glmtree(Y ~ X1 + X2 | X3+X4+X5,
                data = dados_treino,
                family = Gamma(link = "log"),
                alpha = 0.01)
end_time <- Sys.time()
end_time - start_time

### Time difference of 5.288282 mins

plot(MOB_1)

### Arredondado para as linhas
## da arvore
rounded_labels_MOB_1 <- 
  add_splitvar_breaks_index_new(party_object = MOB_1,
                                plot_data = ggparty:::get_plot_data(MOB_1), 
                                round_digits = 2)

g_MOB_1=
  ggparty(MOB_1,
          terminal_space = 0,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta1 = "$node$info$coefficients[2]",
                          beta2 = "$node$info$coefficients[3]")) +
  geom_edge(size = 1) +
  
  geom_edge_label(size=3,
                  mapping = aes(label = unlist(rounded_labels_MOB_1)),
                  data = rounded_labels_MOB_1)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("N?", id)),
                                   aes(label = splitvar),
                                   aes(label = paste("p =", formatC(p.value, digits = 2)))),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 10, col="black")),
                  ids = "inner") +
  
  
  geom_node_label(# map content to label for each line
    line_list = list(
      aes(label = paste("beta[0] == ", round(intercept, 4))),
      aes(label = paste("beta[1] == ",round(beta1, 4))),
      aes(label = paste("beta[2] == ",round(beta2, 4))),
      aes(label = ""),
      aes(label = id)
    ),
    # set graphical parameters for each line in same order
    line_gpar = list(list(size = 12, parse = T),
                     list(size = 12, parse = T),
                     list(size = 12, parse = T),
                     list(size = 6),
                     list(size = 7,
                          col = "black",
                          fontface = "bold",
                          alignment = "left")),
    ids = "terminal",
    # nudge labels towards bottom so that edge labels have enough space
    # alternatively use shift argument of edge_label
    nudge_y = -.05) +
  # don't show legend for splitvar mapping to color since self-explanatory
  theme(legend.position = "none") +
  # html_documents seem to cut off a bit too much at the edges so set limits manually
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 1.1))+
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")


ggarrange(g_MOB_1)


ggsave(filename = "SIM_arvore_MOB.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/MOB",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")

### Predito do MOB
predito_MOB_1 = predict(MOB_1,
                        dados_teste[,2:6])


riscos$MOB_1 = calcula_EQM_e_IC(predito_MOB_1,
                                dados_teste$Y,
                                length(dados_teste$Y))
riscos


###############################
# Random Forest
###############################

### Ajustando floresta com tudo no padrao
floresta_1 = 
  ranger(x = dados_treino[,2:6],
         y = dados_treino$Y,
         importance = "impurity")


#=============================================
##### Fazendo Grafico de Importancia

floresta_importancia = 
  data.frame(names(importance(floresta_1)),
             importance(floresta_1))

names(floresta_importancia) = 
  c("Covariavel","Importancia")

floresta_importancia <- floresta_importancia %>% 
  mutate(rank = dense_rank(desc(Importancia)))

graf_flor_impor = 
  ggplot(data=floresta_importancia[which(floresta_importancia$rank <= 13),], 
         aes(x = reorder(Covariavel, +Importancia), 
             y = Importancia,
             fill = reorder(Covariavel, +Importancia))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  geom_bar(stat="identity",width=0.6) +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "Floresta Aleat?ria", 
       x = "", y = "Importancia da Vari?vel")

graf_flor_impor

ggsave(filename = "SIM_Floresta_Importancia.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/MOB",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")



### predito da floresta
predito_floresta_1 = 
  predict(floresta_1,dados_teste[,2:6])$predictions

riscos$floresta_1 = 
  calcula_EQM_e_IC(predito_floresta_1,
                   dados_teste$Y,
                   length(dados_teste$Y))

riscos

###############################
# XGBoost
###############################

### Fazendo as matrizes pra entrar no 
## XGBoost
xgb_mat_treino = 
  xgb.DMatrix(as.matrix(dados_treino[,2:6]),
              label = as.vector(dados_treino$Y))

xgb_mat_teste = 
  xgb.DMatrix(as.matrix(dados_teste[,2:6]),
              label = as.vector(dados_teste$Y))


#=============================================
##### CV do xgb para escolher a iteracao

cv_xgb <- xgb.cv(data = xgb_mat_treino,
                 nrounds = 1000,
                 nfold = 10,
                 early_stopping_rounds = 30,
                 prediction = TRUE,
                 metric = "rmse",
                 verbose = 0,
                 objective = "reg:squarederror")

### Melhor iteracao
cv_xgb$evaluation_log[cv_xgb$best_iteration,]


#=============================================
##### Fazendo o XBG
modelo_xgb_1 = 
  xgboost(data = xgb_mat_treino,
          nrounds = cv_xgb$best_iteration,
          eval_metric = "rmse",
          verbose = 0,
          objective = "reg:squarederror")


### Grafico de importacia
xgb_dados_importancia = 
  xgb.importance(model = modelo_xgb_1) %>% 
  mutate(rank = dense_rank(desc(Gain)))

ggplot(data=xgb_dados_importancia[which(xgb_dados_importancia$rank <= 13),], 
       aes(x = reorder(Feature, +Gain), 
           y = Gain,
           fill = reorder(Feature, -Gain))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  geom_bar(stat="identity",width=0.6) +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "XG Boost - Import?ncia", 
       x = "", y = "Import?ncia")


ggsave(filename = "SIM_XGBoost_Importancia.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Conteudo/MOB",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")

### Predito do XGBoost
predito_xgb_1 = 
  predict(modelo_xgb_1,xgb_mat_teste)

riscos$XGBoost_1 = 
  calcula_EQM_e_IC(predito_xgb_1,dados_teste$Y,
                   length(dados_teste$Y))

riscos

###############################
# GLM 
###############################

### Ajustando um MLG
glm_1 = glm(Y~X1+X2+X3+X4+X5,
            data=dados_treino,
            family = Gamma(link = "log"))

### Verificando seus coeficientes
glm_1$coefficients
resumo_glm_1  = summary(glm_1)
round(resumo_glm_1$coefficients,digits=)

### Calculando o valor predito 
predito_glm_1 = 
  predict(glm_1,dados_teste[,2:6],type = 'response')

riscos$GLM_1 = 
  calcula_EQM_e_IC(predito_glm_1,dados_teste$Y,
                   length(dados_teste$Y))

riscos

