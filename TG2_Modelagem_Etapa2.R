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
ETAPA2$Dif_TR = ETAPA2$TRFinal - ETAPA2$TRInicial

ETAPA2$MortePerc = ETAPA2$NumMort/ETAPA2$TotalAves

ETAPA2$LogitMort2 =
  log(ETAPA2$MortePerc/(1-ETAPA2$MortePerc))

ETAPA2$log_TotalAves = log(ETAPA2$TotalAves)
ETAPA2$log_NumMorte = log(ETAPA2$NumMort)


#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(ETAPA2,2,pMiss)




set.seed(1633)
divisao = sample(c("Treino","Teste"),
                 prob= c(0.8,0.2),
                 size = nrow(ETAPA2),
                 replace = T)

dados_teste = ETAPA2[divisao == "Teste",]
dados_treino = ETAPA2[divisao == "Treino",]





#############################################
# Funcoes Gerais
#############################################

riscos = list()

calcula_EQM = function(preditos,y_teste){
  return(mean((y_teste - preditos)**2))
}


calcula_VAR_EQM = function(preditos,y_teste){
  EQM = calcula_EQM(preditos,y_teste)
  valor = mean(( (y_teste - preditos)**2 - EQM)**2)
  return(valor)
}

calcula_IC = function(Eqm,Var,n){
  
  LI = ifelse( 
    (Eqm - 1.96*sqrt( (1/n)*Var ))<0,0,
    (Eqm - 1.96*sqrt( (1/n)*Var )) )
  
  c(LI,
    Eqm + 1.96*sqrt( (1/n)*Var ))
}

calcula_EQM_e_IC = function(preditos,y_teste,n){
  EQM = calcula_EQM(preditos,y_teste)
  Var = calcula_VAR_EQM(preditos,y_teste)
  vetor = c(EQM,calcula_IC(EQM,Var,n))
  return(vetor)
}




calcula_EAM = function(preditos,y_teste){
  return(mean(abs(y_teste - preditos) ))
}


calcula_VAR_EAM = function(preditos,y_teste){
  EAM = calcula_EAM(preditos,y_teste)
  valor = mean(( abs(y_teste - preditos) - EAM)**2)
  return(valor)
}


calcula_EAM_e_IC = function(preditos,y_teste,n){
  EAM = calcula_EAM(preditos,y_teste)
  Var = calcula_VAR_EAM(preditos,y_teste)
  vetor = c(EAM,calcula_IC(EAM,Var,n))
  return(vetor)
}









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

### Testes com outros hiperparametros

# cv_mob1 = function(alfa,poda,k=10,s=777){
#   
#   nomes_poda = c("AIC","BIC")
# 
#   embaralhado<-dados_treino[sample(nrow(dados_treino)),]
#   folds <- cut(seq(1,nrow(embaralhado)),breaks=k,labels=FALSE)
#   vetor_EQM = c()
#   
#   for(i in 1:k){
#     testIndexes <- which(folds==i,arr.ind=TRUE)
#     testData <- embaralhado[testIndexes, ]
#     trainData <- embaralhado[-testIndexes, ]
#     
#     ### Se nao houver poda
#     if(poda==3){
#       MOB = glmtree(NumMort ~ TempoEsp+TempExt+URExt| 
#                       Estacao+Turno,
#                     data = trainData,
#                     family = poisson(link = "log"),
#                     offset = log_TotalAves,
#                     alpha = alfa)
#     }else{
#     ### Se for alguma das outras duas podas  
#       MOB = glmtree(NumMort ~ TempoEsp+TempExt+URExt| 
#                       Estacao+Turno,
#                     data = trainData,
#                     family = poisson(link = "log"),
#                     offset = log_TotalAves,
#                     prune= nomes_poda[poda],
#                     alpha = alfa)
#     }
#     
#     predito_MOB = predict(MOB,
#                           newdata=testData)
#     
#     EQM = calcula_EQM(predito_MOB,testData$NumMort)
#     #cat(EQM," da iteracao ",i,'\n')
#     vetor_EQM=c(vetor_EQM,EQM)
#     
#   }
#   return(mean(vetor_EQM))
# }

names(dados_treino)

cv_mob1 = function(alfa,k=5,s=777){
  
  #nomes_poda = c("AIC","BIC")
  set.seed(s)
  embaralhado<-dados_treino[sample(nrow(dados_treino)),]
  folds <- cut(seq(1,nrow(embaralhado)),breaks=k,labels=FALSE)
  vetor_EQM = c()
  
  for(i in 1:k){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- embaralhado[testIndexes, ]
    trainData <- embaralhado[-testIndexes, ]
    
    
    MOB = glmtree(NumMort ~ 
                    TempoEspera|
                    Turno+
                    Tempototal+
                    TExt+URExt+TInt+URInt+
                    TempoViagem+
                    Dif_TR+
                    Distancia,
                  data = trainData,
                  family = poisson(link = "log"),
                  offset = log_TotalAves,
                  alpha = alfa)
    
    predito_MOB = predict(MOB,
                          newdata=testData)
    
    EQM = calcula_EQM(predito_MOB,testData$NumMort)
    cat(EQM," da iteracao ",i,'\n')
    vetor_EQM=c(vetor_EQM,EQM)
    
  }
  return(mean(vetor_EQM))
}


fun_opt_mob1 = function(alfa){
  valor = -1*cv_mob1(alfa)
  return(list(Score=valor,Pred=0))
}

start_time <- Sys.time()
set.seed(222)
opt_mob1 <- BayesianOptimization(fun_opt_mob1,
                                 bounds = list(alfa = c(0, 0.01)),
                                 init_points = 5, n_iter = 30,
                                 acq = "ei",
                                 verbose = TRUE)
end_time <- Sys.time()
end_time - start_time




##### Ajustando o modelo

mob1 = glmtree(NumMort ~ 
                 TempoEspera|
                 Turno+
                 Tempototal+
                 TExt+URExt+TInt+URInt+
                 TempoViagem+
                 Dif_TR+
                 Distancia,
               data = dados_treino,
               family = poisson(link = "log"),
               offset = log_TotalAves,
               alpha = opt_mob1$Best_Par)



##### Plotando

plot(mob1)


rounded_labels_MOB_1 <- 
  add_splitvar_breaks_index_new(party_object = mob1,
                                plot_data = ggparty:::get_plot_data(mob1))

g_MOB_1=
  ggparty(mob1,
          terminal_space = 0,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta1 = "$node$info$coefficients[2]")) +
  
  geom_edge(size = 0.5) +
  
  geom_edge_label(size=3)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
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
      aes(label = paste("beta[1] == ",round(beta1, 4)))
    ),
    # set graphical parameters for each line in same order
    line_gpar = list(list(size = 12, parse = T),
                     list(size = 12, parse = T)),
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
                  size = 2.7,
                  nudge_y = 0.021,
                  nudge_x = 0.005)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")

g_MOB_1

ggarrange(g_MOB_1)


ggsave(filename = "ETAPA2_Mod_MOB1.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")






##### Predizendo

predito_mob1 = predict(mob1,dados_teste)

riscos$MOB_1_quasi = c(calcula_EQM_e_IC(predito_mob1,
                                  dados_teste$NumMort,
                                  length(dados_teste$NumMort)),
                 calcula_EAM_e_IC(predito_mob1,
                                  dados_teste$NumMort,
                                  length(dados_teste$NumMort)))
riscos





#############################################
# MOB - Modelo 2
#############################################

cv_mob2 = function(alfa,k=5,s=777){
  
  #nomes_poda = c("AIC","BIC")
  set.seed(s)
  embaralhado<-dados_treino[sample(nrow(dados_treino)),]
  folds <- cut(seq(1,nrow(embaralhado)),breaks=k,labels=FALSE)
  vetor_EQM = c()
  
  for(i in 1:k){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- embaralhado[testIndexes, ]
    trainData <- embaralhado[-testIndexes, ]
    
    
    MOB = glmtree(NumMort ~ 
                    TempoEspera+TInt+URInt|
                    Turno+
                    Tempototal+
                    TExt+URExt+
                    TempoViagem+
                    Dif_TR+
                    Distancia,
                  data = trainData,
                  family = poisson(link = "log"),
                  offset = log_TotalAves,
                  alpha = alfa)
    
    predito_MOB = predict(MOB,
                          newdata=testData)
    
    EQM = calcula_EQM(predito_MOB,testData$NumMort)
    cat(EQM," da iteracao ",i,'\n')
    vetor_EQM=c(vetor_EQM,EQM)
    
  }
  return(mean(vetor_EQM))
}


fun_opt_mob2 = function(alfa){
  valor = -1*cv_mob2(alfa)
  return(list(Score=valor,Pred=0))
}

start_time <- Sys.time()
set.seed(333)
opt_mob2 <- BayesianOptimization(fun_opt_mob2,
                                 bounds = list(alfa = c(0, 0.01)),
                                 init_points = 5, n_iter = 30,
                                 acq = "ei",
                                 verbose = TRUE)
end_time <- Sys.time()
end_time - start_time




##### Ajustando o modelo

mob2 = glmtree(NumMort ~ 
                 TempoEspera+TInt+URInt|
                 Turno+
                 Tempototal+
                 TExt+URExt+
                 TempoViagem+
                 Dif_TR+
                 Distancia,
               data = dados_treino,
               family = poisson(link = "log"),
               offset = log_TotalAves,
               alpha = opt_mob2$Best_Par)



##### Plotando

plot(mob1)


rounded_labels_MOB_1 <- 
  add_splitvar_breaks_index_new(party_object = mob1,
                                plot_data = ggparty:::get_plot_data(mob1))

g_MOB_1=
  ggparty(mob1,
          terminal_space = 0,
          add_vars = list(intercept = "$node$info$coefficients[1]",
                          beta1 = "$node$info$coefficients[2]")) +
  
  geom_edge(size = 0.5) +
  
  geom_edge_label(size=3)+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
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
      aes(label = paste("beta[1] == ",round(beta1, 4)))
    ),
    # set graphical parameters for each line in same order
    line_gpar = list(list(size = 12, parse = T),
                     list(size = 12, parse = T)),
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
                  size = 2.7,
                  nudge_y = 0.021,
                  nudge_x = 0.005)+
  
  
  theme(legend.position = "none",
        text = element_text(size = 14, 
                            family ="serif"),
        panel.background = element_rect(fill = 'white',colour = 'white'))+
  scale_color_brewer(palette="Dark2")

g_MOB_1

ggarrange(g_MOB_1)


ggsave(filename = "ETAPA2_Mod_MOB1.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")






##### Predizendo

predito_mob2 = predict(mob2,dados_teste)

riscos$MOB_2 = c(calcula_EQM_e_IC(predito_mob2,
                                  dados_teste$NumMort,
                                  length(dados_teste$NumMort)),
                 calcula_EAM_e_IC(predito_mob2,
                                  dados_teste$NumMort,
                                  length(dados_teste$NumMort)))
riscos




























###############################
# CART - Modelo 3
###############################

### 1.Incorporar o log_total como covariavel
### 2.Trabalhar com o percentual

matrix(c(TotalAves,NumMort),ncol=2,
       byrow=F)



### Y_i|X_i ~ Poisson(mu_i)

### mu_i = lambda_i * total_i

### lambda_i =  mu_i/total_i

###

CART_3 = rpart(matrix(c(TotalAves,NumMort),
                      ncol=2,
                      byrow=F) ~ 
                 TempoEspera+
                 Turno+
                 Tempototal+
                 TExt+URExt+TInt+URInt+
                 TempoViagem+
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
    c(1,2,3, #nois 1,2,3
      CART_3_desc[[1]][1,i],
      CART_3_desc[[2]][1,i],
      6,
      7,
      CART_3_desc[[3]][1,i],
      CART_3_desc[[4]][1,i],
      CART_3_desc[[5]][1,i],
      11,12,
      CART_3_desc[[6]][1,i],
      CART_3_desc[[7]][1,i],
      CART_3_desc[[8]][1,i])
  
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
                  size = 3,
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



##### Predizendo 

predito_CART_3 = predict(prune(CART_3,cp=melhorCp),dados_teste)*
  dados_teste$TotalAves

#predito_CART_3 = predict(CART_3,dados_teste)

riscos$CART_3 = c(calcula_EQM_e_IC(predito_CART_3,
                                   dados_teste$NumMort,
                                   length(dados_teste$NumMort)),
                  calcula_EAM_e_IC(predito_CART_3,
                                   dados_teste$NumMort,
                                   length(dados_teste$NumMort)))


riscos







###############################
# CTREE - Modelo 1
###############################

### Minino 0.05 de alfa

CTREE_1 = ctree(NumMort ~ 
                  TempoEspera+
                  Turno+
                  Tempototal+
                  TExt+URExt+TInt+URInt+
                  TempoViagem+
                  Dif_TR+
                  Distancia+TotalAves,
                data= dados_treino,
                control = ctree_control(alpha=0.2))

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
    c(1,2, #nois 1,2
      CTREE_1_desc[[1]][1,i],
      CTREE_1_desc[[2]][1,i],
      CTREE_1_desc[[3]][1,i])
  
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
  geom_edge() +
  
  geom_edge_label()+
  
  geom_node_label(aes(col = splitvar),
                  line_list = list(aes(label = paste("Nó", id)),
                                   aes(label = splitvar)),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 13, col = "black", fontface = "bold"),
                                   list(size = 12)),
                  ids = "inner") +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0.3, 1.1))+
  
  geom_node_label(aes(label = paste0("Nó ", id," (N = ",nodesize,")\n \n",
                                     "Q1 = ", CTREE_1_Q1,"\n",
                                     "Q2 = ", CTREE_1_Q2,"\n",
                                     "Média = ", CTREE_1_Media,"\n",
                                     "Q3 = ", CTREE_1_Q3,"\n",
                                     "Sd = ", CTREE_1_Variancia)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3.2,
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



##### Predizendo
predito_CTREE_1 = predict(CTREE_1,dados_teste)

riscos$CTREE_1 = c(calcula_EQM_e_IC(predito_CTREE_1,
                                    dados_teste$NumMort,
                                    length(dados_teste$NumMort)),
                   calcula_EAM_e_IC(predito_CTREE_1,
                                    dados_teste$NumMort,
                                    length(dados_teste$NumMort)))
riscos





###############################
# CTREE - Modelo 2
###############################

### Com 0.05, a arvore cresce muito.

CTREE_2 = ctree(log_NumMorte ~ 
                  TempoEspera+
                  Turno+
                  Tempototal+
                  TExt+URExt+TInt+URInt+
                  TempoViagem+
                  Dif_TR+
                  Distancia+TotalAves,
                data= dados_treino,
                control = ctree_control(alpha=0.05))

plot(CTREE_2)

### Arvore cresceu muito


##### Predizendo
predito_CTREE_2 = exp( predict(CTREE_2,dados_teste))


riscos$CTREE_2 = c(calcula_EQM_e_IC(predito_CTREE_2,
                                    dados_teste$NumMort,
                                    length(dados_teste$NumMort)),
                   calcula_EAM_e_IC(predito_CTREE_2,
                                    dados_teste$NumMort,
                                    length(dados_teste$NumMort)))
riscos






###############################
# Random Forest - Modelo 1
###############################

indice_col_floresta =
  c(match("TempoEspera",names(ETAPA2)),
    match("Turno",names(ETAPA2)),
    match("Tempototal",names(ETAPA2)),
    
    match("TExt",names(ETAPA2)),
    match("URExt",names(ETAPA2)),
    match("TInt",names(ETAPA2)),
    match("URInt",names(ETAPA2)),
    match("TempoViagem",names(ETAPA2)),
    
    match("Dif_TR",names(ETAPA2)),
    match("Distancia",names(ETAPA2)),
    match("TotalAves",names(ETAPA2)))

# TempoEspera+
#   Turno+
#   Tempototal+
#   TExt+URExt+TInt+URInt+
#   TempoViagem+
#   Dif_TR+
#   Distancia+TotalAves


cv_floresta1 = function(num_arvore,num_no,k=5,s=777){
  
  set.seed(s)
  embaralhado<-dados_treino[sample(nrow(dados_treino)),]
  folds <- cut(seq(1,nrow(embaralhado)),breaks=k,labels=FALSE)
  vetor_EQM = c()
  
  for(i in 1:k){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- embaralhado[testIndexes, ]
    trainData <- embaralhado[-testIndexes, ]

    
    floresta = 
      ranger(x = trainData[,indice_col_floresta],
             y = trainData$NumMort,
             num.trees = num_arvore,
             min.node.size = num_no)
    
    predito_floresta =
      predict(floresta,testData)$predictions
    
    EQM = calcula_EQM(predito_floresta,testData$NumMort)
    #cat(EQM," da iteracao ",i,'\n')
    vetor_EQM=c(vetor_EQM,EQM)
    
  }
  return(mean(vetor_EQM))
}



fun_opt_floresta1 = function(num_arvore,num_no){
  valor = -1*cv_floresta1(num_arvore,num_no)
  return(list(Score=valor,Pred=0))
}


start_time <- Sys.time()
set.seed(4444)
opt_floresta1 <- BayesianOptimization(fun_opt_floresta1,
                                      bounds = list(num_arvore = c(100L, 1000L),
                                                    num_no = c(5L,30L)),
                                      init_points = 5, n_iter = 30,
                                      acq = "ucb",
                                      verbose = TRUE)
end_time <- Sys.time()
end_time - start_time



floresta_1 = 
  ranger(x = dados_treino[,indice_col_floresta],
         y = dados_treino$NumMort,
         num.trees = opt_floresta1$Best_Par[[1]],
         min.node.size = opt_floresta1$Best_Par[[2]],
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
  ggplot(data=floresta_importancia[which(floresta_importancia$rank <= 8),], 
         aes(x = reorder(Covariavel, +Importancia), 
             y = Importancia,
             fill = reorder(Covariavel, +Importancia))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  geom_bar(stat="identity",width=0.6) +
  coord_flip(clip = "off")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "", 
       x = "", y = "Importancia da Variável")

graf_flor_impor



ggsave(filename = "ETAPA2_Mod_RF1_Importancia.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")




predito_floresta_1 = 
  predict(floresta_1,dados_teste)$predictions

riscos$floresta_1 =  c(calcula_EQM_e_IC(predito_floresta_1,
                                        dados_teste$NumMort,
                                        length(dados_teste$NumMort)),
                       calcula_EAM_e_IC(predito_floresta_1,
                                        dados_teste$NumMort,
                                        length(dados_teste$NumMort)))
riscos






###############################
# Random Forest - Modelo 2
###############################



cv_floresta2 = function(num_arvore,num_no,k=5,s=777){
  
  set.seed(s)
  embaralhado<-dados_treino[sample(nrow(dados_treino)),]
  folds <- cut(seq(1,nrow(embaralhado)),breaks=k,labels=FALSE)
  vetor_EQM = c()
  
  for(i in 1:k){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- embaralhado[testIndexes, ]
    trainData <- embaralhado[-testIndexes, ]
    
    floresta = 
      ranger(x = trainData[,indice_col_floresta],
             y = trainData$log_NumMorte,
             num.trees = num_arvore,
             min.node.size = num_no)
    
    predito_floresta =
      exp(predict(floresta,testData)$predictions)
    
    EQM = calcula_EQM(predito_floresta,testData$NumMort)
    #cat(EQM," da iteracao ",i,'\n')
    vetor_EQM=c(vetor_EQM,EQM)
    
  }
  return(mean(vetor_EQM))
}



fun_opt_floresta2 = function(num_arvore,num_no){
  valor = -1*cv_floresta2(num_arvore,num_no)
  return(list(Score=valor,Pred=0))
}


start_time <- Sys.time()
set.seed(444442)
opt_floresta2 <- BayesianOptimization(fun_opt_floresta2,
                                      bounds = list(num_arvore = c(100L, 1000L),
                                                    num_no = c(5L,30L)),
                                      init_points = 5, n_iter = 30,
                                      acq = "ucb",
                                      verbose = TRUE)
end_time <- Sys.time()
end_time - start_time



floresta_2 = 
  ranger(x = dados_treino[,indice_col_floresta],
         y = dados_treino$log_NumMorte,
         num.trees = opt_floresta2$Best_Par[[1]],
         min.node.size = opt_floresta2$Best_Par[[2]],
         importance = 'impurity')



#=============================================
##### Fazendo Grafico de Importancia

floresta_importancia = 
  data.frame(names(importance(floresta_2)),
             importance(floresta_2))

names(floresta_importancia) = 
  c("Covariavel","Importancia")

floresta_importancia <- floresta_importancia %>% 
  mutate(rank = dense_rank(desc(Importancia)))

graf_flor_impor = 
  ggplot(data=floresta_importancia[which(floresta_importancia$rank <= 8),], 
         aes(x = reorder(Covariavel, +Importancia), 
             y = Importancia,
             fill = reorder(Covariavel, +Importancia))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  geom_bar(stat="identity",width=0.6) +
  coord_flip(clip = "off")+
  theme_bw()+
  scale_fill_brewer(palette="Dark2")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = " ", 
       x = "", y = "Importancia da Variável")

graf_flor_impor

ggsave(filename = "ETAPA2_Mod_RF2_Importancia.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa2/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")





predito_floresta_2 = 
  exp(predict(floresta_2,dados_teste)$predictions)

riscos$floresta_2 =  c(calcula_EQM_e_IC(predito_floresta_2,
                                        dados_teste$NumMort,
                                        length(dados_teste$NumMort)),
                       calcula_EAM_e_IC(predito_floresta_2,
                                        dados_teste$NumMort,
                                        length(dados_teste$NumMort)))
riscos









###############################
# XGBoost - Modelo 1
###############################
xgboost::xgb.DMatrix()

xgb.DMatrix(deli,label=Y)


xgb_mat_treino = 
  xgb.DMatrix(model.matrix(NumMort~.,
                           data=dados_treino[,c(11,indice_col_floresta)])[,-1],
              label = dados_treino$NumMort)

xgb_mat_teste = 
  xgb.DMatrix(model.matrix(NumMort~.,
                           data=dados_teste[,c(11,indice_col_floresta)])[,-1],
              label = dados_teste$NumMort)


#=============================================
##### CV do xgb para escolher a iteracao


# fun_opt_xgb = function(num_arvore,max_profun,aprend){
#   
#   cv_xgb <- xgb.cv(data = xgb_mat_treino,
#                    nrounds = num_arvore,
#                    max_depth=max_profun,
#                    eta=aprend,
#                    nfold = 10,
#                    early_stopping_rounds = 30,
#                    prediction = TRUE,
#                    metric = "rmse",
#                    verbose = 0,
#                    objective = "reg:squarederror")
#   
#   valor = -1*cv_xgb$evaluation_log[cv_xgb$best_iteration,][[4]]
#   return(list(Score=valor,Pred=0))
# }
# 
# start_time <- Sys.time()
# set.seed(333)
# opt_xgb <- BayesianOptimization(fun_opt_xgb,
#                                 bounds = list(num_arvore = c(100L, 1000L),
#                                               max_profun = c(2L,10L),
#                                               aprend = c(0,1)),
#                                       init_points = 5, n_iter = 30,
#                                       acq = "ei",
#                                       verbose = TRUE)
# end_time <- Sys.time()
# end_time - start_time
# 
# 
# #=============================================
# ##### Fazendo o XBG
# modelo_xgb_1 = 
#   xgboost(data = xgb_mat_treino,
#           nrounds = opt_xgb$Best_Par[[1]],
#           max_depth = opt_xgb$Best_Par[[2]],
#           eta = opt_xgb$Best_Par[[3]],
#           eval_metric = "rmse",
#           verbose = 0,
#           objective = "reg:squarederror")
# 
# predito_xgb_1 = 
#   predict(modelo_xgb_1,xgb_mat_teste)
# 
# riscos$XGBoost_1 = c(calcula_EQM_e_IC(predito_xgb_1,
#                                       dados_teste$NumMort,
#                                       length(dados_teste$NumMort)),
#                      calcula_EAM_e_IC(predito_xgb_1,
#                                       dados_teste$NumMort,
#                                       length(dados_teste$NumMort)))
# riscos




#=============================================
##### CV do xgb para escolher a iteracao

set.seed(7777)
cv_xgb <- xgb.cv(data = xgb_mat_treino,
                 nrounds = 1000,
                 nfold = 10,
                 early_stopping_rounds = 100,
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




xgb_dados_importancia = 
  xgb.importance(model = modelo_xgb_1) %>% 
  mutate(rank = dense_rank(desc(Gain)))

ggplot(data=xgb_dados_importancia[which(xgb_dados_importancia$rank <= 8),], 
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
  labs(title = "XG Boost - Importância", 
       x = "", y = "Importância")


ggsave(filename = "ETAPA1_Mod_XGB1_Importancia.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")


predito_xgb_1 = 
  predict(modelo_xgb_1,xgb_mat_teste)




riscos$XGBoost_1 = c(calcula_EQM_e_IC(predito_xgb_1,
                                      dados_teste$NumMort,
                                      length(dados_teste$NumMort)),
                     calcula_EAM_e_IC(predito_xgb_1,
                                      dados_teste$NumMort,
                                      length(dados_teste$NumMort)))


riscos















###############################
# XGBoost - Modelo 2
###############################

model.matrix(NumMort~.,data=dados_treino[,c(2:6,9)])[,-1]

xgb_mat_treino2 = 
  xgb.DMatrix(model.matrix(log_NumMorte~.,
                           data=dados_treino[,c(20,indice_col_floresta)])[,-1],
              label = dados_treino$log_NumMorte)

xgb_mat_teste2 = 
  xgb.DMatrix(model.matrix(log_NumMorte~.,
                           data=dados_teste[,c(20,indice_col_floresta)])[,-1],
              label = dados_teste$log_NumMorte)


#=============================================
##### CV do xgb para escolher a iteracao


# fun_opt_xgb = function(num_arvore,max_profun,aprend){
#   
#   cv_xgb <- xgb.cv(data = xgb_mat_treino,
#                    nrounds = num_arvore,
#                    max_depth=max_profun,
#                    eta=aprend,
#                    nfold = 10,
#                    early_stopping_rounds = 30,
#                    prediction = TRUE,
#                    metric = "rmse",
#                    verbose = 0,
#                    objective = "reg:squarederror")
#   
#   valor = -1*cv_xgb$evaluation_log[cv_xgb$best_iteration,][[4]]
#   return(list(Score=valor,Pred=0))
# }
# 
# start_time <- Sys.time()
# set.seed(333)
# opt_xgb <- BayesianOptimization(fun_opt_xgb,
#                                 bounds = list(num_arvore = c(100L, 1000L),
#                                               max_profun = c(2L,10L),
#                                               aprend = c(0,1)),
#                                       init_points = 5, n_iter = 30,
#                                       acq = "ei",
#                                       verbose = TRUE)
# end_time <- Sys.time()
# end_time - start_time
# 
# 
# #=============================================
# ##### Fazendo o XBG
# modelo_xgb_1 = 
#   xgboost(data = xgb_mat_treino,
#           nrounds = opt_xgb$Best_Par[[1]],
#           max_depth = opt_xgb$Best_Par[[2]],
#           eta = opt_xgb$Best_Par[[3]],
#           eval_metric = "rmse",
#           verbose = 0,
#           objective = "reg:squarederror")
# 
# predito_xgb_1 = 
#   predict(modelo_xgb_1,xgb_mat_teste)
# 
# riscos$XGBoost_1 = c(calcula_EQM_e_IC(predito_xgb_1,
#                                       dados_teste$NumMort,
#                                       length(dados_teste$NumMort)),
#                      calcula_EAM_e_IC(predito_xgb_1,
#                                       dados_teste$NumMort,
#                                       length(dados_teste$NumMort)))
# riscos




#=============================================
##### CV do xgb para escolher a iteracao

cv_xgb <- xgb.cv(data = xgb_mat_treino2,
                 nrounds = 1000,
                 nfold = 10,
                 early_stopping_rounds = 100,
                 prediction = TRUE,
                 metric = "rmse",
                 verbose = 0,
                 objective = "reg:squarederror")

### Melhor iteracao
cv_xgb$evaluation_log[cv_xgb$best_iteration,]

#=============================================
##### Fazendo o XBG
modelo_xgb_2 = 
  xgboost(data = xgb_mat_treino2,
          nrounds = cv_xgb$best_iteration,
          eval_metric = "rmse",
          verbose = 0,
          objective = "reg:squarederror")





xgb_dados_importancia = 
  xgb.importance(model = modelo_xgb_2) %>% 
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
  labs(title = "XG Boost - Importância", 
       x = "", y = "Importância")


ggsave(filename = "ETAPA1_Mod_XGB2_Importancia.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/TG/Apliacao/Etapa1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")




predito_xgb_2 = 
  exp(predict(modelo_xgb_2,xgb_mat_teste2))

riscos$XGBoost_2 = c(calcula_EQM_e_IC(predito_xgb_2,
                                      dados_teste$NumMort,
                                      length(dados_teste$NumMort)),
                     calcula_EAM_e_IC(predito_xgb_2,
                                      dados_teste$NumMort,
                                      length(dados_teste$NumMort)))
riscos








###############################
# GLM 
###############################

glm_1 = glm(NumMort ~ TempoEspera+
              Turno+
              Tempototal+
              TExt+URExt+TInt+URInt+
              TempoViagem+
              Dif_TR+
              Distancia,
            data= dados_treino,
            family = poisson(link = "log"),
            offset = log_TotalAves)

glm_1$coefficients

resumo_glm_1  = summary(glm_1)

round(resumo_glm_1$coefficients,digits=4)


predito_glm_1 = 
  predict(glm_1,dados_teste,type = 'response')

mean(predito_glm_1)

riscos$GLM_1 = 
  c(calcula_EQM_e_IC(predito_glm_1,
                     dados_teste$NumMort,
                     length(dados_teste$NumMort)),
    calcula_EAM_e_IC(predito_glm_1,
                     dados_teste$NumMort,
                     length(dados_teste$NumMort)))

riscos



###############################
# GLM 
###############################

glm_2 = glm(NumMort ~ (TempoEspera+
              Turno+
              Tempototal+
              TExt+URExt+TInt+URInt+
              TempoViagem+
              Dif_TR+
              Distancia)**2,
            data= dados_treino,
            family = poisson(link = "log"),
            offset = log_TotalAves)

glm_2$coefficients

resumo_glm_2  = summary(glm_2)

round(resumo_glm_2$coefficients,digits=4)


predito_glm_2 = 
  predict(glm_2,dados_teste,type = 'response')

mean(predito_glm_2)

riscos$GLM_2 = 
  c(calcula_EQM_e_IC(predito_glm_2,
                     dados_teste$NumMort,
                     length(dados_teste$NumMort)),
    calcula_EAM_e_IC(predito_glm_2,
                     dados_teste$NumMort,
                     length(dados_teste$NumMort)))

riscos

### deviance residual e graus de liberdade
### 



