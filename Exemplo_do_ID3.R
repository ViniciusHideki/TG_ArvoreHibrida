#############################
# Pacote
#############################

if(!require(data.tree)){install.packages("data.tree")}
library(data.tree)

#############################
# Funcoes
#############################

### funcao pra ver se o primeiro no eh puro
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

### calcula a entropia
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

### calcula o ganho de informacao
InformationGain <- function( tble ) {
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}


### treina a arvore
TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  if (IsPure(data)) {
    
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    
    feature <- names(which.max(ig))
    node$feature <- feature
    
    childObs <- split(data[ ,names(data) != feature, drop = FALSE], 
                      data[ ,feature], 
                      drop = TRUE)
    
    for(i in 1:length(childObs)) {
      
      child <- node$AddChild(names(childObs)[i])
      TrainID3(child, childObs[[i]])
    }
  }
}

### predicao
Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}


#############################
# Exemplo das series
#############################

### Criando as covariaveis
rede_de_tv = c("A","A","A","B","B","B",
               "C","C","B","A")

forma = c("dublado","legendado","dublado","legendado",
          "dublado","dublado","dublado","legendado",
          "dublado","dublado")

premiacao = c("ganhou","indicado","indicado","indicado",
              "ganhou","nao_indicado","indicado","ganhou",
              "indicado","indicado")

### Variavel resposta
sucesso = c("sim","nao","sim","sim","nao",
            "nao","nao","nao","sim","sim")

### Banco de dados
dados1_treino = data.frame(rede_de_tv,
                           forma,
                           premiacao,
                           sucesso)

dados1_treino

### Criando a Arvore
tree <- Node$new("series")
TrainID3(tree, dados1_treino)
print(tree, "feature", "obsCount")

dados1_teste = data.frame(c("A","A","C"),
                          c("legendado","dublado","legendado"),
                          c("ganhou","nao_indicado","nao_indicado"),
                          c("sim","sim","nao"))

### Predizendo valores
Predict(tree, c(rede_de_tv = "A", 
                forma =  "legendado", 
                premiacao = "ganhou"))