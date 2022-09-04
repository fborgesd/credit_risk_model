# 1ª Fase: Construção dos modelos aplicando as técnicas de árvore de decisão e rede neural com backpropagation

########################
# Instalação de pacotes
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'readxl',     # Leitura de arquivos xls
             'dplyr'       # Manipulação de dados
            
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#INPUTS:

# xls files
my_data <- read_excel("default of credit card clients.xls")

#Converte o formato tibble em dataframe:

my_data <- as.data.frame(my_data)

#Árvore de Decisão:
  
tmp <- my_data

#Exclui a linha 1 (labels da tabela) e coluna 1 (chave primária)
tmp <- tmp[-1,-1]

#Converte Y de 1, 0 para "Y", "N":
tmp <- tmp %>% mutate(Y = ifelse(Y== 1, "Y","N"))

# Listagem das variáveis com algumas características
tmp %>% str

#Convertendo as variáveis contínuas em inteiro ("Estão como chr na base")  

tmp$X1 <- as.integer(tmp$X1) #Valor do Crédito Concedido
tmp$X5 <- as.integer(tmp$X5)
tmp$X12 <- as.integer(tmp$X12)
tmp$X13 <- as.integer(tmp$X13)
tmp$X14 <- as.integer(tmp$X14)
tmp$X15 <- as.integer(tmp$X15)
tmp$X16 <- as.integer(tmp$X16)
tmp$X17 <- as.integer(tmp$X17)
tmp$X18 <- as.integer(tmp$X18)
tmp$X19 <- as.integer(tmp$X19)
tmp$X20 <- as.integer(tmp$X20)
tmp$X21 <- as.integer(tmp$X21)
tmp$X22 <- as.integer(tmp$X22)
tmp$X23 <- as.integer(tmp$X23)


#Variáveis qualitativas devem ser transformada para tipo fator.
tmp$Y = as.factor(tmp$Y) #Inadimplência 1>Sim, 0>Não
tmp$X2 = as.factor(tmp$X2) #Gênero
tmp$X3 = as.factor(tmp$X3) #Experiência Educacional
tmp$X4 = as.factor(tmp$X4) #Estado Civil

  #Histórico de pgto mensal de Abr-Set/2005 [X6:X11] > X6 = Sep/05 ... X11 = Abr/05
#-1 = pay duly; 1 = payment delay for one month; 
#2 = payment delay for two months; . . .; 
#8 = payment delay for eight months; 
#9 = payment delay for nine months and above.

tmp$X6 = as.factor(tmp$X6)
tmp$X7 = as.factor(tmp$X7)
tmp$X8 = as.factor(tmp$X8)
tmp$X9 = as.factor(tmp$X9)
tmp$X10 = as.factor(tmp$X10)
tmp$X11 = as.factor(tmp$X11)
  
# Listagem das variáveis com algumas características
tmp %>% str

#############################################
# Vamos construir a árvore de classificação #
arvore <- rpart(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,
                data=tmp,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta, extra = 106) # Paleta de cores


##############################
# Avaliação básica da árvore - sem validação - v0 #

# Predizendo com a árvore

# Probabilidade de inadimplência:
prob = predict(arvore, tmp)

# Classificação dos inadimplentes #Threshold
class = prob[,2]>.5

# Matriz de confusão
tab <- table(class, tmp$Y)
tab

#Acurácia:
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

#acc = 0.8196 (Gini or information)

###############################################
# Vamos separar a base em treinamento e teste #

set.seed(123)

#Separa-se a base tmp em treino e teste:
bool_treino <- stats::runif(dim(tmp)[1])>.25

treino <- tmp[bool_treino,] #25%
teste  <- tmp[!bool_treino,] #75%

set.seed(123)

#Árvore bem complexa: Folhas 2^30 (max depth), minsplit = 1 e cp = 0 (10% mínimo default do R)
arvore <- rpart::rpart(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,
                       data=treino,
                       method='class',
                       xval=5,
                       control = rpart.control(cp = 0, 
                                               minsplit = 1,  
                                               maxdepth = 30)
)
                       
#Verificando a complexidade da árvore
arvore$frame

# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))

#Ponto de corte 0.5, ou seja , caso probabilidade de inadimplência
#seja maior do que 0.5, a observação é classificada como "Sim" (inadimplente)

p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_treino, treino$Y)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

tab <- table(c_teste, teste$Y)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de teste: %s ', percent(acc))

###############################
# Curva ROC                   #
###############################

#Receiver Operator Characteristic: possibilita determinar de forma gráfica 
#qual o melhor threshold para o problema, ie, qual deve ser o corte de
#probabilidade pra considerar inadimplente x não inadimplente
#A ROC sumariza as matrizes de confusão para cada threshold

#AUC: Area Under Curve: permite comparar duas curvas ROC diferentes,
# a maior AUC representa a melhor alternativa

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2

aval_treino <- data.frame(obs=treino$Y, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

#Podemos usar o mesmo dataframe para fazer a curva ROC:
library("plotROC")

CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$Y, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

##########################

##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]
#Seleciona-se o cp mínimo para podar a árvore:
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda <- rpart::rpart(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,
                            data=treino,
                            method='class',
                            xval=5,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)


p_treino = stats::predict(arvore_poda, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore_poda, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####

#Avaliação da Base de Treino:
aval_treino <- data.frame(obs=treino$Y, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

tab <- table(c_treino, treino$Y)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$Y, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

tab <- table(c_teste, teste$Y)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de teste: %s ', percent(acc))

##########################################
# GRID SEARCH DE FORMA MANUAL Cross-validation ao longo dos CPs     #


# Plotando AUC vs CP por treino e teste
#Inicializa o objeto stats, que vai guardar o AUC dos modelos
stats <- data.frame(NULL)

# Loop ao longo dos valores de CP
for (cp in tab_cp[2:dim(tab_cp)[1],'CP']){
  
  # Treinar a árvore alterando o cp
  arvore <- rpart(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,
                  data=treino,
                  method='class',
                  control = rpart.control(cp = cp, 
                                          minsplit = 1, 
                                          maxdepth = 30),
                  xval=0
  )
  
  # Avaliar a árvore na base de treino
  p_treino = predict(arvore, treino)
  c_treino = factor(ifelse(p_treino[,2]>.5, "Y", "N"))
  
  aval_treino <- data.frame(obs=treino$Y, 
                            pred=c_treino,
                            Y = p_treino[,2],
                            N = 1-p_treino[,2]
  )
  aval_treino
  av_treino <- twoClassSummary(aval_treino, lev=levels(aval_treino$obs))
  
  # Avaliar base de teste  
  p_teste = predict(arvore, teste)
  c_teste = factor(ifelse(p_teste[,2]>.5, "Y", "N"))
  aval_teste <- data.frame(obs=teste$Y, 
                           pred=c_teste,
                           Y = p_teste[,2],
                           N = 1-p_teste[,2]
  )
  
  av_teste <- twoClassSummary(aval_teste, lev=levels(aval_teste$obs))
  
  # Acumular as informações de cp e AUC para cada árvore  
  stat <- cbind(cp, ROC_treino=av_treino[1], ROC_teste=av_teste[1])
  stats <- rbind(stats, stat)
  
}
stats

ggplot(stats) +
  geom_point(aes(x=cp, y=ROC_treino, col='treino')) +
  geom_point(aes(x=cp, y=ROC_teste, col='teste')) +
  scale_color_viridis_d(begin=.4, end=.8) +
  theme(legend.position = "bottom") +
  ggtitle("Curva ROC - base de treino") +
  ylab("AUC") +
  labs(colour='Base') 

#Visualiando a Árvore:

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore_poda,
                       box.palette = paleta) # Paleta de cores


