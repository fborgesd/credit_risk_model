##Perceptron multicamada:

# Instalação de pacotes:
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'viridis',
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'neuralnet',  # Pacote para fazer redes neurais
             'gamlss',
             'gamlss.add',
             'readxl',     # Leitura de arquivos xls
             'fastDummies'#Dummyzaçao de categóricas
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

# Importa os dados para um dataframe:

# xls files
my_data <- read_excel("default of credit card clients.xls")

## Data Prep:

# Exclui a linha 1 (labels da tabela) e coluna 1 (chave primária)
tmp <- my_data[-1,-1]

# Remove duplicatas:
tmp <- unique( tmp)

# Droppa linhas com nulos:
tmp <- na.omit(tmp)

# Checar os formatos dos campos:
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

# Checar os formatos dos campos:
tmp %>% str

# Padroniza colunas quantitativas entre 0 e 1:
cols <- c("X1", "X5", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "X20", "X21", "X22", "X23")

# Função para padronizar
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# Padronizando as variáveis quantitativas:
tmp[cols] <- lapply(tmp[cols], range01)

tmp %>% head


#

# One hot Encoding: transformando variáveis categóricas em colunas de 1 e 0:
tmp <- dummy_cols(.data = tmp, select_columns = c('X2', 'X3', 'X4', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11'),
                  remove_selected_columns = TRUE)

#Renomeia as subcolunas geradas com classe negativa, pois ficam no formato X_Y-Z e travam a rede neural:
colnames(tmp)[which(names(tmp) == "X6_-1")] <- "X6_M1"
colnames(tmp)[which(names(tmp) == "X6_-2")] <- "X6_M2"
colnames(tmp)[which(names(tmp) == "X7_-1")] <- "X7_M1"
colnames(tmp)[which(names(tmp) == "X7_-2")] <- "X7_M2"
colnames(tmp)[which(names(tmp) == "X8_-1")] <- "X8_M1"
colnames(tmp)[which(names(tmp) == "X8_-2")] <- "X8_M2"
colnames(tmp)[which(names(tmp) == "X9_-1")] <- "X9_M1"
colnames(tmp)[which(names(tmp) == "X9_-2")] <- "X9_M2"
colnames(tmp)[which(names(tmp) == "X10_-1")] <- "X10_M1"
colnames(tmp)[which(names(tmp) == "X10_-2")] <- "X10_M2"
colnames(tmp)[which(names(tmp) == "X11_-1")] <- "X11_M1"
colnames(tmp)[which(names(tmp) == "X11_-2")] <- "X11_M2"

# Checar os formatos dos campos:
tmp %>% str

# Converte inteiro do ohe e da variável target em numérico:
tmp[15:92] <- lapply(tmp[15:92], as.numeric)

# Checar os formatos dos campos:
tmp %>% str


## Pré-processamento:

# Vamos separar a base em treinamento e teste #

set.seed(123)

# Separa-se a base tmp em treino e teste:
bool_treino <- stats::runif(dim(tmp)[1])>.25

treino <- tmp[bool_treino,] #25%
teste  <- tmp[!bool_treino,] #75%

set.seed(123)

## Modelagem:

# Perceptron Multicamada: Treinando o modelo
tempo_ini <- Sys.time()

# rn1 <- neuralnet(Y ~ X1 + X5 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 +
#                  X22  +  X23 +  Y  + X2_1 + X2_2 + X3_0 + X3_1 + X3_2 + X3_3 + X3_4 +  X3_5 + X3_6 +
#                  X4_0 + X4_1 + X4_2 + X4_3 + X6_M1 + X6_M2 +X6_0 + X6_1 + X6_2 + X6_3 + X6_4 + X6_5 +
#                  X6_6 + X6_7 + X6_8 + X7_M1 + X7_M2 + X7_0 + X7_1 + X7_2 + X7_3 + X7_4 + X7_5 + X7_6 +
#                  X7_7 + X7_8 + X8_M1 +X8_M2 + X8_0 + X8_1 + X8_2 + X8_3 + X8_4 + X8_5 + X8_6 + X8_7 +
#                  X8_8 + X9_M1 +  X9_M2 +X9_0 + X9_1 + X9_2 +X9_3 + X9_4 + X9_5 + X9_6 + X9_7 + X9_8 +
#                  X10_M1 + X10_M2 + X10_0 + X10_2 + X10_3 + X10_4 + X10_5 + X10_6 + X10_7 + X10_8 + X11_M1 + X11_M2 +
#                  X11_0 + X11_2 + X11_3 + X11_4 + X11_5 + X11_6 + X11_7 + X11_8,
#                  data=treino,
# #                 hidden = c(10,5))
#                  hidden = c(1,1))

rn1 <- neuralnet(Y ~ X1 + X5 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 +
                 X22  +  X23 +  Y  + X2_1 + X2_2 + X3_0 + X3_1 + X3_2 + X3_3 + X3_4 +  X3_5 + X3_6 +
                 X4_0 + X4_1 + X4_2 + X4_3 + X6_M1 + X6_M2 +X6_0 + X6_1 + X6_2 + X6_3 + X6_4 + X6_5 +
                 X6_6 + X6_7 + X6_8 + X7_M1 + X7_M2 + X7_0 + X7_1 + X7_2 + X7_3 + X7_4 + X7_5 + X7_6 +
                 X7_7 + X7_8 + X8_M1 +X8_M2 + X8_0 + X8_1 + X8_2 + X8_3 + X8_4 + X8_5 + X8_6 + X8_7 +
                 X8_8 + X9_M1 +X9_M2 +X9_0 + X9_1 + X9_2 +X9_3 + X9_4 + X9_5 + X9_6 + X9_7 + X9_8 +
                 X10_M1 + X10_M2 + X10_0 + X10_2 + X10_3 + X10_4 + X10_5 + X10_6 + X10_7 + X10_8 + X11_M1 + X11_M2 +
                 X11_0 + X11_2 + X11_3 + X11_4 + X11_5 + X11_6 + X11_7 + X11_8,
                 data = treino,
                 rep = 1000,
                 #hidden = c(10,5))
                 hidden = c(1,1))

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

#Plota a Rede Neural:
plot(rn1)

#Fazendo previsões:
treino_std['pred1'] <- predict(rn1, treino_std)

# Valores esperados e observados
boost0_O_vs_E <- ggplot(treino_std, aes(x,y)) + # gráfico base >> x vs y <<
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x1,pred1, colour='Esperado')) + # Gráfico sobreposto >> x vs pred
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Y") +
  scale_x_continuous(name= "X")

boost0_O_vs_E

##Métricas de Performance:




