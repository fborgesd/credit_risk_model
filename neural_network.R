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

#Convertendo as variáveis contínuas em numérico ("Estão como chr na base")  
tmp[1:24] <- lapply(tmp[1:24], as.numeric)

# Checar os formatos dos campos:
tmp %>% str

# Padroniza colunas quantitativas entre 0 e 1:
cols <- c("X1", "X5", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "X20", "X21", "X22", "X23")

# Função para padronizar
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# Padronizando as variáveis quantitativas:
tmp[cols] <- lapply(tmp[cols], range01)

tmp %>% head

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

#frees up the memory for the variables that you no longer have access to:
gc()

rn1 <- neuralnet(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + 
                 X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + 
                 X22 + X23,
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




