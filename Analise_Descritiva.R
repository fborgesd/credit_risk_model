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
tmp <- my_data

#Exclui a linha 1 (labels das variáveis preditoras) e coluna 1 (chave primária)
tmp <- tmp[-1,-1]

#Converte Y de 1, 0 para "Y", "N":
tmp <- tmp %>% mutate(Y = ifelse(Y== 1, "Y","N"))

#Descritiva:

# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de inadimplentes por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de inadimplentes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="Y", groupvars=c(var))
  #summarySE > calcula média, desv pad, min e max para cada variável. Barra (qtd observações da categoria, ponto percentagem.)
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/30000, fill=as.factor(tgc[,var]))) + 
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Tamanho das fontes do título e dos eixos:
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold", margin(t = 0, r = 20, b = 0, l = 0))) +
    # Rótulo dos eixos
    xlab('Idade') + ylab("Taxa de inadimplência") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*30000, name = "Frequencia"), labels = scales::percent)
}

#Descritiva para cada variável X:

#X1: Valor do Crédito concedido:
tmp$X1 <- as.integer(tmp$X1)
#Vamos categorizar as variáveis contínuas para analisar
tmp$X1 <- quantcut(tmp$X1, 5) 
descritiva("X1")

#X2: Sexo: 
#(1 = male; 2 = female)
#Converte X2 de 1, 2 para "Masculino", "Feminino":
tmp <- tmp %>% mutate(X2 = ifelse(X2== 1, "masculino","femino"))
temp$X2 = as.factor(tmp$X2) #Gênero
descritiva("X2")

#X3: Experiência Educacional:
#(1 = graduate school; 2 = university; 3 = high school; 4 = others).
#(1 = pós-graduação; 2 = universidade; 3 = ensino médio; 4 = outros).
tmp <- tmp %>% mutate(X3 = ifelse(X3== 1, "pós-graduação",ifelse(X3== 2, "graduação",ifelse(X3== 3, "ensino médio","outros"))))
descritiva("X3")

#X4: Estado Civil:
#(1 = married; 2 = single; 3 = others).
#(1 = casado(a); 2 = solteiro(a); 3 = outros).
tmp <- tmp %>% mutate(X4 = ifelse(X4== 1, "casado(a)",ifelse(X3== 2, "solteiro(a)","outros")))
descritiva("X4")

#X5: Idade:
tmp$X5 <- as.integer(tmp$X5)
#Vamos categorizar as variáveis contínuas para analisar
tmp$X5 <- quantcut(tmp$X5,10) 
descritiva("X5")



