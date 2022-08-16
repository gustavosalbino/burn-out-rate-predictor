#=====
#=== Programa de analise do nivel de esgotamento em determinadas empresas
#=== Am?byle Heck
#=== Gustavo Schmitz Albino
#=== Jackson Denner
#=====

# Imports
library(ggplot2)
library(RColorBrewer)
library(e1071)
library(dplyr)

# Leitura dos dados
dados <- read.table("train.csv",
                    stringsAsFactors = T,
                    header=TRUE,
                    sep = ",")

dados_test <- read.table("test.csv",
                    stringsAsFactors = T,
                    header=TRUE,
                    sep = ",")
head(dados_test)
head(dados)


# AJUSTE PARA LOG SE NECESSARIO
#
# dados <- data.frame(dados, # arrumando o data frame
#                    xtr = log(dados$bmi), # adicionando a coluna xtr com ln(x)
#                    ytr = log(dados$charges)) # adicionando a coluna ytr com ln(y)

## Analise de ralacoes graficas entre as variaveis

# Relacao entre nivel de fadiga mental e nivel de esgotamento
ggplot(data=dados, aes(x=Mental.Fatigue.Score,y=Burn.Rate ))+
  geom_point() +
  labs(x = 'Mental Fatigue Score', y = 'Burn Rate') +
  theme_minimal()


# Relacao entre horas diarias de trabalho e nivel de esgotamento
ggplot(data=dados, aes(x=Resource.Allocation,y=Burn.Rate ))+
  geom_point() +
  labs(x = 'Resource Allocation (Daily work hours)', y = 'Burn Rate') +
  theme_minimal()

# Relacao entre disponibilidade de home office e nivel de esgotamento
ggplot(data=dados, aes(x=WFH.Setup.Available,y=Burn.Rate ))+
  geom_boxplot() +
  labs(x = 'WFH Setup Available', y = 'Burn Rate') +
  theme_minimal()

# Relacao entre genero e nivel de esgotamento
ggplot(data=dados, aes(x=Gender,y=Burn.Rate ))+
  geom_boxplot() +
  labs(x = 'Gender', y = 'Burn Rate') +
  theme_minimal()



# Separacao colorida de genero para diferenciar nos 2 plots abaixo
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(dados$Gender )
colScale <- scale_colour_manual(name = "Gender ",values = myColors)

# Grafico colorido diferenciando o genero na curva fadiga mental x burn rate
p <- ggplot(dados,aes(Mental.Fatigue.Score,Burn.Rate,colour = Gender )) + geom_point() + labs(x = 'Mental Fatigue Score', y = 'Burn Rate')
p1 <- p + colScale
p1

# Grafico colorido diferenciando o genero na curva horas de trabalho diarias x burn rate
p <- ggplot(dados,aes(Resource.Allocation,Burn.Rate,colour = Gender )) + geom_point() + labs(x = 'Resource Allocation', y = 'Burn Rate')
p1 <- p + colScale
p1



## Histograma e parametros

# Plot do histograma de burn rate
qplot(dados$Burn.Rate, geom="histogram")

# Parametros de kurtosis e skewness (provavelmente necessario tirar as celulas em branco)
kurtosis(dados$Burn.Rate)
skewness(dados$Burn.Rate)




## Transforma o dataset substituindo os valores NA pela media da coluna
# Pega as colunas que tem linhas com valores NA
list_na <- colnames(dados)[ apply(dados, 2, anyNA) ]
list_na

# Cria a media
average_missing <- apply(dados[,colnames(dados) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing

# Create a new variable with the mean and median
dados_replace <- dados %>%
  mutate(replace_mean_resource  = ifelse(is.na(Resource.Allocation), average_missing[1], Resource.Allocation),
         replace_mean_mental_fatigue = ifelse(is.na(Mental.Fatigue.Score), average_missing[2], Mental.Fatigue.Score),
         replace_mean_burn_rate = ifelse(is.na(Burn.Rate), average_missing[3], Burn.Rate))

# Verificacao de funcionamento, os sum de NA deve retornar 0
sum(is.na(dados_replace$replace_mean_resource))
sum(is.na(dados_replace$replace_mean_mental_fatigue))
sum(is.na(dados_replace$replace_mean_burn_rate))

# Muda Yes para 1 e No para 0 no WFH
dados_replace$WFH.Setup.Available<-ifelse(dados$WFH.Setup.Available=="Yes",1,0)

head(dados_replace)

# Relacao entre nivel de fadiga mental e nivel de esgotamento apos ajusta pela media
ggplot(data=dados_replace, aes(x=replace_mean_mental_fatigue,y=replace_mean_burn_rate ))+
  geom_point() +
  labs(x = 'Mental Fatigue Score', y = 'Burn Rate') +
  theme_minimal()


# Relacao entre horas diarias de trabalho e nivel de esgotamento apos ajuste pela media
ggplot(data=dados_replace, aes(x=replace_mean_resource,y=replace_mean_burn_rate ))+
  geom_point() +
  labs(x = 'Resource Allocation (Daily work hours)', y = 'Burn Rate') +
  theme_minimal()

# Parametros de kurtosis e skewness ap?s ajuste pela media
kurtosis(dados_replace$replace_mean_burn_rate)
skewness(dados_replace$replace_mean_burn_rate)



# Ajuste do modelo de regressao linear multipla sem tratamento dos dados
modelo <- lm(replace_mean_burn_rate ~ replace_mean_mental_fatigue + WFH.Setup.Available + replace_mean_resource,
             data=dados_replace)
coef(modelo) # imprimir coeficientes do modelo
# reta de minimos quadrados: Y=-0.06031922 + 0.06433933mental_fatigue -0.01615350WFH + 0.03403030resource

summary(modelo)

# Realizar predicao para variavel resposta
predict(modelo, # modelo ajustado
        newdata=data.frame(replace_mean_mental_fatigue=7.7, # apontar os valores das variaveis explicativas
                           WFH.Setup.Available=0,
                           replace_mean_resource=5))
# Realizar predicao para variavel resposta
predict(modelo, # modelo ajustado
        newdata=data.frame(replace_mean_mental_fatigue=3.8, # apontar os valores das variaveis explicativas
                           WFH.Setup.Available=0,
                           replace_mean_resource=3))

## Analise de residuos
# Valores preditos versus Residuos padronizados
ggplot(data = modelo) + 
  geom_point(aes(x=.fitted, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores preditos', y = 'Residuos padronizados') +
  theme_minimal()








## MESMO TRATAMENTO FEITO ATEH AQUI POREM EXCLUINDO AS LINHAS COM NA
dados_sem_na <- dados[complete.cases(dados), ]

# Muda Yes para 1 e No para 0 no WFH
dados_sem_na$WFH.Setup.Available<-ifelse(dados_sem_na$WFH.Setup.Available=="Yes",1,0)

head(dados_sem_na)


# Relacao entre nivel de fadiga mental e nivel de esgotamento apos ajusta pela media
ggplot(data=dados_sem_na, aes(x=Mental.Fatigue.Score,y=Burn.Rate ))+
  geom_point() +
  labs(x = 'Mental Fatigue Score', y = 'Burn Rate') +
  theme_minimal()


# Relacao entre horas diarias de trabalho e nivel de esgotamento apos ajuste pela media
ggplot(data=dados_sem_na, aes(x=Resource.Allocation,y=Burn.Rate ))+
  geom_point() +
  labs(x = 'Resource Allocation (Daily work hours)', y = 'Burn Rate') +
  theme_minimal()

# Parametros de kurtosis e skewness ap?s ajuste pela media
kurtosis(dados_sem_na$Burn.Rate)
skewness(dados_sem_na$Burn.Rate)



# Ajuste do modelo de regressao linear multipla sem tratamento dos dados
modelo_sem_na <- lm(Burn.Rate ~ Mental.Fatigue.Score + WFH.Setup.Available + Resource.Allocation,
             data=dados_sem_na)
coef(modelo_sem_na) # imprimir coeficientes do modelo
# reta de minimos quadrados: Y=--0.08352936 + 0.07404853mental_fatigue -0.01191907WFH + 0.02628867resource

summary(modelo_sem_na)

cor(dados_sem_na$Mental.Fatigue.Score, dados_sem_na$Burn.Rate)
cor(dados_sem_na$Resource.Allocation, dados_sem_na$Burn.Rate)

# Realizar predicao para variavel resposta
predict(modelo_sem_na, # modelo ajustado
        newdata=data.frame(Mental.Fatigue.Score=7.7, # apontar os valores das variaveis explicativas
                           WFH.Setup.Available=0,
                           Resource.Allocation=5))
# Realizar predicao para variavel resposta
predict(modelo_sem_na, # modelo ajustado
        newdata=data.frame(Mental.Fatigue.Score=3.8, # apontar os valores das variaveis explicativas
                           WFH.Setup.Available=0,
                           Resource.Allocation=3))

## Analise de residuos
# Valores preditos versus Residuos padronizados
ggplot(data = modelo_sem_na) + 
  geom_point(aes(x=.fitted, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores preditos', y = 'Residuos padronizados') +
  theme_minimal()


# Variavel explicativa 1 (fadiga mental) versus Residuos padronizados
ggplot(data = modelo_sem_na) + 
  geom_point(aes(x=Mental.Fatigue.Score, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Mental Fatigue Score', y = 'Residuos padronizados') + 
  theme_minimal()

# Variavel explicativa 2 (Resource.Allocation) versus Residuos padronizados
ggplot(data = modelo_sem_na) + 
  geom_point(aes(x=Resource.Allocation, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Resource Allocation', y = 'Residuos padronizados') + 
  theme_minimal()


# Grafico de probabilidade normal
ggplot(data = modelo_sem_na, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = 'Valores esperados pela normal', y = 'Residuos padronizados') +
  theme_minimal()

# Histograma dos Residuos padronizados
ggplot(data = modelo_sem_na) + 
  geom_histogram(aes(x = .stdresid), 
                 bins = 5, 
                 fill = 'lightgrey',
                 colour = 'black') +
  labs(x = 'Residuos padronizados', y = 'Frequencia') +
  theme_minimal()

# Teste de Shapiro-Wilk
shapiro.test(rstandard(modelo_sem_na))