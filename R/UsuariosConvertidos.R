#' Machine Learning em Marketing Digital - Prevendo Número de Usuários Convertidos
#'
#' Modelo de Machine Learning capaz de prever quantos usuários serão convertidos (ou seja, quantas pessoas comprarão os produtos da empresa) após cada campanha. Com os resultados obtidos, a empresa poderá ter uma ideia mais clara de quanto deve investir em cada campanha e qual o retorno esperado. Isso também auxiliará no planejamento da empresa para comercialização e entrega de seu produto digital, além da utilização de ferramentas e mídias sociais.
#'
#' @export
# Machine Learning em Marketing Digital - Prevendo Número de Usuários Convertidos

setwd("C:/Users/saulo/Documents/Cursos/DSA/FCD/BigDataRAzure/Cap11") # Para definir o diretório de trabalho.
getwd() # Para saber qual o diretório de trabalho.

# Carrega as bibliotecas necessárias
library(tidyverse) #para manipular os dados
library(corrplot) #para criar um mapa de correlação
library(ggplot2) #para criar outros gráficos

# Carrega o dataset
df_dsa <- read.csv("dataset.csv")

# Visualiza os dados
# Verificar o que cada variável representa. Entender o que cada variável do dataset representa
# Valores de mediana e média quando próximos indica telvez que a variável siga uma distribuição normal
View(df_dsa)

# Verifica os tipos de dados
print(str(df_dsa))

##### Análise Exploratória de Dados #####

# Sumário estatístico
# Interpretar os dados estatísticos
summary(df_dsa)

# Calculando a matriz de correlação
?cor
cor_matrix <- cor(df_dsa)

# Corrplot para criar um gráfico de correlação com base em uma matriz de correlação
# method = 'color': Especifica o método para plotar as correlações. Neste caso, escolheu cores para representar as correlações.
# type = 'upper': Indica o tipo de gráfico de correlação. Com 'upper', apenas o triângulo superior da matriz de correlação será exibido.
# addCoef.col = 'springgreen2': Define a cor para os valores dos coeficientes de correlação.
# tl.col = "black": Especifica a cor para os rótulos de texto.
# tl.srt = 45: Define o ângulo de rotação para os rótulos de texto para 45 graus.
# Obs1.: De posse do gráfico deve-se definir a variável alvo (usuarios_convertidos).
# Entre a variável alvo e as variáveis preditoras pegar a menor correlação possível =
# Entre a variável alvo e as variáveis preditoras pegar a maior correlação possível =
# Obs2.: A multicolinearidade é uma situação em que duas ou mais variáveis independentes em um modelo de regressão encontram-se altamente correlacionadas.
?corrplot
corrplot(cor_matrix,
         method = 'color',
         type = 'upper',
         addCoef.col = 'springgreen2', # adiciona o coeficiente com cor
         tl.col = "black",
         tl.srt = 45)

# Criando três gráficos com a variável preditora em X e a variável alvo em Y.
# Scatter plot entre Valor Gasto em Campanha e Usuários Convertidos
ggplot(df_dsa, aes(x=valor_gasto_campanha, y=usuarios_convertidos)) +
  geom_point(aes(color=valor_gasto_campanha), alpha=0.6) + # geom_point = gráfico de dispersão
  ggtitle("Scatter Plot entre Valor Gasto em Campanha e Usuários Convertidos") +
  xlab("Valor Gasto em Campanha") +
  ylab("Usuários Convertidos")

# Scatter plot entre Número de Visualizações e Usuários Convertidos
ggplot(df_dsa, aes(x=numero_visualizacoes, y=usuarios_convertidos)) +
  geom_point(aes(color=numero_visualizacoes), alpha=0.6) +
  ggtitle("Scatter Plot entre Número de Visualizações e Usuários Convertidos") +
  xlab("Número de Visualizações") +
  ylab("Usuários Convertidos")

# Scatter plot entre Número de Cliques e Usuários Convertidos
ggplot(df_dsa, aes(x=numero_cliques, y=usuarios_convertidos)) +
  geom_point(aes(color=numero_cliques), alpha=0.6) +
  ggtitle("Scatter Plot entre Número de Cliques e Usuários Convertidos") +
  xlab("Número de Cliques") +
  ylab("Usuários Convertidos")

##### Modelagem #####

# Versão 1 do Modelo - Regressão Linear Múltipla (quando se tem diversas variáveis preditoras)
# cria um modelo de regressão linear onde a variável dependente é usuarios_convertidos
# e as variáveis independentes são valor_gasto_campanha, numero_visualizacoes e numero_cliques.
# A função lm() é usada para ajustar o modelo aos dados contidos no dataframe df_dsa.
?lm # Linear Models
modelo_v1 <- lm(usuarios_convertidos ~ valor_gasto_campanha + numero_visualizacoes + numero_cliques, data = df_dsa)
# a função summary() é usada para fornecer um resumo estatístico do modelo ajustado,
# incluindo coeficientes, erros padrão, estatísticas t e p-valores para os coeficientes,
# R-squared, entre outros
summary(modelo_v1)

# Componentes do Sumário

# Fórmula de regressão
# X representa a entrada ou variáveis preditoras
# y representa a saída
# a e b representam os coeficientes
# O Intercept representa o coeficente "a"
# b1, b2, b3 são os coeficientes de cada uma das variáveis peditoras
# y = a + bx
# y = a + b1x1 + b2x2 + b3x3

# Residuals
# Os resíduos são as diferenças entre os valores observados e os valores previstos pelo modelo.
# Os quartis dos resíduos (Min, 1Q, Median, 3Q, Max) dão uma ideia da distribuição dos erros. Em geral,
# você gostaria que esses valores fossem distribuídos simetricamente em torno de zero, o que indica que o
# modelo faz um bom trabalho na previsão.

# Coefficients
# (Intercept): Este é o valor da variável dependente (usuários convertidos) quando todas as variáveis
# independentes são zero. O valor é -1.563, mas o valor-p associado é maior que 0,05, o que indica que o
# intercepto não é significativamente diferente de zero neste modelo.
# Obs.: o Valor p=0,05 é amplamente usado no mercado.


# valor_gasto_campanha: O coeficiente é 0.0078, mas o valor-p associado é 0.621, o que indica que essa
# variável não é estatisticamente significativa na previsão de usuários convertidos, pelo menos neste modelo.

# numero_visualizacoes: O coeficiente é -0.0035, com um valor-p de 0.265. Isso também sugere que a variável
# não é significativa.

# numero_cliques: O coeficiente é 0.944, com um valor-p extremamente baixo (< 2e-16). Isso indica que essa
# variável é altamente significativa na previsão de usuários convertidos.

# Outras Métricas
# Residual standard error: Este é uma medida da qualidade do modelo. Quanto menor, melhor o modelo. Neste caso,
# é 6.865.

# Multiple R-squared e Adjusted R-squared: Estes são indicadores da "qualidade" do modelo em termos
# de sua capacidade de prever a variável dependente. Um valor mais próximo de 1 é geralmente melhor.
# Neste caso, eles são relativamente altos (0.8617 e 0.8609, respectivamente), o que é bom.

# F-statistic e p-value: Um teste F é realizado para determinar se o modelo como um todo é significativo.
# O valor F é 1030 e o valor-p associado é muito baixo (< 2.2e-16), indicando que o modelo é significativo.

# Interpretação Final
# O modelo parece fazer um bom trabalho na previsão de "usuários convertidos" (R-squared alto), mas apenas
# a variável "número de cliques" é estatisticamente significativa na previsão. Isso pode implicar que
# "número de cliques" é a principal variável que você deve se concentrar para entender as conversões de usuários.

# As outras variáveis (valor gasto em campanha e número de visualizações) não são significativas neste modelo,
# o que sugere que elas podem não ser úteis para prever a variável dependente, ou que outros fatores podem
# estar em jogo, como multicolinearidade.

# **Resolvendo o prolbema de multicolinearidade**
# A multicolinearidade é um fenômeno em modelos de regressão onde duas ou mais variáveis independentes
# estão altamente correlacionadas entre si. Isso pode causar problemas na interpretação
# dos coeficientes do modelo e na precisão das previsões.

# Versão 2 do Modelo - Regressão Linear Simples
# chama a variável alvo "usuarios_convertidos" e uma variável preditiva "numero_cliques"
modelo_v2 <- lm(usuarios_convertidos ~ numero_cliques, data = df_dsa)
summary(modelo_v2)

# Versão 3 do Modelo - Engenharia de Atributos Antes da Regressão Linear Múltipla
# A Engenharia de Atributos, é um processo fundamental em ciência de dados no qual
# os dados brutos são transformados em recursos significativos que podem ser usados
# por algoritmos de aprendizado de máquina para construir modelos mais eficazes.
# Criar a nova variável taxa_de_clique
df_dsa$taxa_de_clique <- df_dsa$numero_cliques / df_dsa$numero_visualizacoes
View(df_dsa)

# Verificamos se algum valor ficou igual a zero (sempre verifique quando realizar divisão de valores)
any(df_dsa$taxa_de_clique == 0)

# Calculando a matriz de correlação
cor_matrix <- cor(df_dsa)

# Corrplot
corrplot(cor_matrix,
         method = 'color',
         type = 'upper',
         addCoef.col = 'springgreen2',
         tl.col = "black",
         tl.srt = 45)

# Versão 3 do modelo
# Modelo de regressão linear multipla, pois tem duas variáveis.
# Nesse terceiro modelo as duas variáveis são estatisticamente significativas.
modelo_v3 <- lm(usuarios_convertidos ~ valor_gasto_campanha + taxa_de_clique, data = df_dsa)
summary(modelo_v3)

# Componentes do Sumário

# Residuals:
# Esta seção mostra um resumo estatístico dos resíduos (diferença entre os valores observados e os
# valores previstos pelo modelo).

# Min, 1Q, Median, 3Q, Max descrevem a distribuição dos resíduos.
# Seu objetivo é que esses valores sejam distribuídos simetricamente em torno de zero. Nesse caso, parece que
# a mediana está próxima de zero, o que é um bom sinal.

# Coefficients:
# Esta seção descreve os coeficientes do modelo de regressão.

# Estimate: A estimativa dos coeficientes. Por exemplo, para cada unidade de aumento no valor_gasto_campanha,
# a variável usuarios_convertidos aumenta em média 0.05105 unidades, mantendo a taxa_de_clique constante.

# Std. Error: O erro padrão dos coeficientes, uma medida da variação dos coeficientes.

# t value: A estatística t, usada para testar a hipótese nula de que o coeficiente é igual a zero (sem efeito).
# Um valor t alto pode indicar que a variável é significativa.

# Pr(>|t|): O valor-p associado à estatística t. Um valor muito baixo (< 0,05) indica que você pode rejeitar
# a hipótese nula. Isso significa que o coeficiente é estatisticamente significativo para prever a variável alvo.

# Todos os coeficientes são altamente significativos (p-valor < 2e-16), indicando que ambos são importantes preditores da variável alvo.

# Outras Estatísticas:
# Residual standard error: É uma medida da qualidade do ajuste do modelo aos dados. Quanto menor, melhor,
# embora deva ser interpretado no contexto do problema.

# Multiple R-squared e Adjusted R-squared: São medidas que indicam a proporção da variação na variável
# dependente que é explicada pelo modelo. O seu valor é de 0,8418, o que é relativamente alto e indica um bom ajuste.

# F-statistic e p-value: Estas estatísticas testam a hipótese nula de que todos os coeficientes de regressão
# são iguais a zero. Dado o valor extremamente baixo do valor-p, você pode rejeitar essa hipótese.

# Interpretação:

# O modelo explica aproximadamente 84,18% da variação em usuarios_convertidos, o que é bom.

# O coeficiente para valor_gasto_campanha é 0,05105 e para taxa_de_clique é 3613. Isso significa que,
# mantendo todas as outras variáveis constantes, um aumento de uma unidade em valor_gasto_campanha resultará
# em um aumento de 0,05105 unidades em usuarios_convertidos e um aumento de uma unidade na taxa_de_clique
# aumentará usuarios_convertidos em 3613 unidades.

# Todos os preditores são significativos, com valores-p muito baixos.

# O modelo é estatisticamente significativo, conforme indicado pelo valor-p próximo a zero para a estatística F.

# Lembre-se de que essas são interpretações puramente estatísticas.
# A validade prática desses resultados deve ser avaliada no contexto do problema de negócio que você está
# tentando resolver.

# Vamos checar as suposições do modelo de regressão:

# Obter os resíduos do modelo
# resíduos = erros do modelo
residuals <- resid(modelo_v3)

# Gráfico de Resíduos vs Valores Ajustados
# Este gráfico ajuda a verificar a suposição de homocedasticidade.
# Você espera ver uma nuvem de pontos que não exiba nenhum padrão claro.
ggplot(df_dsa, aes(x = predict(modelo_v3), y = residuals)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  ggtitle("Resíduos vs Valores Ajustados") +
  xlab("Valores Ajustados") +
  ylab("Resíduos")

# Histograma dos Resíduos
# Este gráfico ajuda a verificar a normalidade dos resíduos.
# Um histograma em forma de sino indica que os resíduos estão normalmente distribuídos, indicando que temos um
# bom modelo de regressão.
ggplot(df_dsa, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.7) +
  ggtitle("Histograma dos Resíduos") +
  xlab("Resíduos")

# QQ-plot
# Este gráfico também ajuda a verificar a normalidade dos resíduos.
# Pontos alinhados em torno da linha diagonal sugerem que os resíduos são normalmente distribuídos, indicando
# que temos um bom modelo de regressão.
ggplot(df_dsa, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ-Plot dos Resíduos") +
  xlab("Quantis Teóricos") +
  ylab("Quantis Amostrais")

# Deploy do Modelo

# Salva o modelo treinado em disco
save(modelo_v3, file = "modelo_v3.RData")

# Carrega o modelo do disco
load("modelo_v3.RData")

# Novos dados:
valor_gasto_campanha <- 1350
numero_visualizacoes <- 7300
numero_cliques <- 100

# Criar novos dados para previsão
novos_dados <- data.frame(valor_gasto_campanha = c(1350),
                          numero_visualizacoes = c(7300),
                          numero_cliques = c(100))

# Cria a nova variável conforme foi feito para treinar o modelo
# Toda e qualquer transformação aplicada aos dados de treino
# deve ser aplicada aos dados de teste e novos dados
novos_dados$taxa_de_clique <- novos_dados$numero_cliques / novos_dados$numero_visualizacoes

# Remove as variáveis que não serão usadas
novos_dados$numero_visualizacoes <- NULL
novos_dados$numero_cliques <- NULL

# Visualiza
View(novos_dados)

# Fazer previsões
previsoes <- predict(modelo_v3, newdata = novos_dados)

# Exibir previsões
cat("Esperamos este número de usuários convertidos:", as.integer(previsoes))

# Fim
