library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

subset_salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  summarise( SERVIDORES = n(),CORRELACAO = cor(x = ( 2018 - year (DATA_INGRESSO_ORGAO)), y = (2018 -year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO))))%>%
  ungroup()%>%
  filter(SERVIDORES >=200)%>%
  arrange(SERVIDORES)%>%
  mutate(DIRECAO = (ifelse(CORRELACAO>0,'POS','NEG')))%>%
  mutate(CORRELACAO_ABSOLUTA = (ifelse(CORRELACAO>0,CORRELACAO,(CORRELACAO * (-1)))))%>%
  mutate(FORCA = ifelse(CORRELACAO_ABSOLUTA >=0.9, '5',
                        ifelse(CORRELACAO_ABSOLUTA >= 0.7 & CORRELACAO_ABSOLUTA < 0.9, '4',    
                               ifelse(CORRELACAO_ABSOLUTA >= 0.5 & CORRELACAO_ABSOLUTA < 0.7, '3',
                                      ifelse(CORRELACAO_ABSOLUTA >= 0.3 & CORRELACAO_ABSOLUTA < 0.5, '2','1')))))%>%
  select(DESCRICAO_CARGO, CORRELACAO, DIRECAO, FORCA, CORRELACAO_ABSOLUTA) -> atividade1

atividade1%>%
  select(DESCRICAO_CARGO, CORRELACAO, DIRECAO, FORCA)

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###
