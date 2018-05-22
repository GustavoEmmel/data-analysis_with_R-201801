# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)


# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted_talk <- read_csv("aula-05/data/ted_main.csv.gz")



# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

# O formato apropriado para os valores referentes a film_date e published_date seria year

ted_talk %>%
  summarise(duracao_min = min(duration))

ted_talk %>%
  summarise(film_date_min = min(film_date))

ted_talk %>%
  summarise(published_date_min = min(published_date))

ted_talk %>%
  summarise(duracao_max = max(duration))

ted_talk %>%
  summarise(film_date_max = max(film_date))

ted_talk %>%
  summarise(published_date_max = max(published_date))

ted_talk %>%
  summarise(duracao_media = mean(duration))

ted_talk %>%
  summarise(film_date_media = mean(film_date))

ted_talk %>%
  summarise(published_date_media = mean(published_date))


# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

ted_talk %>% 
  mutate( duration = duration(duration),
          published_date = (as_datetime(published_date)) ,
          film_date = (as_datetime(film_date)) 
          ) -> ted_talk

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted_talk %>% 
  mutate( event = factor(event),
          speaker_occupation = (factor(speaker_occupation))
  ) -> ted_talk


# Retire do dataframe a variável name

ted_talk %>% 
  select(-name) -> ted_talk


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
ted_talk %>%
  summary()


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.
ted_talk %>% 
  filter(languages == 0) %>%
  mutate(languages = 1) %>%
  select(description, languages)


# Verifique os 15 registros com menor data de filmagem. 
ted_talk %>%
  arrange(film_date) %>%
  head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
ted_talk %>% 
  group_by(year(film_date)) %>%
  mutate(qtde = n()) %>%
  ungroup() %>%
  select(description, qtde) -> exibicoes_por_ano

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
quantile(exibicoes_por_ano$qtde, probs = seq(from=0.1, to=1, by=0.1))

ted_talk %>% 
  group_by(film_date) %>%
  mutate(qtde = n()) %>%
  ungroup() %>%
  filter(qtde > 4) -> top_ted_talks


# Verifique novamente o resumo dos dados do dataframe
top_ted_talks %>%
  summary()



# Verifique os 10 registros com maior duração.
top_ted_talks %>%
  arrange(duration) %>%
  tail(10)


# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas




# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil




# Visualize os 10 quantis da quantidade de visualizações




# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro




# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




