# bibliotecas utilizadas
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")
library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)

ted_talks <- read_csv("aula-05/data/ted_main.csv.gz") %>%
  mutate( film_date = as_datetime(film_date) %>% as_date()
          , published_date = as_datetime(published_date)) %>%
  filter(published_date >= ymd(20120101))%>%
  filter(published_date <= ymd(20171231))%>%
  select(title, views, published_date) -> subset_multifacetado

subset_multifacetado %>%
  mutate( year = year( published_date )) %>%
  group_by(year) %>%
  summarise(sum_views = sum(views)) -> subset_recentes

ggplot(subset_recentes, aes(x = year)) +
geom_histogram(binwidth = 0.1) +
geom_histogram(color="black", fill="white") +
geom_histogram(aes(weight = sum_views), binwidth = 0.1) + ylab("visualições") + labs(title="QUANTIDADE DE VISUALIZAÇÕES POR ANO")
