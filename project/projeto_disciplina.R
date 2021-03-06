#Everton Thomas, Gustavo Emmel e Michel Batalha

# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?
tmp <- merge(insta_products, products, by = 'product_id', all.y = TRUE) %>%
  mutate(n_v = ifelse(is.na(order_id), 0, 1)) %>%
  select(product_id, product_name, n_v) %>%
  group_by(product_id, product_name) %>%
  summarise(num_vendas = sum(n_v)) %>%
  filter(num_vendas == 0)

paste('Qtd. de produtos nunca comprados: ', nrow(tmp))

#------------------------------------------------------------------------

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

df_2 <- 
  merge( merge(products, departments), aisles) %>%
  as.data.frame()


#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

df_3 <- df_2 %>%
  group_by(department_id, department, aisle_id, aisle) %>%
  summarise(num_products = n()) %>% 
  arrange(desc(num_products)) %>%
  head(10) %>%
  as.data.frame()

print.data.frame(df_3)

#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

df_corredor_depto <- merge(df_2, df_3)

cnt_pedidos <- merge( df_corredor_depto, insta_products, by = "product_id") %>%
  select(order_id) %>%
  distinct() %>%
  count()

total_pedidos <- nrow(insta_orders)

paste("percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior: ", round(cnt_pedidos * 100 / total_pedidos, digits = 2), "%")

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

prod_ped <- insta_products %>%
  left_join(products, by = "product_id") %>%
  left_join(departments, by="department_id") %>%
  left_join(aisles, by="aisle_id") %>%
  filter(department != "missing" | aisle != "missing")

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4

full_df <- 
  merge(prod_ped, insta_orders)


# Transforme as variáveis user_id, department e aisle em factor
full_df$user_id <- factor(full_df$user_id)
full_df$department <- factor(full_df$department)
full_df$aisle <- factor(full_df$aisle)
# Transforme a variável order_hour_of_day em um factor ordenado (ordered)
full_df$order_hour_of_day <- factor(full_df$order_hour_of_day, ordered = TRUE)
# Este dataframe deverá ser utilizado em todas as atividades seguintes

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
hr_mais_ped <- full_df %>%
  select(order_id, user_id, order_hour_of_day) %>%
  distinct() %>%
  group_by(order_hour_of_day)

top_5_hr <- hr_mais_ped %>%
  group_by(order_hour_of_day) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(5)

top_5_hr %>% View()

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
top_15_prods <- full_df %>%
  filter(order_hour_of_day %in% as.factor(top_5_hr$order_hour_of_day)) %>%
  group_by(product_id, product_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15) 

top_15_prods %>%
  View()

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
full_df %>%
  filter(product_id %in% as.factor(top_15_prods$product_id))  %>%
  group_by(order_dow, order_hour_of_day, product_name) %>%
  summarise(qtd_vendas = n()) %>%
  group_by(order_hour_of_day, product_name) %>%
  summarise(med_vendas_hora = mean(qtd_vendas)) -> med_vendas_hora

# e faça um gráfico de linhas mostrando a venda média por hora destes produtos.

ggplot(med_vendas_hora, aes(x=order_hour_of_day, y=med_vendas_hora)) +
  geom_line(aes(group=product_name, color=product_name))

# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais? 
#Resposta: Geralmente esses produtos possuem um padrão de venda semelhante, apesar de alguns possuirem mais vendas no mesmo
#horário em que outros possuem poucas vendas. É possível verificar que dois produtos se destacam na venda as 00 horas.
#Há um decréscimo na venda entre 01 e 04 horas para todos os produtos, bem como um acréscimo após as 06 horas, o pico entre
#as 10 e 16 horas (aprox), e uma queda nas vendas a partir das 16 horas (aprox).


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
est_descr <- 
  full_df %>%
  group_by(order_hour_of_day) %>%
  summarise(mean = mean(order_id), sd = sd(order_id), max = max(order_id), min = min(order_id), cnt = n()) %>% ungroup()

full_df %>%
  group_by(order_hour_of_day) -> x

# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 
# R.: Sim, conforme mostra o gráfico, a distribuição é uma gaussiana, 
#já que possui um pico próximo a média e as extremidades são menores, no formato de sino.

#{VALIDAR}

ggplot(x, aes(x=order_hour_of_day)) +
  geom_histogram(stat = "count" )


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

full_df %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(qtd_vendas = n()) %>%
  group_by(order_hour_of_day) %>%
  summarise(med = mean(qtd_vendas)) %>%
  mutate(sd_abaixo = med - 2 * sd(med), 
         sd_acima = med + 2 * sd(med)) -> summary_product

ggplot(summary_product, aes(x=order_hour_of_day, y=med, ymin=sd_abaixo, ymax=sd_acima, group=1)) +
  geom_line() + 
  geom_ribbon(fill = "orange", alpha = 0.5) + 
  geom_jitter(alpha = .2, height = 0, width = 0.3)


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

full_df %>%
  group_by(order_dow) %>%
  count(order_id) -> dow_group

ggplot(dow_group, aes(x=order_dow, group=order_dow)) +
  geom_boxplot(aes(y=n)) +
  scale_x_continuous( breaks = 0:6 ) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",")) +
  labs( x = "Dia Semana", y = "Qtde Pedidos")


#13 # Identifique, por usuário, o tempo médio entre pedidos
tempo_med_entre_pedidos <- full_df %>%
  group_by(user_id) %>%
  summarise(tempo = mean(days_since_prior_order))


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado
tempo_med_entre_pedidos %>% 
  group_by(tempo) %>%
  count() %>%
  View()

ggplot(tempo_med_entre_pedidos, aes(x=tempo)) +
  geom_bar( alpha = 0.5, fill="orange", color = "orange" ) +
  scale_x_continuous() +
  labs( x = "Tempo médio entre pedidos (em dias)", y = "Qtde usuarios" )

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 

ggplot(full_df, aes(x=days_since_prior_order)) +
  geom_bar( alpha = 0.5, fill="orange", color = "orange" ) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs( x = "Nro. dias do pedido anterior", y = "Qtde usuarios" )

#Sim, os gráficos são bem semelhantes

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

user_min_cinco_pedidos <- full_df %>%
  group_by(user_id) %>%
  summarize(num_pedidos = n()) %>%
  filter(num_pedidos >= 5) %>%
  ungroup()

tempo_med_entre_pedidos %>%
  filter(user_id %in% user_min_cinco_pedidos$user_id) -> tmp

ggplot(tmp, aes(x=tempo)) +
  geom_bar( alpha = 0.5, fill="orange", color = "orange" ) +
  scale_x_continuous() +
  labs( x = "Tempo médio entre pedidos (em dias)", y = "Qtde usuarios" )

#Sim, o padrão praticamente se mantém.

#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
bananas <- c(24852, 13176, 39276, 37067, 29259)

full_df %>%
  filter(product_id %in% bananas) %>%
  group_by(order_id, product_id) %>% 
  count() %>%
  group_by(order_id) %>%
  summarise(num_bananas = n()) %>%
  filter(num_bananas > 1) -> ped_bananas

paste("Resposta: ", nrow(ped_bananas), "pedidos possuem mais de um tipo de banana")


#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

full_df %>%
  filter(order_id %in% ped_bananas$order_id & product_id %in% bananas) %>%
  group_by(order_id, product_id) %>%
  summarise(cnt_bananas = n()) -> ped_tmp

ped_tmp %>%
  group_by(product_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(3) %>% pull(var = product_id) -> vet_bananas

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 

full_df %>%
  filter(product_id %in% vet_bananas) %>%
  group_by(order_dow, order_hour_of_day) %>%
  count(product_id) %>% 
  summarise(med = mean(n)) -> ped_med_hora_semana

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora

ggplot(ped_med_hora_semana, aes(x=order_dow, y=order_hour_of_day, size = med)) +
  geom_point(colour = "orange", fill = "orange", alpha = 0.5) +
  scale_x_continuous(breaks = as_vector(ped_med_hora_semana$order_dow)) +
  labs(x = "Dia da Semana", y = "Hora do dia") + 
  ggtitle("Média de pedidos de bananas")


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana

ggplot(x, aes(x=order_hour_of_day)) +
  geom_histogram(stat = "count" )

ggplot(ted_main_transf, aes( x = views)) +
  geom_histogram(bins = 1000) +
  scale_x_continuous(labels = scales::comma_format()) +
  facet_wrap(~ ano_publicacao, ncol = 3) + 
  labs( y = 'Total de apresentações',
        x = 'Visualizações',
        title = 'Histograma de visualizações facetado por ano')


ggplot(ped_med_hora_semana, aes(x=order_hour_of_day, y=med)) +
  geom_histogram(stat = "identity") +
  facet_wrap(~ order_dow, ncol=2) +
  labs( y = 'Média',
        x = 'Hora do dia',
        title = 'Histograma de média de pedidos de bananas por dias da semana')
#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

wilcox.test(med ~ order_dow, 
            data = ped_med_hora_semana, 
            alternative = "two.sided", 
            subset = order_dow %in% c(3, 4), 
            conf.int = TRUE, exact = FALSE)

ped_med_hora_semana %>%
  filter(order_dow %in% c(3,4)) -> vendas_hora

pairwise.wilcox.test(vendas_hora$med, 
                     vendas_hora$order_dow, 
                     p.adjust.method = "BH", exact = FALSE)

