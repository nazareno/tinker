library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)

setwd("C:/Users/mczfe/Transparencia Brasil/Projetos/Tinker/Bases de dados/Tanamesa")
item_contrato <- fread("info_item_contrato.csv", encoding="UTF-8")
item_licit <- fread("info_item_licitacao.csv", encoding="UTF-8")
info_licit <- fread("info_licitacao.csv", encoding="UTF-8")
glimpse(item_contrato)
glimpse(item_licit)
glimpse(info_licit)

# 425 prefeituras
info_licit %>%
  summarise(n_distinct(nm_orgao))

# num itens nas licit 96593
item_licit %>%
  summarise(n_distinct(id_item))

# 47057 descrições únicas
item_licit %>%
  summarise(n_distinct(ds_item))

# num itens nas contrato 72657
item_contrato %>%
  summarise(n_distinct(id_item_contrato))

# 33097 descrições distintas
item_contrato %>%
  summarise(n_distinct(ds_item))

num_itens <- item_contrato %>%
  group_by(ds_item) %>%
  summarise(num_itens = n(),
            media_valor = mean(vl_item_contrato)) %>%
  arrange(desc(num_itens))

#num de items repetidos: 12.828. MEdiana é 3, quartil 3 é , centil 95 é 9.

num_itens %>%
  filter(num_itens > 1) %>%
  summarise(total = n(),
            mediana = median(num_itens),
            quartil1 = quantile(num_itens, .25),
            quartil3 = quantile(num_itens, .75),
            centil_95 = quantile(num_itens, .95))

num_itens %>%
  filter(num_itens > 1) %>%
  ggplot(aes(log(num_itens, base=2))) + geom_histogram(bins = 40) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
  xlab("Número de repetições de itens, escala logarítmica, base 2")

num_itens %>%
  ggplot(aes(log(num_itens, base=2))) + geom_histogram(bins = 40) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  xlab("Número de repetições de itens, escala logarítmica, base 2")

num_itens %>%
  filter(num_itens > 1) %>%
  ggplot(aes(num_itens)) + geom_histogram(bins = 100) +
  xlab("Número de repetições de itens")

num_itens %>%
  filter(num_itens > 50) %>%
  mutate(ds_item = fct_reorder(ds_item, num_itens)) %>%
  ggplot(aes(x=ds_item, y= num_itens)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("número de itens") +
  ggtitle("Itens com mais de 50 repeticões") +
  theme_bw()

num_itens %>%
  filter(grepl("arroz", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  summarise(n_distinct(ds_item), n())

set.seed(234)
arroz <- num_itens %>%
  filter(grepl("arroz", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  filter(!grepl("granola", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  filter(!grepl("flocos de arroz em barra", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  sample_n(50)

arroz %>%
  mutate(num_caracteres = nchar(ds_item),
         ds_item = tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")),
         ds_item = str_trunc(ds_item, 30),
         ds_item = fct_reorder(ds_item, num_caracteres)) %>%
  ggplot(aes(x=ds_item, y= num_caracteres)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("tamanho da descrição") +
  ggtitle("Itens com mais de 50 repeticões") +
  theme_bw()
  
num_itens %>%
  filter(grepl("arroz", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  filter(!grepl("granola", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  filter(!grepl("flocos de arroz em barra", tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")))) %>%
  mutate(num_caracteres = nchar(ds_item),
         ds_item = tolower(iconv(ds_item, "UTF-8", "ASCII//TRANSLIT")),
         ds_item = str_trunc(ds_item, 30),
         ds_item = fct_reorder(ds_item, num_caracteres)) %>%
  ggplot(aes(x=num_caracteres, y=media_valor)) + geom_point() 
  
xlab("Número de repetições de itens")

num_itens %>%
  filter(num_itens > 1) %>%
  ggplot(aes(y=num_itens)) + geom_boxplot() +
  xlab("Número de repetições de itens")

num_itens %>%
  summarise(n())

num_itens_limpos1 <- num_itens_limpos %>%
  filter(grepl("abacate", ds_item))
## fazendo alguma limpeza de textos
#
num_itens_limpos <- item_contrato %>%
  mutate(ds_item = gsub("[[:punct:]]", "", ds_item ),
         ds_item = str_squish(ds_item),
         ds_item = str_to_lower(ds_item)) %>%
  group_by(ds_item) %>%
  summarise(num_itens = n()) %>%
  arrange(desc(num_itens))

num_itens_limpos %>%
  summarise(n())

num_itens_limpos %>%
  filter(num_itens > 1) %>%
  summarise(total = n(),
              mediana = median(num_itens),
              quartil1 = quantile(num_itens, .25),
              quartil3 = quantile(num_itens, .75),
              centil_95 = quantile(num_itens, .95))
  
# num licitações 2.364
info_licit %>%
   summarise(n_distinct(id_licitacao))

# 1.063 de 2018, 963 de 2019, 338 de 2020
info_licit %>%
  group_by(ano_licitacao) %>%
  summarise(n_distinct(id_licitacao))

# 96593 itens distintos (ids distintos)
item_licit %>%
  summarise(n_distinct(id_item))

# 47057 descrições distintas
item_licit %>%
  summarise(n_distinct(ds_item))
#

itens <- item_contrato %>%
  dplyr::select(id_item_licitacao, id_contrato, id_licitacao, id_orgao,
                 vl_item_contrato, vl_total_item_contrato, ds_item, cd_tipo_modalidade) %>%
  rename(id_item = id_item_licitacao) %>%
  inner_join(dplyr::select(item_licit, id_item, id_licitacao, id_orgao,
                           vl_unitario_estimado, vl_total_estimado),
             by=c("id_orgao","id_licitacao", "id_item")) %>%
  mutate(dif_total = vl_total_item_contrato - vl_total_estimado) %>%
  group_by(id_orgao, id_licitacao, id_contrato, ds_item) %>%
  summarise(soma = sum(dif_total), media = mean(dif_total),
            num_itens = n_distinct(id_item),
            num_itens_economia = sum(dif_total < 0),
            num_itens_waste = sum(dif_total > 0),
            num_itens_zero = sum(isTRUE(all.equal(dif_total, 0))),
            cd_tipo_modalidade = max(cd_tipo_modalidade))

itens <- itens %>%
  mutate(ds_item = gsub("[:punct:]", "", ds_item ))
# 33096 descrições únicas
length(unique(itens$ds_item))

# 70630 linhas
itens %>%
  ungroup() %>%
  summarise(n())
## ranking para 2020
ranking_2020 <- itens %>%
  summarise(dif_total = sum(soma),
            cd_tipo_modalidade = max(cd_tipo_modalidade)) %>%
  ungroup() %>%
  mutate(ranking_contrato_munic = cume_dist(dif_total))

ranking_2020 %>%
  filter(dif_total > -5000000) %>%
  ggplot(aes(ranking_contrato_munic, dif_total)) +
  geom_point() +
  theme_bw() + xlab("ranking") + ylab("contratado menos estimado")

ranking_2020 %>%
  filter(dif_total > -5000000) %>%
  ggplot(aes(ranking_contrato_munic)) + geom_histogram()

ranking_2020 %>%
  summarise(quantile(dif_total, .5), n())

quantile(log(ranking_2020$dif_total[ranking_2020$dif_total > 0]), (0:99)/100)

ranking_2020 %>%
  filter(dif_total != 0) %>%
  mutate(nao_positivo = ifelse(dif_total < 0, "economia", "desperdício")) %>%
  ggplot(aes(log(abs(dif_total)))) + geom_histogram() +
  facet_grid(~nao_positivo, scales ="free") +
  theme_bw() + 
  xlab("estimado menos contratado, valor absoluto, em escala logarítmica")
           
ranking_2020 %>%
  filter(dif_total != 0) %>%
  mutate(nao_positivo = ifelse(dif_total < 0, "economia", "desperdício")) %>%
  ggplot(aes(log(abs(dif_total)))) + geom_histogram() +
  facet_grid(nao_positivo ~., scales ="free") +
  theme_bw() + 
  xlab("estimado menos contratado, valor absoluto, em escala logarítmica")

ranking_2020 %>%
  filter(dif_total != 0) %>%
  mutate(nao_positivo = ifelse(dif_total < 0, "economia", "desperdício")) %>%
  group_by(nao_positivo) %>%
  summarise(sd(log(abs(dif_total))),
            median(log(abs(dif_total))),
            media = mean(log(abs(dif_total))))

ranking_2020 %>%
  mutate(size = ifelse( dif_total < -500000, "big", 
                        ifelse(dif_total < -50000, "medio", 
                               ifelse(dif_total <= 1, "small", "positivo")))) %>%
  group_by(size) %>%
  summarise(n())

ranking_2020 %>%
  filter(dif_total <= 0) %>%
  filter(cd_tipo_modalidade != "CNC") %>%
  ggplot(aes(log(abs(dif_total) + 1 ))) +
  geom_histogram() + 
  facet_grid(cd_tipo_modalidade ~ ., scales ="free") +
  xlab("valor economizado + 1 real, em escala logarítmica") +
  theme_bw()

ranking_2020 %>%
  filter(dif_total <= 0) %>%
  filter(cd_tipo_modalidade != "CNC") %>%
  ggplot(aes(log(abs(dif_total) + 1 ))) +
  geom_histogram(aes(y = stat(density))) + 
  facet_grid(cd_tipo_modalidade ~., scales = "free") +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("valor economizado + 1 real, em escala logarítmica") +
  theme_bw()
  

ranking_2020 %>%
  filter(dif_total > -500000) %>%
  filter(cd_tipo_modalidade != "CNC") %>%
  ungroup() %>%
  group_by(cd_tipo_modalidade) %>%
  summarise(num_zero = sum(dif_total == 0),
            num_linhas = n(),
            perc = num_zero/num_linhas)

itens_arranged <- itens %>%
  arrange(ds_item)
glimpse(itens)

robust_sd <- function(x) {
  squared <- (x - median(x))^2
  sqrt(sum(squared))
}

itens %>%
  ungroup() %>%
  summarise(media = mean(soma), dp = sd(soma),
            mediana = median(soma),
            dp_robust = robust_sd(soma)) %>%
  mutate(up_l = 2*dp_robust + media,
         lo_l = media - 2*dp_robust)
df %>%
  dplyr::ungroup() %>%
  mutate(fora = soma > 1081721 | soma < -1056617) %>% # 3dp
  group_by(fora) %>%
  summarise(n())

df %>%
  dplyr::ungroup() %>%
  mutate(fora = soma > 725331 | soma < -700227) %>% # 2dp
  group_by(fora) %>%
  summarise(n())

# 9 casos fora com 2dp

df %>%
  ggplot(aes(y=soma)) + geom_boxplot()

df %>%
  filter(soma > 0) %>%
  ggplot(aes(x=log(soma))) + geom_histogram() +
  xlab("log do valor total desperdiçado")

df %>%
  filter(soma < 0) %>%
  ggplot(aes(x=log(-soma))) + geom_histogram() +
  xlab("log do valor total economizado")

df %>%
  filter(soma < 0) %>%
  ggplot(aes(x=log(num_itens_economia), y=log(-soma))) + geom_point() +
  geom_smooth(method= "lm")

df %>%
  filter(soma < 0) %>%
  ggplot(aes(x=log(num_itens_waste+1), y=log(-soma))) + geom_point() +
  geom_smooth(method= "lm")

df %>%
  filter(soma > 0) %>%
  ggplot(aes(x=log(num_itens_waste+1), y=log(soma))) + geom_point() +
  geom_smooth(method= "lm")

df %>%
  ggplot(aes(x=num_itens_waste, y=soma)) + geom_point() +
  geom_smooth(method= "lm")

df %>%
  filter(soma < 0) %>%
  ggplot(aes(x=num_itens_waste, y=log(-soma))) + geom_point()

df %>%
  filter(soma < 0) %>%
  ggplot(aes(log(num_itens_waste))) + geom_histogram()

df %>%
  filter(soma > 0) %>%
  ggplot(aes(x=num_itens_economia, y=log(soma))) + geom_point()

df %>%
  filter(soma > 0) %>%
  ggplot(aes(x=num_itens_waste, y=log(soma))) + geom_point()

glimpse(df)
summary(df)
