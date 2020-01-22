library(tidyverse)
library(readxl)

merenda_transparente_a <- read_excel("dados/merenda transparente - A.xlsx")

names(merenda_transparente_a) <- c("id", "ciencia", "escola", "estado", "cidade", "email",
                                   "sobre", "freq_comeu_B", "freq_comeu_A", "freq_comeu_B_v2",
                                   "qualidade_merenda", "cardapio_transparente", 
                                   "como_consulta_cardapio", "corresp_cardapio_merenda", 
                                   "fazer_refeicao", "data_foto", "qual_refeicao",
                                   "campo_aberto", "telefone", "status", "data_criado", "dado_enviado",
                                   "dado_update")

merenda_transparente_b <- read_excel("dados/merenda transparente - B.xlsx")

names(merenda_transparente_b) <- c("id", "ciencia", "escola", "estado", "cidade", "email",
                                   "sobre", "freq_comeu_B", "freq_comeu_A", "freq_comeu_B_v2",
                                   "qualidade_merenda", "cardapio_transparente", 
                                   "como_consulta_cardapio", "corresp_cardapio_merenda", 
                                   "fazer_refeicao", "data_foto", "qual_refeicao",
                                   "campo_aberto", "telefone", "status", "data_criado", "dado_enviado",
                                   "dado_update")

glimpse(merenda_transparente_a)
glimpse(merenda_transparente_b)


merenda_transparente_a <- merenda_transparente_a %>%
  mutate(tipo = "A") 

merenda_transparente_b <- merenda_transparente_b %>%
  mutate(tipo = "B") 

resp_merenda <- bind_rows(merenda_transparente_a, merenda_transparente_b)
  
## demográficos

resp_merenda %>%
  group_by(ciencia) %>%
  summarise(total=n())

# não         1
# sim        62
# NA          2

## trabalhar só com sim.

resp_merenda_valido <- resp_merenda %>%
  filter(ciencia == "sim")

resp_merenda_valido <- resp_merenda_valido %>%
  mutate(estado = gsub("Pernambuco", "PE", estado),
         estado = gsub("Pe", "PE", estado),
         estado = gsub("Alagoas", "AL", estado),
         estado = gsub("Rio de Janeiro", "RJ", estado),
         estado = gsub("Santa Catarina", "SC", estado),
         estado = ifelse(grepl("[pP]aulo", estado), "SP", estado),
         estado = gsub("Sp", "SP", estado),
         estado = gsub("SÃO PAULO", "SP", estado)) %>%
  rename(uf = estado)

resp_merenda_valido <- resp_merenda_valido %>%
  mutate(cidade = gsub("Mauá SP", "Mauá", cidade),
         cidade = gsub("osasco", "Osasco", cidade),
         cidade = ifelse(grepl("[pP][Aa]", cidade), "São Paulo", cidade)) 

setwd("C:/Users/mczfe/Transparencia Brasil/Projetos/Tinker/Github/tinker/dados")
save(resp_merenda_valido, file="resp_merenda_valido.RData")

# 62 respostas válidas
resp_merenda_valido %>%
  summarise(n())

# 53 respostas tipo B e 9 tipo A
resp_merenda_valido %>%
  group_by(tipo) %>%
  summarise(n())

# UF



resp_merenda_valido %>%
  group_by(uf) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(perc = round(total/sum(total), 2))

# uf    total  perc
# 1 AL        1  0.02
# 2 PE        3  0.05
# 3 RJ        1  0.02
# 4 SC        1  0.02
# 5 SP       56  0.9 

# quem respondeu
resp_merenda_valido %>%
  group_by(sobre) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(perc = round(total/sum(total), 2))

# sobre                      total  perc
# 1 estudante                  44  0.71
# 2 funcionária/o da escola     6  0.1 
# 3 mãe, pai ou responsável    12  0.19

# teste do tipo de resposta

## freq_comeu_A -> Quantas Vezes Na Semana Você Costuma Comer As Refeições Oferecidas Pela Escola
## freq_comeu_B -> Você Comeu As Refeições Oferecidas Pela Escola Mais De Uma Vez Neste Ano
resp_merenda_valido %>%
  filter(tipo == "A") %>%
  group_by(freq_comeu_A) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(perc = round(total/sum(total), 2))
  
# freq_comeu_A                total  perc
# 5 ou mais vezes por semana     5  0.56
# não como regularmente          2  0.22
# nunca                          1  0.11
# sempre                         1  0.11
  
resp_merenda_valido %>%
  filter(tipo == "B") %>%
  group_by(freq_comeu_B) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(perc = round(total/sum(total), 2))

# freq_comeu_B total  perc
# não             15  0.28
# sim             38  0.72

## relação entre quem respondeu e frequencia
p1 <- resp_merenda_valido %>%
  filter(tipo == "B") %>%
  ggplot(aes(x=freq_comeu_B)) + geom_bar() +
  facet_grid( ~ sobre)

ggsave(p1, file= "bar_plot_freq_comeu_e_quem_Respondeu.png")  

p2 <- resp_merenda_valido %>%
  filter(tipo == "A") %>%
  ggplot(aes(x=freq_comeu_A)) + geom_bar() + coord_flip() + 
  facet_grid( ~ sobre)

ggsave(p2, file= "bar_plot_freq_comeu_e_quem_Respondeu_A.png") 

## qualidade da merenda
## VocêAvaliaQueAsRefeiçõesOferecidasPelaEscolaTemQualidadeSatisfatória
resp_merenda_valido %>%
  group_by(qualidade_merenda) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(perc = round(total/sum(total), 2))

p3 <- resp_merenda_valido %>%
  ggplot(aes(x=qualidade_merenda)) + geom_bar() + coord_flip() + 
  facet_grid( ~ sobre)

ggsave(p3, file= "qualidade_e_quem_Respondeu.png") 


resp_merenda_valido %>%
  filter(!is.na(como_consulta_cardapio)) %>%
  mutate(uf1 = ifelse(uf == "SP", uf, "outros")) %>%
  ggplot(aes(x=corresp_cardapio_merenda)) + geom_bar() + coord_flip()


resp_merenda_valido %>%
  ggplot(aes(x=fazer_refeicao)) + geom_bar() + coord_flip() +
  facet_wrap( ~ cidade)
