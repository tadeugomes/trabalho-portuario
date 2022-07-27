library(basedosdados)
library(tidyverse)

# aqui você define o seu projeto billing_id

basedosdados::set_billing_id("rais-321402")  

#### Dados da RAIS para operacoes portuarias 

rais_ <- bdplyr("basedosdados.br_me_rais.microdados_vinculos") %>%
  select(ano, sigla_uf, vinculo_ativo_3112, ano,cnae_2_subclasse, tipo_vinculo, valor_remuneracao_media, sexo, grau_instrucao_apos_2005, cbo_2002) %>% 
  #vamos filtrar o município pelo código da CNAE-IBGE e o ano de 2020
  filter(cnae_2_subclasse == "5231102",
         ano == 2020, 
         vinculo_ativo_3112 == 1, 
         ) %>% 
  bd_collect()





uf <- rais_ %>% 
  group_by(sigla_uf) %>% 
    count (vinculo_ativo_3112) %>% 
    arrange(desc(n))


# cumsum(uf$n) # total de vinculos acumulados. Mas basta observar que o filtro é pelo valor positivo, então nem precisaria fazer esse cálculo

uf$sigla_uf <- factor(uf$sigla_uf, levels = uf$sigla_uf[order(uf$n)])


ggplot(uf, aes(x = sigla_uf, y = n, stat = 'identity'))+
  geom_bar(position = "dodge", stat = "identity", fill = "#4682B4") +
  coord_flip()+
  labs(x = "UF", y = "Número de vínculos", 
       title = "Trabalhadores na Operação de Terminais Portuários por estado", 
       subtitle = "São 35.357 vínculos de trabalho em todo o país",
         caption = "Fonte: Observatório Portuário do Maranhão - Dados da RAIS 2000. ") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 16L, face = "bold"), 
        plot.caption = element_text(size = 10L,face = "bold", hjust = 0), 
        axis.title.y = element_text(size = 10L, face = "bold"), 
        axis.title.x = element_text(size = 10L, face = "bold"))+
  geom_point(size=8, color = "lightblue")+ 
  geom_text(aes(label=prettyNum(n, big.mark = ".")), size =2.5,  
            hjust=0.5, vjust=0.5, colour="black")+
  scale_colour_manual(values = c("red", "blue", "green"))+
  theme(legend.position = "none")


###### 

# O arquivo rais_teste foi baixado com o código acima também, mas com alteração apenas da CNAE. Vamos verificar 
# Trta-se do grupo Trabalhadores na Administração da Infraestrutura Portuária (CNAE 5231-1/01)

uf_adm <- rais_teste %>% 
  group_by(sigla_uf) %>% 
  count (vinculo_ativo_3112) %>% 
  arrange(desc(n))


cumsum(uf_adm$n) # total de vinculos acumulados 

uf_adm$sigla_uf <- factor(uf_adm$sigla_uf, levels = uf_adm$sigla_uf[order(uf_adm$n)])


ggplot(uf_adm, aes(x = sigla_uf, y = n, stat = 'identity'))+
  geom_bar(position = "dodge", stat = "identity", fill = "#4682B4") +
  coord_flip()+
  labs(x = "UF", y = "Número de vínculos", 
       title = " Trabalhadores na Administração da Infraestrutura Portuária por UF", 
       subtitle = "São 3.090 vínculos de trabalho em todo o país",
       caption = "Fonte: Observatório Portuário do Maranhão - Dados da RAIS 2000. ") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 16L, face = "bold"), 
        plot.caption = element_text(size = 10L,face = "bold", hjust = 0), 
        axis.title.y = element_text(size = 10L, face = "bold"), 
        axis.title.x = element_text(size = 10L, face = "bold"))+
  geom_point(size=8, color = "lightblue")+ 
  geom_text(aes(label=prettyNum(n, big.mark = ".")), size =2.5,  
            hjust=0.5, vjust=0.5, colour="black")+
  scale_colour_manual(values = c("red", "blue", "green"))+
  theme(legend.position = "none")



##### Vamos melhorar recodificar os dados 



rais_completa <- rais_ %>% 
  mutate( 
    sigla_uf = fct_reorder(sigla_uf, desc(sigla_uf)), 
    sexo = ifelse(sexo == 1, "Homem", "Mulher"), 
    grau_instrucao_apos_2005 = factor( 
      case_when( 
        grau_instrucao_apos_2005 %in% 1:4 ~ "Fundamental incompleto", 
        grau_instrucao_apos_2005 == 5 ~ "Fundamental completo", 
        grau_instrucao_apos_2005 == 6 ~ "Médio incompleto", 
        grau_instrucao_apos_2005 == 7 ~ "Médio completo",
        grau_instrucao_apos_2005 == 8 ~ "Superior incompleto", 
        grau_instrucao_apos_2005 == 9 ~ "Superior completo", 
        grau_instrucao_apos_2005 == 10 ~ "Pós-graduação", 
        grau_instrucao_apos_2005 == -1 ~ "Não informado"
      ), 
      levels = c( 
        "Fundamental incompleto", "Fundamental completo", 
        "Médio incompleto", "Médio completo", "Superior incompleto",
        "Superior completo", "Pós-graduação", "Não informado"
      ) 
    ))






ggplot(rais_completa) +
 aes(x = sexo, y = valor_remuneracao_media) +
 geom_boxplot(fill = "#4682B4") +
 geom_jitter() +
 labs(title = "Valor da remuneração média por sexo dos trabalhadores em Operações Portuárias no Brasil") +
 theme_minimal()
library(dplyr)
library(ggplot2)


rais_completa %>%
  filter(!is.na(grau_instrucao_apos_2005)) %>%
  ggplot() +
  aes(x = fct_infreq(grau_instrucao_apos_2005)) +
  geom_bar(fill = "#112446") +
  labs(title = " Escolaridade dos profissionais que atuam na Operação de Terminais Portuários no Brasil") +
  coord_flip() +
  theme_minimal()



rais_completa %>%
 filter(!is.na(grau_instrucao_apos_2005)) %>%
 ggplot() +
 aes(x = sexo) +
 geom_bar(fill = "#112446") +
 labs(title = "Sexo dos fucionários que atuam em Operação de Terminais Portuários") +
 theme_minimal()



rais_completa %>%
  filter(!is.na(grau_instrucao_apos_2005)) %>%
  ggplot() +
  aes(x = fct_infreq(cbo_2002)) +
  geom_bar(fill = "#112446") +
  labs(title = " Escolaridade dos profissionais que atuam na Operação de Terminais Portuários no Brasil") +
  coord_flip() +
  theme_minimal()


saveRDS(rais_completa, file = "rais_completa.rds")

readRDS(file = "rais_completa.rds")