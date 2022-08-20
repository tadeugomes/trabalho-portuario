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

View(rais_completa)






rais %>% 
  count(sexo)


rais %>%
  filter(!is.na(sexo)) %>% 
  count(sexo)
  
rais %>% 
filter(!is.na(grau_instrucao_apos_2005)) %>% 
filter(ano==2020) %>% 
  count(sexo)


10582 * 100 / 76317

4710 * 100/ 10582

11630 * 100 / 65735 



library(tidyverse)
library(bbplot)

lvls_ano <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")




rais %>%
  group_by(ano, raca_cor, sexo,) %>% 
  summarise(n = n()) %>% 
  mutate(raca_cor = forcats::fct_reorder(raca_cor, n)) %>% 
  mutate(ano = factor(ano, lvls_ano)) %>%
  ggplot(aes(x =ano, y = n, group = raca_cor, color = raca_cor)) +
  geom_line(size = 2) +
  geom_point(
    data = . %>% filter(ano %in% c(2010, 2020)),
    aes(x = ano, y = n), 
    color = "black", size = 5,
    show.legend = F) +
  geom_point(
    data = . %>%  group_by(raca_cor, sexo) %>% filter(n == max(n)),
    aes(x = ano, y = n), 
    color = "red", size = 5,
    show.legend = F) +
  geom_text_repel(
    data = . %>% filter(ano %in% c(2010, 2020)),
    aes(
      label = scales::number(
        n,
        scale = 1/10^3,
        accuracy = 0.01,
        suffix = " mil",
        decimal.mark = ",",
        big.mark = "."),
    ),
    size = 5,
    min.segment.length  =  0.5,
    box.padding  =  1,
    force = 8,
  ) +
  geom_text_repel(
    data = . %>% group_by(raca_cor) %>% filter(n == max(n)),
    aes(x =ano, y = n,
        label = scales::number(
          n,
          scale = 1/10^3,
          suffix = " mil",
          accuracy = 0.01,
          decimal.mark = ",",
          big.mark = "."
        )
    ),
    size = 5,
    min.segment.length  =  .5,
    box.padding  =  1,
    force =8,
  ) +
  scale_y_log10(labels = label_number(scale = 1/10^3,
                                      suffix = " mil",
                                      accuracy = 0.1,
                                      decimal.mark = ",",
                                      big.mark = ".")) +
  # scale_y_continuous(
  #   labels = label_number(scale = 1/1000, big.mark = ".", decimal.mark = ",")) +
  labs(x = "Ano", y = "Número de Vínculos", color = "Raça ou cor",
       caption = "Fonte: Observatório Portuário - Dados da RAIS") +
  facet_wrap(~sexo, scales = 'free_x', ncol = 2) +
  bbc_style()
  
  
  
  
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold"),
        plot.caption = element_text(size = 12L,face = "bold", hjust = 0), 
        axis.title.y = element_text(size = 14L, face = "bold"), 
        axis.title.x = element_text(size = 14L, face = "bold"),
        axis.text.x = element_text(size = 12L, face = "bold"),
        axis.text.y = element_text(size = 12L, face = "bold"),
        #tema legenda
        legend.text = element_text(size = 14L),
        panel.grid.major.x  = element_line(color = "black", linetype = 3, size = 0.1,),
        legend.position = "top",
        legend.justification = c("top"),
        legend.title = element_text(size = 16L, face = "bold"),
        #legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(1.5, "cm"),
        #
  ) +
  guides(
    col = guide_legend(nrow = 1)) +
  ggthemes::scale_color_gdocs()

  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold"),
  plot.caption = element_text(size = 12L,face = "bold", hjust = 0), 
  axis.title.y = element_text(size = 14L, face = "bold"), 
  axis.title.x = element_text(size = 14L, face = "bold"),
  axis.text.x = element_text(size = 12L, face = "bold"),
  axis.text.y = element_text(size = 12L, face = "bold"),
  panel.grid.major.x  = element_line(color = "black", linetype = 3, size = 0.1)) + scale_y_continuous(labels = label_number(scale = 1/1000)) +
  ggthemes::scale_color_gdocs()



####################################333
  
  lvls_ano <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  
  
  
  rais %>%
    group_by(ano, raca_cor, sexo) %>% 
    summarise(n = n()) %>% 
    mutate(raca_cor = forcats::fct_reorder(raca_cor, n)) %>% 
    mutate(ano = factor(ano, lvls_ano)) %>%
    ggplot(aes(x =ano, y = n, group = raca_cor, color = raca_cor)) +
    geom_line(size = 2) +
    geom_point(
      data = . %>% filter(ano %in% c(2010, 2020)),
      aes(x = ano, y = n), 
      color = "black", size = 5,
      show.legend = F) +
    geom_point(
      data = . %>%  group_by(raca_cor, sexo) %>% filter(n == max(n)),
      aes(x = ano, y = n), 
      color = "red", size = 5,
      show.legend = F) +
    geom_text_repel(
      data = . %>% filter(ano %in% c(2010, 2020)),
      aes(
        label = scales::number(
          n,
          scale = 1/10^3,
          accuracy = 0.01,
          suffix = " mil",
          decimal.mark = ",",
          big.mark = "."),
      ),
      size = 5,
      min.segment.length  =  0.5,
      box.padding  =  1,
      force = 8,
    ) +
    geom_text_repel(
      data = . %>% group_by(raca_cor) %>% filter(n == max(n)),
      aes(x =ano, y = n,
          label = scales::number(
            n,
            scale = 1/10^3,
            suffix = " mil",
            accuracy = 0.01,
            decimal.mark = ",",
            big.mark = "."
          )
      ),
      size = 5,
      min.segment.length  =  .5,
      box.padding  =  1,
      force =8,
    ) +
    scale_y_log10(labels = label_number(scale = 1/10^3,
                                        suffix = " mil",
                                        accuracy = 0.1,
                                        decimal.mark = ",",
                                        big.mark = ".")) +
    # scale_y_continuous(
    #   labels = label_number(scale = 1/1000, big.mark = ".", decimal.mark = ",")) +
    labs(x = "Ano", y = "Número de Vínculos", color = "Raça ou cor",
         caption = "Fonte: Observatório Portuário - Dados da RAIS") +
    facet_wrap(~sexo, scales = 'free_x', ncol = 2)+
    bbc_style()
    
    
    
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold"),
          plot.caption = element_text(size = 12L,face = "bold", hjust = 0), 
          axis.title.y = element_text(size = 14L, face = "bold"), 
          axis.title.x = element_text(size = 14L, face = "bold"),
          axis.text.x = element_text(size = 12L, face = "bold"),
          axis.text.y = element_text(size = 12L, face = "bold"),
          #tema legenda
          legend.text = element_text(size = 14L),
          panel.grid.major.x  = element_line(color = "black", linetype = 3, size = 0.1,),
          legend.position = "top",
          legend.justification = c("top"),
          legend.title = element_text(size = 16L, face = "bold"),
          #legend.background = element_rect(fill = "white", colour = "black"),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(1.5, "cm"),
          #
    ) +
    guides(
      col = guide_legend(nrow = 1)) +
    ggthemes::scale_color_gdocs()

    
    
    
    
    
    
    
    
    
    
    
    
    rais %>%
      group_by(ano, cbo_2002) %>% 
      summarise(n = n()) %>% 
      mutate(ano = factor(ano, lvls_ano)) %>%
      arrange(n) %>% 
      top_n(6, n) %>% 
      ggplot(aes(x = ano, y = n, group = cbo_2002, color = cbo_2002)) +
      geom_line(size = 1.25) +
      geom_point(
        data = . %>% filter(ano %in% c(2010, 2020)),
        
        color = "black", size = 3) +
      geom_point(
        data = . %>%  group_by(cbo_2002) %>% filter(n == max(n)),
        aes(x = ano, y = n), 
        color = "red", size = 2) +
      ggrepel::geom_text_repel(aes(x = ano, y = n, label = scales::number(n, scale = 1/1000, decimal.mark = ",", big.mark = ".", accuracy = 0.01, suffix = " mil")), nudge_x = -0.2, direction = "y", hjust = "right", size = 4) +
      
      #scale_color_manual(values = cores) +
      #scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
      scale_y_continuous(
        labels = label_number(scale = 1/1000, big.mark = ".", decimal.mark = ",",
                              prefix = "")) +
      theme_classic() +
      theme(plot.title = element_text(size = 16L, face = "bold"),
            plot.caption = element_text(size = 12L,face = "bold", hjust = 0), 
            axis.title.y = element_text(size = 14L, face = "bold"), 
            axis.title.x = element_text(size = 14L, face = "bold"),
            axis.text.x = element_text(size = 12L, face = "bold"),
            axis.text.y = element_text(size = 12L, face = "bold"),
            legend.text = element_text(size = 14L),
            panel.grid.major.x  = element_line(color = "black", linetype = 3, size = 0.1,),
            legend.position = "top",
            legend.justification = c("left", "top"),
            legend.title = element_text(size = 16L, face = "bold")) +
      labs(x = "Ano", y = "Número de vínculos", color = " ",
           caption = "Fonte: Observatório Portuário - Dados da RAIS") +
      scale_colour_discrete(name = str_wrap("Grupos da CNAE", 10), labels = label_wrap_gen(width = 18)) +
      ggthemes::scale_color_gdocs()
    
    

      ggrepel::geom_text_repel(aes(x = ano, y = n, label = scales::number(n, scale = 1/1000, decimal.mark = ",", big.mark = ".", accuracy = 0.01, suffix = " mil")), nudge_x = -0.2, direction = "y", hjust = "right", size = 4) +
      labs(x = "Ocupação", y = "Número de vínculos (em mil)",
           title = "Número de Vínculos do Setor Portuário e aquaviário no Maranhão (2010 - 2020)", subtitle = "Seção da CNAE H",
           caption = "Fonte: Observatório Portuário - Dados da RAIS") +
      guides(color = "none") +
      theme_classic() +
      theme(plot.title = element_text(size = 16L, face = "bold"),
            plot.caption = element_text(size = 12L,face = "bold", hjust = 0), 
            axis.title.y = element_text(size = 14L, face = "bold"), 
            axis.title.x = element_text(size = 14L, face = "bold"),
            axis.text.x = element_text(size = 12L, face = "bold"),
            axis.text.y = element_text(size = 12L, face = "bold"),
            panel.grid.major.x  = element_line(color = "black", linetype = 3, size = 0.1)) + scale_y_continuous(labels = label_number(scale = 1/1000)) +
      ggthemes::scale_color_gdocs()
      
  
library(tidyverse)
      
grupo_ma <- rais %>% filter(sigla_uf=="MA")
      

# manipulação dos dados. Grupos CNAE
grupo_ma.2 <- 
        manipulacao(
          dataset = grupo_ma, 
          nivel1 = n_divisao, 
          nivel2 = n_grupo
        )
      
      #grafico
      a <- 
        grafico(
          dataset = grupo_ma.2,
          nivel1 = n_divisao, 
          nivel2 = n_grupo, 
          titulo = "", 
          titulo_legenda = ""
        )+
        theme_classic()+
        theme(plot.subtitle= element_blank(), 
              legend.text = element_text(size = 12), 
              panel.grid.major.x  = element_line(color = "black", linetype = 2, size = 0.1,),
              legend.position = "top",
              legend.justification = c("left", "top"),
              legend.title = element_text(size = 10, face = "bold"), 
              strip.text.x = element_text(size=10)
        )
      
      
      print(a)
      