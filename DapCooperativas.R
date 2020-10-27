
#filtra para exibir apenas os resultados do RS

DAP_cooperativas <- DAP_cooperativas %>%
  filter(UF == "RS")

#formata o campo do cpnpj para o formato usado

DAP_cooperativas <- DAP_cooperativas %>%
  mutate(CNPJEspecifico  = gsub("\\D", "",CNPJEspecifico ))

#testes

#juntando tabela de empenhos com tabela das cooperativas a partir de
#cnpj iguais

empenhos_para_cooperativas <- merge(empenho, DAP_cooperativas, by.x = "cnpj_cpf", by.y = "CNPJEspecifico")

#reduzindo campos da tabela apenas para os utilizados nessa analise

empenhos_para_cooperativas <- empenhos_para_cooperativas %>%
  select(cnpj_cpf,id_contrato,id_orgao,nome_orgao,cd_projeto,nm_projeto,nm_credor,vl_liquidacao,NumeroDeControleExterno,TIPO_DAP)

#Removendo os valores NA das liquidacoes ainda nao efetuadas, substituindo eles por 0

empenhos_para_cooperativas <- empenhos_para_cooperativas  %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0))


#criando tabela com quantidade de contratos concluidos de cadas prefeitura com
#cada cooperativa e tambem a quantidade de contratos concluidos totais dessa prefeitura
#com cooperativas diferentes

contratos_concluidos_com_cooperativas <- empenhos_para_cooperativas %>%
  group_by(id_orgao) %>%
  select(id_orgao,nome_orgao,cnpj_cpf,nm_credor,id_contrato) %>%
  mutate(contr_totais=n_distinct(id_contrato))

#removendo possiveis repeticoes na tabela

contratos_concluidos_com_cooperativas <- unique(contratos_concluidos_com_cooperativas)

#ordenando a tabela pela quantidade de contratos concluidos totais
contratos_concluidos_com_cooperativas <- contratos_concluidos_com_cooperativas %>%
  arrange(desc(contr_totais))

contratos_concluidos_com_cooperativas <- contratos_concluidos_com_cooperativas %>%
  group_by(id_orgao)%>%
  mutate(total_contratos_com_cooperativa = n_distinct(contr_totais))

#contratos totais da prefeitura com todas as cooperativas

contrato_totais_com_cooperativas <- contratos_concluidos_com_cooperativas %>%
  select(id_orgao,nome_orgao,contr_totais)

#retirando possiveis repetições

contrato_totais_com_cooperativas <- unique(contrato_totais_com_cooperativas)

#criando tabela com a soma total das liquidacoes direcionadas para
#cooperativas com DAP por parte da prefeitura

soma_total_liq_orgao_para_cooperativas<- empenhos_para_cooperativas %>%
  group_by(id_orgao )%>%
  summarise(soma_liq = sum(vl_liquidacao))

#ordenacao por id das prefeituras

soma_total_liq_orgao_para_cooperativas <- soma_total_liq_orgao_para_cooperativas %>%
  arrange(desc(id_orgao))

#criando tabela com soma total das liquidacoes feitas pelas prefeitura e
#com quantas cooperativas com DAP elas tem contrato

contrato_totais_com_cooperativas <- soma_total_liq_orgao_para_cooperativas %>%
  left_join(contrato_totais_com_cooperativas, by = "id_orgao")

#ordenacao por soma total das liquidacoes da maior para a menor

contrato_totais_com_cooperativas <- contrato_totais_com_cooperativas %>%
  arrange(desc(soma_liq))

#liquidacao total orgao para comparacao com cooperativa
liquidacao_orgao <- empenho %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0)) %>%
  group_by(id_orgao) %>%
  summarise(somas_liquidacoes = sum(vl_liquidacao))
#comparacao
teste_grafico <- liquidacao_orgao %>%
  left_join(select(soma_total_liq_orgao_para_cooperativas,id_orgao,soma_liq), by="id_orgao")

teste_grafico <- teste_grafico %>%
  mutate(soma_liq= tidyr::replace_na(soma_liq, 0)) %>%
  mutate(perct = (100*soma_liq)/somas_liquidacoes)

pie(table(teste_grafico), main="Gráfico de setores:liquidação total x liq para cooperativas", col=c("red", "blue")) 
