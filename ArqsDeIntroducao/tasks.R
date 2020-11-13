#organizando soma fornecedores de forma decrescente
soma_fornecedores <- soma_fornecedores %>%
  arrange(desc(total_liquidado))

#filtrando orgaos por MUNICIPAL
orgaos <- orgaos %>%
  filter(esfera == "MUNICIPAL")
#separando apenas id e nome dos orgaos
quem_pagou_mais <- orgaos %>%
  select(id_orgao,nm_orgao)

#separa alguns dados de quem esta envolvido no empenho
dados_vendendo <- empenho %>%
  select(id_orgao,id_empenho,cnpj_cpf,vl_liquidacao)

#coloca o vl_liquidacao vazio para 0
dados_vendendo <- dados_vendendo %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0))

#cria uma tabela organizando pelo maior valor de liquidação para o menor
maior_liquidacao<- dados_vendendo %>%
  arrange(desc(vl_liquidacao))

#faz as somas das liquidações de um cnpj-cpf especifico para cada orgao

#quem_mais_recebeu <- orgaos%>%
 # left_join(maior_liquidacao, by = "id_orgao") %>%
  #group_by(cnpj_cpf) %>%
  #summarize(total_liq = (sum(vl_liquidacao)))

quem_mais_recebeu<- maior_liquidacao %>%
  group_by(cnpj_cpf,id_orgao) %>%
  summarise(soma_liq = sum(vl_liquidacao))

quem_mais_recebeu <- unique(quem_mais_recebeu)


#organiza por ordem decrescente de valor de soma das liquidações
quem_mais_recebeu <- quem_mais_recebeu%>%
  arrange(desc(soma_liq))

#insere o nome do orgão a tabela 
quem_mais_recebeu <- quem_mais_recebeu %>%
  left_join(quem_pagou_mais, by = "id_orgao")

#
teste<- maior_liquidacao %>%
  group_by(cnpj_cpf) %>%
  summarise(soma_liq = sum(vl_liquidacao))

teste <- teste %>%
  left_join(select(quem_mais_recebeu,id_orgao,nm_orgao),by =c("cnpj_cpf"="cnpj_cpf"))




#separa apenas o nome do credor e o cnpj_cpf dele
credores <- empenho %>%
  select(nm_credor,cnpj_cpf)
#remove as repetições deixando apenas a primeira aparição do credor
credores <- unique(credores)

  