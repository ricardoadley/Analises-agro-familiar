## Introdução 

##filtrando apenas item_contrato pelo ano de 2020
item_contrato <- item_contrato %>%
  filter(ano_contrato == 2020)

## adaptando dados da tabela para pesquisa
item_contrato <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item , from="UTF-8", to="ASCII//TRANSLIT")))

#criando tabela com todos os contratos que possuem o item alface
item_contrato_alface <- item_contrato %>%
  filter(stringr::str_detect(ds_item, "alface"))

soma_fornecedores <- empenho %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0)) %>%
  # Como algumas linhas dizem respeito a empenhos e outras a pagamentos, 
  #a coluna de valor liquidado estará vazia (NA), 
  #por isso é necessário usar o comando "replace_na" 
  #para substituir esses campos por 0, evitando erros na hora de somar os valores
  group_by(cnpj_cpf) %>%
  # Aqui, estamos dizendo ao R que os comandos a seguir devem ser agrupados pela coluna "cnpj_cpf"
  summarize(total_liquidado = sum(vl_liquidacao), contr_concluidos=n_distinct(id_contrato))
# O comando summarize vai gerar uma nova tabela consistindo no CNPJ como 
#ID (pois este é o agrupamento feito acima), a nova variável "total_liquidado", 
#que equivale a soma de todas as observações "vl_liquidacao" para aquele CNPJ,
#bem como a coluna "contr_concluidas", que equivalente a todos os valores únicos
#da coluna "id_contrato", usada para identificar os contratos.

liquidacao_alface <- item_contrato_alface %>%
  left_join(empenho, by = "id_contrato") %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0)) %>%
  group_by(cnpj_cpf) %>%
  summarize(total_liquidado = sum(vl_liquidacao), contr_concluidos=n_distinct(id_contrato))
#filtrando apenas para orgaos municipais
orgaos <- orgaos %>%
  filter(esfera == "MUNICIPAL")