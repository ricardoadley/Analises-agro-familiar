#filtra para exibir apenas os resultados do RS

DAP_cooperativas <- DAP_cooperativas %>%
  filter(UF == "RS")

#formata o campo do cpnpj para o formato usado

DAP_cooperativas <- DAP_cooperativas %>%
  mutate(CNPJEspecifico  = gsub("\\D", "",CNPJEspecifico ))

#testes

h <- merge(empenho, DAP_cooperativas, by.x = "cnpj_cpf", by.y = "CNPJEspecifico")
