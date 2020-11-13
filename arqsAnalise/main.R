## Criacao das tabelas base
info_contrato <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/bd/info_contrato.csv",encoding = "UTF-8",colClasses = c("id_orgao"="character","nr_documento_contratado"="character"))
item_contrato <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/bd/info_item_contrato.csv",encoding = "UTF-8",colClasses = c("id_orgao"="character"))
orgaos <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/bd/info_orgaos.csv",encoding = "UTF-8",colClasses = c("id_orgao"="character"))
empenho <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/bd/info_empenhos.csv",encoding = "UTF-8",colClasses = c("id_orgao"="character","cnpj_cpf"="character"))
licitacoes <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/bd/info_licitacao.csv",encoding = "UTF-8")

##

DAP_ativa <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/cafdapativa.csv", encoding = "UTF-8")
DAP_pessoa_fisica <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/DAP_Pessoa_Fisica.csv", encoding = "UTF-8")
DAP_cooperativas <- data.table::fread("/home/ricardo/Documentos/ta_na_mesa/data/cooperativasDAP.csv", encoding = "UTF-8",colClasses = c("CNPJEspecifico"="character"))



DAP_ativa <- DAP_ativa  %>%
  mutate(MUNICIPIO= tolower(iconv(MUNICIPIO, from="UTF-8", to="ASCII//TRANSLIT")))
DAP_ativa_RS <- DAP_ativa %>%
  filter(stringr::str_detect(UF,"RS"))
DAP_ativa_RS <- DAP_ativa_RS %>%
  mutate(NOME_T1 = toupper(iconv(NOME_T1,from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  mutate(NOME_T2 = toupper(iconv(NOME_T2,from="UTF-8", to="ASCII//TRANSLIT")))


