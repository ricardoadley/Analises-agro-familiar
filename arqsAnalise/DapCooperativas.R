
#filtra para exibir apenas os resultados do RS

DAP_cooperativas <- DAP_cooperativas %>%
  filter(UF == "RS")


empenho_corrigido <- empenho %>%
  left_join(select(info_contrato, id_contrato, nr_documento_contratado), by = "id_contrato") %>%
  mutate(nr_documento_contratado = ifelse(is.na(nr_documento_contratado), cnpj_cpf, nr_documento_contratado)) %>%
  mutate(nr_documento_contratado = paste0("000000", nr_documento_contratado)) %>%
  mutate(nr_documento_contratado = ifelse(tp_pessoa == "PF", substr(nr_documento_contratado, start = nchar(nr_documento_contratado)-10, stop=nchar(nr_documento_contratado)), substr(nr_documento_contratado, start = nchar(nr_documento_contratado)-13, stop=nchar(nr_documento_contratado))))

#formata o campo do cpnpj para o formato usado

DAP_cooperativas <- DAP_cooperativas %>%
  mutate(CNPJEspecifico  = gsub("\\D", "",CNPJEspecifico ))

#testes

#juntando tabela de empenhos com tabela das cooperativas a partir de
#cnpj iguais

empenhos_para_cooperativas <- merge(empenho_corrigido, DAP_cooperativas, by.x = "cnpj_cpf", by.y = "CNPJEspecifico")


#tabela com coluna de quem contem dap
#todas as pessoas fisicas (PF) foram consideradas com DAP

contem_dap <- empenho_corrigido %>%
  left_join(DAP_cooperativas, by = c("nr_documento_contratado" = "CNPJEspecifico")) %>%
              mutate(agri_fam = ifelse(!is.na(CDDAPJ) | tp_pessoa == "PF", 1, 0))

#separando a tabela para apenas pf

contem_dap_pf <- contem_dap %>%
  filter(tp_pessoa == "PF")

#separando a tabela para apenas pj

contem_dap_pj <- contem_dap %>%
  filter(tp_pessoa == "PJ")

#soma de Pj que contem dap e das que nao contem

pj_sem_dap <- contem_dap_pj %>%
  group_by(agri_fam) %>%
  count()

#calculando contratos concluidos com pessoas com DAP e sem

contratos_concluidos_com_coop_pj_pf <- contem_dap %>%
  group_by(id_orgao) %>%
  select(id_orgao,nome_orgao,cnpj_cpf,nm_credor,id_contrato,agri_fam) %>%
  mutate(contr_totais=n_distinct(id_contrato))

#removendo possiveis repetiççoes na tabela

contratos_concluidos_com_coop_pj_pf <- unique(contratos_concluidos_com_coop_pj_pf)

#filtrando a tabela apenas por pessoas com DAP

contratos_concluidos_com_coop_pj_pf_DAP <- contratos_concluidos_com_coop_pj_pf %>%
  filter(agri_fam == 1)

#Calculando a quantidade de contratos por orgao

qtd_contratos_coop_pj <- contratos_concluidos_com_coop_pj_pf %>%
group_by(nome_orgao,agri_fam,id_orgao) %>%
 count()

#separando a quantidade para apenas com DAP

qtd_contratos_agro_fam <- qtd_contratos_coop_pj %>%
  filter(agri_fam == 1)


qtd_contratos_agro_fam<- rename(qtd_contratos_agro_fam, ContratoComAgroFam = n )

#Separando a quantidade para apenas sem DAP

qtd_contratos_sem_agrofam <-
  contratos_concluidos_com_coop_pj_pf %>%
  group_by(nome_orgao,agri_fam,id_orgao) %>%
  count()

qtd_contratos_sem_agrofam <- qtd_contratos_sem_agrofam%>%
  filter(agri_fam == 0)

qtd_contratos_sem_agrofam <- rename(qtd_contratos_sem_agrofam, ContratoSemAgroFam = n)

#Juntando os dois calculos 

qtd_contratos_totais <- qtd_contratos_agro_fam %>%
  left_join(select(qtd_contratos_sem_agrofam ,id_orgao,nome_orgao,ContratoSemAgroFam), by = "id_orgao")

qtd_contratos_totais <- qtd_contratos_totais %>%
  select(id_orgao,nome_orgao.x,ContratoComAgroFam,ContratoSemAgroFam)

qtd_contratos_totais <- unique(qtd_contratos_totais)

#calculando quantidade de contratos totais

qtd_contratos_totais <- qtd_contratos_totais %>%
  mutate(QtdContratos = ContratoComAgroFam + ContratoSemAgroFam)

qtd_contratos_totais <- rename(qtd_contratos_totais, nome_orgao = nome_orgao.x)

#ordenando por quantidade de contratos

qtd_contratos_totais <- qtd_contratos_totais %>%
  arrange(desc(QtdContratos))

#cria tabela para exibicao

qtd_contratos_totais %>%
  head(10)%>%
  kable(align="l", format.args = list(big.mark = "."),digits=2) %>%
  kable_styling(bootstrap_options=c("striped"),position = "center")

#removendos valores nan

contem_dap_pf <- contem_dap_pf %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0))

#soma liquidacao para pessoas fisicas

soma_liq_pessoas_fisicas <- contem_dap_pf %>%
  group_by(id_orgao )%>%
  summarise(soma_liq_pf = sum(vl_liquidacao))

#removendo valores nan

contem_dap_pj <- contem_dap_pj %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0))

#soma liquidacoes para pj e cooperativas com dap

soma_liq_pj_cooperaivas <- contem_dap_pj %>%
  filter(agri_fam == 1) %>%
  group_by(id_orgao )%>%
  summarise(soma_liq_pj_cooperativas = sum(vl_liquidacao))

#soma liquidacoes para pj e cooperativas sem dap

soma_liq_coop_pj_sem_dap <- contem_dap_pj %>%
  filter(agri_fam == 0) %>%
  group_by(id_orgao )%>%
  summarise(soma_liq_pj_cooperativas_sem_dap = sum(vl_liquidacao))

#tabela de exibicao dos dados

exibicao <- contem_dap %>%
  select(id_orgao,nome_orgao)

exibicao <- unique(exibicao)

exibicao <- exibicao %>%
  left_join(soma_liq_coop_pj_sem_dap, by="id_orgao")

exibicao <- exibicao %>%
  left_join(soma_liq_pj_cooperaivas, by="id_orgao")

exibicao <- exibicao %>%
  left_join(soma_liq_pessoas_fisicas,by = "id_orgao")

exibicao <- exibicao %>%
  mutate(soma_liq_pj_cooperativas_sem_dap = tidyr::replace_na(soma_liq_pj_cooperativas_sem_dap, 0))%>%
  mutate(soma_liq_pj_cooperativas = tidyr::replace_na(soma_liq_pj_cooperativas, 0))%>%
  mutate(soma_liq_pf = tidyr::replace_na(soma_liq_pf, 0))


exibicao <- exibicao %>%
  mutate(soma_liq_total = soma_liq_pj_cooperativas_sem_dap + soma_liq_pj_cooperativas +soma_liq_pf)

exibicao <- exibicao %>%
  mutate(perct = (100*(soma_liq_pj_cooperativas + soma_liq_pf)/soma_liq_total))

exibicao <- exibicao %>%
  arrange(desc(soma_liq_total))

exibicao %>%
  head(10)%>%
  kable(align="l", format.args = list(big.mark = "."),digits=2) %>%
  kable_styling(bootstrap_options=c("striped"),position = "center")


#para baixo esta a analise anterior atualmente desconsiderada
#a superior esta mais completa e com menos incosistencias.





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

#retirando possiveis repetiÃ§Ãµes

contrato_totais_com_cooperativas <- unique(contrato_totais_com_cooperativas)

#criando tabela com a soma total das liquidacoes direcionadas para
#cooperativas com DAP por parte da prefeitura

soma_total_liq_orgao_para_cooperativas<- empenhos_para_cooperativas %>%
  group_by(id_orgao )%>%
  summarise(soma_liq_cooperativas = sum(vl_liquidacao))

#ordenacao por id das prefeituras

soma_total_liq_orgao_para_cooperativas <- soma_total_liq_orgao_para_cooperativas %>%
  arrange(desc(id_orgao))

#criando tabela com soma total das liquidacoes feitas pelas prefeitura e
#com quantas cooperativas com DAP elas tem contrato

contrato_totais_com_cooperativas <- soma_total_liq_orgao_para_cooperativas %>%
  left_join(contrato_totais_com_cooperativas, by = "id_orgao")

#ordenacao por soma total das liquidacoes da maior para a menor

contrato_totais_com_cooperativas <- contrato_totais_com_cooperativas %>%
  arrange(desc(soma_liq_cooperativas))

#liquidacao total orgao para comparacao com cooperativa
liquidacao_orgao <- empenho %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0)) %>%
  group_by(id_orgao) %>%
  summarise(somas_liquidacoes = sum(vl_liquidacao))
#comparacao
porcentagem_para_cooperativas_e_pj<- liquidacao_orgao %>%
  left_join(select(soma_total_liq_orgao_para_cooperativas,id_orgao,soma_liq_cooperativas), by="id_orgao")

porcentagem_para_cooperativas_e_pj <- porcentagem_para_cooperativas_e_pj %>%
  mutate(soma_liq_cooperativas= tidyr::replace_na(soma_liq_cooperativas, 0)) %>%
  mutate(perct = (100*soma_liq_cooperativas)/somas_liquidacoes)


#soma de integrantes pf e pj das cooperativas que possuem DAP proprio de pf ou de pj

socios_com_dap <- DAP_cooperativas %>%
  summarize(soma_de_socios = sum(N_SOCIOS_COM_DAP_PF) + sum(N_SOCIOS_COM_DAP_PJ))

#Separando municipios para calculo do total de municipios analisados

total_municipios <- orgaos %>%
  select(nome_municipio)

#Removendo duplicações de nomes do municipios para evitar dupla contagem

total_municipios <- unique(total_municipios)

#soma de valor licitacoes

valor_licitacoes <- licitacoes %>%
  mutate(vl_estimado_licitacao = tidyr::replace_na(vl_estimado_licitacao, 0))%>%
  summarize(soma_valores = sum(vl_estimado_licitacao))

#ordena tabela por maior quantidade de contratos

contrato_totais_com_cooperativas <- contrato_totais_com_cooperativas%>%
    arrange(desc(contr_totais))

#ordena tabela por maior valor de liquidacao para cooperativas
contrato_totais_com_cooperativas <- contrato_totais_com_cooperativas%>%
  arrange(desc(soma_liq_cooperativas))

#removendo NaN da tabela

porcentagem_para_cooperativas_e_pj<- porcentagem_para_cooperativas_e_pj %>%
  mutate(perct = tidyr::replace_na(perct, 0))

porcentagem_zero <- porcentagem_para_cooperativas_e_pj %>%
  group_by(perct) %>%
  count()

contrato_totais_com_cooperativas %>%
  head(10)%>%
  kable(align="l", format.args = list(big.mark = "."),digits=2) %>%
  kable_styling(bootstrap_options=c("striped"),position = "center")