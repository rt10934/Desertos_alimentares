################################################################################
# TABELA 9 - NÚMERO DE ITENS ADQUIRIDOS POR CLASSE DE PRODUTO, LOCAL DE COMPRA E UF ----
################################################################################
# pacotes liberados
library(dplyr)
library(haven)

# Definindo o "diretório" de trabalho, necessita colocar o caminho
setwd("diretório/Etapa_3/")
# Bases necessarias:
pof_morador <- read_dta("pof_morador.dta") |> dplyr::mutate(uf=as.numeric(uf))
nome_local <- read_dta("nome_local.dta") |> dplyr::mutate(cod_local=as.numeric(cod_local))
uf <- read_dta("uf.dta") 
# operações:

Perfil_estabelec <- pof_morador  %>%
  dplyr::mutate(pof_morador, itens = contagem * peso_final)%>% #Contagem total de itens com peso de famílias
  dplyr::filter(!(classe_prod==""))%>% # filtro de NA
  group_by(classe_prod, cod_local,uf) %>%
  summarize(total_itens = sum(itens, na.rm = TRUE))%>%
  dplyr::ungroup()%>%
  tidyr::pivot_wider(names_from=classe_prod,
                     values_from=total_itens)%>%# transformando os produtos em coluna:
  janitor::clean_names()%>%#limpando nomes
  dplyr::mutate(total = rowSums(select(.,in_natura_ou_minimamente_processado, oleos_gorduras_sal_e_acucar,
                                       preparacoes_culinarias, processado, sem_classificacao, ultraprocessado),
                                na.rm = TRUE)) 
# merge dos dados com nome do local e uf:
Perfil_estabelec <- dplyr::full_join(Perfil_estabelec, nome_local,by="cod_local")
Perfil_estabelec <- dplyr::full_join(Perfil_estabelec, uf,by="uf")
# filtro para excluir os NA
Perfil_estabelec <- Perfil_estabelec |>
  dplyr::select(-cod_local,-uf)|>
  dplyr::filter(!(is.na(nome_uf)))
# Renomeando as colunas
Perfil_estabelec <- Perfil_estabelec %>%
  mutate(nome_local = ifelse(is.na(nome_local), "Excluidos",
                             ifelse(nome_local == "Padaria_prod", "Padaria_prod",
                                    ifelse(nome_local == "Padaria_revenda", "Padaria_revenda",
                                           ifelse(nome_local == "Restaurante", "Restaurante",
                                                  ifelse(nome_local == "Lanchonetes", "Lanchonetes",
                                                         ifelse(nome_local == "Cantinas", "Cantinas",
                                                                ifelse(nome_local == "Hortifruti", "Hortifruti",
                                                                       ifelse(nome_local == "Bares", "Bares",
                                                                              ifelse(nome_local == "AliGeral", "AliGeral",
                                                                                     ifelse(nome_local == "Ambulantes", "Ambulantes",
                                                                                            ifelse(nome_local == "Bebidas", "Bebidas",
                                                                                                   ifelse(nome_local == "Doces", "Doces",
                                                                                                          ifelse(nome_local == "Peixaria", "Peixaria",
                                                                                                                 ifelse(nome_local == "LaticiniosFrios", "LaticiniosFrios",
                                                                                                                        ifelse(nome_local == "FornecimentoDom", "FornecimentoDom",
                                                                                                                               ifelse(nome_local == "Excluidos", "Excluidos", nome_local))))))))))))))))) %>%
  dplyr::group_by(nome_uf,nome_local) %>% # passo para somar os nomes modificados
  dplyr::summarise(
    in_natura_ou_minimamente_processado = sum(in_natura_ou_minimamente_processado,na.rm = T),
    ultraprocessado = sum(ultraprocessado,na.rm = T),
    processado = sum(processado,na.rm = T),
    preparacoes_culinarias = sum(preparacoes_culinarias,na.rm = T),
    oleos_gorduras_sal_e_acucar = sum(oleos_gorduras_sal_e_acucar,na.rm = T),
    sem_classificacao = sum(sem_classificacao,na.rm = T),
    total = sum(total,na.rm = T))%>%
  dplyr::ungroup()

# criando coluna com as somas dos itens
Perfil_estabelec <- Perfil_estabelec %>%dplyr::mutate(
  p_natura=(in_natura_ou_minimamente_processado/total),
  p_ultraprocessado=(ultraprocessado/total),
  p_processado=(processado/total),
  p_prep_culinaria=(preparacoes_culinarias/total),
  p_oleos_gorduras_sal_acucar=(oleos_gorduras_sal_e_acucar/total),
  p_sem_classe=(sem_classificacao/total)
) %>% # criando a participacao de cada classe de alimento:
  dplyr::filter(!(is.na(nome_uf))) %>%
  dplyr::mutate_all(~replace(., is.na(.), 0)) # substituindo "NA" por zero


#-------------------------------------------------------------------------------
unique(Perfil_estabelec$nome_uf)
Perfil_estabelec <- Perfil_estabelec |>
  dplyr::mutate(
    nome_uf =ifelse(nome_uf=="Rondônia","Rondonia",
                    ifelse(nome_uf=="Pará","Para",
                           ifelse(nome_uf=="Amapá","Amapa",
                                  ifelse(nome_uf=="Maranhão","Maranhao",
                                         ifelse(nome_uf=="Piauí","Piaui",
                                                ifelse(nome_uf=="Ceará","Ceara",
                                                       ifelse(nome_uf=="Paraíba","Paraiba",
                                                              ifelse(nome_uf=="Espírito Santo","Espirito Santo",
                                                                     ifelse(nome_uf=="São Paulo","Sao Paulo",
                                                                            ifelse(nome_uf=="Paraná","Parana",
                                                                                   ifelse(nome_uf=="Goiás","Goias",nome_uf)))))))))))
  )

unique(Perfil_estabelec$nome_uf)


# fazendo as siglas:
# Vetores com os nomes completos e siglas dos estados
nomes_completos <- c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins", "Maranhao", 
                     "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba", "Pernambuco", "Alagoas", "Sergipe", 
                     "Bahia", "Minas Gerais", "Espirito Santo", "Rio de Janeiro", "Sao Paulo", "Parana", 
                     "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goias", 
                     "Distrito Federal", NA)



siglas <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", 
            "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF", NA)

regiao = c("Norte", "Norte", "Norte", "Norte", "Norte", "Norte", "Norte", "Nordeste", "Nordeste", "Nordeste", 
           "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Sudeste", "Sudeste", 
           "Sudeste", "Sudeste", "Sul", "Sul", "Sul", "Centro-Oeste", "Centro-Oeste", "Centro-Oeste", "Centro-Oeste", NA)

# Criando um dataframe para mapear nomes completos as siglas
mapeamento <- data.frame(nome_uf = nomes_completos, sigla_uf = siglas, Greg=regiao)

# Realizando o merge para adicionar a sigla correspondente ao dataframe original
Perfil_estabelec <- Perfil_estabelec %>%
  dplyr::left_join(mapeamento, by = "nome_uf")
# indicador---------------------------------------------------------------------
Perfil_estabelec <- Perfil_estabelec |>
  dplyr::select(Greg,sigla_uf,nome_uf,nome_local,p_natura, p_ultraprocessado, p_processado, 
                p_prep_culinaria, p_oleos_gorduras_sal_acucar, p_sem_classe)

# criando indicador:
Perfil_estabelec <- Perfil_estabelec |>
  mutate(I_natura=ifelse(p_natura>=0.5,1,0),
         I_ultraprocessado=ifelse(p_ultraprocessado>=0.5,1,0),
         I_processado=ifelse(p_processado>=0.5,1,0),
         I_prep_culinaria=ifelse(p_prep_culinaria>=0.5,1,0),
         I_oleos_gorduras_sal_acucar=ifelse(p_oleos_gorduras_sal_acucar>=0.5,1,0),
         I_sem_classe=ifelse(p_sem_classe>=0.5,1,0))

Perfil_estabelec <- Perfil_estabelec |>
  dplyr::arrange(Greg,sigla_uf,nome_local)
# indicador desconsiderando os sem classificacao:-------------------------------
Perfil_estabelec <- Perfil_estabelec %>% 
  mutate(S_total= rowSums(select(.,p_natura,p_ultraprocessado,p_processado,p_prep_culinaria,p_oleos_gorduras_sal_acucar),na.rm=T),
         PAR_natura=p_natura/S_total,
         PAR_ultraprocessado=p_ultraprocessado/S_total,
         PAR_processado=p_processado/S_total,
         PAR_prep_culinaria=p_prep_culinaria/S_total,
         PAR_oleos_gorduras_sal_acucar= p_oleos_gorduras_sal_acucar/S_total)

# criando indicador:
Perfil_estabelec <- Perfil_estabelec |>
  mutate(PAR_I_natura=ifelse(PAR_natura>=0.5,1,0),
         PAR_I_ultraprocessado=ifelse(PAR_ultraprocessado>=0.5,1,0),
         PAR_I_processado=ifelse(PAR_processado>=0.5,1,0),
         PAR_I_prep_culinaria=ifelse(PAR_prep_culinaria>=0.5,1,0),
         PAR_I_oleos_gorduras_sal_acucar=ifelse(PAR_oleos_gorduras_sal_acucar>=0.5,1,0))
################################################################################
openxlsx::write.xlsx(Perfil_estabelec, file = "Diretório/BD Perfil Estabelecimentos.xlsx")

#OBS: substituir "Diretório" pelo caminho da pasta de salvamento das tabelas