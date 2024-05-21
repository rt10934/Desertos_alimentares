# pacotes liberados 
library(dplyr)
library(haven)
library(tidyr)

# Definindo o "diretório" de trabalho, necessita colocar o caminho
setwd("diretório/Etapa_3/")
################################################################################
# Bases necessarias:
pof_morador <- read_dta("pof_morador.dta")
nome_local <- read_dta("nome_local.dta")
# Operacoes com as bases--------------------------------------------------------
# Localizacao:
pof_morador <- pof_morador |>
  dplyr::mutate(
    situacao=ifelse(tipo_situacao_reg==1,"Urbano",
                    ifelse(tipo_situacao_reg==2,"Rural",tipo_situacao_reg))
  )
# Renda 
# Calcular os quantis de renda
quantis <- quantile(pof_morador$renda_total, c(0, 0.25, 0.5, 0.75), na.rm = TRUE)
# Usar os quantis calculados no case_when
pof_morador <- pof_morador %>%
  mutate(renda = case_when(
    between(renda_total, quantis[1], quantis[2]) ~ "Primeiro_sm",
    between(renda_total, quantis[2], quantis[3]) ~ "Segundo_sm",
    between(renda_total, quantis[3], quantis[4]) ~ "Terceiro_sm",
    renda_total > quantis[4] ~ "Quarto_sm",
    TRUE ~ NA_character_  # Define como NA se nao atender nenhuma condicao acima
  ))
remove(quantis)

# funcao para realizar as operacoes:--------------------------------------------

tabela_8_tex <- function(pof_morador, nome_local) {
  # Manipulações
  tabela8_itens_classeprod_local <- pof_morador %>%
    mutate(itens = contagem * peso_final) %>%
    filter(!(classe_prod == "")) %>%
    group_by(classe_prod, cod_local) %>%
    summarise(total_itens = sum(itens, na.rm = TRUE)) %>%
    pivot_wider(names_from = classe_prod, values_from = total_itens) %>%
    mutate(cod_local = as.character(cod_local))
  
  # Merge dos dados
  tabela8_itens_classeprod_local <- full_join(tabela8_itens_classeprod_local,
                                                  nome_local, by = "cod_local")
  
  # Tabulação para o word
  tabela8 <- tabela8_itens_classeprod_local %>%
    rename(natura = `In natura ou minimamente processado`,
           ultraprocessado = Ultraprocessado,
           processado = Processado,
           prep_culinaria = `Preparações culinárias`,
           ingredientes_culinarios = `Oleos, gorduras, sal e acucar`,
           sem_classe = `Sem classificacao`) %>%
    select(nome_local, natura, ultraprocessado, processado,
           prep_culinaria, ingredientes_culinarios, sem_classe) %>%
    mutate(Total = rowSums(select(., natura, ultraprocessado, processado, prep_culinaria, ingredientes_culinarios, sem_classe), na.rm = TRUE)) %>%
    mutate(P_Total = Total / sum(Total)) 
  
  # Renomeando as colunas
  tabela8 <- tabela8 %>%
    mutate(nome_local = ifelse(is.na(nome_local), "Sem correspondência",
                               ifelse(nome_local == "Padaria_prod", "Padaria e Confeitarias",
                                      ifelse(nome_local == "Padaria_revenda", "Padaria e Confeitarias",
                                             ifelse(nome_local == "Restaurante", "Restaurantes",
                                                    ifelse(nome_local == "Lanchonetes", "Lanchonetes, Casas de Chá, de Sucos e Similares",
                                                           ifelse(nome_local == "Cantinas", "Cantinas - Serviços de Alimentação Privativos",
                                                                  ifelse(nome_local == "Hortifruti", "Varejistas de Hortifrutigranjeiros",
                                                                         ifelse(nome_local == "Bares", "Bares e Similares",
                                                                                ifelse(nome_local == "AliGeral", "Varejistas de Produtos Alimentícios em Geral",
                                                                                       ifelse(nome_local == "Ambulantes", "Serviços Ambulantes de Alimentação",
                                                                                              ifelse(nome_local == "Bebidas", "Varejista de Bebidas",
                                                                                                     ifelse(nome_local == "Doces", "Varejistas de Doces, Balas, Bombons e Similares",
                                                                                                            ifelse(nome_local == "Peixaria", "Peixarias",
                                                                                                                   ifelse(nome_local == "LaticiniosFrios", "Varejistas de Laticínios e Frios",
                                                                                                                          ifelse(nome_local == "FornecimentoDom", "Fornecimento de Alimentos Preparados para Consumo Domiciliar",
                                                                                                                                 ifelse(nome_local == "Excluidos", "Sem correspondência", nome_local))))))))))))))))) %>%
    dplyr::group_by(nome_local) %>% # passo para somar os nomes modificados
    dplyr::summarise(
      natura = sum(natura,na.rm = T),
      ultraprocessado = sum(ultraprocessado,na.rm = T),
      processado = sum(processado,na.rm = T),
      prep_culinaria = sum(prep_culinaria,na.rm = T),
      ingredientes_culinarios = sum(ingredientes_culinarios,na.rm = T),
      sem_classe = sum(sem_classe,na.rm = T),
      Total = sum(Total,na.rm = T),
      P_Total = sum(P_Total,na.rm = T)
    ) %>% dplyr::ungroup() %>%
    arrange(desc(P_Total))
  
  tabela8 <- tabela8 %>%
    filter(!is.na(nome_local)) %>%
    mutate(
      p_natura = round(ifelse(is.na(natura / Total * 100), 0, natura / Total * 100), digits = 9),
      p_ultraprocessado = round(ifelse(is.na(ultraprocessado / Total * 100), 0, ultraprocessado / Total * 100), digits = 9),
      p_processado = round(ifelse(is.na(processado / Total * 100), 0, processado / Total * 100), digits = 9),
      p_prep_culinaria = round(ifelse(is.na(prep_culinaria / Total * 100), 0, prep_culinaria / Total * 100), digits = 9),
      p_ingredientes_culinarios = round(ifelse(is.na(ingredientes_culinarios / Total * 100), 0, ingredientes_culinarios / Total * 100), digits = 9),
      p_sem_classe = round(ifelse(is.na(sem_classe / Total * 100), 0, sem_classe / Total * 100), digits = 9),
      p_Total = round(ifelse(is.na(Total / Total * 100), 0, Total / Total * 100), digits = 9),
      natura = round(ifelse(is.na(natura), 0, natura / 1000000), digits = 9),
      ultraprocessado = round(ifelse(is.na(ultraprocessado), 0, ultraprocessado / 1000000), digits = 9),
      processado = round(ifelse(is.na(processado), 0, processado / 1000000), digits = 9),
      prep_culinaria = round(ifelse(is.na(prep_culinaria), 0, prep_culinaria / 1000000), digits = 9),
      ingredientes_culinarios = round(ifelse(is.na(ingredientes_culinarios), 0, ingredientes_culinarios / 1000000), digits = 9),
      sem_classe = round(ifelse(is.na(sem_classe), 0, sem_classe / 1000000), digits = 9),
      Total = round(ifelse(is.na(Total), 0, Total / 1000000), digits = 9)
    ) %>%
    select(nome_local, natura, p_natura, ultraprocessado, p_ultraprocessado, processado, p_processado,
           prep_culinaria, p_prep_culinaria, ingredientes_culinarios, p_ingredientes_culinarios,
           sem_classe, p_sem_classe, Total, p_Total)|>
    dplyr::arrange(desc(Total))
  
}

################################################################################
# Tabelas de Milhões de itens de aquisição de alimentos pela população por grupo
# do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE):
################################################################################
# aplicando o funcao:
# completa
t8_completa <- tabela_8_tex(pof_morador, nome_local)

# localizacao:
## Urbano:
t8_Urbano <- tabela_8_tex(dplyr::filter(pof_morador,situacao=="Urbano"),
                          nome_local)
## Rural:
t8_Rural <- tabela_8_tex(dplyr::filter(pof_morador,situacao=="Rural"),
                         nome_local)
# Renda:
## primeiro_sm
t8_Primeiro_sm <- tabela_8_tex(dplyr::filter(pof_morador,renda=="Primeiro_sm"),
                               nome_local)
## Segundo_sm
t8_Segundo_sm <- tabela_8_tex(dplyr::filter(pof_morador,renda=="Segundo_sm"),
                              nome_local)
## Terceiro_sm
t8_Terceiro_sm <- tabela_8_tex(dplyr::filter(pof_morador,renda=="Terceiro_sm"),
                               nome_local)
## Quarto_sm
t8_Quarto_sm <- tabela_8_tex(dplyr::filter(pof_morador,renda=="Quarto_sm"),
                             nome_local)

# segunda funcao----------------------------------------------------------------
# funcao:
library(dplyr)

calcular_percentuais <- function(t8_completa) {
  t8_complementar <- t8_completa %>%
    select(nome_local, natura, ultraprocessado, processado,
           prep_culinaria, ingredientes_culinarios, sem_classe, Total) %>%
    add_row(
      nome_local = "Total",
      natura = sum(t8_completa$natura),
      ultraprocessado = sum(t8_completa$ultraprocessado),
      processado = sum(t8_completa$processado),
      prep_culinaria = sum(t8_completa$prep_culinaria),
      ingredientes_culinarios = sum(t8_completa$ingredientes_culinarios), 
      sem_classe = sum(t8_completa$sem_classe), 
      Total = sum(t8_completa$Total)
    ) %>%
    mutate(p_natura = (natura / sum(natura[1:18])*100),
           p_ultraprocessado = (ultraprocessado / sum(ultraprocessado[1:18])*100),
           p_processado = (processado / sum(processado[1:18])*100),
           p_prep_culinaria = (prep_culinaria / sum(prep_culinaria[1:18])*100),
           p_ingredientes_culinarios = (ingredientes_culinarios / sum(ingredientes_culinarios[1:18])*100),
           p_sem_classe = (sem_classe / sum(sem_classe[1:18])*100),
           p_Total = (Total / sum(Total[1:18])*100)) %>%
    select(nome_local, natura, p_natura, ultraprocessado, p_ultraprocessado,
           processado, p_processado, prep_culinaria, p_prep_culinaria,
           ingredientes_culinarios, p_ingredientes_culinarios,
           sem_classe, p_sem_classe, Total, p_Total)
  
  return(t8_complementar)
}
################################################################################
# Tabela 7. Milhões de itens e percentual de aquisição de alimentos pela população
# por grupo do Guia Alimentar para a População Brasileira e estabelecimento de 
# alimentação (CNAE) e o percentual proveniente de aquisição de cada estabelecimento
# de compra por grupo alimentar
################################################################################
# aplicando o funcao:
# completa
t9_completa <- calcular_percentuais(t8_completa)
# localizacao:
## Urbano:
t9_Urbano <- calcular_percentuais(t8_Urbano)
## Rural:
t9_Rural <- calcular_percentuais(t8_Rural)
# Renda:
## primeiro_sm
t9_Primeiro_sm <- calcular_percentuais(t8_Primeiro_sm)
## Segundo_sm
t9_Segundo_sm <- calcular_percentuais(t8_Segundo_sm)
## Terceiro_sm
t9_Terceiro_sm <- calcular_percentuais(t8_Terceiro_sm)
## Quarto_sm
t9_Quarto_sm <- calcular_percentuais(t8_Quarto_sm)



################################################################################
# funcoes extras: para arendondamento das tabelas:------------------------------
################################################################################

ajustar_arredondamento <- function(t9_completa) {
  t9_completa <- t9_completa %>%
    mutate(natura = round(natura, digits = 0),
           ultraprocessado = round(ultraprocessado, digits = 0),
           processado = round(processado, digits = 0),
           prep_culinaria = round(prep_culinaria, digits = 0),
           ingredientes_culinarios = round(ingredientes_culinarios, digits = 0), 
           sem_classe = round(sem_classe, digits = 0), 
           Total = round(Total, digits = 0),
           p_natura = round(p_natura, digits = 1),
           p_ultraprocessado = round(p_ultraprocessado, digits = 1),
           p_processado = round(p_processado, digits = 1),
           p_prep_culinaria = round(p_prep_culinaria, digits = 1),
           p_ingredientes_culinarios = round(p_ingredientes_culinarios, digits = 1),
           p_sem_classe = round(p_sem_classe, digits = 1),
           p_Total = round(p_Total, digits = 1)) 
  
  return(t9_completa)
}
# exportando a tabelas ---------------------------------------------------------

# comleta 
t8_completa <- ajustar_arredondamento(t8_completa)
# para salvar as tabelas
openxlsx::write.xlsx(t8_completa, file = "Diretório/t8_completa_R.xlsx")

# rural 
t8_Rural <- ajustar_arredondamento(t8_Rural)
# para salvar as tabelas
openxlsx::write.xlsx(t8_Rural, file = "Diretório/t8_Rural_R.xlsx")

# urbano 
t8_Urbano <- ajustar_arredondamento(t8_Urbano)
# para salvar as tabelas
openxlsx::write.xlsx(t8_Urbano, file = "Diretório/t8_Urbano_R.xlsx")

# t9_Primeiro_sm 
t8_Primeiro_sm <- ajustar_arredondamento(t8_Primeiro_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t8_Primeiro_sm, file = "Diretório/t8_Primeiro_sm_R.xlsx")

# t9_Segundo_sm 
t8_Segundo_sm <- ajustar_arredondamento(t8_Segundo_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t8_Segundo_sm, file = "Diretório/t8_Segundo_sm_R.xlsx")

# t9_Terceiro_sm 
t8_Terceiro_sm <- ajustar_arredondamento(t8_Terceiro_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t8_Terceiro_sm, file = "Diretório/t8_Terceiro_sm_R.xlsx")

# urbano 
t8_Quarto_sm <- ajustar_arredondamento(t8_Quarto_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t8_Quarto_sm, file = "Diretório/t8_Quarto_sm_R.xlsx")

# segunda tabela 

# comleta 
t9_completa <- ajustar_arredondamento(t9_completa)
# para salvar as tabelas
openxlsx::write.xlsx(t9_completa, file = "Diretório/t9_completa_R.xlsx")

# rural 
t9_Rural <- ajustar_arredondamento(t9_Rural)
# para salvar as tabelas
openxlsx::write.xlsx(t9_Rural, file = "Diretório/t9_Rural_R.xlsx")

# urbano 
t9_Urbano <- ajustar_arredondamento(t9_Urbano)
# para salvar as tabelas
openxlsx::write.xlsx(t9_Urbano, file = "Diretório/t9_Urbano_R.xlsx")

# t9_Primeiro_sm 
t9_Primeiro_sm <- ajustar_arredondamento(t9_Primeiro_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t9_Primeiro_sm, file = "Diretório/t9_Primeiro_sm_R.xlsx")

# t9_Segundo_sm 
t9_Segundo_sm <- ajustar_arredondamento(t9_Segundo_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t9_Segundo_sm, file = "Diretório/t9_Segundo_sm_R.xlsx")

# t9_Terceiro_sm 
t9_Terceiro_sm <- ajustar_arredondamento(t9_Terceiro_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t9_Terceiro_sm, file = "Diretório/t9_Terceiro_sm_R.xlsx")

# urbano 
t9_Quarto_sm <- ajustar_arredondamento(t9_Quarto_sm)
# para salvar as tabelas
openxlsx::write.xlsx(t9_Quarto_sm, file = "Diretório/t9_Quarto_sm_R.xlsx")

#OBS: substituir "Diretório" pelo caminho da pasta de salvamento das tabelas



















