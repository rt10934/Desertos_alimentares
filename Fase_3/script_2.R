# pacotes utilizados 
library(dplyr)
library(haven)
library(tidyr)

# Definindo o "diretório" de trabalho, necessita colocar o caminho
setwd("diretório/Etapa_3/")
# bases necessarias:------------------------------------------------------------
pof_compras <- read_dta("pof_compras.dta") # 
morador_uc_key <- read_dta("morador_uc_key.dta") |>
  dplyr::select(key_morador,peso_final) #selecionando apenas as variáveis necessárias:
dicionario_locais <- read_dta("dicionario_locais.dta")|>
  dplyr::mutate(v9004=as.character(v9004))
#-------------------------------------------------------------------------------
tabela6_NA <- pof_compras |> filter(desc_local=="") 
# Operações com as bases para criar filtragens sobre localização e renda -------
## Localização:
pof_compras <- pof_compras |>
  dplyr::mutate(
    situacao=ifelse(tipo_situacao_reg==1,"Urbano",
                    ifelse(tipo_situacao_reg==2,"Rural",tipo_situacao_reg))
  )
## Renda: 
# Calcular os quantis de renda
quantis <- quantile(pof_compras$renda_total, c(0, 0.25, 0.5, 0.75), na.rm = TRUE)
# Usar os quantis calculados
pof_compras <- pof_compras %>%
  mutate(renda = case_when(
    between(renda_total, quantis[1], quantis[2]) ~ "Primeiro_sm",
    between(renda_total, quantis[2], quantis[3]) ~ "Segundo_sm",
    between(renda_total, quantis[3], quantis[4]) ~ "Terceiro_sm",
    renda_total > quantis[4] ~ "Quarto_sm",
    TRUE ~ NA_character_  # Define como NA se nao atender nenhuma condição acima
  ))
remove(quantis)

# FAZENDO A FUNÇÂO:-------------------------------------------------------------
tabela_4_I <- function(pof_compras, morador_uc_key, dicionario_locais) {
  # operacao de contagem
  tabela6_itens_classeprod_local <- pof_compras  |>
    mutate(contagem = 1) |>
    group_by(uf, estrato_pof, tipo_situacao_reg, cod_upa,
             num_dom, num_uc, renda_total, classe_prod, v9004) |>
    summarise(contagem = sum(contagem, na.rm = TRUE),
              valor_mensal = sum(valor_mensal, na.rm = TRUE)) |>
    ungroup() |>
    mutate(key_morador = paste0(uf, estrato_pof, tipo_situacao_reg, cod_upa, num_dom, num_uc))
  
  # merge dos dados com o código morador 
  tabela6_itens_classeprod_local <- inner_join(tabela6_itens_classeprod_local,
                                                          morador_uc_key, by = "key_morador")
  
  # Operacoes soma de pesos para os codigos de produto CNAE
  tabela6_itens_classeprod_local <- tabela6_itens_classeprod_local |>
    mutate(itens = contagem * peso_final) |>
    group_by(classe_prod, v9004) |>
    summarise(itens = sum(itens, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = classe_prod, values_from = itens)
  
  # adicionando dicionario de codigos de locais
  tabela6_itens_classeprod_local <- full_join(tabela6_itens_classeprod_local,
                                                         dicionario_locais, by = "v9004")
  
  # Tabulacao para o word
  tabela6 <- tabela6_itens_classeprod_local |>
    rename(natura = `In natura ou minimamente processado`,
           ultraprocessado = Ultraprocessado,
           processado = Processado,
           prep_culinaria = `Preparações culinárias`,
           ingredientes_culinarios = `Oleos, gorduras, sal e acucar`,
           sem_classe = `Sem classificacao`) |>
    select(desc_local, natura, ultraprocessado, processado, prep_culinaria,
           ingredientes_culinarios, sem_classe)
  
  # fazendo o total
  tabela6 <- tabela6 %>%
    mutate(Total = rowSums(select(., natura, ultraprocessado, processado,
                                  prep_culinaria, ingredientes_culinarios,
                                  sem_classe), na.rm = TRUE)) %>%
    mutate(P_Total = Total / sum(Total)) %>%
    arrange(desc(P_Total))
  
  # tabela NA
  tabela6_NA <- tabela6 |> filter(is.na(desc_local))|>
    dplyr::mutate(desc_local="NA")
  
  # excluindo NA
  tabela6 <- tabela6 |> filter(!(is.na(desc_local)))
  
  # tabela 10 maiores
  tabela_p9s <- head(tabela6, 9)
  
  # tabela com as demais
  tabela_out <- tabela6 %>%
    slice(10:n())
  
  # modificando para ficar outros
  tabela_out <- tabela_out %>% 
    mutate(desc_local = "Outros") %>%
    group_by(desc_local) %>%
    summarise(
      natura = sum(natura, na.rm = TRUE),
      ultraprocessado = sum(ultraprocessado, na.rm = TRUE),
      processado = sum(processado, na.rm = TRUE),
      prep_culinaria = sum(prep_culinaria, na.rm = TRUE),
      ingredientes_culinarios = sum(ingredientes_culinarios, na.rm = TRUE),
      sem_classe = sum(sem_classe, na.rm = TRUE),
      Total = sum(Total, na.rm = TRUE),
      P_Total = sum(P_Total, na.rm = TRUE)
    )
  
  # juntando as tabelas
  tabela6 <- bind_rows(tabela_p9s, tabela6_NA, tabela_out)
  
  # calculando as proporções
  tabela6 <- tabela6 %>%
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
    select(desc_local, natura, p_natura, ultraprocessado, p_ultraprocessado, processado, p_processado,
           prep_culinaria,p_prep_culinaria,ingredientes_culinarios,p_ingredientes_culinarios,
           sem_classe,p_sem_classe,Total,p_Total)
}

# Aplicando o função:-----------------------------------------------------------
# completa
t4_completa <- tabela_4_I(pof_compras,morador_uc_key, dicionario_locais)
# localização:
## Urbano:
t4_Urbano <- tabela_4_I(dplyr::filter(pof_compras,situacao=="Urbano"),
                        morador_uc_key, dicionario_locais)
## Rural:
t4_Rural <- tabela_4_I(dplyr::filter(pof_compras,situacao=="Rural"),
                       morador_uc_key, dicionario_locais)
# Renda:
## primeiro_sm
t4_Primeiro_sm <- tabela_4_I(dplyr::filter(pof_compras,renda=="Primeiro_sm"),
                             morador_uc_key, dicionario_locais)
## Segundo_sm
t4_Segundo_sm <- tabela_4_I(dplyr::filter(pof_compras,renda=="Segundo_sm"),
                            morador_uc_key, dicionario_locais)
## Terceiro_sm
t4_Terceiro_sm <- tabela_4_I(dplyr::filter(pof_compras,renda=="Terceiro_sm"),
                             morador_uc_key, dicionario_locais)
## Quarto_sm
t4_Quarto_sm <- tabela_4_I(dplyr::filter(pof_compras,renda=="Quarto_sm"),
                           morador_uc_key, dicionario_locais)

################################################################################
# TABELA: Milhões de itens de aquisição de alimentos pela população por grupo do 
# Guia Alimentar para a População Brasileira e local de compra da POF 2017/2018 e o 
# percentual proveniente de aquisição de cada local de compra por grupo alimentar.
################################################################################
# função:
calcular_percentual <- function(t8_completa) {
  t8_complementar <- t8_completa %>%
    select(desc_local, natura, ultraprocessado, processado,
           prep_culinaria, ingredientes_culinarios, sem_classe, Total) %>%
    add_row(
      desc_local = "Total",
      natura = sum(t8_completa$natura),
      ultraprocessado = sum(t8_completa$ultraprocessado),
      processado = sum(t8_completa$processado),
      prep_culinaria = sum(t8_completa$prep_culinaria),
      ingredientes_culinarios = sum(t8_completa$ingredientes_culinarios), 
      sem_classe = sum(t8_completa$sem_classe), 
      Total = sum(t8_completa$Total)
    ) %>%
    mutate(p_natura = (natura / sum(natura[1:11])*100),
           p_ultraprocessado = (ultraprocessado / sum(ultraprocessado[1:11])*100),
           p_processado = (processado / sum(processado[1:11])*100),
           p_prep_culinaria = (prep_culinaria / sum(prep_culinaria[1:11])*100),
           p_ingredientes_culinarios = (ingredientes_culinarios / sum(ingredientes_culinarios[1:11])*100),
           p_sem_classe = (sem_classe / sum(sem_classe[1:11])*100),
           p_Total = (Total / sum(Total[1:11])*100)) %>%
    select(desc_local, natura, p_natura, ultraprocessado, p_ultraprocessado,
           processado, p_processado, prep_culinaria, p_prep_culinaria,
           ingredientes_culinarios, p_ingredientes_culinarios,
           sem_classe, p_sem_classe, Total, p_Total)
  
  return(t8_complementar)
}
# chamei de tabela 10
# aplicando o função:
# completa
t10_completa <- calcular_percentual(t4_completa)
# localizacao:
## Urbano:
t10_Urbano <- calcular_percentual(t4_Urbano)
## Rural:
t10_Rural <- calcular_percentual(t4_Rural)
# Renda:
## primeiro_sm
t10_Primeiro_sm <- calcular_percentual(t4_Primeiro_sm)
## Segundo_sm
t10_Segundo_sm <- calcular_percentual(t4_Segundo_sm)
## Terceiro_sm
t10_Terceiro_sm <- calcular_percentual(t4_Terceiro_sm)
## Quarto_sm
t10_Quarto_sm <- calcular_percentual(t4_Quarto_sm)

################################################################################
# funções extras: para arendondamento das tabelas:------------------------------
################################################################################
ajustar_arredondamento_base <- function(base) {
  base <- base %>%
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
  
  return(base)
}
# aplicando a funções de arendodamento:
t4_completa  <- ajustar_arredondamento_base(t4_completa)
t10_completa <- ajustar_arredondamento_base(t10_completa)

# comando para salvar tabelas no formato excel:
 openxlsx::write.xlsx(t4_completa, file = "Diretório/t4_completa.xlsx")
 openxlsx::write.xlsx(t10_completa, file = "Diretório/t10_completa.xlsx")
 #OBS: substituir "Diretório" pelo caminho da pasta de salvamento das tabelas
 
