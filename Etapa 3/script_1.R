# pacotes necessários-----------------------------------------------------------
library(dplyr)
library(haven)

# Definindo o "diretório" de trabalho, necessita colocar o caminho
setwd("diretório/Etapa_3/")
################################################################################
# Primeira etapa----------------------------------------------------------------
################################################################################
# Bases necessárias:------------------------------------------------------------
pof_morador <- read_dta("pof_morador.dta")

# Operacões com as bases para criar filtragens sobre localização e renda -------
# Localização:
pof_morador <- pof_morador |>
  dplyr::mutate(
    situacao=ifelse(tipo_situacao_reg==1,"Urbano",
                    ifelse(tipo_situacao_reg==2,"Rural",tipo_situacao_reg))
  )

# Renda 
# Calcular os quantis de renda
quantis <- quantile(pof_morador$renda_total, c(0, 0.25, 0.5, 0.75), na.rm = TRUE)
# Usar os quantis calculados no case_when paraa criar classes:
pof_morador <- pof_morador %>%
  mutate(renda = case_when(
    between(renda_total, quantis[1], quantis[2]) ~ "Primeiro_sm",
    between(renda_total, quantis[2], quantis[3]) ~ "Segundo_sm",
    between(renda_total, quantis[3], quantis[4]) ~ "Terceiro_sm",
    renda_total > quantis[4] ~ "Quarto_sm",
    TRUE ~ NA_character_  # Define como NA se não atender nenhuma condição acima
  ))
remove(quantis)

################################################################################
# Primeira etapa----------------------------------------------------------------
################################################################################
manipulacoes <- function(data) {
  # Etapa 1: Calculando o total de itens com peso de familias
  data <- data %>%
    mutate(itens = contagem * peso_final)
  
  #Etapa 2: Filtrando linhas com classe_prod nao nulas
  data <- data %>%
    filter(!(classe_prod=="")) 
  
  # Etapa 3: Agrupando por classe_prod e calculando o total de itens
  resultado <- data %>%
    group_by(classe_prod) %>%
    summarize(pof_2017_2018 = sum(itens, na.rm = TRUE))
  
  # Retornar o resultado
  return(resultado)
}
# aplicacao na função, analise completa,localização e nível de renda:-----------
# completa
completa <- pof_morador %>% manipulacoes()
# localização:
## Urbano:
Urbano <- pof_morador %>% dplyr::filter(situacao=="Urbano") %>% manipulacoes()
## Rural:
Rural <- pof_morador %>% dplyr::filter(situacao=="Rural") %>% manipulacoes()
# Renda:
## primeiro_sm
Primeiro_sm <- pof_morador %>% dplyr::filter(renda=="Primeiro_sm") %>% manipulacoes()
## Segundo_sm
Segundo_sm <- pof_morador %>% dplyr::filter(renda=="Segundo_sm") %>% manipulacoes()
## Terceiro_sm
Terceiro_sm <- pof_morador %>% dplyr::filter(renda=="Terceiro_sm") %>% manipulacoes()
## Quarto_sm
Quarto_sm <- pof_morador %>% dplyr::filter(renda=="Quarto_sm") %>% manipulacoes()

################################################################################
# Segunda etapa----------------------------------------------------------------
################################################################################
# caregando novamente as bases:
pof_compras_final <- read_dta("pof_compras.dta")
dic_alimentos <- read_dta("dicionario_alimentos.dta")
morador_uc_key <- read_dta("morador_uc_key.dta")|>
  dplyr::select(key_morador,peso_final)

# Operações com as bases--------------------------------------------------------
# Localização:
pof_compras_final <- pof_compras_final |>
  dplyr::mutate(
    situacao=ifelse(tipo_situacao_reg==1,"Urbano",
                    ifelse(tipo_situacao_reg==2,"Rural",tipo_situacao_reg))
  )

# Renda 
# Calcular os quantis de renda
quantis <- quantile(pof_compras_final$renda_total, c(0, 0.25, 0.5, 0.75), na.rm = TRUE)
# Usar os quantis calculados no case_when
pof_compras_final <- pof_compras_final %>%
  mutate(renda = case_when(
    between(renda_total, quantis[1], quantis[2]) ~ "Primeiro_sm",
    between(renda_total, quantis[2], quantis[3]) ~ "Segundo_sm",
    between(renda_total, quantis[3], quantis[4]) ~ "Terceiro_sm",
    renda_total > quantis[4] ~ "Quarto_sm",
    TRUE ~ NA_character_  # Define como NA se nao atender nenhuma condição acima
  ))
remove(quantis)
# Definindo a funcao------------------------------------------------------------
manipular_tabela5 <- function(pof_compras_final, dic_alimentos, morador_uc_key) {
  # Removendo colunas da tabela pof_compras_final
  tabela5 <- pof_compras_final %>%
    select(-nome_v9001, -classe_prod)
  
  # Juntando as bases de dados
  tabela5 <- inner_join(tabela5, dic_alimentos, by = "v9001")
  
  # Substituindo valores na coluna 'classe_prod'
  tabela5 <- tabela5 %>%
    mutate(classe_prod = case_when(
      classe_prod == "Não alimento" | classe_prod == "Bebida alcoolica" ~ "Sem classificacao",
      classe_prod == "Prepara��o culin�ria" ~ "Preparações culinárias",
      classe_prod == "�leos, gorduras, sal e a��car" ~ "Oleos, gorduras, sal e acucar",
      TRUE ~ classe_prod  # mantem outros valores inalterados
    )) %>%
    mutate(contagem = 1) %>%  # Criando variável de contagem
    group_by(uf, estrato_pof, tipo_situacao_reg, cod_upa, num_dom, num_uc, renda_total, classe_prod, cod_local) %>%
    summarize(contagem = sum(contagem, na.rm = TRUE),
              valor_mensal = sum(valor_mensal, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(key_morador = paste0(uf, estrato_pof, tipo_situacao_reg, cod_upa, num_dom, num_uc))  # Criando código "key"
  
  # Juntando com a base morador_uc_key
  tabela5 <- inner_join(tabela5, morador_uc_key, by = "key_morador")
  
  # Calculando 'itens' para os códigos de produto CNAE
  tabela5 <- tabela5 %>%
    mutate(itens = contagem * peso_final) %>%
    group_by(classe_prod) %>%
    summarise(class_prop = sum(itens, na.rm = TRUE))
  
  return(tabela5)
}
# aplicação na funcao, analise completa,localização e nível de renda:-----------
# completa
completa_1 <- manipular_tabela5(pof_compras_final, dic_alimentos, morador_uc_key)
# localização:
## Urbano:
Urbano_1 <- manipular_tabela5(dplyr::filter(pof_compras_final,situacao=="Urbano"), 
                              dic_alimentos, morador_uc_key)
## Rural:
Rural_1 <- manipular_tabela5(dplyr::filter(pof_compras_final,situacao=="Rural"), 
                             dic_alimentos, morador_uc_key)
# Renda:
## primeiro_sm
Primeiro_sm_1 <- manipular_tabela5(dplyr::filter(pof_compras_final,renda=="Primeiro_sm"), 
                                   dic_alimentos, morador_uc_key)
## Segundo_sm
Segundo_sm_1 <- manipular_tabela5(dplyr::filter(pof_compras_final,renda=="Segundo_sm"), 
                                  dic_alimentos, morador_uc_key)
## Terceiro_sm
Terceiro_sm_1 <- manipular_tabela5(dplyr::filter(pof_compras_final,renda=="Terceiro_sm"), 
                                   dic_alimentos, morador_uc_key)
## Quarto_sm
Quarto_sm_1 <- manipular_tabela5(dplyr::filter(pof_compras_final,renda=="Quarto_sm"), 
                                 dic_alimentos, morador_uc_key)
################################################################################
# Terceira etapa----------------------------------------------------------------
################################################################################
# Criando o dataframe desertos alimentares
dados <- data.frame(
  classe_prod = c("In natura ou minimamente processado", "Oleos, gorduras, sal e acucar", 
                  "Preparações culinárias", "Processado", "Sem classificacao", "Ultraprocessado"),
  Desertos_Alimentare = c(364872583, 44559924, 54974226, 168642323, 9571750, 286183800))
#-------------------------------------------------------------------------------
# Tabela 3. Quantidade de itens e percentual de aquisição de alimentos pela população 
# por categoria do Guia Alimentar para a População Brasileira. Deserto Alimentar de 2018 e
# POF 2017/2018-----------------------------------------------------------------
# Definindo a função
criar_completaX <- function(dados, completa) {
  # Realizando o full_join
  completaX <- full_join(dados, completa, by = "classe_prod")
  
  # Adicionando uma linha com o total de cada coluna
  completaX <- completaX %>%
    add_row(classe_prod = "Total", Desertos_Alimentare = sum(completaX$Desertos_Alimentare),
            pof_2017_2018 = sum(completaX$pof_2017_2018))
  
  # Calculando as proporções
  completaX <- completaX %>%
    mutate(P_Desertos_Alimentare = (Desertos_Alimentare / sum(Desertos_Alimentare[1:6])*100),
           P_pof_2017_2018 = (pof_2017_2018 / sum(pof_2017_2018[1:6])*100)) %>%
    select(classe_prod, Desertos_Alimentare, P_Desertos_Alimentare, pof_2017_2018, P_pof_2017_2018)
  
  return(completaX)
}

# aplicando o função:
# completa
t3_completa <- criar_completaX(dados, completa)
# localizacao:
## Urbano:
t3_Urbano <- criar_completaX(dados, Urbano)
## Rural:
t3_Rural <- criar_completaX(dados, Rural)
# Renda:
## primeiro_sm
t3_Primeiro_sm <- criar_completaX(dados, Primeiro_sm)
## Segundo_sm
t3_Segundo_sm <- criar_completaX(dados, Segundo_sm)
## Terceiro_sm
t3_Terceiro_sm <- criar_completaX(dados, Terceiro_sm)
## Quarto_sm
t3_Quarto_sm <- criar_completaX(dados, Quarto_sm)

# comando para salvar tabelas no formato excel:
 openxlsx::write.xlsx(t3_completa, file = "Diretório/t3_completa.xlsx") 
 #OBS: substituir "Diretório" pelo caminho da pasta de salvamento das tabelas

#-------------------------------------------------------------------------------
# Tabela 10. Quantidade de itens e percentual de aquisição de alimentos pela 
# população por grupo do Guia Alimentar para a População Brasileira POF 2017/2018
#para as localizações.
# -----------------------------------------------------------------------------
tabela_10 <- dplyr::bind_cols(select(t3_completa,classe_prod, pof_2017_2018,P_pof_2017_2018),
                              select(t3_Rural, pof_2017_2018,P_pof_2017_2018),
                              select(t3_Urbano, pof_2017_2018,P_pof_2017_2018)) |> 
  janitor::clean_names() |> 
  rename(Completa=pof_2017_2018_2,Completa_P=p_pof_2017_2018_3,
         Rural=pof_2017_2018_4,Rural_P=p_pof_2017_2018_5,
         Urbano=pof_2017_2018_6,Urbano_P=p_pof_2017_2018_7)

# comando para salvar tabelas no formato excel:
 openxlsx::write.xlsx(tabela_10, file = "Diretório/tabela_10.xlsx")
 #OBS: substituir "Diretório" pelo caminho da pasta de salvamento das tabelas
 
#-------------------------------------------------------------------------------
# Tabela 15. Quantidade de itens e percentual de aquisição de alimentos pela 
# população por grupo do Guia Alimentar para a População Brasileira POF 2017/2018 
# para os níveis de renda.
# -----------------------------------------------------------------------------
tabela_15 <- dplyr::bind_cols(select(t3_completa,classe_prod, pof_2017_2018,P_pof_2017_2018),
                              select(t3_Primeiro_sm, pof_2017_2018,P_pof_2017_2018),
                              select(t3_Segundo_sm, pof_2017_2018,P_pof_2017_2018),
                              select(t3_Terceiro_sm, pof_2017_2018,P_pof_2017_2018),
                              select(t3_Quarto_sm, pof_2017_2018,P_pof_2017_2018)) |> 
  janitor::clean_names() |> 
  rename(Completa=pof_2017_2018_2,Completa_P=p_pof_2017_2018_3,
         Primeiro_sm=pof_2017_2018_4, Primeiro_sm_P=p_pof_2017_2018_5,
         Segundo_sm=pof_2017_2018_6, Segundo_sm_P=p_pof_2017_2018_7,
         Terceiro_sm=pof_2017_2018_8, Terceiro_sm_P=p_pof_2017_2018_9,
         Quarto_sm=pof_2017_2018_10, Quarto_sm_P=p_pof_2017_2018_11)

# comando para salvar tabelas no formato excel:
 openxlsx::write.xlsx(tabela_15, file = "Diretório/tabela_15.xlsx")
 #OBS: substituir "Diretório" pelo caminho da pasta de salvamento das tabelas
