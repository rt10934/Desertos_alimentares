setwd("D:/MDS/estabelecimentos_tabel_gio")
#-------------------------------------------------------------------------------
# base dos dados ---------------------------------------------------------------
#-------------------------------------------------------------------------------
library(readxl)
Total_Estabelec <- readxl::read_excel("Total Estabelecimentos Categorias.xlsx")|>
  dplyr::rename(code_muni = codmun7)

#-------------------------------------------------------------------------------
# Primeira Etapa ---------------------------------------------------------------
#-------------------------------------------------------------------------------
# Contabilizando os estabelecimentos:
dados_geoespaciais1 <- Total_Estabelec|>
  sf::st_drop_geometry() |>
  dplyr::summarise(natura = sum(natura, na.rm = T),
                   misto_natura = sum(misto_natura, na.rm = T),
                   misto = sum(misto, na.rm = T),
                   misto_proc = sum(misto_proc, na.rm = T),
                   ultra = sum(ultra, na.rm = T)) |>
  dplyr::mutate(total= natura + misto_natura + misto + misto_proc + ultra )
openxlsx::write.xlsx(dados_geoespaciais1, file = "D:/MDS/estabelecimentos_tabel_gio/dados_geoespaciais1.xlsx")
dados_geoespaciais1 |> dplyr::glimpse()
# descricao dos dados 
Total_Estabelec |> dplyr::glimpse()


#-------------------------------------------------------------------------------
# Segunda Etapa ----------------------------------------------------------------
#-------------------------------------------------------------------------------
# Contabilizando os estabelecimentos:
# shapefile 
muni <- geobr::read_municipality(year = 2020)
estado <- geobr::read_state(year=2020)
# juntando os dados
Total_Estabelec_sha <- Total_Estabelec |> dplyr::mutate(misto_mistosaud = misto_natura+misto)
Total_Estabelec_sha <- full_join(muni, Total_Estabelec_sha, by="code_muni")

# de comum uso 
# Nomes dos percentis
percentiles <- c("0", "25", "50", "75", "99", "100")
percentiles1 <- c("0", "50", "75", "99", "100")
percentiles2 <- c("0", "30", "50", "75", "99", "100")
# para alimentos In natura ------------------------------------------------------
# Calcular quantis com os breakpoints especificados
quantiles <- round(quantile(Total_Estabelec_sha$natura, na.rm = TRUE,
                            probs = c(0, 0.25, 0.5, 0.75, 0.99, 1)), digits = 2)

# Criar as etiquetas para esses quantis
# quantile_labels <- paste("(", quantiles[-length(quantiles)], ",", quantiles[-1],
#                         "] Percentil ", percentiles[-1], sep = "") deixa casas decimais 

quantile_labels <- paste("(", round(quantiles,digits = 0)[-length(round(quantiles,digits = 0))], ",",
                         round(quantiles,digits = 0)[-1],"] Percentil ", percentiles[-1], sep = "")
# Nomeando 
# Classificar os valores de 'natura' nos intervalos dos quantis
Total_Estabelec_sha <- Total_Estabelec_sha |>
  dplyr::mutate(cat_natura = cut(natura, breaks = quantiles, labels = quantile_labels,
                                 include.lowest = TRUE, right = TRUE))
# Mapa
grafico1 <- ggplot() +
  geom_sf(data = Total_Estabelec_sha, mapping = aes(fill = cat_natura), color = "NA", show.legend = TRUE) +
  labs(fill = "Número de estabelecimentos", title = "DISTRIBUIÇÃO DOS ESTABELECIMENTOS DE AQUISIÇÃO DE IN NATURA") +
  scale_fill_manual(values = c("#e5f5e0", "#a1d99b", "#41ab5d", "#238b45", "#005a32")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centraliza o título horizontalmente
    legend.position = c(0.8, 0.15),  # Posição personalizada da legenda (x, y)
    panel.background = element_rect(fill = "white", colour = "white"), # Define fundo do painel como branco
    panel.grid.major = element_blank(),  # Remove as linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove as linhas de grade secundárias
    plot.background = element_rect(fill = "white", colour = "white") # Define o fundo do plot como branco
  ) +
  geom_sf(data = estado, mapping = aes(), color = "black", fill = NA)
# salvando 
ggsave("in_natura_sem.png", plot = grafico1, width = 10, height = 8, dpi = 300)
ggsave("in_natura_sem.svg", grafico1, width = 10, height = 8, dpi = 300)
rm(quantiles, quantile_labels)
# para alimentos misto e misto saudavel-----------------------------------------
# Calcular quantis com os breakpoints especificados
quantiles <- round(quantile(Total_Estabelec_sha$misto_mistosaud, na.rm = TRUE,
                            probs = c(0, 0.25, 0.5, 0.75, 0.99, 1)), digits = 2)

# Criar as etiquetas para esses quantis
quantile_labels <- paste("(", round(quantiles,digits = 0)[-length(round(quantiles,digits = 0))], ",",
                         round(quantiles,digits = 0)[-1],"] Percentil ", percentiles[-1], sep = "")

# Nomeando 
# Classificar os valores de 'misto_mistosaud' nos intervalos dos quantis
Total_Estabelec_sha <- Total_Estabelec_sha |>
  dplyr::mutate(cat_misto_mistosaud = cut(misto_mistosaud, breaks = quantiles, labels = quantile_labels,
                                          include.lowest = TRUE, right = TRUE))
# Mapa
grafico5 <- ggplot() +
  geom_sf(data = Total_Estabelec_sha, mapping = aes(fill = cat_misto_mistosaud), color = "NA", show.legend = TRUE) +
  labs(fill = "Número de estabelecimentos", title = "DISTRIBUIÇÃO DOS ESTABELECIMENTOS DE AQUISIÇÃO MISTO E MISTO SAUDÁVEL") +
  scale_fill_manual(values = c("#E9F6AB", "#FCFFA4", "#FFFF66", "#E8D92E", "#A8780D")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centraliza o título horizontalmente
    legend.position = c(0.8, 0.15),  # Posição personalizada da legenda (x, y)
    panel.background = element_rect(fill = "white", colour = "white"), # Define fundo do painel como branco
    panel.grid.major = element_blank(),  # Remove as linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove as linhas de grade secundárias
    plot.background = element_rect(fill = "white", colour = "white") # Define o fundo do plot como branco
  ) +
  geom_sf(data = estado, mapping = aes(), color = "black", fill = NA)
ggsave("cat_misto_mistosaud_sem.png", plot = grafico5, width = 10, height = 8, dpi = 300)
ggsave("cat_misto_mistosaud_sem.svg", grafico5, width = 10, height = 8, dpi = 300)
rm(quantiles, quantile_labels)
# para alimentos misto processado ----------------------------------------------
# Calcular quantis com os breakpoints especificados
quantiles <- round(quantile(Total_Estabelec_sha$misto_proc, na.rm = TRUE,
                            probs = c(0, 0.5, 0.75, 0.99, 1)), digits = 2)

# Criar as etiquetas para esses quantis
quantile_labels <- paste("(", round(quantiles,digits = 0)[-length(round(quantiles,digits = 0))], ",",
                         round(quantiles,digits = 0)[-1],"] Percentil ", percentiles1[-1], sep = "")

# Nomeando 
# Classificar os valores de 'misto_mistosaud' nos intervalos dos quantis
Total_Estabelec_sha <- Total_Estabelec_sha |>
  dplyr::mutate(cat_misto_proces= cut(misto_proc, breaks = quantiles, labels = quantile_labels,
                                      include.lowest = TRUE, right = TRUE))

# cat_misto_proc
grafico4 <- ggplot() +
  geom_sf(data = Total_Estabelec_sha, mapping = aes(fill = cat_misto_proces), color = "NA", show.legend = TRUE) +
  labs(fill = "Número de estabelecimentos", title = "DISTRIBUIÇÃO DOS ESTABELECIMENTOS MISTOS PROCESSADOS") +
  scale_fill_manual(values = c("#FDE2AB", "#FCB966", "#FCA82E", "#E8880D")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centraliza o título horizontalmente
    legend.position = c(0.8, 0.15),  # Posição personalizada da legenda (x, y)
    panel.background = element_rect(fill = "white", colour = "white"), # Define fundo do painel como branco
    panel.grid.major = element_blank(),  # Remove as linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove as linhas de grade secundárias
    plot.background = element_rect(fill = "white", colour = "white") # Define o fundo do plot como branco
  ) +
  geom_sf(data = estado, mapping = aes(), color = "black", fill = NA)
ggsave("cat_misto_proc_sem.png", plot = grafico4, width = 10, height = 8, dpi = 300)
ggsave("cat_misto_proc_sem.svg", grafico4, width = 10, height = 8, dpi = 300)
rm(quantiles, quantile_labels)

# para alimentos ultraprocessados ------------------------------------------------------
# Calcular quantis com os breakpoints especificados
quantiles <- round(quantile(Total_Estabelec_sha$ultra, na.rm = TRUE,
                            probs = c(0, 0.30, 0.5, 0.75, 0.99, 1)), digits = 2)

# Criar as etiquetas para esses quantis
quantile_labels <- paste("(", round(quantiles,digits = 0)[-length(round(quantiles,digits = 0))], ",",
                         round(quantiles,digits = 0)[-1],"] Percentil ", percentiles[-1], sep = "")

# Nomeando 
# Classificar os valores de 'natura' nos intervalos dos quantis
Total_Estabelec_sha <- Total_Estabelec_sha |>
  dplyr::mutate(cat_ultra = cut(ultra, breaks = quantiles, labels = quantile_labels,
                                include.lowest = TRUE, right = TRUE))
# Mapa
grafico3 <- ggplot() +
  geom_sf(data = Total_Estabelec_sha, mapping = aes(fill = cat_ultra), color = "NA", show.legend = TRUE) +
  labs(fill = "Número de estabelecimentos", title = "DISTRIBUIÇÃO DOS ESTABELECIMENTOS DE AQUISIÇÃO DE ULTRAPROCESSADOS") +
  scale_fill_manual(values = c("#FEE5D9","#FC9272", "#EF3B2C", "#CB181D", "#99000D")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centraliza o título horizontalmente
    legend.position = c(0.8, 0.15),  # Posição personalizada da legenda (x, y)
    panel.background = element_rect(fill = "white", colour = "white"), # Define fundo do painel como branco
    panel.grid.major = element_blank(),  # Remove as linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove as linhas de grade secundárias
    plot.background = element_rect(fill = "white", colour = "white") # Define o fundo do plot como branco
  ) +
  geom_sf(data = estado, mapping = aes(), color = "black", fill = NA)
ggsave("ultra_sem.png", plot = grafico3, width = 10, height = 8, dpi = 300)
ggsave("ultra_sem.svg", grafico3, width = 10, height = 8, dpi = 300)
rm(quantiles, quantile_labels)
# ------------------------------------------------------------------------------
# TERCEIRA ETAPA ---------------------------------------------------------------
#-------------------------------------------------------------------------------

# etapa de descricao dos dados: ------------------------------------------------
dados2 <- Total_Estabelec_sha |>
  sf::st_drop_geometry()
# TABELA COM AS CLASSES:
openxlsx::write.xlsx(dados2, file = "D:/MDS/estabelecimentos_tabel_gio/CATEGORIAS.xlsx")
# OPERACOES:
dados2 <- dados2 |>
  dplyr::mutate(count=1)
dados2 |> dplyr::glimpse()
# in natura:
dados2_cat_saud <- dados2 |>
  dplyr::group_by(name_region, cat_natura)|>
  dplyr::summarise(count=sum(count, na.rm = T)) |>
  dplyr::group_by(name_region) |>
  dplyr::mutate(total=sum(count, na.rm = T))|>
  dplyr::ungroup() |>
  dplyr::mutate(prop = (count/total)*100)
openxlsx::write.xlsx(dados2_cat_saud, file = "D:/MDS/estabelecimentos_tabel_gio/cat_natura.xlsx")
# misto e misto saudavel:
dados2_cat_misto_mistosaud <- dados2 |>
  dplyr::group_by(name_region, cat_misto_mistosaud)|>
  dplyr::summarise(count=sum(count, na.rm = T)) |>
  dplyr::group_by(name_region) |>
  dplyr::mutate(total=sum(count, na.rm = T))|>
  dplyr::ungroup() |>
  dplyr::mutate(prop = (count/total)*100)
openxlsx::write.xlsx(dados2_cat_misto_mistosaud, file = "D:/MDS/estabelecimentos_tabel_gio/cat_misto_mistosaud.xlsx")
# misto processado:
dados2_cat_misto_proc <- dados2 |>
  dplyr::group_by(name_region, cat_misto_proces)|>
  dplyr::summarise(count=sum(count, na.rm = T)) |>
  dplyr::group_by(name_region) |>
  dplyr::mutate(total=sum(count, na.rm = T))|>
  dplyr::ungroup() |>
  dplyr::mutate(prop = (count/total)*100)
openxlsx::write.xlsx(dados2_cat_misto_proc, file = "D:/MDS/estabelecimentos_tabel_gio/cat_misto_proces.xlsx")
# ultraprocessado:
dados2_cat_ultra <- dados2 |>
  dplyr::group_by(name_region, cat_ultra)|>
  dplyr::summarise(count=sum(count, na.rm = T)) |>
  dplyr::group_by(name_region) |>
  dplyr::mutate(total=sum(count, na.rm = T))|>
  dplyr::ungroup() |>
  dplyr::mutate(prop = (count/total)*100)
openxlsx::write.xlsx(dados2_cat_ultra, file = "D:/MDS/estabelecimentos_tabel_gio/cat_ultra.xlsx")