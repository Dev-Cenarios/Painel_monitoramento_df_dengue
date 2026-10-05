# =============================================================================
# etl_hotspots.R — Dengue DF
# Detecção de hotspots espaciais persistentes (Getis-Ord Gi*), método de
# Dzul-Manzanilla et al. 2021 (Lancet Planetary Health), aplicado aos
# setores censitários do DF.
#
# Pré-requisito: rodar etl.R normalmente antes (esse script assume que o
# PostgreSQL já foi consultado e reaproveita a mesma lógica de tratamento).
# Rodar periodicamente (ex: junto do cron do etl.R principal).
# =============================================================================
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(tidyr)
library(arrow)
library(sf)
library(spdep)
library(purrr)

# =============================================================================
# 1. Lê do PostgreSQL (mesma query do etl.R, mantendo CD_SETOR)
# =============================================================================
con_pg <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS")  # credenciais no .Renviron (ver .Renviron.example)
)

dados <- dbGetQuery(con_pg, '
  SELECT
    "SEM_PRI", "DT_SIN_PRI",
    "CS_SEXO", "NU_IDADE_N", "CLASSI_FIN",
    "EVOLUCAO", "Regiao_adm", "Regiao_sau1", "CD_SETOR"
  FROM dados_geolink."DF_geo_Dengue"
')

dbDisconnect(con_pg)
cat("Leitura PostgreSQL: ", nrow(dados), "registros\n")

# =============================================================================
# 2. De-para de RAs via setor censitário (igual ao etl.R principal)
# =============================================================================
depara <- read.csv("data/setores_RAs_DF.csv")

dados <- dados |>
  left_join(
    depara |> select(CD_SETOR, ra_nome_nova = ra_nome),
    by = "CD_SETOR"
  ) |>
  mutate(Regiao_adm = coalesce(ra_nome_nova, Regiao_adm)) |>
  select(-ra_nome_nova)

# =============================================================================
# 3. Processamento — igual ao etl.R, mas MANTENDO CD_SETOR
#    (no etl.R principal essa coluna é descartada na seleção final)
# =============================================================================
casos_individuais <- dados |>
  filter(
    !is.na(Regiao_adm),
    Regiao_adm != "",
    Regiao_adm != "Não Informado",
    !is.na(CD_SETOR),
    CD_SETOR != ""
  ) |>
  mutate(
    Regiao_adm = toupper(Regiao_adm),
    DT_SIN_PRI = as.Date(substr(DT_SIN_PRI, 1, 10)),
    NU_ANO     = lubridate::epiyear(DT_SIN_PRI),
    SEM_NUM    = lubridate::epiweek(DT_SIN_PRI),
    CD_SETOR   = as.character(CD_SETOR)
  ) |>
  filter(
    !is.na(DT_SIN_PRI),
    lubridate::year(DT_SIN_PRI) >= 2021,
    lubridate::year(DT_SIN_PRI) <= lubridate::year(Sys.Date()),
    SEM_NUM >= 1,
    SEM_NUM <= 52
  )

# =============================================================================
# 4. Casos agregados por setor censitário x ano
#    (equivalente ao "case counts by year" do artigo, nível AGEB)
# =============================================================================
casos_por_setor_ano <- casos_individuais |>
  count(CD_SETOR, NU_ANO, name = "casos")

cat("Setores com pelo menos 1 caso:", n_distinct(casos_por_setor_ano$CD_SETOR), "\n")
cat("Anos disponíveis:", paste(sort(unique(casos_por_setor_ano$NU_ANO)), collapse = ", "), "\n")

# =============================================================================
# 5. Malha de setores censitários + matriz de vizinhança (contiguidade queen)
#    Mesma lógica de vizinhança espacial que você já usou no BYM2 de TB
#    (lá era municípios via geobr; aqui é setores censitários do DF)
# =============================================================================
setores <- st_read("data/DF_setores_CD2022.shp", quiet = TRUE) |>
  mutate(CD_SETOR = as.character(CD_SETOR)) |>
  filter(!st_is_empty(geometry))

# Corrige geometrias inválidas antes de calcular vizinhança (comum em shapefiles do IBGE)
setores <- st_make_valid(setores)

viz   <- poly2nb(setores, queen = TRUE)
pesos <- nb2listw(viz, style = "W", zero.policy = TRUE)

cat("Setores na malha espacial:", nrow(setores), "\n")
cat("Setores sem nenhum vizinho (isolados):", sum(card(viz) == 0), "\n")

# =============================================================================
# 6. Getis-Ord Gi*(d) por ano, com correção de Bonferroni (Z >= 3.71),
#    igual ao critério do artigo (em vez do clássico Z = 1.96)
# =============================================================================
LIMIAR_HOTSPOT <- 3.71  # Bonferroni, conforme Bisanzio et al. / Dzul-Manzanilla et al.

anos <- sort(unique(casos_por_setor_ano$NU_ANO))

resultado_hotspots <- map_dfr(anos, function(ano_i) {
  
  base_ano <- setores |>
    st_drop_geometry() |>
    select(CD_SETOR) |>
    left_join(
      casos_por_setor_ano |> filter(NU_ANO == ano_i),
      by = "CD_SETOR"
    ) |>
    mutate(
      casos   = replace_na(casos, 0),
      z_casos = as.numeric(scale(casos))  # padroniza (Z-score) os casos do ano
    )
  
  gi <- localG(base_ano$z_casos, pesos, zero.policy = TRUE)
  
  base_ano |>
    mutate(
      NU_ANO  = ano_i,
      gi_star = as.numeric(gi),
      hotspot = as.integer(!is.na(gi_star) & gi_star >= LIMIAR_HOTSPOT)
    ) |>
    select(CD_SETOR, NU_ANO, casos, gi_star, hotspot)
})

cat("Total de (setor x ano) avaliados:", nrow(resultado_hotspots), "\n")
cat("Total de hotspots detectados (todos os anos):", sum(resultado_hotspots$hotspot, na.rm = TRUE), "\n")

# =============================================================================
# 7. Persistência: quantos anos cada setor foi hotspot (equivalente à
#    Figura 2 do artigo — mapa de "anos como hotspot")
# =============================================================================
persistencia_hotspot <- resultado_hotspots |>
  group_by(CD_SETOR) |>
  summarise(
    anos_avaliados    = n(),
    anos_como_hotspot = sum(hotspot, na.rm = TRUE),
    .groups = "drop"
  )

setores_hotspot <- setores |>
  left_join(persistencia_hotspot, by = "CD_SETOR") |>
  mutate(anos_como_hotspot = replace_na(anos_como_hotspot, 0L))

# =============================================================================
# 8. Diagnóstico
# =============================================================================
cat("\n--- Resumo de persistência ---\n")
print(
  setores_hotspot |>
    st_drop_geometry() |>
    count(anos_como_hotspot) |>
    arrange(desc(anos_como_hotspot))
)

# =============================================================================
# 9. Salva resultados
# =============================================================================
dir.create("data", showWarnings = FALSE)

# tabela anual (setor x ano x gi* x hotspot) — para série temporal / filtros
arrow::write_parquet(resultado_hotspots, "data/dengue_df_hotspots_anual.parquet")

# geojson com a persistência acumulada — para o mapa (equivalente à Figura 2)
# Só CD_SETOR + geometria: o app (modules/mod_analises_hotspot.R) usa
# CD_SETOR como chave de junção e busca as métricas (anos_como_hotspot,
# casos_totais) via consulta no parquet acima, não neste arquivo. As demais
# colunas do shapefile do IBGE (região, UF, distrito, subdistrito, bairro,
# área etc.) nunca são lidas pelo app - mantê-las só infla o payload que
# viaja pro navegador a cada renderização do mapa (~30 colunas repetidas em
# 5.400+ feições). Ver também prep/01_simplifica_malhas.R, que reduz o
# número de vértices da geometria.
setores_hotspot |>
  select(CD_SETOR) |>
  st_transform(4326) |>
  st_write("data/dengue_df_setores_hotspot.geojson", delete_dsn = TRUE, quiet = TRUE)

cat("\nArquivos salvos em data/:\n")
cat("  dengue_df_hotspots_anual.parquet   —", nrow(resultado_hotspots), "linhas\n")
cat("  dengue_df_setores_hotspot.geojson  —", nrow(setores_hotspot), "setores\n")

cat("\nETL de hotspots concluído:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n")