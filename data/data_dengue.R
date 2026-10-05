# =============================================================================
# etl.R — Dengue DF
# Pipeline: PostgreSQL → processamento → Parquet + DuckDB
# Rodar periodicamente (ex: cron diário) para atualizar os dados do app
# =============================================================================
library(DBI)
library(RPostgres)
library(duckdb)
library(dplyr)
library(lubridate)
library(tidyr)
library(arrow)

# =============================================================================
# 1. Lê do PostgreSQL
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
# 2. De-para de RAs via setor censitário
# =============================================================================
depara <- read.csv("data/setores_RAs_DF.csv", colClasses = c(CD_SETOR = "character"))

dados <- dados |>
  left_join(
    depara |> select(CD_SETOR, ra_nome_nova = ra_nome),
    by = "CD_SETOR"
  ) |>
  mutate(Regiao_adm = coalesce(ra_nome_nova, Regiao_adm)) |>
  select(-ra_nome_nova)

# =============================================================================
# 3. Tabelas de domínio
# =============================================================================
# Códigos conforme o Dicionário de Dados do SINAN Online (Dengue/Chikungunya),
# os mesmos usados pelo painel de Vigilância (R/fct_db_update.R de lá).
rotulos_evolucao <- c(
  "1" = "Cura",
  "2" = "Óbito pelo agravo",
  "3" = "Óbito por outras causas",
  "4" = "Óbito em investigação",
  "9" = "Ignorado"
)

# 10/11/12 são os códigos atuais do SINAN Online; 1/2/3 (SINAN Net) continuam
# aceitos e mapeados para as mesmas categorias.
rotulos_classi_fin <- c(
  "10" = "Dengue",
  "11" = "Dengue com sinais de alarme",
  "12" = "Dengue grave",
  "1"  = "Dengue",
  "2"  = "Dengue com sinais de alarme",
  "3"  = "Dengue grave",
  "5"  = "Descartado",
  "8"  = "Inconclusivo"
)

harmonizar_ra <- c(
  "SCIA (ESTRUTURAL)" = "SCIA",
  "SIA"               = "SIA"
)

# =============================================================================
# 4. Faixa etária
# =============================================================================
derivar_faixa_etaria <- function(nu_idade_n) {
  tipo  <- as.integer(substr(as.character(nu_idade_n), 1, 1))
  valor <- as.integer(substr(as.character(nu_idade_n), 2, 4))
  
  idade_anos <- dplyr::case_when(
    tipo == 4 ~ valor,
    tipo == 3 ~ 0L,
    tipo == 2 ~ 0L,
    tipo == 1 ~ 0L,
    TRUE      ~ NA_integer_
  )
  
  dplyr::case_when(
    is.na(idade_anos) ~ NA_character_,
    idade_anos <   1  ~ "menos_1_ano",
    idade_anos <=  4  ~ "1_4_anos",
    idade_anos <=  9  ~ "5_9_anos",
    idade_anos <= 14  ~ "10_14_anos",
    idade_anos <= 19  ~ "15_19_anos",
    idade_anos <= 29  ~ "20_29_anos",
    idade_anos <= 39  ~ "30_39_anos",
    idade_anos <= 49  ~ "40_49_anos",
    idade_anos <= 59  ~ "50_59_anos",
    idade_anos <= 64  ~ "60_64_anos",
    idade_anos <= 69  ~ "65_69_anos",
    idade_anos <= 79  ~ "70_79_anos",
    idade_anos <= 89  ~ "80_89_anos",
    TRUE              ~ "90_anos_e_mais"
  )
}

# =============================================================================
# 5. Processamento completo — casos individuais
# =============================================================================
casos_individuais <- dados |>
  filter(
    !is.na(Regiao_adm),
    Regiao_adm != "",
    Regiao_adm != "Não Informado"
  ) |>
  mutate(
    Regiao_adm   = recode(Regiao_adm, !!!harmonizar_ra, .default = toupper(Regiao_adm)),
    DT_SIN_PRI   = as.Date(substr(DT_SIN_PRI, 1, 10)),
    NU_ANO       = lubridate::epiyear(DT_SIN_PRI),
    SEM_NUM      = lubridate::epiweek(DT_SIN_PRI),
    MES          = lubridate::month(DT_SIN_PRI),
    FAIXA_ETARIA = derivar_faixa_etaria(NU_IDADE_N),
    EVOLUCAO     = recode(as.character(EVOLUCAO),
                          !!!rotulos_evolucao,   .default = "Ignorado"),
    CLASSI_FIN   = recode(as.character(CLASSI_FIN),
                          !!!rotulos_classi_fin, .default = "Ignorado")
  ) |>
  filter(
    !is.na(DT_SIN_PRI),
    lubridate::year(DT_SIN_PRI) >= 2021,
    lubridate::year(DT_SIN_PRI) <= lubridate::year(Sys.Date()),
    SEM_NUM >= 1,
    SEM_NUM <= 52
  ) |>
  select(-SEM_PRI, -NU_IDADE_N, -CD_SETOR)

# =============================================================================
# 6. Populações por RA
# =============================================================================
populacao_ra <- tibble::tribble(
  ~ra,                            ~populacao,
  "PLANO PILOTO",                  220000,
  "GAMA",                          140000,
  "TAGUATINGA",                    200000,
  "BRAZLÂNDIA",                     55000,
  "SOBRADINHO",                    220000,
  "PLANALTINA",                    185000,
  "PARANOÁ",                        55000,
  "NÚCLEO BANDEIRANTE",             45000,
  "CEILÂNDIA",                     480000,
  "GUARÁ",                         135000,
  "CRUZEIRO",                       65000,
  "SAMAMBAIA",                     250000,
  "SANTA MARIA",                   130000,
  "SÃO SEBASTIÃO",                 110000,
  "RECANTO DAS EMAS",              160000,
  "LAGO SUL",                       30000,
  "RIACHO FUNDO",                   40000,
  "LAGO NORTE",                     38000,
  "CANDANGOLÂNDIA",                 18000,
  "ÁGUAS CLARAS",                  155000,
  "RIACHO FUNDO II",                42000,
  "SUDOESTE/OCTOGONAL",             55000,
  "VARJÃO",                         10000,
  "PARK WAY",                       25000,
  "SCIA",                           16000,
  "SOBRADINHO II",                 110000,
  "JARDIM BOTÂNICO",                28000,
  "ITAPOÃ",                         70000,
  "SIA",                             3000,
  "VICENTE PIRES",                  75000,
  "FERCAL",                          9000,
  "SOL NASCENTE E POR DO SOL",      95000,
  "ARNIQUEIRA",                     45000,
  "ARAPOANGA",                      35000,
  "AGUA QUENTE",                     5000
)

# =============================================================================
# 7. Agregados pré-computados
# =============================================================================

# Resumo por RA (mapa de incidência, KPIs)
dados_ra <- casos_individuais |>
  count(Regiao_adm, name = "casos") |>
  left_join(populacao_ra, by = c("Regiao_adm" = "ra")) |>
  mutate(
    populacao  = replace_na(populacao, 10000),
    incidencia = round((casos / populacao) * 100000, 1)
  )

# Agregado diário por RA (curva epidêmica)
agregado_diario <- casos_individuais |>
  count(DT_SIN_PRI, Regiao_adm, NU_ANO, SEM_NUM, Regiao_sau1, name = "casos")

# Agregado semanal por RA (linha temporal, Rt)
agregado_semanal <- casos_individuais |>
  count(NU_ANO, SEM_NUM, Regiao_adm, Regiao_sau1, name = "casos")

# Agregado mensal por RA (heatmap)
agregado_mensal <- casos_individuais |>
  count(NU_ANO, MES, Regiao_adm, Regiao_sau1, name = "casos")

# =============================================================================
# 7b. Agregado "DISTRITO FEDERAL" (total, para cobrir filtro_ra == "Todas")
# =============================================================================
agregado_semanal_df <- agregado_semanal |>
  group_by(NU_ANO, SEM_NUM) |>
  summarise(casos = sum(casos), .groups = "drop") |>
  mutate(Regiao_adm = "DISTRITO FEDERAL", Regiao_sau1 = NA_character_)

agregado_semanal_completo <- bind_rows(agregado_semanal, agregado_semanal_df)

pop_total_df <- sum(populacao_ra$populacao)

dados_ra_completo <- bind_rows(
  dados_ra,
  tibble::tibble(Regiao_adm = "DISTRITO FEDERAL", populacao = pop_total_df)
)

# =============================================================================
# 7c. Canal endêmico (histórico de mediana e percentis por RA/semana)
# =============================================================================
anos_excluir_canal <- c(2024)  # ajuste: ano(s) epidêmico(s) atípico(s) a excluir da mediana

canal_endemico <- agregado_semanal_completo |>
  left_join(
    dados_ra_completo |> select(Regiao_adm, populacao),
    by = "Regiao_adm"
  ) |>
  filter(!NU_ANO %in% anos_excluir_canal) |>
  mutate(inc100k = (casos / populacao) * 100000) |>
  group_by(Regiao_adm, SEM_NUM) |>
  summarise(
    mediana = median(inc100k, na.rm = TRUE),
    qlow    = quantile(inc100k, probs = 0.25, na.rm = TRUE),
    qhigh   = quantile(inc100k, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(semanadiagrama = ifelse(SEM_NUM >= 27, SEM_NUM - 26, SEM_NUM + 26))

# =============================================================================
# 8. Diagnóstico
# =============================================================================
cat("Registros brutos:   ", nrow(dados), "\n")
cat("Registros finais:   ", nrow(casos_individuais), "\n")
cat("Excluídos:          ", nrow(dados) - nrow(casos_individuais), "\n")
cat("Período:            ",
    format(min(casos_individuais$DT_SIN_PRI), "%d/%m/%Y"), "a",
    format(max(casos_individuais$DT_SIN_PRI), "%d/%m/%Y"), "\n")
cat("Anos presentes:     ", paste(sort(unique(casos_individuais$NU_ANO)), collapse = ", "), "\n")

# Diagnóstico de RAs sem correspondência de população (afeta incidência e canal endêmico)
ras_sem_pop <- dados_ra |> filter(is.na(populacao) | populacao == 10000) |> pull(Regiao_adm)
if (length(ras_sem_pop) > 0) {
  cat("ATENÇÃO — RAs sem população cadastrada (usando fallback 10000):\n")
  cat(" ", paste(ras_sem_pop, collapse = ", "), "\n")
}

# =============================================================================
# 9. Salva Parquets
# =============================================================================
dir.create("data", showWarnings = FALSE)

arrow::write_parquet(casos_individuais, "data/dengue_df_processado.parquet")
arrow::write_parquet(dados_ra,          "data/dengue_df_dados_ra.parquet")
arrow::write_parquet(agregado_diario,   "data/dengue_df_diario.parquet")
arrow::write_parquet(agregado_semanal,  "data/dengue_df_semanal.parquet")
arrow::write_parquet(agregado_mensal,   "data/dengue_df_mensal.parquet")
arrow::write_parquet(canal_endemico,    "data/dengue_df_canal_endemico.parquet")

cat("Parquets salvos em: data/\n")
cat("  dengue_df_processado.parquet     —", nrow(casos_individuais), "linhas\n")
cat("  dengue_df_dados_ra.parquet       —", nrow(dados_ra), "linhas\n")
cat("  dengue_df_diario.parquet         —", nrow(agregado_diario), "linhas\n")
cat("  dengue_df_semanal.parquet        —", nrow(agregado_semanal), "linhas\n")
cat("  dengue_df_mensal.parquet         —", nrow(agregado_mensal), "linhas\n")
cat("  dengue_df_canal_endemico.parquet —", nrow(canal_endemico), "linhas\n")

# =============================================================================
# 10. Grava no DuckDB (backup/consulta ad hoc)
# =============================================================================
con_duck <- dbConnect(duckdb(), "data/dengue_df.duckdb")

dbExecute(con_duck, "CREATE OR REPLACE TABLE dengue           AS SELECT * FROM read_parquet('data/dengue_df_processado.parquet')")
dbExecute(con_duck, "CREATE OR REPLACE TABLE dados_ra         AS SELECT * FROM read_parquet('data/dengue_df_dados_ra.parquet')")
dbExecute(con_duck, "CREATE OR REPLACE TABLE agregado_diario  AS SELECT * FROM read_parquet('data/dengue_df_diario.parquet')")
dbExecute(con_duck, "CREATE OR REPLACE TABLE agregado_semanal AS SELECT * FROM read_parquet('data/dengue_df_semanal.parquet')")
dbExecute(con_duck, "CREATE OR REPLACE TABLE agregado_mensal  AS SELECT * FROM read_parquet('data/dengue_df_mensal.parquet')")
dbExecute(con_duck, "CREATE OR REPLACE TABLE canal_endemico   AS SELECT * FROM read_parquet('data/dengue_df_canal_endemico.parquet')")

cat("DuckDB atualizado: data/dengue_df.duckdb\n")
cat("  dengue:          ", dbGetQuery(con_duck, "SELECT COUNT(*) AS n FROM dengue")$n, "linhas\n")

dbDisconnect(con_duck, shutdown = TRUE)
cat("ETL concluído:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n")