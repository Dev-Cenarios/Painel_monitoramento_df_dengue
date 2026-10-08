# ==============================================================================
# Dengue DF | global.R
# Ambiente, fonte de dados (Parquet via DuckDB), dicionarios de dominio e
# helpers de consulta. Executado UMA vez, antes de ui.R e server.R.
#
# Arquitetura portada do dengue_ride: DuckDB com views sobre os Parquets do ETL
# (data/data_dengue.R + data/etl_hotspots.R), consultas lazy com pushdown de
# filtro/coluna, e reactives compartilhados no server.R (um por grao de dado,
# consumidos por varios modulos) em vez de cada modulo reagregar os mesmos
# dados. Ao contrario do dengue_ride (uma tabela bruta, tudo agregado em tempo
# de consulta), o ETL do DF ja pre-agrega por grao (individual/diario/
# semanal/mensal/canal endemico/hotspots) - aqui cada Parquet vira uma view,
# e as consultas fazem so o filtro (WHERE), nao a agregacao pesada.
# ==============================================================================

library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(echarts4r)
library(DT)
library(lubridate)
library(tidyr)
library(stringr)
library(jsonlite)
library(zoo)
library(scales)
library(cachem)

shinyOptions(cache = cachem::cache_mem(max_size = 512 * 1024^2))

# ------------------------------------------------------------ fonte de dados --
DENGUE_DATA_DIR <- Sys.getenv("DENGUE_DF_DATA_DIR", "data")

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir  = ":memory:",
  bigint = "numeric"
)

onStop(function() {
  message("[Dengue DF] Encerrando DuckDB.")
  DBI::dbDisconnect(con, shutdown = TRUE)
})

try(DBI::dbExecute(con, "SET threads TO 4"), silent = TRUE)

#' Cria (ou substitui) uma view DuckDB sobre um Parquet do ETL.
criar_view <- function(nome, arquivo) {
  caminho <- file.path(DENGUE_DATA_DIR, arquivo)
  if (!file.exists(caminho)) {
    stop("Parquet nao encontrado em '", caminho, "'. Rode data/data_dengue.R ",
         "(e data/etl_hotspots.R para a aba Analises) antes de subir o app.",
         call. = FALSE)
  }
  caminho <- normalizePath(caminho, winslash = "/", mustWork = TRUE)
  caminho <- gsub("'", "''", caminho, fixed = TRUE)
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s')",
    nome, caminho
  ))
}

criar_view("casos",          "dengue_df_processado.parquet")
criar_view("diario",         "dengue_df_diario.parquet")
criar_view("semanal",        "dengue_df_semanal.parquet")
criar_view("mensal",         "dengue_df_mensal.parquet")
criar_view("canal_endemico", "dengue_df_canal_endemico.parquet")
criar_view("hotspots",       "dengue_df_hotspots_anual.parquet")

tbl_casos          <- function() dplyr::tbl(con, "casos")
tbl_diario         <- function() dplyr::tbl(con, "diario")
tbl_semanal        <- function() dplyr::tbl(con, "semanal")
tbl_mensal         <- function() dplyr::tbl(con, "mensal")
tbl_canal_endemico <- function() dplyr::tbl(con, "canal_endemico")
tbl_hotspots       <- function() dplyr::tbl(con, "hotspots")

message(sprintf("[Dengue DF] Fonte: %s (views DuckDB sobre Parquet).", DENGUE_DATA_DIR))

# --------------------------------------------------------- populacao por RA --
# Fonte de referencia (nao vem de Parquet: e uma tabela de dominio pequena,
# igual a UF_LOOKUP do dengue_ride). replace_na(10000) no join e o fallback para
# RA sem populacao cadastrada.
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

POP_TOTAL_DF <- sum(populacao_ra$populacao)

# -------------------------------------------------- de-para setor -> RA -----
# Usado so pela aba Analises (mapa de hotspots), pra enriquecer o tooltip com
# o nome da RA - o geojson de setores so carrega CD_SETOR.
SETOR_RA_LOOKUP <- utils::read.csv(
  file.path(DENGUE_DATA_DIR, "setores_RAs_DF.csv"),
  colClasses = c(CD_SETOR = "character")
)

# --------------------------------------------------- dicionarios de dominio --
# DISTINCT sobre as views: os seletores refletem o que existe na base, sem
# lista codificada em lugar nenhum do app (mesmo principio do GEO_DICT do
# dengue_ride).
message("[Dengue DF] Carregando dicionários de domínio...")

ANOS_DISPONIVEIS <- tbl_semanal() |>
  dplyr::distinct(NU_ANO) |>
  dplyr::arrange(dplyr::desc(NU_ANO)) |>
  dplyr::pull(NU_ANO)

ANO_MIN <- min(ANOS_DISPONIVEIS)
ANO_MAX <- max(ANOS_DISPONIVEIS)

RAS_DISPONIVEIS <- tbl_semanal() |>
  dplyr::distinct(Regiao_adm) |>
  dplyr::arrange(Regiao_adm) |>
  dplyr::pull(Regiao_adm)

REGIOES_SAUDE_DISPONIVEIS <- tbl_semanal() |>
  dplyr::filter(!is.na(Regiao_sau1)) |>
  dplyr::distinct(Regiao_sau1) |>
  dplyr::arrange(Regiao_sau1) |>
  dplyr::pull(Regiao_sau1)

message(sprintf(
  "[Dengue DF] Escopo: %d RA(s) · %d região(ões) de saúde · %d–%d.",
  length(RAS_DISPONIVEIS), length(REGIOES_SAUDE_DISPONIVEIS), ANO_MIN, ANO_MAX
))

# ------------------------------------------- serie semanal completa (RAs) ---
# Usada exclusivamente por modules/mod_mapa_rt.R (mapa de razao historica por
# RA). Ao contrario das reactives filtradas do server.R, aqui NADA e
# restrito pelo recorte global: o modulo compara todas as RAs entre si contra
# o proprio historico, entao sempre precisa da serie inteira (mesma logica de
# SERIE_MUNICIPIOS no dengue_ride - pequena, cabe inteira em memoria, calculada
# uma unica vez no start).
SEMANAL_COMPLETO <- tbl_semanal() |> dplyr::collect()

# ------------------------------------------- canal endemico (pre-computado) --
# Ao contrario do dengue_ride (que deriva Q1/Q2/Q3 ao vivo de serie_semanal()),
# o canal aqui vem do ETL (data/data_dengue.R): exclui anos epidemicos
# atipicos (ver `anos_excluir_canal` no ETL) antes de calcular os quartis -
# uma decisao de curadoria que precisa ficar explicita e versionada no script,
# nao recalculada silenciosamente a cada sessao. Carregado uma vez (~1700
# linhas: RA x semana).
CANAL_ENDEMICO <- tbl_canal_endemico() |> dplyr::collect()

# ----------------------------------------------------------------- helpers ---

#' Aplica o recorte (ano/semana/RA/regiao de saude) a uma query lazy.
#'
#' Generico o bastante pra servir as quatro views (casos/diario/semanal/
#' mensal): cada filtro so entra se a coluna existir na tabela (mensal nao
#' tem SEM_NUM) e se o input nao for "Todos"/"Todas". O filtro desce ate a
#' leitura do Parquet (predicate pushdown); nada e materializado aqui.
aplicar_filtro <- function(dados, f) {
  # tbl_vars(), nao colnames(): funciona tanto em tbl lazy (DuckDB) quanto em
  # data.frame ja coletado, sem disparar a query so pra ler o schema.
  cols <- dplyr::tbl_vars(dados)

  if (!identical(f$ano, "Todos") && "NU_ANO" %in% cols) {
    dados <- dplyr::filter(dados, NU_ANO == !!as.integer(f$ano))
  }
  if (!identical(f$semana, "Todas") && "SEM_NUM" %in% cols) {
    dados <- dplyr::filter(dados, SEM_NUM == !!as.integer(f$semana))
  }
  if (!identical(f$ra, "Todas") && "Regiao_adm" %in% cols) {
    dados <- dplyr::filter(dados, Regiao_adm == !!f$ra)
  }
  if (!identical(f$regiao_saude, "Todas") && "Regiao_sau1" %in% cols) {
    dados <- dplyr::filter(dados, Regiao_sau1 == !!f$regiao_saude)
  }
  dados
}

#' Agregado por RA (casos + populacao + incidencia) a partir de casos
#' filtrados (ja coletados, data.frame).
#'
#' Consumido tanto pelo mapa de incidencia quanto pelo KPI "Incidência
#' Média" - uma unica agregacao, dois consumidores (mesmo principio do
#' serie_semanal compartilhado no dengue_ride, aplicado ao grao RA).
calcular_ra_agregado <- function(casos) {
  casos |>
    dplyr::count(Regiao_adm, name = "casos") |>
    dplyr::left_join(populacao_ra, by = c("Regiao_adm" = "ra")) |>
    dplyr::mutate(
      populacao  = tidyr::replace_na(populacao, 10000),
      incidencia = round((casos / populacao) * 100000, 1)
    )
}

#' Formata inteiros no padrao brasileiro (12.345).
fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

#' Formata numeros com casas decimais no padrao brasileiro (1.234,5).
fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

# ------------------------------------------------------------- paleta dengue --
# Identidade visual comum aos tres paineis (dengue_df, dengue_ride e
# PVdengue_df), baseada no guia Cenarios+: azul Cenarios na interface e uma
# rampa terracota que representa a dengue (nao ha cor oficial no MS nem na
# SES-DF; a familia laranja-avermelhada e a associacao mais comum). Mesmos
# valores em R/utils_ui.R do dengue_ride e R/utils_app_theme.R do PV.
#
# Regras de uso:
#   - magnitude (incidencia, casos)   -> TONS_DENGUE, claro = pouco, escuro = muito
#   - polaridade (Rt, razao historica) -> azul abaixo do limiar, terracota acima
#   - estado (alerta, obito)           -> ok/atencao/grave/critico, sempre com rotulo
#   - categorias (sexo, modelos)       -> SERIES_CENARIOS, na ordem, sem reciclar
# Validado para daltonismo (protan/deutan) e contraste sobre fundo branco.
TONS_DENGUE <- c(
  `50`  = "#FFF2EB", `100` = "#FDDDCF", `200` = "#F7C0A8", `300` = "#ED9E7C",
  `400` = "#D97F55", `500` = "#C0663C", `600` = "#A34F25", `700` = "#853B15",
  `800` = "#682B0A", `900` = "#4E1D03"
)

TONS_AZUL <- c(
  `50`  = "#ECF7FE", `100` = "#CFE9F9", `200` = "#A8D5F1", `300` = "#7ABEE6",
  `400` = "#4FA4D3", `500` = "#3891C0", `600` = "#0E7BA8", `700` = "#00638C",
  `800` = "#004565", `900` = "#00324C"
)

PALETA_DENGUE <- c(
  fundo       = "#F2F6F9",
  superficie  = "#FFFFFF",
  linha       = "#D8E3EA",
  tinta       = "#12222B",  # titulos de grafico, numeros, serie em foco
  texto       = "#4A4A4A",  # corpo
  sub         = "#7A8B95",  # eixos, legendas, linhas de referencia
  neutro      = "#B8C4CC",  # "sem dado", "ignorado"
  azul        = TONS_AZUL[["500"]],
  azul_acao   = TONS_AZUL[["600"]],
  azul_titulo = TONS_AZUL[["700"]],
  dengue      = TONS_DENGUE[["500"]],
  dengue_forte = TONS_DENGUE[["700"]],
  ok          = "#17895B",
  atencao     = "#DCA744",
  grave       = "#C65D26",
  critico     = "#8F2639"
)

SERIES_CENARIOS <- c("#3891C0", "#C0663C", "#6C5CA8", "#5A9E2E",
                     "#8E3B46", "#B08A1E", "#3F4E9E", "#1B9AAA")

RAMPA_DENGUE <- unname(TONS_DENGUE[c("50", "200", "300", "500", "700", "900")])

# Rt e razao historica: abaixo do limiar em azul, acima em terracota; dentro
# de cada lado, o tom mais forte e o pior cenario (crescendo).
CORES_QUADRANTE <- c(
  abaixo_caindo    = TONS_AZUL[["700"]],
  abaixo_subindo   = TONS_AZUL[["400"]],
  acima_caindo     = TONS_DENGUE[["400"]],
  acima_subindo    = TONS_DENGUE[["700"]],
  sem_base         = PALETA_DENGUE[["sub"]]
)

#' Converte "#RRGGBB" em "rgba(r,g,b,a)" (echarts aceita os dois; rgba
#' quando a cor precisa de transparencia).
cor_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# Nota: o dengue_ride usa cortes fixos do MS (piecewise) pra incidencia, porque
# la o valor plotado e sempre incidencia ANUAL (o mapa tem seu proprio
# slider de periodo, default 1-2 anos). Aqui o mapa de incidencia
# (modules/mod_mapa_incidencia.R) usa o recorte global (ano/semana), que
# pode somar uma unica semana ou vários anos - a magnitude varia demais pra
# uma escala fixa fazer sentido, entao o mapa usa visualMap continuo,
# auto-ajustado ao maximo do recorte atual (RAMPA_DENGUE).

# Paleta fixa de texto/grade/tooltip para os graficos echarts4r em "e_list()"
# manual. Substitui o antigo theme_cores() reativo claro/escuro: o painel
# agora tem uma unica identidade visual (mesma decisao do dengue_ride, que
# tambem nao tem alternancia de tema).
CORES_GRAFICO <- list(
  texto      = PALETA_DENGUE[["tinta"]],
  sub        = PALETA_DENGUE[["sub"]],
  grid       = PALETA_DENGUE[["linha"]],
  bg_tooltip = "rgba(255,255,255,0.97)",
  borda_tt   = PALETA_DENGUE[["linha"]],
  texto_tt   = PALETA_DENGUE[["tinta"]]
)

# Fonte dos graficos echarts (o echarts nao herda a fonte do CSS da pagina).
FONTE_GRAFICO <- "'DM Sans', system-ui, sans-serif"

# --------------------------------------------------------- malha das RAs -----
GEO_DIR <- DENGUE_DATA_DIR

.cache_geojson <- new.env(parent = emptyenv())

#' Carrega (e memoriza) um GeoJSON como lista pronta para o echarts4r.
carregar_geojson <- function(chave, arquivo) {
  if (!is.null(.cache_geojson[[chave]])) return(.cache_geojson[[chave]])
  caminho <- file.path(GEO_DIR, arquivo)
  if (!file.exists(caminho)) return(NULL)
  js <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)
  assign(chave, js, envir = .cache_geojson)
  js
}

# NENHUM geojson e carregado aqui (global.R), de proposito. df_ras_wgs84.
# geojson tem ~2,75MB de coordenadas de setores unidas por RA (bem mais
# pesado do que os 33 poligonos sugerem - a fronteira de cada RA ainda carrega
# a densidade de vertice de setor censitario, ver data/gerar_geojson.R) e
# dengue_df_setores_hotspot.geojson tem ~5.400 poligonos / ~11MB. Parse-los
# com jsonlite::fromJSON() e sem vetorizacao - da ordem de dezenas de
# segundos para o primeiro, cerca de 1 minuto pro segundo, ANTES de
# simplificar. Carrega-los aqui bloquearia a subida do processo R inteiro
# (ninguem consegue nem abrir o app) so pra popular um cache que a maioria
# das sessoes nem usa de imediato.
#
# Os dois sao carregados SOB DEMANDA por carregar_geojson(chave, arquivo)
# dentro do renderEcharts4r de cada mapa (modules/mod_mapa_incidencia.R e
# modules/mod_analises_hotspot.R) e ficam memorizados em .cache_geojson dali
# em diante - mesmo cache, so que preenchido na primeira renderizacao em vez
# de no boot.
#
# ANTES DE USAR EM DEV OU SUBIR EM PRODUCAO: rode prep/01_simplifica_malhas.R
# (uma vez) - reduz o numero de vertices dos dois arquivos com
# sf::st_simplify(), mesma tecnica do 01_simplifica_malhas.R do dengue_ride.
# Sem isso, a PRIMEIRA renderizacao de cada mapa ainda vai demorar (o parse
# so foi adiado, nao eliminado).

# --------------------------------------------------------- modulos do app ----
for (.f in c(
  "utils_ui.R",
  "mod_filtros.R",
  "mod_kpi_cards.R",
  "mod_mapa_incidencia.R",
  "mod_mapa_rt.R",
  "mod_rt_temporal.R",
  "mod_curva_epidemia.R",
  "mod_distribuicao_temporal.R",
  "mod_perfil_casos.R",
  "mod_canal_endemico.R",
  "mod_analises_hotspot.R"
)) {
  source(file.path("modules", .f), local = FALSE, encoding = "UTF-8")
}
rm(.f)
