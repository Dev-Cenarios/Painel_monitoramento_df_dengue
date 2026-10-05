# ==============================================================================
# Dengue DF | prep/01_simplifica_malhas.R
#
# Executar UMA VEZ (ou de novo, se data/gerar_geojson.R ou data/etl_hotspots.R
# rodarem e regravarem os GeoJSON) - reduz o numero de vertices dos poligonos
# nos dois GeoJSON usados pelos mapas do app. Portado de
# 01_simplifica_malhas.R do dengue_ride, mesma tecnica e mesmo motivo.
#
# POR QUE ISSO IMPORTA:
#   Os mapas coropleticos (modules/mod_mapa_incidencia.R e modules/mod_
#   analises_hotspot.R) embutem a malha inteira no payload do grafico a cada
#   sessao do navegador - nao e um arquivo estatico cacheavel, viaja de novo
#   toda vez (e de novo a cada re-render, ex.: ao trocar a metrica na aba
#   Analises - ver nota no fim deste arquivo). Sem simplificar, o parse no R
#   (jsonlite::fromJSON) e o payload no navegador ficam pesados o bastante
#   pra fazer a PRIMEIRA renderizacao de cada mapa demorar dezenas de
#   segundos:
#     data/df_ras_wgs84.geojson              ~2,75MB (33 RAs, mas cada
#       fronteira ainda carrega a densidade de vertice de SETOR CENSITARIO -
#       ver data/gerar_geojson.R, que faz st_union de setores por RA sem
#       simplificar antes)
#     data/dengue_df_setores_hotspot.geojson ~11MB (~5.400 setores, e cada
#       feicao carrega ~30 colunas do IBGE - regiao, UF, distrito,
#       subdistrito, bairro, area etc. - que o app NUNCA le: so usa CD_SETOR
#       como chave de juncao. sf::st_simplify() reduz vertices da geometria,
#       mas nao remove essas colunas; este script tambem descarta as colunas
#       nao usadas, o que sozinho ja derruba boa parte do tamanho do arquivo,
#       ja que "Centro-Oeste"/"Distrito Federal"/etc. se repetem em cada uma
#       das 5.400+ linhas)
#   carregar_geojson() (config/global.R) so parseia cada um na primeira vez
#   que o mapa correspondente e aberto (nao no boot do processo R) - isso
#   evita bloquear o app inteiro, mas NAO substitui simplificar: o parse
#   adiado ainda vai ser lento se os arquivos continuarem do tamanho atual.
#
# POR QUE ISSO E UM PASSO DE PREPARO, NAO CODIGO DO APP:
#   sf/GDAL/GEOS sao dependencias pesadas de instalar num servidor de deploy.
#   Simplificar aqui, uma vez, e regravar os MESMOS arquivos .geojson menores
#   mantem o app rodando so com jsonlite.
#
# Uso:  setwd("~/dengue_df"); source("prep/01_simplifica_malhas.R")
# ==============================================================================

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Instale antes: install.packages('sf')", call. = FALSE)
}

sf::sf_use_s2(FALSE)

message(sprintf("[prep] sf %s | GEOS %s | s2 ativo: %s",
                as.character(utils::packageVersion("sf")),
                sf::sf_extSoftVersion()[["GEOS"]],
                sf::sf_use_s2()))

contar_vertices <- function(shp) {
  sum(vapply(sf::st_geometry(shp), function(g) nrow(sf::st_coordinates(g)), integer(1)))
}

#' Simplifica um GeoJSON no lugar (regrava o mesmo arquivo).
#'
#' @param caminho arquivo .geojson a simplificar.
#' @param chave   coluna usada como chave de junção pelo app - também a
#'   única coluna de atributo mantida no arquivo final (além da geometria).
#' @param tolerancia_graus ~111m por 0,001 grau no equador. RAs são regiões
#'   grandes vistas em zoom baixo (tolerância mais alta é segura); setores
#'   são pequenos e adjacentes entre si (tolerância mais conservadora, para
#'   não abrir buracos/sobreposições visíveis no zoom que a aba usa).
simplificar_arquivo <- function(caminho, chave, tolerancia_graus) {
  if (!file.exists(caminho)) {
    message(sprintf("  %s: arquivo ausente, pulando.", caminho))
    return(invisible(NULL))
  }

  antes_kb   <- file.size(caminho) / 1024
  shp        <- sf::st_read(caminho, quiet = TRUE)
  antes_v    <- contar_vertices(shp)
  antes_cols <- ncol(sf::st_drop_geometry(shp))

  if (!chave %in% names(shp)) {
    warning(sprintf("[prep] %s sem coluna '%s' - nao mexido.", caminho, chave), call. = FALSE)
    return(invisible(NULL))
  }

  # Descarta toda coluna de atributo exceto a chave de junção - o app só
  # precisa dela (as métricas vêm de fora, via parquet/DuckDB, não do
  # próprio geojson). Reduz o tamanho do arquivo por si só, independente da
  # simplificação de geometria abaixo.
  if (antes_cols > 1) {
    shp <- shp[, chave, drop = FALSE]
  }

  shp_simples <- sf::st_simplify(shp, dTolerance = tolerancia_graus, preserveTopology = TRUE)

  vazio <- sf::st_is_empty(shp_simples)
  if (any(vazio)) {
    sf::st_geometry(shp_simples)[vazio] <- sf::st_geometry(shp)[vazio]
    message(sprintf("  %d polígono(s) mantido(s) sem simplificar (ficariam vazios).", sum(vazio)))
  }

  depois_v <- contar_vertices(shp_simples)

  # Regrava se a geometria encolheu OU se colunas foram descartadas acima -
  # as duas reduções são independentes, uma pode acontecer sem a outra.
  if (depois_v >= antes_v && antes_cols <= 1) {
    message(sprintf(
      "  SEM REDUCAO (%d -> %d vertices, %d coluna(s)). Nao regravado - tente uma tolerancia maior.",
      antes_v, depois_v, antes_cols
    ))
    return(invisible(NULL))
  }

  sf::st_write(shp_simples, caminho, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  depois_kb <- file.size(caminho) / 1024

  message(sprintf(
    "  %.0f KB -> %.0f KB (%.0f%%) | %d -> %d vertices (%.0f%%) | %d -> %d coluna(s) | %d feição(ões)",
    antes_kb, depois_kb, 100 * depois_kb / antes_kb,
    antes_v, depois_v, 100 * depois_v / max(antes_v, 1),
    antes_cols, ncol(sf::st_drop_geometry(shp_simples)), nrow(shp)
  ))
}

message("\n[prep] Malha de RAs (data/df_ras_wgs84.geojson)...")
simplificar_arquivo(
  file.path("data", "df_ras_wgs84.geojson"),
  chave = "ra_nome", tolerancia_graus = 0.001
)

message("\n[prep] Malha de setores censitários (data/dengue_df_setores_hotspot.geojson)...")
simplificar_arquivo(
  file.path("data", "dengue_df_setores_hotspot.geojson"),
  chave = "CD_SETOR", tolerancia_graus = 0.0003
)

message("\n[prep] Concluído.")

# ------------------------------------------------------------------------------
# NOTA: reduzir geometria/colunas encolhe o payload por renderizacao, mas nao
# elimina uma segunda causa na aba Analises (modules/mod_analises_hotspot.R):
# o mapa e um shiny::renderEcharts4r() que depende de input$metrica, entao
# TROCAR a metrica (persistencia/casos totais) reconstroi o grafico inteiro
# e reenvia a malha pro navegador de novo - mesmo padrao do Epi Scanner do
# dengue_ride (R/mod_epi_scanner_server.R), so que la a malha e 29 municipios
# (nao pesa) e aqui e 5.400+ setores. Se depois de rodar este script a aba
# ainda estiver lenta especificamente AO TROCAR A METRICA (nao so na
# primeira abertura), o proximo passo seria reescrever aquele render pra
# registrar a malha uma unica vez e atualizar so a cor via
# echarts4r::echarts4rProxy() - nao fiz essa mudanca agora porque muda o
# padrao de renderizacao do modulo e eu nao tinha como testar no ambiente
# em que escrevi isso.
