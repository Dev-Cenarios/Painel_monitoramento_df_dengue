# ==============================================================================
# Dengue DF | server.R
# Orquestra os modulos e monta o pipeline lazy de consulta (DuckDB).
#
# Um reactive por grao de dado (casos/diario/semanal/mensal), cada um
# consultado UMA vez por recorte e reaproveitado por varios modulos - mesmo
# principio do serie_semanal no server.R do dengue_ride, aplicado aos quatro
# graos do ETL do DF. Antes, kpi_cards e mapa_incidencia recalculavam o
# mesmo count(Regiao_adm) cada um por conta propria; agora os dois consomem
# ra_agregado().
# ==============================================================================

server <- function(input, output, session) {

  # -- barramento de foco ----------------------------------------------------
  foco_bus <- shiny::reactiveVal(NULL)

  # -- filtros -----------------------------------------------------------------
  filtros <- mod_filtros_server(
    "filtros",
    foco_externo = shiny::reactive(foco_bus())
  )

  # -- reactives compartilhadas, uma consulta por grao/recorte ----------------
  casos_filtrados <- shiny::reactive({
    aplicar_filtro(tbl_casos(), filtros()) |> dplyr::collect()
  }) |> shiny::bindCache(filtros())

  diario_filtrado <- shiny::reactive({
    aplicar_filtro(tbl_diario(), filtros()) |> dplyr::collect()
  }) |> shiny::bindCache(filtros())

  semanal_filtrado <- shiny::reactive({
    aplicar_filtro(tbl_semanal(), filtros()) |> dplyr::collect()
  }) |> shiny::bindCache(filtros())

  mensal_filtrado <- shiny::reactive({
    aplicar_filtro(tbl_mensal(), filtros()) |> dplyr::collect()
  }) |> shiny::bindCache(filtros())

  # RA agregado (casos + populacao + incidencia) - compartilhado entre o KPI
  # "Incidência Média" e o mapa de incidencia.
  ra_agregado <- shiny::reactive({
    calcular_ra_agregado(casos_filtrados())
  }) |> shiny::bindCache(filtros())

  # -- KPIs --------------------------------------------------------------------
  mod_kpi_cards_server(
    "kpis",
    casos_filtrados  = casos_filtrados,
    ra_agregado      = ra_agregado,
    semanal_filtrado = semanal_filtrado
  )

  # -- mapa de incidencia --------------------------------------------------------
  clique_mapa <- mod_mapa_incidencia_server("mapa_inc", ra_agregado = ra_agregado)

  shiny::observeEvent(clique_mapa(), {
    foco_bus(clique_mapa())
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # -- mapa de tendencia (Rt x razao historica) ---------------------------------
  # Sem parametros: sempre compara TODAS as RAs contra o proprio historico
  # (ver nota no topo de modules/mod_mapa_rt.R) - le SEMANAL_COMPLETO direto
  # do global.R, sem consultar o DuckDB de novo.
  mod_mapa_rt_server("mapa_rt")

  # -- evolucao temporal --------------------------------------------------------
  mod_curva_epidemia_server("curva_epi", diario_filtrado = diario_filtrado)

  mod_rt_temporal_server("rt_temporal", semanal_filtrado = semanal_filtrado)

  mod_distribuicao_temporal_server(
    "temporal",
    semanal_filtrado = semanal_filtrado,
    mensal_filtrado  = mensal_filtrado
  )

  # -- canal endemico ------------------------------------------------------------
  # Temporada observada tem seletor proprio (ver nota no topo de modules/
  # mod_canal_endemico.R) - so recebe o filtro de RA, nao o ano/semana global.
  mod_canal_endemico_server(
    "canal_endemico",
    filtro_ra = shiny::reactive(filtros()$ra)
  )

  # -- perfil dos casos ------------------------------------------------------------
  mod_perfil_casos_server("perfil", casos_filtrados = casos_filtrados)

  # -- aba Analises (hotspots por setor) ------------------------------------------
  # Tambem nao recebe filtros(): mapa+tabela global por setor censitario, de
  # proposito (ver nota no topo de modules/mod_analises_hotspot.R).
  mod_analises_hotspot_server("hotspots")

  # -- titulo do recorte, para o cabecalho da secao de dados brutos -----------
  output$titulo_recorte <- shiny::renderText({
    filtros()$rotulo
  })

  # -- tabela das ultimas semanas (agregada no DuckDB) -------------------------
  serie <- shiny::reactive({
    aplicar_filtro(tbl_semanal(), filtros()) |>
      dplyr::group_by(NU_ANO, SEM_NUM) |>
      dplyr::summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(NU_ANO), dplyr::desc(SEM_NUM)) |>
      head(12) |>
      dplyr::collect()
  }) |> shiny::bindCache(filtros())

  output$tab_serie <- shiny::renderTable({
    serie() |>
      dplyr::mutate(
        `Ano`   = as.character(NU_ANO),
        `SE`    = as.character(SEM_NUM),
        `Casos` = fmt_int(casos),
        .keep = "none"
      )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "llr")

  # -- SQL gerado (transparencia do lazy eval) ---------------------------------
  output$sql_gerado <- shiny::renderText({
    q <- aplicar_filtro(tbl_semanal(), filtros()) |>
      dplyr::group_by(NU_ANO, SEM_NUM) |>
      dplyr::summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(NU_ANO), dplyr::desc(SEM_NUM)) |>
      head(12)

    paste(as.character(dbplyr::sql_render(q)), collapse = "\n")
  })
}
