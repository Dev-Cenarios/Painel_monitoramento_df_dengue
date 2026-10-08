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
  # list(ra = <RA ou "Todas">, .ts): clique no mapa de incidencia ou botoes
  # "Visao geral". O .ts faz cliques repetidos sempre notificarem.
  foco_bus <- shiny::reactiveVal(NULL)

  # -- filtros -----------------------------------------------------------------
  filtros <- mod_filtros_server(
    "filtros",
    foco_externo = shiny::reactive(foco_bus())
  )

  # RA em foco (NULL = visao geral do DF). Destacada no mapa de incidencia e
  # no grafico Rt x razao historica - mesmo papel do selected_location do
  # dengue_ride, so que derivado direto do recorte (o DF tem um unico nivel).
  ra_selecionada <- shiny::reactive({
    ra <- filtros()$ra
    if (identical(ra, "Todas")) NULL else ra
  })

  # Botao "Visao geral" no cabecalho do card de recorte (config/ui.R).
  shiny::observeEvent(input$limpar_selecao, {
    foco_bus(list(ra = "Todas", .ts = as.numeric(Sys.time())))
  }, ignoreInit = TRUE)

  # -- reactives compartilhadas, uma consulta por grao/recorte ----------------
  # So as colunas que os modulos leem (RA, evolucao, sexo, faixa etaria): sem
  # recorte, a abertura do painel traz o microdado inteiro para o R; cortar
  # datas, classificacao e semana reduz memoria e tempo de transferencia.
  casos_filtrados <- shiny::reactive({
    aplicar_filtro(tbl_casos(), filtros()) |>
      dplyr::select(Regiao_adm, EVOLUCAO, CS_SEXO, FAIXA_ETARIA) |>
      dplyr::collect()
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

  # RA agregado (casos + populacao + incidencia) do recorte - usado pelo KPI
  # "Incidência Média". O mapa usa ra_mapa(), abaixo, que ignora o filtro de RA.
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
  # O mapa desenha SEMPRE todas as RAs: recorte de ano/semana/regiao de saude,
  # mas nao o de RA (escolher uma RA so a destaca, nao esvazia o mapa). A
  # contagem por RA e feita no DuckDB - so ~33 linhas trafegam para o R.
  ra_mapa <- shiny::reactive({
    f <- filtros()
    f$ra <- "Todas"
    aplicar_filtro(tbl_casos(), f) |>
      dplyr::count(Regiao_adm, name = "casos") |>
      dplyr::collect() |>
      dplyr::mutate(casos = as.numeric(casos)) |>
      dplyr::left_join(populacao_ra, by = c("Regiao_adm" = "ra")) |>
      dplyr::mutate(
        populacao  = tidyr::replace_na(populacao, 10000),
        incidencia = round((casos / populacao) * 100000, 1)
      )
  }) |> shiny::bindCache(filtros()$ano, filtros()$semana, filtros()$regiao_saude)

  clique_mapa <- mod_mapa_incidencia_server(
    "mapa_inc",
    ra_mapa        = ra_mapa,
    ra_selecionada = ra_selecionada
  )

  shiny::observeEvent(clique_mapa(), {
    foco_bus(clique_mapa())
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # -- mapa de tendencia (Rt x razao historica) ---------------------------------
  # Sempre compara TODAS as RAs contra o proprio historico (ver nota no topo
  # de modules/mod_mapa_rt.R) - le SEMANAL_COMPLETO direto do global.R, sem
  # consultar o DuckDB de novo. Recebe a RA em foco so para DESTACA-la.
  mod_mapa_rt_server("mapa_rt", ra_selecionada = ra_selecionada)

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
