# ==============================================================================
# Dengue DF | Modulo: aba "Análises" — hotspots espaciais persistentes
#
# Equivalente ao Epi Scanner do dengue_ride (R/mod_epi_scanner_*.R): mapa +
# tabela ranqueada, escopo GLOBAL (nao usa o recorte da aba Painel, pelo
# mesmo motivo - aqui a pergunta e "quais setores concentram casos ao longo
# dos anos", nao "o que aconteceu num recorte"). Fonte: data/etl_hotspots.R
# (Getis-Ord Gi*, metodo de Dzul-Manzanilla et al. 2021), que ate agora nao
# tinha aba/modulo no app.
#
# PERFORMANCE: a malha de setores censitarios (data/dengue_df_setores_
# hotspot.geojson) tem ~5.400 poligonos e ~11MB brutos - MUITO mais pesada
# que a malha municipal do dengue_ride (29 poligonos). Antes de subir esta aba
# em producao, rode prep/01_simplifica_malhas.R (mesma tecnica do
# 01_simplifica_malhas.R do dengue_ride: sf::st_simplify() reduz o numero de
# vertices sem trocar o arquivo de lugar), senao o payload embutido no
# grafico deixa a aba lenta pra carregar no navegador.
# ==============================================================================

mod_analises_hotspot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$p(
      class = "text-muted small mb-3",
      "Persistência de hotspots espaciais (Getis-Ord Gi*, limiar de Bonferroni Z ≥ 3,71) ",
      "por setor censitário — método de Dzul-Manzanilla et al. 2021. Escopo global: não usa o recorte acima."
    ),
    shiny::radioButtons(
      ns("metrica"), "Métrica",
      choices = c(
        "Persistência (anos como hotspot)" = "persistencia",
        "Casos totais (todos os anos)"     = "casos_totais"
      ),
      selected = "persistencia",
      inline   = TRUE
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),

      cartao_grafico(
        titulo = "Mapa de persistência por setor",
        saida  = echarts4r::echarts4rOutput(ns("mapa"), height = "520px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "bg-primary text-white", "Top 15 setores"),
        bslib::card_body(DT::DTOutput(ns("tabela")))
      )
    )
  )
}

mod_analises_hotspot_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # -- agregado por setor (uma unica consulta, reaproveitada pelo mapa e
    # pela tabela) -----------------------------------------------------------
    setores_agregado <- shiny::reactive({
      tbl_hotspots() |>
        dplyr::group_by(CD_SETOR) |>
        dplyr::summarise(
          anos_avaliados    = dplyr::n(),
          anos_como_hotspot = sum(hotspot, na.rm = TRUE),
          casos_totais      = sum(casos, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(CD_SETOR = as.character(CD_SETOR)) |>
        dplyr::left_join(
          dplyr::distinct(SETOR_RA_LOOKUP, CD_SETOR, ra_nome),
          by = "CD_SETOR"
        )
    })

    metrica_valor <- shiny::reactive({
      switch(input$metrica,
        persistencia = "anos_como_hotspot",
        casos_totais = "casos_totais"
      )
    })

    rotulo_metrica <- shiny::reactive({
      switch(input$metrica,
        persistencia = "Anos como hotspot",
        casos_totais = "Casos (total)"
      )
    })

    # -- mapa ------------------------------------------------------------------
    output$mapa <- echarts4r::renderEcharts4r({
      # Carregada sob demanda (nao em global.R): ver nota em config/global.R
      # sobre por que esta malha so e parseada quando a aba Analises e
      # aberta pela primeira vez, e por que rodar prep/01_simplifica_
      # malhas.R antes continua necessario mesmo assim.
      malha <- carregar_geojson("HOTSPOTS", "dengue_df_setores_hotspot.geojson")
      shiny::validate(shiny::need(
        !is.null(malha),
        "Malha de setores indisponível (data/dengue_df_setores_hotspot.geojson)."
      ))

      d   <- setores_agregado()
      col <- metrica_valor()
      shiny::req(nrow(d) > 0)

      nomes_js <- jsonlite::toJSON(
        stats::setNames(as.list(dplyr::coalesce(d$ra_nome, "—")), d$CD_SETOR),
        auto_unbox = TRUE
      )

      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(p){
           var ras = %s;
           var ra = ras[p.name] || '—';
           if (p.value === undefined || p.value === null || isNaN(p.value)) {
             return '<b>Setor ' + p.name + '</b><br/>' + ra + '<br/>sem dado';
           }
           return '<b>Setor ' + p.name + '</b><br/>' + ra + '<br/>%s: ' +
                  p.value.toLocaleString('pt-BR', {maximumFractionDigits: 0});
         }",
        nomes_js, rotulo_metrica()
      ))

      d |>
        dplyr::transmute(chave = CD_SETOR, valor = .data[[col]]) |>
        echarts4r::e_charts(chave) |>
        echarts4r::e_map_register("SETORES_DF", malha) |>
        echarts4r::e_map(
          valor,
          map          = "SETORES_DF",
          name         = rotulo_metrica(),
          nameProperty = "CD_SETOR",
          roam         = TRUE,
          itemStyle    = list(borderColor = "#FFFFFF", borderWidth = 0.15),
          label        = list(show = FALSE),
          emphasis     = list(
            itemStyle = list(areaColor = PALETA_CHOCOLATE[["ambar"]]),
            label     = list(show = FALSE)
          )
        ) |>
        echarts4r::e_visual_map(
          min     = 0,
          max     = max(d[[col]], na.rm = TRUE),
          orient  = "vertical",
          left    = "left",
          bottom  = "6%",
          inRange = list(color = RAMPA_CHOCOLATE),
          formatter = htmlwidgets::JS(
            "function(v){return v.toLocaleString('pt-BR',{maximumFractionDigits:0});}"
          )
        ) |>
        echarts4r::e_tooltip(trigger = "item", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem")
    })

    # -- tabela ------------------------------------------------------------------
    output$tabela <- DT::renderDT({
      col <- metrica_valor()

      tab <- setores_agregado() |>
        dplyr::filter(!is.na(.data[[col]])) |>
        dplyr::arrange(dplyr::desc(.data[[col]])) |>
        head(15) |>
        dplyr::transmute(
          Setor              = CD_SETOR,
          `RA`               = dplyr::coalesce(ra_nome, "—"),
          `Anos c/ hotspot`  = anos_como_hotspot,
          `Casos (total)`    = casos_totais
        )

      # Índice 0-based (convenção DataTables/JS) da coluna que corresponde à
      # métrica escolhida, para pré-ordenar a tabela por ela.
      idx_destaque <- if (identical(col, "anos_como_hotspot")) 2L else 3L

      DT::datatable(
        tab,
        rownames = FALSE,
        options  = list(dom = "t", pageLength = 15, order = list(list(idx_destaque, "desc"))),
        class    = "compact stripe"
      )
    })
  })
}
