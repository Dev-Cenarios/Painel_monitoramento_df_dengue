# ==============================================================================
# Dengue DF | Modulo: mapa coropletico de incidencia por RA (echarts4r)
#
# Consome ra_agregado() - reactive compartilhada com o KPI "Incidência
# Média" (config/server.R), calculada uma unica vez por recorte via
# calcular_ra_agregado() (config/global.R). Antes, este modulo recalculava o
# mesmo count(Regiao_adm) por conta propria.
#
# Clique numa RA envia um pedido de foco para modules/mod_filtros.R - mesmo
# barramento de foco do mod_mapa.R do dengue_ride.
# ==============================================================================

mod_mapa_incidencia_saida <- function(id, height = "500px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("mapa"), height = height)
}

mod_mapa_incidencia_server <- function(id, ra_agregado) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$mapa <- echarts4r::renderEcharts4r({
      # Carregada sob demanda (nao em global.R): ver nota em config/global.R
      # sobre por que este geojson (2,75MB) so e parseado na primeira
      # renderizacao do mapa, nao no boot do processo R.
      malha <- carregar_geojson("RAS", "df_ras_wgs84.geojson")
      shiny::validate(shiny::need(
        !is.null(malha),
        "Malha de RAs indisponível (data/df_ras_wgs84.geojson)."
      ))

      df <- ra_agregado() |>
        dplyr::mutate(
          tooltip_html = paste0(
            "<b>", Regiao_adm, "</b><br/>",
            "Casos: ", fmt_int(casos), "<br/>",
            "Incidência: ", fmt_num(incidencia, 1), " /100 mil hab."
          )
        )

      map_data <- lapply(seq_len(nrow(df)), function(i) list(
        name         = df$Regiao_adm[i],
        value        = df$incidencia[i],
        tooltip_html = df$tooltip_html[i]
      ))

      js_click <- htmlwidgets::JS(sprintf(
        "function(p){
           if (p && p.name) {
             Shiny.setInputValue('%s', p.name, {priority: 'event'});
           }
         }",
        ns("clique")
      ))

      opts <- list(
        backgroundColor = "transparent",
        tooltip = list(
          trigger   = "item",
          formatter = htmlwidgets::JS(
            "function(p){ return p.data && p.data.tooltip_html ? p.data.tooltip_html : p.name; }"
          ),
          backgroundColor = CORES_GRAFICO$bg_tooltip, borderColor = CORES_GRAFICO$borda_tt,
          borderWidth = 1, textStyle = list(color = CORES_GRAFICO$texto_tt, fontSize = 12)
        ),
        # Continuo, auto-ajustado ao maximo do recorte atual - NAO usa os
        # cortes fixos do MS (piecewise): a incidencia aqui e cumulativa
        # sobre o periodo do filtro global (ano/semana), que pode ser uma
        # unica semana ou anos inteiros somados. Uma escala fixa calibrada
        # pra incidencia ANUAL deixaria o mapa inteiro numa faixa so
        # (todo claro numa semana, todo escuro somando vários anos).
        visualMap = list(
          type       = "continuous",
          min        = 0,
          max        = max(c(0, df$incidencia), na.rm = TRUE),
          show       = TRUE,
          orient     = "vertical",
          left       = 15,
          bottom     = 60,
          inRange    = list(color = RAMPA_CHOCOLATE),
          outOfRange = list(color = PALETA_CHOCOLATE[["creme"]]),
          text       = list("Alta", "Baixa"),
          textStyle  = list(color = CORES_GRAFICO$texto, fontSize = 11)
        ),
        series = list(list(
          type         = "map",
          map          = "DF_RAs",
          roam         = TRUE,
          nameProperty = "ra_nome",
          data         = map_data,
          itemStyle    = list(borderColor = "#FFFFFF", borderWidth = 0.8),
          emphasis     = list(
            itemStyle = list(areaColor = PALETA_CHOCOLATE[["ambar"]]),
            label     = list(show = TRUE, color = CORES_GRAFICO$texto, fontSize = 10, fontWeight = "bold")
          ),
          label = list(show = FALSE)
        ))
      )

      echarts4r::e_charts() |>
        echarts4r::e_map_register("DF_RAs", malha) |>
        echarts4r::e_list(opts) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem") |>
        echarts4r::e_on(query = NULL, handler = js_click, event = "click")
    })

    # -- clique -> pedido de foco para o filtro global ----------------------
    shiny::eventReactive(input$clique, {
      shiny::req(input$clique)
      input$clique
    }, ignoreInit = TRUE)
  })
}
