# ==============================================================================
# Dengue DF | Modulo: mapa coropletico de incidencia por RA (echarts4r)
#
# SEMPRE desenha todas as RAs: consome ra_mapa() (config/server.R), que
# aplica o recorte de ano/semana/regiao de saude mas NAO o de RA - escolher
# uma RA nao esvazia o mapa (mesma decisao do mod_mapa.R do dengue_ride, onde
# clicar num municipio nao recorta o mapa). A RA escolhida e so DESTACADA.
#
# Clique numa RA envia um pedido de foco para modules/mod_filtros.R, que muda
# o recorte global (KPIs, series, perfil...). O botao "Visao geral", sobreposto
# ao mapa, pede o recorte "Todas".
#
# FEEDBACK VISUAL (entrada `ra_selecionada`): um observer traduz a RA do
# recorte numa mensagem para o handler JS 'dengue_df_map_highlight'
# (mod_mapa_incidencia_saida), que marca o poligono com o estado 'select' do
# ECharts e reduz a opacidade das demais RAs - SEM re-renderizar o mapa.
# ==============================================================================

mod_mapa_incidencia_saida <- function(id, height = "500px") {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Script unico da pagina (singleton), portado do dengue_ride:
    # (a) DESTAQUE pelo estado 'select' do ECharts (persistente, casado por
    #     nome = RA) + opacidade reduzida no resto da serie. Re-afirmado 4x
    #     numa janela de ~480 ms porque, quando o recorte muda, o mapa pode
    #     ser re-renderizado junto com a mensagem, sem ordem garantida.
    # (b) WAKE UP do input de clique: o primeiro Shiny.setInputValue disparado
    #     de dentro do handler de clique do ECharts as vezes nao chega ao
    #     servidor. Registrar a chave com "" ao conectar deixa o canal pronto.
    shiny::singleton(shiny::tags$script(shiny::HTML(sprintf("
      Shiny.addCustomMessageHandler('dengue_df_map_highlight', function(m){
        var n = 0;
        function apply(){
          n++;
          var el = document.getElementById(m.id);
          var chart = null;
          if (el && window.echarts) {
            chart = echarts.getInstanceByDom(el) ||
                    (el.firstElementChild && echarts.getInstanceByDom(el.firstElementChild));
          }
          if (chart) {
            var opt = chart.getOption();
            var nData = (opt.series && opt.series[0] && opt.series[0].data) ? opt.series[0].data.length : 0;
            var hasSel = (m.name !== null && m.name !== undefined && m.name !== '');
            chart.setOption({ series: [{
              selectedMode: 'single',
              itemStyle: { opacity: hasSel ? 0.35 : 1 },
              select: { itemStyle: { opacity: 1 } }
            }]});
            if (nData > 0) {
              var all = [];
              for (var i = 0; i < nData; i++) all.push(i);
              chart.dispatchAction({ type: 'unselect', seriesIndex: 0, dataIndex: all });
            }
            if (hasSel) {
              chart.dispatchAction({ type: 'select', seriesIndex: 0, name: String(m.name) });
            }
          }
          if (n < 4) setTimeout(apply, 120);
        }
        apply();
      });

      (function(){
        var cliqueId = '%s';
        function wake(){ try { Shiny.setInputValue(cliqueId, ''); } catch (e) {} }
        if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected &&
            Shiny.shinyapp.isConnected()) {
          wake();
        } else {
          document.addEventListener('shiny:connected', wake);
        }
      })();
    ", ns("clique"))))),

    shiny::div(
      class = "position-relative",
      shiny::actionButton(
        ns("limpar"),
        label = "Visão geral",
        icon  = shiny::icon("rotate-left"),
        class = "btn btn-sm btn-light shadow-sm",
        style = paste(
          "position:absolute; top:8px; left:8px; z-index:5;",
          "padding:.15rem .5rem; font-size:.8rem;"
        )
      ),
      echarts4r::echarts4rOutput(ns("mapa"), height = height)
    )
  )
}

mod_mapa_incidencia_server <- function(id, ra_mapa, ra_selecionada = shiny::reactive(NULL)) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$mapa <- echarts4r::renderEcharts4r({
      # Carregada sob demanda (nao em global.R): ver nota em config/global.R.
      malha <- carregar_geojson("RAS", "df_ras_wgs84.geojson")
      shiny::validate(shiny::need(
        !is.null(malha),
        "Malha de RAs indisponível (data/df_ras_wgs84.geojson)."
      ))

      df <- ra_mapa() |>
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

      # setTimeout minimo: tira o setInputValue de dentro do stack do evento de
      # clique do ECharts (junto com o "wake up" acima, garante que ate o
      # primeiro clique da sessao chegue ao servidor).
      js_click <- htmlwidgets::JS(sprintf(
        "function(p){
           if (p && p.name) {
             var alvo = p.name;
             setTimeout(function(){
               Shiny.setInputValue('%s', alvo, {priority: 'event'});
             }, 60);
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
          inRange    = list(color = RAMPA_DENGUE),
          outOfRange = list(color = TONS_DENGUE[["50"]]),
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
            itemStyle = list(areaColor = TONS_AZUL[["300"]]),
            label     = list(show = TRUE, color = CORES_GRAFICO$texto, fontSize = 10, fontWeight = "bold")
          ),
          # Estado "select": destaque persistente da RA escolhida (aplicado
          # pelo handler JS). Azul, fora da rampa terracota, com o nome.
          selectedMode = "single",
          select       = list(
            itemStyle = list(areaColor = PALETA_DENGUE[["azul_acao"]], opacity = 1,
                             borderColor = PALETA_DENGUE[["tinta"]], borderWidth = 1.5),
            label     = list(show = TRUE, color = "#FFFFFF", fontSize = 11, fontWeight = "bold")
          ),
          label = list(show = FALSE)
        )),
        textStyle = list(fontFamily = FONTE_GRAFICO)
      )

      echarts4r::e_charts() |>
        echarts4r::e_map_register("DF_RAs", malha) |>
        echarts4r::e_list(opts) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem") |>
        echarts4r::e_on(query = NULL, handler = js_click, event = "click")
    })

    # -- feedback visual: destaque da RA do recorte --------------------------
    # Depende tambem de ra_mapa(): quando o recorte re-renderiza o mapa, o
    # observer re-roda e re-emite a mensagem (o handler re-afirma 4x).
    shiny::observe({
      ra    <- ra_selecionada()
      nomes <- tryCatch(ra_mapa()$Regiao_adm, error = function(e) character(0))
      alvo  <- if (!is.null(ra) && length(ra) == 1 && ra %in% nomes) ra else NULL
      session$sendCustomMessage("dengue_df_map_highlight", list(
        id   = session$ns("mapa"),
        name = alvo
      ))
    })

    # -- pedido de foco para o filtro global --------------------------------
    # Clique numa RA -> nome da RA; botao "Visao geral" -> "Todas". O .ts
    # garante que cliques repetidos sempre notifiquem o observeEvent do
    # server.R.
    pedido_foco <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$clique, {
      shiny::req(nzchar(input$clique))
      pedido_foco(list(ra = input$clique, .ts = as.numeric(Sys.time())))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$limpar, {
      pedido_foco(list(ra = "Todas", .ts = as.numeric(Sys.time())))
    }, ignoreInit = TRUE)

    pedido_foco
  })
}
