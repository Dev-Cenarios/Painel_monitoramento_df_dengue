# =============================================================================
# mod_distribuicao_temporal.R — Dengue DF
# Consome duas reactives compartilhadas do config/server.R (mesma fonte usada
# pelos KPIs, pelo mapa de tendência e pelo canal endêmico):
#   semanal_filtrado — agregado semanal (NU_ANO, SEM_NUM, casos)
#   mensal_filtrado  — agregado mensal  (NU_ANO, MES, casos)
# =============================================================================

mod_linha_temporal_saida <- function(id, height = "420px") {
  echarts4rOutput(NS(id)("linha_temporal"), height = height)
}

mod_heatmap_temporal_saida <- function(id, height = "420px") {
  echarts4rOutput(NS(id)("heatmap"), height = height)
}

mod_distribuicao_temporal_server <- function(id, semanal_filtrado, mensal_filtrado) {
  moduleServer(id, function(input, output, session) {

    # ── Linha: série contínua semanal ─────────────────────────────────────────
    output$linha_temporal <- renderEcharts4r({
      tc <- CORES_GRAFICO

      df <- semanal_filtrado() |>
        filter(!is.na(SEM_NUM), !is.na(NU_ANO), SEM_NUM >= 1, SEM_NUM <= 52) |>
        group_by(NU_ANO, SEM_NUM) |>
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
        arrange(NU_ANO, SEM_NUM) |>
        mutate(label = paste0(NU_ANO, " SE", str_pad(SEM_NUM, 2, pad = "0")))

      if (nrow(df) == 0) return(e_charts())

      mark_areas <- lapply(
        seq(1, length(unique(df$NU_ANO)), by = 2),
        function(i) {
          anos_unicos <- sort(unique(df$NU_ANO))
          if (i > length(anos_unicos)) return(NULL)
          ano_i       <- anos_unicos[i]
          label_start <- paste0(ano_i, " SE01")
          label_end   <- paste0(ano_i, " SE52")
          list(
            list(xAxis = label_start, itemStyle = list(color = "rgba(74,42,16,0.05)")),
            list(xAxis = label_end)
          )
        }
      )
      mark_areas <- Filter(Negate(is.null), mark_areas)

      opts <- list(
        backgroundColor = "transparent",
        title = list(
          text    = "Série Temporal Contínua de Casos",
          subtext = "Todos os anos em sequência | Faixas alternadas por ano",
          textStyle    = list(color = tc$texto, fontSize = 14, fontWeight = "bold"),
          subtextStyle = list(color = tc$sub,   fontSize = 10)
        ),
        tooltip = list(
          trigger = "axis",
          backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
          textStyle = list(color = tc$texto_tt),
          formatter = htmlwidgets::JS(
            "function(p){
               return '<b>' + p[0].axisValue + '</b><br/>Casos: <b>' +
                      Math.round(p[0].value).toLocaleString('pt-BR') + '</b>';
             }"
          )
        ),
        xAxis = list(
          type = "category",
          data = as.list(df$label),
          axisLabel = list(
            color    = tc$sub,
            fontSize = 8,
            rotate   = 45,
            interval = htmlwidgets::JS(
              "function(idx, val){ return val.includes('SE01'); }"
            ),
            formatter = htmlwidgets::JS(
              "function(val){ return val.includes('SE01') ? val.split(' ')[0] : ''; }"
            )
          ),
          splitLine = list(show = FALSE)
        ),
        yAxis = list(
          type = "value",
          name = "Casos",
          nameTextStyle = list(color = tc$sub, fontSize = 10),
          axisLabel = list(
            color     = tc$sub,
            formatter = htmlwidgets::JS("function(v){ return v.toLocaleString('pt-BR'); }")
          ),
          splitLine = list(lineStyle = list(color = tc$grid, type = "dashed"))
        ),
        series = list(list(
          type      = "line",
          data      = as.list(df$casos),
          smooth    = FALSE,
          symbol    = "none",
          lineStyle = list(
            color = htmlwidgets::JS(
              "new echarts.graphic.LinearGradient(0,0,1,0,[
                 {offset:0,   color:'#EBD5B8'},
                 {offset:0.4, color:'#CBA074'},
                 {offset:0.7, color:'#A9743F'},
                 {offset:1,   color:'#4A2A10'}
               ])"
            ),
            width = 1.8
          ),
          areaStyle = list(
            color = htmlwidgets::JS(
              "new echarts.graphic.LinearGradient(0,0,0,1,[
                 {offset:0, color:'rgba(74,42,16,0.22)'},
                 {offset:1, color:'rgba(74,42,16,0.01)'}
               ])"
            )
          ),
          markArea = list(silent = TRUE, data = mark_areas)
        )),
        dataZoom = list(
          list(type = "inside"),
          list(type = "slider", bottom = 5, height = 18,
               textStyle = list(color = tc$sub, fontSize = 9))
        ),
        grid = list(left = "10%", right = "3%", bottom = "18%", top = "16%")
      )

      e_charts() |> e_list(opts)
    })

    # ── Heatmap: MES × NU_ANO ─────────────────────────────────────────────────
    output$heatmap <- renderEcharts4r({
      tc       <- CORES_GRAFICO
      meses_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun",
                    "Jul","Ago","Set","Out","Nov","Dez")

      df_heat <- mensal_filtrado() |>
        filter(!is.na(NU_ANO), !is.na(MES)) |>
        group_by(NU_ANO, MES) |>
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
        mutate(
          mes_label = meses_pt[MES],
          ano_label = as.character(NU_ANO)
        )

      if (nrow(df_heat) == 0) return(e_charts())

      anos_str  <- as.list(as.character(sort(unique(df_heat$NU_ANO))))
      max_casos <- max(c(0, df_heat$casos), na.rm = TRUE)

      heat_data <- lapply(seq_len(nrow(df_heat)), function(i)
        list(df_heat$mes_label[i], df_heat$ano_label[i], df_heat$casos[i]))

      opts <- list(
        backgroundColor = "transparent",
        title = list(
          text = "Heatmap: Mês × Ano",
          textStyle = list(color = tc$texto, fontSize = 14, fontWeight = "bold")
        ),
        tooltip = list(
          formatter = htmlwidgets::JS(
            "function(p){ return p.value[1]+' · '+p.value[0]+'<br/>Casos: <b>'+p.value[2].toLocaleString('pt-BR')+'</b>'; }"
          ),
          backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
          textStyle = list(color = tc$texto_tt)
        ),
        xAxis = list(
          type = "category", data = as.list(meses_pt),
          axisLabel = list(color = tc$sub, fontSize = 10),
          splitArea = list(show = TRUE, areaStyle = list(
            color = list("rgba(74,42,16,0.02)", "rgba(74,42,16,0.06)")
          ))
        ),
        yAxis = list(
          type = "category", data = anos_str,
          axisLabel = list(color = tc$sub, fontSize = 10),
          splitArea = list(show = TRUE, areaStyle = list(
            color = list("rgba(74,42,16,0.02)", "rgba(74,42,16,0.06)")
          ))
        ),
        visualMap = list(
          min = 0, max = max_casos, show = TRUE, calculable = TRUE,
          orient = "horizontal", left = "center", bottom = 0,
          inRange    = list(color = RAMPA_CHOCOLATE),
          outOfRange = list(color = "#FFFFFF"),
          text       = list("Alto","Baixo"),
          textStyle  = list(color = tc$sub, fontSize = 10)
        ),
        series = list(list(
          type     = "heatmap",
          data     = heat_data,
          label    = list(show = FALSE),
          emphasis = list(itemStyle = list(shadowBlur = 10, shadowColor = "rgba(0,0,0,.5)"))
        )),
        grid = list(left = "8%", right = "3%", bottom = "20%", top = "14%")
      )

      e_charts() |> e_list(opts)
    })
  })
}
