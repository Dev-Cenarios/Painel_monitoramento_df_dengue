# ==============================================================================
# Dengue DF | Modulo: Rt (razão de crescimento) ao longo do tempo
#
# O DF nao tem uma coluna de Rt real (numero de reproducao efetivo, como no
# dengue_ride, que le `rt` direto da estimativa do InfoDengue) - o SINAN
# individual nao chega com esse modelo embutido. Este grafico usa a MESMA
# aproximacao ja empregada no KPI "Rt Médio (DF)" (modules/mod_kpi_cards.R)
# e no mapa modules/mod_mapa_rt.R: razao entre a soma de casos de uma janela
# de 4 semanas e a soma da janela de 4 semanas imediatamente anterior. Um
# valor > 1 indica crescimento; < 1, retracao. Nao e o Rt epidemiologico de
# verdade (por isso o titulo do card e explicito: "aprox.") - e uma
# generalizacao pra serie temporal do MESMO calculo que ja existia,
# so que repetido em CADA semana da serie em vez de so na mais recente.
#
# Respeita o recorte global (ano/semana/RA/regiao de saude), igual as outras
# abas de "Evolução temporal" (Série, Curva epidêmica) - ao contrario de
# modules/mod_mapa_rt.R, que ignora o recorte de proposito pra comparar
# todas as RAs entre si.
# ==============================================================================

mod_rt_temporal_saida <- function(id, height = "420px") {
  echarts4rOutput(NS(id)("rt"), height = height)
}

mod_rt_temporal_server <- function(id, semanal_filtrado) {
  moduleServer(id, function(input, output, session) {

    output$rt <- renderEcharts4r({
      tc <- CORES_GRAFICO

      serie <- semanal_filtrado() |>
        filter(!is.na(SEM_NUM), !is.na(NU_ANO), SEM_NUM >= 1, SEM_NUM <= 52) |>
        group_by(NU_ANO, SEM_NUM) |>
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
        arrange(NU_ANO, SEM_NUM)

      if (nrow(serie) < 9) return(e_charts())  # precisa de pelo menos 2 janelas + 1

      serie <- serie |>
        mutate(
          janela_atual    = zoo::rollsum(casos, k = 4, fill = NA, align = "right"),
          janela_anterior = dplyr::lag(janela_atual, 4),
          rt   = dplyr::if_else(janela_anterior > 0, janela_atual / janela_anterior, NA_real_),
          label = paste0(NU_ANO, " SE", str_pad(SEM_NUM, 2, pad = "0"))
        )

      opts <- list(
        backgroundColor = "transparent",
        title = list(
          text    = "Rt (aprox.) — razão de crescimento semanal",
          subtext = "Soma de casos de 4 semanas ÷ soma das 4 semanas anteriores. Acima de 1 = crescimento.",
          textStyle    = list(color = tc$texto, fontSize = 14, fontWeight = "bold"),
          subtextStyle = list(color = tc$sub,   fontSize = 10)
        ),
        tooltip = list(
          trigger = "axis",
          backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
          textStyle = list(color = tc$texto_tt),
          formatter = htmlwidgets::JS(
            "function(p){
               if (p[0].value === null || p[0].value === undefined) {
                 return '<b>' + p[0].axisValue + '</b><br/>sem base suficiente';
               }
               return '<b>' + p[0].axisValue + '</b><br/>Rt (aprox.): <b>' +
                      Number(p[0].value).toFixed(2) + '</b>';
             }"
          )
        ),
        xAxis = list(
          type = "category",
          data = as.list(serie$label),
          axisLabel = list(
            color    = tc$sub,
            fontSize = 8,
            rotate   = 45,
            interval = htmlwidgets::JS("function(idx, val){ return val.includes('SE01'); }"),
            formatter = htmlwidgets::JS("function(val){ return val.includes('SE01') ? val.split(' ')[0] : ''; }")
          ),
          splitLine = list(show = FALSE)
        ),
        yAxis = list(
          type = "value",
          name = "Rt (aprox.)",
          nameTextStyle = list(color = tc$sub, fontSize = 10),
          axisLabel = list(color = tc$sub, formatter = htmlwidgets::JS("function(v){ return v.toFixed(1); }")),
          splitLine = list(lineStyle = list(color = tc$grid, type = "dashed"))
        ),
        series = list(list(
          type      = "line",
          data      = as.list(serie$rt),
          connectNulls = FALSE,
          symbol    = "none",
          lineStyle = list(color = PALETA_CHOCOLATE[["chocolate"]], width = 1.8),
          areaStyle = list(color = "rgba(122,74,34,0.12)"),
          markLine  = list(
            silent    = TRUE,
            symbol    = list("none", "none"),
            data      = list(list(
              yAxis     = 1,
              lineStyle = list(color = PALETA_CHOCOLATE[["amargo"]], type = "dashed", width = 1.3),
              label     = list(formatter = "limiar = 1", color = PALETA_CHOCOLATE[["amargo"]], fontSize = 10)
            ))
          )
        )),
        dataZoom = list(
          list(type = "inside"),
          list(type = "slider", bottom = 5, height = 18, textStyle = list(color = tc$sub, fontSize = 9))
        ),
        grid = list(left = "10%", right = "5%", bottom = "20%", top = "22%")
      )

      e_charts() |> e_list(opts)
    })
  })
}
