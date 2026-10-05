# =============================================================================
# mod_curva_epidemia.R — Dengue DF
# Recebe agregado diário filtrado (reactive compartilhada diario_filtrado do
# config/server.R) — não casos individuais.
# Colunas esperadas: DT_SIN_PRI, NU_ANO, SEM_NUM, casos
# =============================================================================

mod_curva_epidemia_saida <- function(id, height = "420px") {
  echarts4rOutput(NS(id)("curva"), height = height)
}

mod_curva_epidemia_server <- function(id, diario_filtrado) {
  moduleServer(id, function(input, output, session) {

    output$curva <- renderEcharts4r({
      tc     <- CORES_GRAFICO
      df_raw <- diario_filtrado()

      if (nrow(df_raw) == 0 || !("DT_SIN_PRI" %in% names(df_raw)))
        return(e_charts())

      # ── 1. MA-5 sobre contagem diária já agregada ─────────────────────────
      df_daily <- df_raw |>
        group_by(DT_SIN_PRI) |>
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
        arrange(DT_SIN_PRI) |>
        mutate(
          casos_ma5 = as.numeric(
            zoo::rollmean(casos, k = 5, fill = NA, align = "center")
          ),
          casos_use = dplyr::coalesce(casos_ma5, as.numeric(casos))
        )

      # ── 2. Semana epi correta derivada de DT_SIN_PRI ─────────────────────
      df_daily <- df_daily |>
        mutate(
          se_iso   = lubridate::epiweek(DT_SIN_PRI),
          ano_iso  = lubridate::epiyear(DT_SIN_PRI),
          se_final = dplyr::case_when(
            se_iso == 53 & lubridate::month(DT_SIN_PRI) == 12 ~ 52L,
            se_iso == 53 & lubridate::month(DT_SIN_PRI) == 1  ~ 1L,
            TRUE ~ se_iso
          ),
          ano_final = ano_iso
        )

      # ── 3. Re-agrega por semana corrigida ─────────────────────────────────
      df_semana <- df_daily |>
        group_by(NU_ANO = ano_final, SEM_NUM = se_final) |>
        summarise(casos = round(sum(casos_use, na.rm = TRUE)), .groups = "drop") |>
        filter(SEM_NUM >= 1, SEM_NUM <= 52)

      # ── 4. Separa histórico do ano mais recente ───────────────────────────
      ano_recente <- ANO_MAX
      df_hist   <- df_semana |> filter(NU_ANO >= ANO_MIN, NU_ANO < ano_recente)
      df_atual  <- df_semana |> filter(NU_ANO == ano_recente)

      # ── 5. Histórico: total e mediana por semana ──────────────────────────
      df_agg <- df_hist |>
        group_by(SEM_NUM) |>
        summarise(
          casos_total = sum(casos, na.rm = TRUE),
          mediana     = median(casos, na.rm = TRUE),
          .groups     = "drop"
        )

      # ── 6. Reordena SE 37→52 → SE 01→38 (setembro a setembro) ────────────
      ordem_se  <- c(37:52, 1:38)
      se_labels <- paste0("SE ", str_pad(ordem_se, 2, pad = "0"))

      df_ord <- tibble(SEM_NUM = ordem_se) |>
        left_join(df_agg, by = "SEM_NUM")

      df_atual_ord <- tibble(SEM_NUM = ordem_se) |>
        left_join(df_atual |> select(SEM_NUM, casos), by = "SEM_NUM")

      max_val <- max(c(df_ord$casos_total, df_atual_ord$casos), na.rm = TRUE)
      if (is.infinite(max_val) || is.na(max_val)) max_val <- 1

      # ── 7. Gráfico ────────────────────────────────────────────────────────
      opts <- list(
        backgroundColor = "transparent",
        title = list(
          text    = "Casos por Semana Epidemiológica",
          subtext = sprintf(
            "Barras: total histórico %d–%d | Linha âmbar: mediana histórica | Linha escura: %d",
            ANO_MIN, ano_recente - 1L, ano_recente
          ),
          textStyle    = list(color = tc$texto, fontSize = 15, fontWeight = "bold"),
          subtextStyle = list(color = tc$sub,   fontSize = 11)
        ),
        legend = list(
          data      = list(sprintf("Histórico (%d–%d)", ANO_MIN, ano_recente - 1L),
                           "Mediana histórica", as.character(ano_recente)),
          textStyle = list(color = tc$texto, fontSize = 11),
          top       = 5
        ),
        tooltip = list(
          trigger     = "axis",
          axisPointer = list(type = "shadow"),
          backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
          textStyle   = list(color = tc$texto_tt),
          formatter   = htmlwidgets::JS(
            "function(params){
               var s = params[0].axisValue + '<br/>';
               params.forEach(function(p){
                 if(p.value != null && p.value !== undefined){
                   s += p.marker + p.seriesName + ': <b>' +
                        Math.round(p.value).toLocaleString('pt-BR') + '</b><br/>';
                 }
               });
               return s;
             }"
          )
        ),
        xAxis = list(
          type = "category",
          data = as.list(se_labels),
          axisLabel = list(color = tc$sub, fontSize = 9, rotate = 45, interval = 3),
          splitLine  = list(show = FALSE)
        ),
        yAxis = list(
          type = "value",
          name = "Casos",
          nameTextStyle = list(color = tc$sub, fontSize = 11),
          axisLabel = list(
            color     = tc$sub,
            formatter = htmlwidgets::JS("function(v){ return v.toLocaleString('pt-BR'); }")
          ),
          splitLine = list(lineStyle = list(color = tc$grid, type = "dashed"))
        ),
        series = list(
          list(
            name = sprintf("Histórico (%d–%d)", ANO_MIN, ano_recente - 1L),
            type = "bar",
            data = as.list(df_ord$casos_total),
            itemStyle = list(
              color = htmlwidgets::JS(paste0(
                "function(p){ var v=p.value, m=", max_val, ";",
                "if(!v) return 'rgba(203,160,116,0.35)';",
                "if(v > m*.8) return 'rgba(74,42,16,0.85)';",
                "if(v > m*.5) return 'rgba(122,74,34,0.8)';",
                "return 'rgba(169,116,63,0.7)'; }"
              )),
              borderRadius = list(3, 3, 0, 0)
            )
          ),
          list(
            name      = "Mediana histórica",
            type      = "line",
            data      = as.list(df_ord$mediana),
            smooth    = TRUE,
            symbol    = "none",
            lineStyle = list(color = PALETA_CHOCOLATE[["ambar"]], width = 2.5, type = "solid"),
            itemStyle = list(color = PALETA_CHOCOLATE[["ambar"]])
          ),
          list(
            name       = as.character(ano_recente),
            type       = "line",
            data       = as.list(df_atual_ord$casos),
            smooth     = TRUE,
            symbol     = "circle",
            symbolSize = 5,
            lineStyle  = list(color = PALETA_CHOCOLATE[["amargo"]], width = 2.5, type = "dashed"),
            itemStyle  = list(color = PALETA_CHOCOLATE[["amargo"]])
          )
        ),
        grid = list(left = "12%", right = "5%", bottom = "22%", top = "20%")
      )

      e_charts() |> e_list(opts)
    })
  })
}
