# =============================================================================
# mod_perfil_casos.R — Dengue DF
# Colunas SINAN: CS_SEXO, FAIXA_ETARIA (derivada de NU_IDADE_N no ETL)
# Consome casos_filtrados() — a mesma reactive de caso-nível usada pelos
# KPIs (óbitos) e por ra_agregado().
# =============================================================================

mod_perfil_casos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, echarts4rOutput(ns("sexo"),     height = "320px")),
      column(8, echarts4rOutput(ns("piramide"), height = "320px"))
    )
  )
}

mod_perfil_casos_server <- function(id, casos_filtrados) {
  moduleServer(id, function(input, output, session) {

    # Paleta de sexo compartilhada entre os dois gráficos (amarrada ao NOME,
    # não à posição). Deliberadamente fora da paleta chocolate: aqui a cor
    # distingue categorias (não indica severidade), e precisa de contraste
    # real entre si — mesma lógica das cores de quadrante em mod_mapa_rt.R.
    cores_sexo <- c(
      "Masculino" = "#2980B9",
      "Feminino"  = "#C9791F",
      "Ignorado"  = "#8A6A4F"
    )

    rotular_sexo <- function(x) {
      dplyr::recode(toupper(trimws(x)),
                    "M" = "Masculino",
                    "F" = "Feminino",
                    "I" = "Ignorado",
                    .default = "Ignorado"
      )
    }

    mapear_faixa_larga <- function(x) {
      dplyr::case_when(
        x %in% c("menos_1_ano", "1_4_anos", "5_9_anos", "10_14_anos")        ~ "0-14",
        x %in% c("15_19_anos", "20_29_anos")                                 ~ "15-29",
        x %in% c("30_39_anos", "40_49_anos")                                 ~ "30-44",
        x %in% c("50_59_anos", "60_64_anos")                                 ~ "45-59",
        x %in% c("65_69_anos", "70_79_anos", "80_89_anos", "90_anos_e_mais") ~ "60+",
        TRUE                                                                  ~ NA_character_
      )
    }

    ordem_faixas <- c("0-14", "15-29", "30-44", "45-59", "60+")

    # ── 1. Distribuição por Sexo (rosca) ──────────────────────────────────────
    output$sexo <- renderEcharts4r({
      tc <- CORES_GRAFICO

      df_sexo <- casos_filtrados() %>%
        filter(!is.na(CS_SEXO), CS_SEXO != "") %>%
        mutate(sexo_lbl = rotular_sexo(CS_SEXO)) %>%
        count(sexo_lbl, name = "n") %>%
        mutate(sexo_lbl = factor(sexo_lbl, levels = names(cores_sexo))) %>%
        arrange(sexo_lbl) %>%
        mutate(sexo_lbl = as.character(sexo_lbl))

      cores_ordenadas <- unname(cores_sexo[df_sexo$sexo_lbl])

      df_sexo %>%
        e_charts(sexo_lbl) %>%
        e_pie(n, radius = c("42%","70%"), center = c("50%","55%"), name = "Casos",
              itemStyle = list(borderRadius = 6, borderColor = tc$bg_tooltip, borderWidth = 2),
              label = list(show = TRUE,
                           formatter = htmlwidgets::JS("function(p){ return p.name+'\\n'+p.value.toLocaleString('pt-BR')+'\\n('+p.percent.toFixed(1)+'%)'; }"),
                           color = tc$texto, fontSize = 10)) %>%
        e_color(cores_ordenadas) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("function(p){ return p.name+': '+p.value.toLocaleString('pt-BR')+' ('+p.percent.toFixed(1)+'%)'; }"),
          backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
          textStyle = list(color = tc$texto_tt)) %>%
        e_legend(show = FALSE) %>%
        e_title(text = "Distribuição por Sexo",
                textStyle = list(color = tc$texto, fontSize = 13, fontWeight = "bold"),
                left = "center") %>%
        e_theme_custom('{"backgroundColor":"transparent"}')
    })

    # ── 2. Pirâmide Etária (Masculino × Feminino) ─────────────────────────────
    output$piramide <- renderEcharts4r({
      tc <- CORES_GRAFICO

      df_pir <- casos_filtrados() %>%
        filter(!is.na(FAIXA_ETARIA), FAIXA_ETARIA != "",
               !is.na(CS_SEXO), CS_SEXO != "") %>%
        mutate(
          faixa = mapear_faixa_larga(FAIXA_ETARIA),
          sexo  = rotular_sexo(CS_SEXO)
        ) %>%
        filter(!is.na(faixa), sexo %in% c("Masculino", "Feminino")) %>%
        count(faixa, sexo, name = "n")

      grade <- expand.grid(
        faixa = ordem_faixas,
        sexo  = c("Masculino", "Feminino"),
        stringsAsFactors = FALSE
      )

      df_pir <- grade %>%
        left_join(df_pir, by = c("faixa", "sexo")) %>%
        mutate(n = tidyr::replace_na(n, 0L))

      masc <- df_pir %>% filter(sexo == "Masculino") %>%
        arrange(match(faixa, ordem_faixas)) %>% pull(n)
      fem  <- df_pir %>% filter(sexo == "Feminino")  %>%
        arrange(match(faixa, ordem_faixas)) %>% pull(n)

      masc_neg <- as.list(-masc)
      fem_pos  <- as.list(fem)

      max_val <- max(c(masc, fem), na.rm = TRUE)
      if (is.infinite(max_val) || max_val == 0) max_val <- 1

      opts <- list(
        backgroundColor = "transparent",
        title = list(
          text = "Pirâmide Etária por Sexo",
          textStyle = list(color = tc$texto, fontSize = 13, fontWeight = "bold"),
          left = "center"
        ),
        legend = list(
          data = list("Masculino", "Feminino"),
          top = 28, textStyle = list(color = tc$texto, fontSize = 11)
        ),
        tooltip = list(
          trigger = "axis",
          axisPointer = list(type = "shadow"),
          backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
          textStyle = list(color = tc$texto_tt),
          formatter = htmlwidgets::JS(
            "function(params){
               var s = params[0].axisValue + '<br/>';
               params.forEach(function(p){
                 var v = Math.abs(p.value).toLocaleString('pt-BR');
                 s += p.marker + p.seriesName + ': <b>' + v + '</b><br/>';
               });
               return s;
             }"
          )
        ),
        grid = list(left = "3%", right = "4%", top = "22%", bottom = "8%", containLabel = TRUE),
        xAxis = list(
          type = "value",
          axisLabel = list(
            color = tc$sub, fontSize = 9,
            formatter = htmlwidgets::JS("function(v){ return Math.abs(v).toLocaleString('pt-BR'); }")
          ),
          splitLine = list(lineStyle = list(color = tc$grid, type = "dashed")),
          min = -max_val, max = max_val
        ),
        yAxis = list(
          type = "category",
          data = as.list(ordem_faixas),
          axisLabel = list(color = tc$sub, fontSize = 11),
          axisTick = list(show = FALSE),
          splitLine = list(show = FALSE)
        ),
        series = list(
          list(
            name = "Masculino",
            type = "bar",
            stack = "total",
            data = masc_neg,
            itemStyle = list(color = cores_sexo[["Masculino"]], borderRadius = list(3, 0, 0, 3)),
            label = list(
              show = TRUE, position = "left", color = tc$sub, fontSize = 9,
              formatter = htmlwidgets::JS("function(p){ return Math.abs(p.value).toLocaleString('pt-BR'); }")
            )
          ),
          list(
            name = "Feminino",
            type = "bar",
            stack = "total",
            data = fem_pos,
            itemStyle = list(color = cores_sexo[["Feminino"]], borderRadius = list(0, 3, 3, 0)),
            label = list(
              show = TRUE, position = "right", color = tc$sub, fontSize = 9,
              formatter = htmlwidgets::JS("function(p){ return p.value.toLocaleString('pt-BR'); }")
            )
          )
        )
      )

      e_charts() %>% e_list(opts)
    })
  })
}
