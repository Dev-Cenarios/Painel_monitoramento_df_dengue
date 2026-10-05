# =============================================================================
# mod_canal_endemico.R — Dengue DF (equivalente ao mod_diagrama_controle.R do
# dengue_ride — canal endêmico por quartis históricos + coeficiente observado).
#
# A temporada (ano) observada tem SELETOR PRÓPRIO (mod_canal_endemico_
# controles), independente do filtro global de Ano/Semana — mesmo padrão do
# mod_diagrama_controle_controles do dengue_ride (seletor de temporada no
# popover do card). Antes, este módulo usava o `ano_atual` derivado do
# filtro global, então só dava pra ver a temporada corrente: pra comparar
# 2025 era preciso trocar o filtro "Ano" geral, o que também truncava todo
# o resto do painel (KPIs, mapa etc.) para aquele ano. Por isso o módulo lê
# SEMANAL_COMPLETO (global.R, não filtrado por ano/semana) direto, e só
# respeita o filtro de RA do recorte global.
# =============================================================================

mod_canal_endemico_controles <- function(id) {
  ns <- NS(id)
  # Temporadas no formato "Y/Y+1" (semanas 27-52 de Y + semanas 1-26 de Y+1),
  # espelhando a janela do eixo `semanadiagrama` (que começa na SE27). Cada
  # temporada exige Y+1 em ANOS_DISPONIVEIS (garante ao menos o 1º semestre).
  temporadas <- paste0(seq(ANO_MIN - 1, ANO_MAX - 1), "/", seq(ANO_MIN, ANO_MAX))
  selectInput(
    ns("temporada"), "Temporada (safra observada)",
    choices  = rev(temporadas),
    selected = paste0(ANO_MAX - 1, "/", ANO_MAX)
  )
}

mod_canal_endemico_saida <- function(id, height = "420px") {
  echarts4rOutput(NS(id)("chart"), height = height)
}

# `filtro_ra` é a reactive compartilhada do filtro de RA (config/server.R).
# `canal_endemico` (CANAL_ENDEMICO, global.R) é pré-computado pelo ETL — ver
# nota em global.R sobre por que isso diverge do dengue_ride (que deriva os
# quartis ao vivo).
mod_canal_endemico_server <- function(id, filtro_ra) {
  moduleServer(id, function(input, output, session) {

    dados_grafico <- reactive({
      req(input$temporada)
      partes  <- as.integer(strsplit(input$temporada, "/")[[1]])
      ano_ini <- partes[1]  # fornece as semanas 27-52 (1ª metade do diagrama)
      ano_fim <- partes[2]  # fornece as semanas 1-26  (2ª metade do diagrama)
      ra_sel  <- if (filtro_ra() == "Todas") "DISTRITO FEDERAL" else filtro_ra()

      pop <- if (ra_sel == "DISTRITO FEDERAL") POP_TOTAL_DF
      else populacao_ra$populacao[populacao_ra$ra == ra_sel]

      # "DISTRITO FEDERAL" não existe como Regiao_adm no agregado semanal real
      # (só existe em CANAL_ENDEMICO, que já vem agregado do ETL histórico).
      # Então, pra pegar o observado da temporada, quando for DF a gente
      # soma todas as RAs em vez de filtrar por um nome que não existe.
      base_atual <- SEMANAL_COMPLETO
      if (ra_sel != "DISTRITO FEDERAL") {
        base_atual <- base_atual |> filter(Regiao_adm == ra_sel)
      }
      # Temporada cruza dois anos-calendário: cada SEM_NUM (1-52/53) só entra
      # uma vez, então o left_join abaixo não duplica linhas em CANAL_ENDEMICO.
      base_atual <- base_atual |> filter(
        (NU_ANO == ano_ini & SEM_NUM >= 27) |
        (NU_ANO == ano_fim & SEM_NUM <= 26)
      )

      atual <- base_atual |>
        group_by(SEM_NUM) |>
        summarise(casos = sum(casos), .groups = "drop") |>
        mutate(p_inc100k = (casos / pop) * 100000) |>
        select(SEM_NUM, p_inc100k)

      CANAL_ENDEMICO |>
        filter(Regiao_adm == ra_sel) |>
        left_join(atual, by = "SEM_NUM") |>
        arrange(semanadiagrama) |>
        mutate(
          # --- Réplica da lógica de ribbon do script de referência ---
          faixa_inferior = mediana - qlow,
          faixa_superior = qhigh   - mediana,
          semana_rotulo  = factor(as.character(SEM_NUM), levels = as.character(SEM_NUM))
        )
    })

    output$chart <- renderEcharts4r({
      cores      <- CORES_GRAFICO
      df         <- dados_grafico()
      temporada  <- input$temporada

      idx_corte <- which(df$SEM_NUM == 1)[1]

      df |>
        e_charts(semana_rotulo) |>

        # --- faixa: base invisível + duas camadas empilhadas ---
        e_line(qlow, name = "base", stack = "canal",
               lineStyle = list(opacity = 0), symbol = "none",
               legend = FALSE) |>
        e_line(faixa_inferior, name = "Limite inferior", stack = "canal",
               areaStyle = list(opacity = 0.9, color = "#f0f0f0"),
               lineStyle = list(opacity = 0), symbol = "none") |>
        e_line(faixa_superior, name = "Limite superior", stack = "canal",
               areaStyle = list(opacity = 0.9, color = "#d9d9d9"),
               lineStyle = list(opacity = 0), symbol = "none") |>

        # --- mediana histórica ---
        e_line(mediana, name = "Mediana",
               color = "#0c2c84", symbol = "none", lineStyle = list(width = 1)) |>

        # --- observado (temporada selecionada) ---
        e_line(p_inc100k, name = paste0("Coeficiente de Incidência (", temporada, ")"),
               color = "#feb24c", symbol = "none", lineStyle = list(width = 1.6)) |>

        e_mark_line(data = list(xAxis = idx_corte - 1),
                    title = "",
                    lineStyle = list(color = "gray", type = "solid")) |>

        e_x_axis(name = "Semana Epidemiológica",
                 axisLabel = list(color = cores$texto, interval = 6)) |>
        e_y_axis(name = "Coeficiente de Incidência\n(por 100.000 hab.)",
                 axisLabel = list(color = cores$texto)) |>
        e_tooltip(trigger = "axis") |>
        e_legend(bottom = 0, textStyle = list(color = cores$texto))
    })
  })
}
