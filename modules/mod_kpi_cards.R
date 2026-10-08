# ==============================================================================
# Dengue DF | Modulo: painel de KPIs
#
# Seis bslib::value_box no topo do painel (duas linhas de tres) - portado de
# R/mod_kpi.R do dengue_ride: a cor do box carrega informacao (escurece
# conforme a situacao se agrava, rampa TONS_DENGUE), entao os boxes
# sao renderizados via uiOutput em vez de fixados na UI.
#
# Nao recalcula agregacoes que ja existem: consome os reactives compartilhados
# do server.R (casos_filtrados, ra_agregado, semanal_filtrado) - antes, este
# modulo e mod_mapa_incidencia.R recalculavam o mesmo count(Regiao_adm) cada
# um por conta propria.
# ==============================================================================

mod_kpi_cards_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("boxes"))
}

mod_kpi_cards_server <- function(id, casos_filtrados, ra_agregado, semanal_filtrado) {

  shiny::moduleServer(id, function(input, output, session) {

    indicadores <- shiny::reactive({
      casos  <- casos_filtrados()
      semana <- semanal_filtrado()
      ra     <- ra_agregado()

      shiny::req(nrow(semana) > 0)

      ano_max <- max(semana$NU_ANO, na.rm = TRUE)
      se_max  <- max(semana$SEM_NUM[semana$NU_ANO == ano_max], na.rm = TRUE)
      casos_ultima_semana <- sum(
        semana$casos[semana$NU_ANO == ano_max & semana$SEM_NUM == se_max],
        na.rm = TRUE
      )

      # -- Rt medio: janela de 4 SEs vs. as 4 anteriores, por RA, depois a
      # media entre RAs (Rt nao e aditivo - mesma logica do dengue_ride ao
      # agregar acima do nivel municipal).
      atual_ini <- max(se_max - 3, 1)
      ant_fim   <- max(atual_ini - 1, 1)
      ant_ini   <- max(ant_fim - 3, 1)

      jan_atual <- semana |>
        dplyr::filter(NU_ANO == ano_max, SEM_NUM >= atual_ini, SEM_NUM <= se_max) |>
        dplyr::group_by(Regiao_adm) |>
        dplyr::summarise(n_atual = sum(casos, na.rm = TRUE), .groups = "drop")

      jan_ant <- semana |>
        dplyr::filter(NU_ANO == ano_max, SEM_NUM >= ant_ini, SEM_NUM <= ant_fim) |>
        dplyr::group_by(Regiao_adm) |>
        dplyr::summarise(n_ant = sum(casos, na.rm = TRUE), .groups = "drop")

      rt_por_ra <- jan_atual |>
        dplyr::left_join(jan_ant, by = "Regiao_adm") |>
        dplyr::mutate(
          n_ant = tidyr::replace_na(n_ant, 0),
          rt    = dplyr::if_else(n_ant > 0, n_atual / n_ant, NA_real_)
        ) |>
        dplyr::filter(!is.na(rt))

      rt_medio <- if (nrow(rt_por_ra) > 0) mean(rt_por_ra$rt, na.rm = TRUE) else NA_real_

      list(
        total_casos    = sum(semana$casos, na.rm = TRUE),
        inc_media      = mean(ra$incidencia, na.rm = TRUE),
        n_ra           = nrow(ra),
        ano_max        = ano_max,
        se_max         = se_max,
        casos_ultima   = casos_ultima_semana,
        rt_medio       = rt_medio,
        obitos         = sum(casos$EVOLUCAO == "Óbito pelo agravo", na.rm = TRUE),
        obitos_invest  = sum(casos$EVOLUCAO == "Óbito em investigação", na.rm = TRUE)
      )
    })

    tom <- function(bg, claro = FALSE) {
      bslib::value_box_theme(
        bg = bg,
        fg = if (claro) PALETA_DENGUE[["tinta"]] else "#FFFFFF"
      )
    }

    # Incidencia: rampa da dengue (mais escuro = mais casos), cortes do MS.
    tema_incidencia <- function(inc) {
      if (is.na(inc)) return(tom(PALETA_DENGUE[["linha"]], claro = TRUE))
      if (inc >= 300) return(tom(TONS_DENGUE[["800"]]))
      if (inc >= 100) return(tom(TONS_DENGUE[["600"]]))
      tom(TONS_DENGUE[["100"]], claro = TRUE)
    }

    # Rt: azul abaixo do limiar, terracota acima (mesma logica do mapa de
    # tendencia).
    tema_rt <- function(rt) {
      if (is.na(rt)) return(tom(PALETA_DENGUE[["linha"]], claro = TRUE))
      if (rt >= 1.25) return(tom(TONS_DENGUE[["800"]]))
      if (rt >= 1)    return(tom(TONS_DENGUE[["600"]]))
      tom(TONS_AZUL[["100"]], claro = TRUE)
    }

    tema_obitos <- function(n) {
      if (n > 0) tom(PALETA_DENGUE[["critico"]]) else tom(PALETA_DENGUE[["linha"]], claro = TRUE)
    }

    output$boxes <- shiny::renderUI({
      k <- indicadores()

      shiny::tagList(
        bslib::layout_columns(
          fill  = FALSE,
          class = "mb-1",

          bslib::value_box(
            title = "Total de casos",
            value = fmt_int(k$total_casos),
            theme = tom(PALETA_DENGUE[["dengue_forte"]]),
            shiny::tags$span(class = "small", "no recorte atual")
          ),
          bslib::value_box(
            title = "Incidência média",
            value = fmt_num(k$inc_media, 1),
            theme = tema_incidencia(k$inc_media),
            shiny::tags$span(class = "small",
              sprintf("por 100 mil · média entre %s RA(s)", fmt_int(k$n_ra)))
          ),
          bslib::value_box(
            title = sprintf("SE %s (%d)", k$se_max, k$ano_max),
            value = fmt_int(k$casos_ultima),
            theme = tom(PALETA_DENGUE[["azul_titulo"]]),
            shiny::tags$span(class = "small", "casos na semana mais recente")
          )
        ),
        bslib::layout_columns(
          fill  = FALSE,
          class = "mb-1",

          bslib::value_box(
            title = "Rt médio (DF)",
            value = if (is.na(k$rt_medio)) "—" else fmt_num(k$rt_medio, 2),
            theme = tema_rt(k$rt_medio),
            shiny::tags$span(class = "small",
              if (is.na(k$rt_medio)) "sem dados suficientes"
              else if (k$rt_medio >= 1) "acima do limiar" else "abaixo do limiar")
          ),
          bslib::value_box(
            title = "Óbitos confirmados",
            value = fmt_int(k$obitos),
            theme = tema_obitos(k$obitos),
            shiny::tags$span(class = "small", "óbitos pelo agravo")
          ),
          bslib::value_box(
            title = "Óbitos em investigação",
            value = fmt_int(k$obitos_invest),
            theme = tema_obitos(k$obitos_invest),
            shiny::tags$span(class = "small", "casos em apuração")
          )
        )
      )
    })
  })
}
