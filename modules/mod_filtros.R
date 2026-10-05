# ==============================================================================
# Dengue DF | Modulo: filtros do recorte (ano, semana, RA, regiao de saude)
#
# Unica fonte de verdade do recorte - equivalente ao mod_geo_filter.R do
# dengue_ride, so que sem cascata hierarquica (o DF nao tem UF/municipio, so
# RAs no mesmo nivel). Os seletores existiam soltos no antigo config/ui.R e
# eram lidos direto de input$filtro_* no server.R; aqui viram um modulo com
# contrato de saida explicito, o que permite:
#   1. um resumo compacto no cabecalho do card (como mod_geo_filter_resumo);
#   2. receber foco externo (clique no mapa de incidencia) sem duplicar
#      estado - mesmo barramento de foco do dengue_ride.
#
# Contrato de saida (reactive): list(ano, semana, ra, regiao_saude, rotulo).
# ==============================================================================

mod_filtros_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1/4,
    gap   = "1rem",

    shiny::selectInput(
      ns("ano"), "Ano",
      choices = c("Todos", as.character(ANOS_DISPONIVEIS))
    ),
    shiny::selectInput(
      ns("semana"), "Semana Epi.",
      choices = c("Todas", as.character(1:52))
    ),
    shiny::selectInput(
      ns("ra"), "Região Administrativa",
      choices = c("Todas", RAS_DISPONIVEIS)
    ),
    shiny::selectInput(
      ns("regiao_saude"), "Região de Saúde",
      choices = c("Todas", REGIOES_SAUDE_DISPONIVEIS)
    )
  )
}

#' Resumo compacto do recorte, para o cabecalho do card de filtros.
mod_filtros_resumo <- function(id) {
  shiny::uiOutput(shiny::NS(id)("resumo"), inline = TRUE)
}

#' @param foco_externo reactive que devolve NULL ou o nome de uma RA. Permite
#'   que outros componentes (o mapa de incidencia, por clique) reposicionem o
#'   filtro sem duplicar estado.
mod_filtros_server <- function(id, foco_externo = shiny::reactive(NULL)) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(foco_externo(), {
      ra <- foco_externo()
      shiny::req(!is.null(ra), ra %in% RAS_DISPONIVEIS)
      shiny::updateSelectInput(session, "ra", selected = ra)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    filtros <- shiny::reactive({
      shiny::req(input$ano, input$semana, input$ra, input$regiao_saude)
      list(
        ano          = input$ano,
        semana       = input$semana,
        ra           = input$ra,
        regiao_saude = input$regiao_saude,
        rotulo       = if (identical(input$ra, "Todas")) "Distrito Federal" else input$ra
      )
    })

    output$resumo <- shiny::renderUI({
      f <- filtros()
      partes <- c(
        f$rotulo,
        if (!identical(f$ano, "Todos"))          paste("ano", f$ano),
        if (!identical(f$semana, "Todas"))       paste("SE", f$semana),
        if (!identical(f$regiao_saude, "Todas")) f$regiao_saude
      )
      shiny::tags$span(
        class = "small",
        shiny::tags$strong(partes[1]),
        if (length(partes) > 1) {
          shiny::tags$span(class = "opacity-75", paste0(" · ", paste(partes[-1], collapse = " · ")))
        }
      )
    })

    filtros
  })
}
