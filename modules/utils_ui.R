# ==============================================================================
# Dengue DF | modules/utils_ui.R
# Tema e helpers de composicao de cards - portado de R/utils_ui.R do
# dengue_ride. Toda decisao de apresentacao mora aqui e em config/ui.R; os
# modulos so expoem controles e saidas.
# ==============================================================================

TEMA_DENGUE_DF <- bslib::bs_theme(
  version = 5,
  preset  = "zephyr",

  bg = "#FAF6F1",
  fg = "#3B2A1D",

  primary   = PALETA_CHOCOLATE[["chocolate"]],
  secondary = PALETA_CHOCOLATE[["taupe"]],
  success   = PALETA_CHOCOLATE[["cacau"]],
  warning   = PALETA_CHOCOLATE[["ambar"]],
  danger    = PALETA_CHOCOLATE[["amargo"]],
  info      = PALETA_CHOCOLATE[["caramelo"]],

  base_font = bslib::font_collection(
    bslib::font_google("Inter", local = FALSE),
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
  ),

  "border-radius"      = "0.65rem",
  "border-radius-sm"   = "0.45rem",
  "card-border-radius" = "0.65rem",
  "card-cap-padding-y" = "0.6rem",
  "card-border-color"  = "rgba(74,42,16,.12)",
  "card-bg"            = "#FFFFFF",
  "enable-shadows"     = TRUE
)

# --------------------------------------------------------------- helpers ----

#' Gatilho de popover para os controles de um card.
gatilho_opcoes <- function(rotulo = "Opções") {
  shiny::tags$span(
    class = "badge rounded-pill bg-light text-primary fw-normal",
    style = "cursor: pointer;",
    rotulo
  )
}

#' Card padrao de grafico.
#'
#' @param titulo    texto do cabecalho.
#' @param saida     output do grafico (mod_*_saida()).
#' @param controles UI de inputs; vai para um popover no cabecalho. NULL omite.
#' @param tom       classe de fundo do cabecalho.
cartao_grafico <- function(titulo, saida, controles = NULL, tom = "bg-primary") {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = paste(tom, "text-white d-flex justify-content-between align-items-center"),
      shiny::tags$span(class = "fw-semibold", titulo),
      if (!is.null(controles)) {
        bslib::popover(
          gatilho_opcoes(),
          controles,
          title     = titulo,
          placement = "left"
        )
      }
    ),
    bslib::card_body(class = "p-0", saida)
  )
}
