# ==============================================================================
# Dengue DF | modules/utils_ui.R
# Tema e helpers de composicao de cards - portado de R/utils_ui.R do
# dengue_ride. Toda decisao de apresentacao mora aqui e em config/ui.R; os
# modulos so expoem controles e saidas.
# ==============================================================================

TEMA_DENGUE_DF <- bslib::bs_theme(
  version = 5,
  preset  = "zephyr",

  bg = PALETA_DENGUE[["fundo"]],
  fg = PALETA_DENGUE[["texto"]],

  # Interface em azul Cenarios; a terracota fica para os dados de dengue.
  primary   = PALETA_DENGUE[["azul_acao"]],
  secondary = PALETA_DENGUE[["sub"]],
  success   = PALETA_DENGUE[["ok"]],
  warning   = PALETA_DENGUE[["atencao"]],
  danger    = PALETA_DENGUE[["critico"]],
  info      = PALETA_DENGUE[["azul"]],

  base_font = bslib::font_collection(
    bslib::font_google("DM Sans", wght = c(400, 500, 700), local = FALSE),
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
  ),
  heading_font = bslib::font_collection(
    bslib::font_google("Poppins", wght = c(600, 700), local = FALSE),
    "system-ui", "sans-serif"
  ),

  "headings-color"     = PALETA_DENGUE[["azul_titulo"]],
  "body-color"         = PALETA_DENGUE[["texto"]],
  "link-color"         = PALETA_DENGUE[["azul_acao"]],
  "border-radius"      = "0.65rem",
  "border-radius-sm"   = "0.45rem",
  "card-border-radius" = "0.65rem",
  "card-cap-padding-y" = "0.6rem",
  "card-border-color"  = PALETA_DENGUE[["linha"]],
  "card-bg"            = PALETA_DENGUE[["superficie"]],
  "enable-shadows"     = TRUE
) |>
  bslib::bs_add_rules(sprintf("
    .card-header.bg-primary { background-color: %s !important; }
    .card-header, .card-header .fw-semibold, .navbar-brand {
      font-family: Poppins, system-ui, sans-serif;
    }
    .value-box-value, .bslib-value-box .value-box-value, table td {
      font-variant-numeric: tabular-nums;
    }
    .value-box-value { font-weight: 700; }
  ", PALETA_DENGUE[["azul_titulo"]]))

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
