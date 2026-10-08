# ==============================================================================
# Dengue DF | ui.R
#
# Layout portado do dengue_ride (config/ui.R la, mesma estrutura de secoes e
# proporcoes de coluna) — duas abas no navbar:
#   "Painel"    - filtros, KPIs, mapa + evolucao temporal, mapa de tendencia,
#                 canal endemico + sazonalidade, perfil dos casos, dados brutos.
#   "Análises"  - hotspots por setor censitario (modules/mod_analises_
#                 hotspot.R), escopo GLOBAL (nao usa os filtros da aba Painel).
#
# TEMA_DENGUE_DF e cartao_grafico() vivem em modules/utils_ui.R.
# ==============================================================================

ui <- bslib::page_navbar(
  title        = "Dengue DF",
  theme        = TEMA_DENGUE_DF,
  window_title = "Dengue DF — Painel de Monitoramento Epidemiológico",
  padding      = "1rem",
  fillable     = FALSE,

  # --------------------------------------------------------------- Painel --
  bslib::nav_panel(
    "Painel",

    shiny::tagList(
      # z-index do dropdown dos seletores + popover que fecha ao clicar fora
      # (modules/utils_ui.R), portado do dengue_ride.
      dengue_df_ui_assets(),

      # ----------------------------------------------------------- cabecalho --
      shiny::tags$div(
        class = "d-flex justify-content-between align-items-end mb-3",
        shiny::tags$div(
          shiny::tags$h4(class = "mb-0 fw-semibold", "Dengue DF"),
          shiny::tags$span(class = "text-muted small",
                           "Painel de monitoramento epidemiológico · fonte SINAN")
        ),
        shiny::tags$span(
          class = "text-muted small",
          sprintf("%s RAs · %d–%d", fmt_int(length(RAS_DISPONIVEIS)), ANO_MIN, ANO_MAX)
        )
      ),

      # ---------------------------------------------------- 1. filtros no topo --
      bslib::card(
        class = "mb-3",
        bslib::card_header(
          class = "bg-primary text-white d-flex justify-content-between align-items-center",
          shiny::tags$span(class = "fw-semibold", "Recorte"),
          shiny::tags$div(
            class = "d-flex align-items-center gap-3",
            mod_filtros_resumo("filtros"),
            # Volta a visao geral do DF (RA e Regiao de Saude = Todas),
            # mesmo efeito do botao sobreposto ao mapa. Ver server.R.
            shiny::actionButton(
              "limpar_selecao", "Visão geral",
              class = "btn btn-sm btn-light"
            )
          )
        ),
        bslib::card_body(
          mod_filtros_ui("filtros")
        )
      ),

      # ------------------------------------------------------------- 2. KPIs --
      mod_kpi_cards_ui("kpis"),

      # ----------------------------------- 3. mapa | evolucao temporal (7/5) --
      bslib::layout_columns(
        col_widths = c(7, 5),
        row_heights = "auto",

        cartao_grafico(
          titulo = "Mapa de incidência por RA",
          saida  = mod_mapa_incidencia_saida("mapa_inc")
        ),

        bslib::navset_card_tab(
          full_screen = TRUE,
          title = shiny::tags$span(class = "fw-semibold", "Evolução temporal"),

          bslib::nav_panel(
            "Série",
            bslib::card_body(class = "p-0", mod_linha_temporal_saida("temporal"))
          ),
          bslib::nav_panel(
            "Curva epidêmica",
            bslib::card_body(class = "p-0", mod_curva_epidemia_saida("curva_epi"))
          ),
          bslib::nav_panel(
            "Rt",
            bslib::card_body(class = "p-0", mod_rt_temporal_saida("rt_temporal"))
          )
        )
      ),

      # ------------------------------- 4. mapa de tendencia (largura total) --
      cartao_grafico(
        titulo = "Rt × Razão Histórica por RA",
        saida     = mod_mapa_rt_saida("mapa_rt"),
        controles = mod_mapa_rt_controles("mapa_rt")
      ),

      # -------------------------------- 5. canal endemico | heatmap (6/6) ----
      bslib::layout_columns(
        col_widths = c(6, 6),

        cartao_grafico(
          titulo    = "Canal endêmico",
          saida     = mod_canal_endemico_saida("canal_endemico"),
          controles = mod_canal_endemico_controles("canal_endemico")
        ),

        cartao_grafico(
          titulo = "Sazonalidade — mês × ano",
          saida  = mod_heatmap_temporal_saida("temporal")
        )
      ),

      # ---------------------------------------------------- 6. perfil dos casos --
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "bg-primary text-white fw-semibold", "Perfil dos casos"),
        bslib::card_body(mod_perfil_casos_ui("perfil"))
      ),

      # ------------------------------------------------ 7. dados brutos / SQL --
      bslib::navset_card_tab(
        title = shiny::tags$span(class = "fw-semibold",
                                 shiny::textOutput("titulo_recorte", inline = TRUE)),

        bslib::nav_panel(
          "Últimas semanas",
          bslib::card_body(
            shiny::tableOutput("tab_serie"),
            shiny::tags$p(
              class = "small text-muted mb-0",
              "Agregação feita no DuckDB; apenas as linhas exibidas trafegam para o R."
            )
          )
        ),

        bslib::nav_panel(
          "SQL gerado",
          bslib::card_body(
            shiny::tags$p(
              class = "small text-muted",
              "Consulta que o dbplyr envia ao DuckDB para o recorte atual (agregado semanal)."
            ),
            shiny::verbatimTextOutput("sql_gerado")
          )
        )
      )
    )
  ),

  # ------------------------------------------------------------- Análises --
  bslib::nav_panel(
    "Análises",
    mod_analises_hotspot_ui("hotspots")
  )
)
