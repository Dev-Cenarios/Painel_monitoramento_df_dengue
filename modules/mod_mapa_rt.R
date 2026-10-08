# =============================================================================
# mod_mapa_rt.R — Dengue DF
# Colunas SINAN: Regiao_adm, SEM_NUM, NU_ANO (derivadas no ETL)
#
# Eixo X = casos da RA na SE de referência ÷ mediana da RA naquela SE
#          (todos os anos disponíveis).
# Eixo Y = casos da RA na SE de referência ÷ casos da SE anterior
#          (mesmo ano recente).
# Referência 1 = estável em ambos os eixos.
# Forma = limiares epidemiológicos fixos de incidência acumulada.
#
# NÃO recebe o recorte global (ano/semana/RA/região de saúde): sempre compara
# TODAS as RAs contra o próprio histórico, igual ao mod_mapa_rt.R do
# dengue_ride (mapa de tendência por município) — se filtrasse por ano, a
# mediana histórica perderia justamente os anos de comparação; se filtrasse
# por RA, o gráfico deixaria de comparar. Consome SEMANAL_COMPLETO
# (config/global.R), calculada uma única vez no start.
#
# DESTAQUE (entrada `ra_selecionada`): a RA escolhida (clique no mapa de
# incidência ou seletor do recorte) ganha cor escura, símbolo maior e rótulo
# com o nome completo; as demais ficam com opacidade reduzida - mesmo
# comportamento do dengue_ride. O cálculo (calcular_quadro_ra) é feito uma
# vez por processo e memorizado: clicar numa RA só redesenha os pontos.
#
# RAs "sem base" (janela anterior sem casos, crescimento indefinido) ficam
# ocultas por padrão - opção no popover do card para exibi-las.
# =============================================================================

FORMAS_RT <- list(
  list(id = "triangle", label = "△ < 10 /100k",   symbol = "triangle"),
  list(id = "diamond",  label = "◇ 10–50 /100k",  symbol = "diamond"),
  list(id = "circle",   label = "● 50–300 /100k", symbol = "circle"),
  list(id = "rect",     label = "■ > 300 /100k",  symbol = "rect")
)

mod_mapa_rt_controles <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(
      ns("mostrar_sem_base"),
      "Mostrar RAs sem base (janela anterior sem casos)",
      value = FALSE
    ),
    helpText(tags$small(
      "Sem casos na janela anterior o crescimento não pode ser calculado; ",
      "essas RAs ficam numa faixa própria, no topo do gráfico."
    ))
  )
}

mod_mapa_rt_saida <- function(id, height = "460px") {
  echarts4rOutput(NS(id)("mapa_rt"), height = height)
}

# Memo por processo: SEMANAL_COMPLETO não muda enquanto o app está no ar.
.cache_quadro_ra <- new.env(parent = emptyenv())

#' Monta o quadro Rt × razão histórica: uma linha por RA + as janelas usadas.
calcular_quadro_ra <- function(df = SEMANAL_COMPLETO) {
  if (!is.null(.cache_quadro_ra$q)) return(.cache_quadro_ra$q)

  if (nrow(df) == 0 || all(is.na(df$SEM_NUM)) || all(is.na(df$NU_ANO))) return(NULL)

  # ── 1. Referências temporais ──────────────────────────────────────────
  min_casos_se <- 10
  lag_se       <- 0
  janela_se    <- 4   # nº de SEs somadas em cada janela (atual vs anterior)

  # SE de corte automática: descarta as `se_nao_consolidadas` SEs mais
  # recentes da base, cuja notificação ainda está incompleta (gera RAs
  # "sem base" e quedas artificiais). Conta a partir da última SE que a
  # base realmente tem - não de "hoje" -, então acompanha cada atualização
  # do ETL sem edição manual. O ETL só mantém SE 1–52 (52 SEs por ano).
  se_nao_consolidadas <- 2
  idx_ultima <- max(df$NU_ANO * 52L + df$SEM_NUM, na.rm = TRUE)
  idx_corte  <- idx_ultima - se_nao_consolidadas
  ano_corte  <- (idx_corte - 1L) %/% 52L
  se_corte   <- (idx_corte - 1L) %%  52L + 1L

  ano_recente <- min(max(df$NU_ANO, na.rm = TRUE), ano_corte)

  # Mantém apenas SEs até a data de corte (no ano do corte) e anos anteriores
  casos_ra_se_ano <- df %>%
    filter(!is.na(SEM_NUM), !is.na(NU_ANO),
           NU_ANO <= ano_corte,
           !(NU_ANO == ano_corte & SEM_NUM > se_corte)) %>%
    group_by(Regiao_adm, NU_ANO, SEM_NUM) %>%
    summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

  vol_se_recente <- casos_ra_se_ano %>%
    filter(NU_ANO == ano_recente) %>%
    group_by(SEM_NUM) %>%
    summarise(total = sum(casos), .groups = "drop") %>%
    filter(total >= min_casos_se)

  se_teto <- if (nrow(vol_se_recente) > 0) {
    max(vol_se_recente$SEM_NUM)
  } else {
    max(casos_ra_se_ano$SEM_NUM[casos_ra_se_ano$NU_ANO == ano_recente], na.rm = TRUE)
  }

  # Nunca ultrapassa a SE de corte quando o ano recente é o ano do corte
  if (ano_recente == ano_corte) se_teto <- min(se_teto, se_corte)

  se_ref <- max(se_teto - lag_se, 1)

  # Limites das duas janelas de 4 SEs, terminando em se_ref
  #   atual:    [atual_ini .. se_ref]            ex.: SE 13–16
  #   anterior: [ant_ini   .. ant_fim]           ex.: SE 09–12
  atual_ini <- max(se_ref - janela_se + 1, 1)
  atual_fim <- se_ref
  ant_fim   <- max(atual_ini - 1, 1)
  ant_ini   <- max(ant_fim - janela_se + 1, 1)

  # ── 2. Eixo X: soma da janela atual ÷ mediana da mesma janela ─────────
  janela_por_ano <- casos_ra_se_ano %>%
    filter(SEM_NUM >= atual_ini, SEM_NUM <= atual_fim) %>%
    group_by(Regiao_adm, NU_ANO) %>%
    summarise(casos_janela = sum(casos, na.rm = TRUE), .groups = "drop")

  mediana_ra_janela <- janela_por_ano %>%
    group_by(Regiao_adm) %>%
    summarise(mediana_janela = median(casos_janela, na.rm = TRUE), .groups = "drop")

  casos_jan_atual <- janela_por_ano %>%
    filter(NU_ANO == ano_recente) %>%
    select(Regiao_adm, casos_jan_atual = casos_janela)

  # ── 3. Eixo Y: janela atual ÷ janela anterior (mesmo ano recente) ─────
  casos_jan_anterior <- casos_ra_se_ano %>%
    filter(SEM_NUM >= ant_ini, SEM_NUM <= ant_fim, NU_ANO == ano_recente) %>%
    group_by(Regiao_adm) %>%
    summarise(casos_jan_ant = sum(casos, na.rm = TRUE), .groups = "drop")

  casos_ano_recente <- casos_ra_se_ano %>%
    filter(NU_ANO == ano_recente) %>%
    group_by(Regiao_adm) %>%
    summarise(casos_ano = sum(casos, na.rm = TRUE), .groups = "drop")

  # ── 4. Agrega + incidência ────────────────────────────────────────────
  casos_totais_corte <- casos_ra_se_ano %>%
    group_by(Regiao_adm) %>%
    summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

  df_calc <- casos_totais_corte %>%
    left_join(populacao_ra,        by = c("Regiao_adm" = "ra")) %>%
    left_join(mediana_ra_janela,   by = "Regiao_adm") %>%
    left_join(casos_jan_atual,     by = "Regiao_adm") %>%
    left_join(casos_jan_anterior,  by = "Regiao_adm") %>%
    left_join(casos_ano_recente,   by = "Regiao_adm") %>%
    mutate(
      populacao      = replace_na(populacao, 10000),
      casos_ano      = replace_na(casos_ano, 0),
      incidencia_acum = (casos / populacao) * 100000,
      incidencia      = (casos_ano / populacao) * 100000,
      mediana_janela  = replace_na(mediana_janela, 0),
      casos_jan_atual = replace_na(casos_jan_atual, 0),
      casos_jan_ant   = replace_na(casos_jan_ant, 0),
      x_val      = ifelse(mediana_janela > 0, casos_jan_atual / mediana_janela, 1.0),
      sem_base   = casos_jan_ant == 0,
      taxa_cresc = ifelse(casos_jan_ant > 0, casos_jan_atual / casos_jan_ant, NA_real_),
      bubble     = scales::rescale(incidencia, to = c(8, 30)),
      bubble     = ifelse(is.nan(bubble) | is.na(bubble), 16, bubble),
      # ── 5. Forma por limiares epidemiológicos fixos (incidência anual) ──
      forma = dplyr::case_when(
        incidencia <  10  ~ "triangle",
        incidencia <  50  ~ "diamond",
        incidencia < 300  ~ "circle",
        TRUE              ~ "rect"
      ),
      cor = dplyr::case_when(
        sem_base                      ~ CORES_QUADRANTE[["sem_base"]],
        x_val <  1 & taxa_cresc <  1 ~ CORES_QUADRANTE[["abaixo_caindo"]],
        x_val <  1 & taxa_cresc >= 1 ~ CORES_QUADRANTE[["abaixo_subindo"]],
        x_val >= 1 & taxa_cresc <  1 ~ CORES_QUADRANTE[["acima_caindo"]],
        x_val >= 1 & taxa_cresc >= 1 ~ CORES_QUADRANTE[["acima_subindo"]],
        TRUE                          ~ CORES_QUADRANTE[["sem_base"]]
      )
    )

  q <- list(
    dados = df_calc, ano_recente = ano_recente, se_ref = se_ref,
    atual_ini = atual_ini, atual_fim = atual_fim, ant_ini = ant_ini, ant_fim = ant_fim
  )
  assign("q", q, envir = .cache_quadro_ra)
  q
}

mod_mapa_rt_server <- function(id, ra_selecionada = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    output$mapa_rt <- renderEcharts4r({
      tc <- CORES_GRAFICO
      q  <- calcular_quadro_ra()
      if (is.null(q)) return(e_charts())
      df_calc <- q$dados

      # ── RA em foco (clique no mapa / seletor do recorte) ──────────────────
      sel <- ra_selecionada()
      has_sel <- !is.null(sel) && length(sel) == 1 && !is.na(sel) &&
                 sel %in% df_calc$Regiao_adm
      cor_destaque <- PALETA_DENGUE[["tinta"]]
      mostrar_sem_base <- isTRUE(input$mostrar_sem_base)

      ponto <- function(r, y, cor_base, borda_base, rotulo_cor) {
        sel_i <- has_sel && identical(r$Regiao_adm, sel)
        list(
          value      = list(r$x_val, y),
          symbolSize = if (sel_i) r$bubble * 1.7 else r$bubble,
          name       = r$Regiao_adm,
          itemStyle  = list(
            color       = if (sel_i) cor_destaque else cor_base,
            borderColor = if (sel_i) PALETA_DENGUE[["superficie"]] else borda_base,
            borderWidth = if (sel_i) 2 else 1.5,
            opacity     = if (has_sel && !sel_i) 0.3 else 1
          ),
          label = list(
            show            = TRUE,
            formatter       = if (sel_i) r$Regiao_adm else word(r$Regiao_adm, 1),
            position        = "top",
            color           = if (sel_i) PALETA_DENGUE[["tinta"]] else rotulo_cor,
            fontSize        = if (sel_i) 12 else 8,
            fontWeight      = if (sel_i) "bold" else "normal",
            opacity         = if (has_sel && !sel_i) 0.35 else 1,
            backgroundColor = if (sel_i) "rgba(255,255,255,0.92)" else "transparent",
            padding         = if (sel_i) c(2, 5, 2, 5) else 0,
            borderRadius    = if (sel_i) 3 else 0
          )
        )
      }

      df_validas  <- df_calc %>% filter(!sem_base)
      # A RA em foco aparece mesmo com o grupo oculto, para o destaque nao sumir.
      ra_foco <- if (has_sel) sel else NA_character_
      df_sem_base <- df_calc %>%
        filter(sem_base, mostrar_sem_base | (!is.na(ra_foco) & Regiao_adm %in% ra_foco))

      y_faixa_sem_base <- {
        ymax <- suppressWarnings(max(df_validas$taxa_cresc, na.rm = TRUE))
        if (is.infinite(ymax) || is.na(ymax)) ymax <- 1
        round(ymax * 1.15 + 0.2, 2)
      }

      # ── Uma série por forma — symbol APENAS no nível da série ─────────────
      series_list <- lapply(FORMAS_RT, function(f) {
        df_f <- df_validas %>% filter(forma == f$id)
        if (nrow(df_f) == 0) return(NULL)
        pontos <- lapply(seq_len(nrow(df_f)), function(i) {
          r <- df_f[i, ]
          ponto(r, r$taxa_cresc, r$cor, paste0(r$cor, "99"), tc$sub)
        })
        list(
          name     = jsonlite::unbox(f$label),
          type     = jsonlite::unbox("scatter"),
          symbol   = jsonlite::unbox(f$symbol),
          data     = pontos,
          emphasis = list(itemStyle = list(shadowBlur = 12, shadowColor = cor_alpha(PALETA_DENGUE[["tinta"]], 0.35)))
        )
      })
      series_list <- Filter(Negate(is.null), series_list)

      # ── Série "Sem base" (opcional) ───────────────────────────────────────
      if (nrow(df_sem_base) > 0) {
        pontos_sb <- lapply(seq_len(nrow(df_sem_base)), function(i) {
          r <- df_sem_base[i, ]
          ponto(r, y_faixa_sem_base, cor_alpha(CORES_QUADRANTE[["sem_base"]], 0.15),
                CORES_QUADRANTE[["sem_base"]], tc$sub)
        })
        series_list <- c(series_list, list(list(
          name     = jsonlite::unbox("Sem base (janela anterior = 0)"),
          type     = jsonlite::unbox("scatter"),
          symbol   = jsonlite::unbox("circle"),
          data     = pontos_sb,
          emphasis = list(itemStyle = list(shadowBlur = 8, shadowColor = cor_alpha(PALETA_DENGUE[["tinta"]], 0.25)))
        )))
      }

      # ── Série de referência (markLine) ────────────────────────────────────
      markline_data <- list(
        list(
          yAxis     = 1,
          lineStyle = list(color = PALETA_DENGUE[["tinta"]], type = "dashed", width = 1.5),
          label     = list(formatter = "Crescimento = 1", position = "end",
                           color = PALETA_DENGUE[["tinta"]], fontSize = 10)
        ),
        list(
          xAxis     = 1,
          lineStyle = list(color = tc$sub, type = "dashed", width = 1.2),
          label     = list(formatter = "Mediana da RA = 1",
                           position = "insideEndTop", color = tc$sub, fontSize = 10)
        )
      )
      if (nrow(df_sem_base) > 0) {
        markline_data <- c(markline_data, list(list(
          yAxis     = round(y_faixa_sem_base - 0.1, 2),
          lineStyle = list(color = PALETA_DENGUE[["sub"]], type = "dotted", width = 1),
          label     = list(formatter = "Sem base ↑", position = "start",
                           color = PALETA_DENGUE[["sub"]], fontSize = 9)
        )))
      }

      series_list <- c(series_list, list(list(
        name     = jsonlite::unbox("__ref__"),
        type     = jsonlite::unbox("scatter"),
        data     = list(),
        silent   = jsonlite::unbox(TRUE),
        tooltip  = list(show = FALSE),
        markLine = list(
          silent    = jsonlite::unbox(TRUE),
          symbol    = list("none", "none"),
          animation = jsonlite::unbox(FALSE),
          data      = markline_data
        )
      )))

      yaxis_max_js <- if (nrow(df_sem_base) > 0) {
        htmlwidgets::JS(paste0(
          "function(v){ return Math.max(v.max * 1.05, ",
          round(y_faixa_sem_base + 0.15, 2),
          ", 1.3).toFixed(1); }"
        ))
      } else {
        htmlwidgets::JS("function(v){ return Math.max(v.max * 1.15, 1.3).toFixed(1); }")
      }

      legenda_quadrantes <- list(
        list(name = "Abaixo · Cresc < 1", itemStyle = list(color = CORES_QUADRANTE[["abaixo_caindo"]])),
        list(name = "Abaixo · Cresc ≥ 1", itemStyle = list(color = CORES_QUADRANTE[["abaixo_subindo"]])),
        list(name = "Acima · Cresc < 1",  itemStyle = list(color = CORES_QUADRANTE[["acima_caindo"]])),
        list(name = "Acima · Cresc ≥ 1",  itemStyle = list(color = CORES_QUADRANTE[["acima_subindo"]]))
      )
      if (nrow(df_sem_base) > 0) {
        legenda_quadrantes <- c(legenda_quadrantes, list(
          list(name = "Sem base (janela anterior = 0)", itemStyle = list(color = CORES_QUADRANTE[["sem_base"]]))
        ))
      }

      n_ocultas <- sum(df_calc$sem_base) - nrow(df_sem_base)

      e_charts() %>%
        e_list(list(
          backgroundColor = "transparent",
          title = list(
            text    = paste0("Rt × Razão Histórica — SE ",
                             q$atual_ini, "–", q$atual_fim, "/", q$ano_recente),
            subtext = paste0("Forma/tamanho = Incidência ", q$ano_recente,
                             " até SE ", q$se_ref, " | ",
                             "Eixo X: casos (SE ", q$atual_ini, "–", q$atual_fim,
                             ") ÷ mediana da RA na janela (todos os anos) | ",
                             "Eixo Y: janela atual ÷ janela anterior (SE ",
                             q$ant_ini, "–", q$ant_fim, ")",
                             if (n_ocultas > 0) sprintf(" | %d RA(s) sem base oculta(s)", n_ocultas) else ""),
            textStyle    = list(color = tc$texto, fontSize = 14, fontWeight = "bold"),
            subtextStyle = list(color = tc$sub,   fontSize = 10)
          ),
          legend = list(
            list(
              data = lapply(FORMAS_RT, function(f) list(name = f$label)),
              top = 52, left = "center",
              textStyle = list(color = tc$sub, fontSize = 10),
              itemWidth = 14, itemHeight = 12, itemGap = 14
            ),
            list(
              data = legenda_quadrantes,
              bottom = 8, left = "center",
              textStyle = list(color = tc$texto, fontSize = 10),
              itemWidth = 14, itemHeight = 12, itemGap = 14
            )
          ),
          tooltip = list(
            trigger   = "item",
            formatter = htmlwidgets::JS(
              "function(p){
                 if(p.seriesName === '__ref__') return '';
                 if(p.seriesName.indexOf('Sem base') === 0){
                   return '<b>' + p.name + '</b><br/>' +
                          'Razão histórica: <b>' +
                            p.value[0].toFixed(2) + 'x</b><br/>' +
                          '<i>Sem casos na janela anterior —<br/>crescimento indisponível</i>';
                 }
                 return '<b>' + p.name + '</b><br/>' +
                        'Razão histórica: <b>' +
                          p.value[0].toFixed(2) + 'x</b> (mediana da janela = 1x)<br/>' +
                        'Crescimento: <b>' +
                          p.value[1].toFixed(2) + 'x</b> (janela anterior = 1x)';
               }"
            ),
            backgroundColor = tc$bg_tooltip, borderColor = tc$borda_tt,
            borderWidth = 1, textStyle = list(color = tc$texto_tt)
          ),
          xAxis = list(
            type         = "value",
            name         = "Razão Histórica (casos da janela ÷ mediana da RA)",
            nameLocation = "middle", nameGap = 32, min = 0,
            max          = htmlwidgets::JS(
              "function(v){ return Math.max(v.max * 1.15, 1.3).toFixed(1); }"
            ),
            nameTextStyle = list(color = tc$sub, fontSize = 11),
            axisLabel     = list(color = tc$sub, fontSize = 10,
                                 formatter = htmlwidgets::JS(
                                   "function(v){ return v.toFixed(1)+'x'; }"
                                 )),
            splitLine     = list(lineStyle = list(color = tc$grid, type = "dashed"))
          ),
          yAxis = list(
            type         = "value",
            name         = "Crescimento (janela atual ÷ janela anterior)",
            nameLocation = "middle", nameGap = 48, min = 0,
            max          = yaxis_max_js,
            nameTextStyle = list(color = tc$sub, fontSize = 11),
            axisLabel     = list(color = tc$sub, fontSize = 10,
                                 formatter = htmlwidgets::JS(
                                   "function(v){ return v.toLocaleString('pt-BR',
                                     {minimumFractionDigits:1}); }"
                                 )),
            splitLine     = list(lineStyle = list(color = tc$grid, type = "dashed"))
          ),
          series   = series_list,
          dataZoom = list(
            list(
              type             = "inside",
              xAxisIndex       = 0,
              filterMode       = "none",
              zoomOnMouseWheel = TRUE,
              moveOnMouseMove  = TRUE,
              moveOnMouseWheel = FALSE
            ),
            list(
              type             = "inside",
              yAxisIndex       = 0,
              filterMode       = "none",
              zoomOnMouseWheel = TRUE,
              moveOnMouseMove  = TRUE,
              moveOnMouseWheel = FALSE
            )
          ),
          grid = list(left = "13%", right = "5%", top = "30%", bottom = "20%"),
          textStyle = list(fontFamily = FONTE_GRAFICO)
        ))
    })
  })
}
