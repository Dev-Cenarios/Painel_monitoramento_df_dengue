# Painel de Monitoramento de Dengue — Distrito Federal

Painel interativo em **R Shiny + echarts4r + bslib**, com dados servidos via
**DuckDB** sobre Parquet, por Região Administrativa (RA). Arquitetura irmã do
[painel de monitoramento da RIDE-DF](https://github.com/InVictorLopes/Dengue_monitoramento_ride):
consultas lazy com pushdown de filtro/coluna, reactives compartilhados entre
módulos (uma consulta por grão de dado, vários consumidores), tema
`bslib::bs_theme` e cards com controles em popover.

> ⚠️ **Dado sensível.** A fonte primária (`data/dengue_df_processado.parquet`)
> é **microdado de caso** do SINAN (uma linha por notificação, com data, sexo,
> faixa etária, RA, setor censitário e evolução clínica). Nenhum Parquet ou
> DuckDB de `data/` é versionado — eles são gerados pelos ETLs (ver
> [Atualizando os dados](#atualizando-os-dados)).

## Estrutura do Projeto

```
dengue_df/
├── app.R                              # Ponto de entrada
├── config/
│   ├── global.R                       # Pacotes, views DuckDB, dicionários, tema
│   ├── ui.R                           # Interface (bslib::page_navbar)
│   └── server.R                       # Reactives compartilhadas + orquestração dos módulos
├── modules/
│   ├── utils_ui.R                     # Tema (bs_theme) e cartao_grafico()
│   ├── mod_filtros.R                  # Ano / semana / RA / região de saúde
│   ├── mod_kpi_cards.R                # value_box no topo
│   ├── mod_mapa_incidencia.R          # Mapa coroplético por RA (clique → filtra RA)
│   ├── mod_mapa_rt.R                  # Razão histórica por RA (escopo global, todas as RAs)
│   ├── mod_rt_temporal.R              # Série temporal do Rt
│   ├── mod_curva_epidemia.R           # Casos por semana epidemiológica (MA-5)
│   ├── mod_distribuicao_temporal.R    # Série contínua + heatmap mês×ano
│   ├── mod_canal_endemico.R           # Canal endêmico (quartis pré-computados no ETL)
│   ├── mod_perfil_casos.R             # Sexo + pirâmide etária
│   └── mod_analises_hotspot.R         # Aba "Análises": hotspots por setor censitário (Getis-Ord Gi*)
├── prep/
│   └── 01_simplifica_malhas.R         # Rodar 1x: simplifica as malhas GeoJSON
└── data/
    ├── data_dengue.R                  # ETL principal (PostgreSQL → Parquet)
    ├── etl_hotspots.R                 # ETL de hotspots espaciais (Getis-Ord Gi*)
    ├── gerar_geojson.R                # Gera df_ras_wgs84.geojson a partir dos setores
    ├── setores_RAs_DF.csv             # De-para setor censitário → RA
    ├── DF_setores_CD2022.*            # Malha de setores censitários (IBGE, Censo 2022)
    ├── df_ras_wgs84.geojson           # Malha de RAs (mapa principal)
    ├── dengue_df_setores_hotspot.geojson  # Malha de setores (aba "Análises")
    └── *.parquet, *.duckdb            # Saídas dos ETLs (NÃO versionadas)
```

## Pacotes necessários

```r
install.packages(c(
  "shiny", "bslib", "DBI", "duckdb", "dplyr", "dbplyr",
  "echarts4r", "DT", "lubridate", "tidyr", "stringr",
  "jsonlite", "zoo", "scales", "cachem"
))
```

Para os ETLs e scripts de preparação (rodados fora do app): `RPostgres`,
`arrow`, `sf`, `spdep`, `purrr`.

## Configuração

Copie `.Renviron.example` para `.Renviron` (não versionado) e preencha as
credenciais do PostgreSQL usadas pelos ETLs:

```
PG_HOST=...
PG_PORT=5432
PG_DB=...
PG_USER=...
PG_PASS=...
```

Reinicie a sessão do R para que o `.Renviron` seja lido.

## Como rodar

```r
# working directory = raiz do app (dengue_df/), como no botão "Run App" do RStudio
shiny::runApp()
```

`config/global.R` espera os Parquets do ETL em `data/` (ou no diretório
apontado por `DENGUE_DF_DATA_DIR`). Se eles não existirem, rode os ETLs antes.

### Antes de rodar (dev ou produção)

Rode uma vez (requer o pacote `sf`):

```r
source("prep/01_simplifica_malhas.R")
```

Isso simplifica a geometria de `data/df_ras_wgs84.geojson` (mapa principal) e
de `data/dengue_df_setores_hotspot.geojson` (aba "Análises" — ~5.400
polígonos). Os dois são carregados sob demanda, mas sem simplificar a
primeira renderização de cada mapa demora dezenas de segundos.

## Atualizando os dados

```r
source("data/data_dengue.R")   # PostgreSQL → Parquets por grão (individual/diário/semanal/mensal/canal endêmico)
source("data/etl_hotspots.R")  # hotspots por setor censitário (aba "Análises")
```

Rode os dois periodicamente (ex.: cron) para manter o painel atualizado; o
app em si só lê os Parquets via DuckDB, nunca consulta o PostgreSQL
diretamente.

## Personalização

- **Tema/cores**: `PALETA_CHOCOLATE` e `CORES_GRAFICO` em `config/global.R`;
  `TEMA_DENGUE_DF` (bs_theme) em `modules/utils_ui.R`.
- **Filtros**: `modules/mod_filtros.R` — adicionar um seletor aqui expõe o
  campo no `list()` de saída, que `aplicar_filtro()` (`config/global.R`) já
  sabe aplicar a qualquer view (se a coluna existir nela).
- **Novos módulos**: criar em `modules/`, adicionar ao loop de `source()` no
  fim de `config/global.R`, e registrar UI/server em `config/ui.R` /
  `config/server.R`.

---
Desenvolvido com R + Shiny + echarts4r + bslib + DuckDB • Secretaria de Saúde do DF
