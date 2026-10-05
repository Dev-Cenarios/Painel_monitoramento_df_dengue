# Painel de Monitoramento de Dengue — Distrito Federal

Painel interativo para acompanhar os casos de dengue notificados no
**Distrito Federal**, por **Região Administrativa (RA)** e semana
epidemiológica. Ele mostra quantos casos existem, onde estão, se estão
aumentando ou diminuindo e se a situação atual está acima do esperado para a
época do ano.

Feito em **R + Shiny**, com gráficos em **echarts4r** e dados servidos via
**DuckDB** sobre arquivos **Parquet**. Painéis irmãos:
[Monitoramento RIDE-DF](https://github.com/InVictorLopes/Dengue_monitoramento_ride) ·
[Vigilância DF](https://github.com/InVictorLopes/Dengue_vigilancia_df).

> ⚠️ **Dado sensível.** O ETL deste painel trabalha com **microdados de
> notificação do SINAN** (dados de saúde individualizados, protegidos pela
> LGPD). Nenhum dado de caso é versionado neste repositório — os arquivos de
> dados são gerados localmente a partir do banco. Veja
> [Dados e privacidade](#10-dados-e-privacidade).

---

## Sumário

1. [O que o painel mostra](#1-o-que-o-painel-mostra)
2. [Início rápido (passo a passo)](#2-início-rápido-passo-a-passo)
3. [De onde vêm os dados](#3-de-onde-vêm-os-dados)
4. [Ordem de execução dos scripts](#4-ordem-de-execução-dos-scripts)
5. [Metodologia](#5-metodologia)
6. [Estrutura do projeto](#6-estrutura-do-projeto)
7. [Publicação no Shiny Server](#7-publicação-no-shiny-server)
8. [Atualização automática (cron)](#8-atualização-automática-cron)
9. [Solução de problemas](#9-solução-de-problemas)
10. [Dados e privacidade](#10-dados-e-privacidade)
11. [Limitações conhecidas](#11-limitações-conhecidas)
12. [Glossário](#12-glossário)

---

## 1. O que o painel mostra

O painel tem duas abas.

**Aba "Painel"** — responde ao **recorte** escolhido no topo (Ano, Semana
epidemiológica, Região Administrativa e Região de Saúde):

| Visualização | O que responde |
|---|---|
| **Indicadores (KPIs)** | Total de casos, incidência média, casos na última semana, Rt aproximado, óbitos e óbitos em investigação. A cor do quadro escurece conforme a situação piora. |
| **Mapa de incidência por RA** | Onde há mais casos por habitante. Clicar numa RA aplica o filtro dela no painel inteiro. |
| **Série** | Evolução semanal dos casos ao longo do tempo. |
| **Curva epidêmica** | Casos por data de início de sintomas, suavizados por média móvel de 5 dias. |
| **Rt aproximado** | Se os casos estão crescendo (> 1) ou caindo (< 1), semana a semana. |
| **Rt × Razão histórica por RA** | Compara **todas** as RAs: quais estão acima do normal para a época **e** crescendo. Ignora o recorte de propósito. |
| **Canal endêmico** | A incidência da temporada escolhida contra a faixa "esperada" (quartis históricos). |
| **Sazonalidade — mês × ano** | Em quais meses a transmissão costuma subir. |
| **Perfil dos casos** | Distribuição por sexo e pirâmide etária. |
| **Últimas semanas / SQL gerado** | A tabela das últimas semanas e a consulta usada (transparência). |

**Aba "Análises"** — **hotspots espaciais persistentes**: quais setores
censitários concentram casos de forma estatisticamente significativa, ano
após ano. Não usa o recorte da aba Painel.

---

## 2. Início rápido (passo a passo)

Este roteiro é para quem nunca rodou o painel. Siga na ordem.

### 2.1 Pré-requisitos

- **R 4.1 ou superior** (https://cran.r-project.org) e, de preferência, o
  **RStudio** (https://posit.co/download/rstudio-desktop/).
- **Notificações de dengue geocodificadas** num banco PostgreSQL:
  - **equipe do projeto**: acesso ao banco `cenarios_ai` (credenciais
    fornecidas pela equipe, na rede interna onde ele está acessível);
  - **demais usuários**: os seus próprios dados do SINAN, geocodificados
    pelo mesmo sistema usado neste painel — veja
    [Usando os seus próprios dados](#32-usando-os-seus-próprios-dados).

### 2.2 Baixar o projeto

```bash
git clone https://github.com/InVictorLopes/Dengue_monitoramento_df.git dengue_df
```

Ou, no GitHub, use **Code → Download ZIP** e descompacte. Depois, abra a
pasta no RStudio (**File → Open Project...** ou defina como diretório de
trabalho com **Session → Set Working Directory → Choose Directory...**).

> Todos os comandos abaixo devem ser executados **com o diretório de
> trabalho na raiz do projeto** (a pasta que contém `app.R`). Confira com
> `getwd()`.

### 2.3 Instalar os pacotes (uma vez só)

Cole no console do R:

```r
install.packages(c(
  # painel
  "shiny", "bslib", "DBI", "duckdb", "dplyr", "dbplyr", "echarts4r", "DT",
  "lubridate", "tidyr", "stringr", "jsonlite", "zoo", "scales", "cachem",
  # ETLs e preparação (rodados fora do painel)
  "RPostgres", "arrow", "sf", "spdep", "purrr", "tibble"
))
```

> No Linux, o pacote `sf` precisa das bibliotecas de sistema GDAL, GEOS e
> PROJ (ex.: `sudo apt install libgdal-dev libgeos-dev libproj-dev`).

### 2.4 Configurar as credenciais do banco

1. Copie o arquivo `.Renviron.example` e renomeie a cópia para `.Renviron`
   (na mesma pasta).
2. Abra o `.Renviron` e preencha:

   ```
   PG_HOST=endereço do servidor do banco
   PG_PORT=5432
   PG_DB=nome do banco (cenarios_ai, para a equipe do projeto)
   PG_USER=seu usuário
   PG_PASS=sua senha
   ```

3. **Reinicie o R** (RStudio: **Session → Restart R**) para ele ler o
   arquivo. Confira com `Sys.getenv("PG_HOST")`.

> O `.Renviron` **nunca** vai para o GitHub (está no `.gitignore`). Não
> compartilhe esse arquivo nem cole seu conteúdo em chats ou e-mails.

### 2.5 Gerar os dados

```r
source("data/data_dengue.R")          # ~1 a 3 min: casos, séries e canal endêmico
source("data/etl_hotspots.R")         # alguns minutos: hotspots por setor censitário
source("prep/01_simplifica_malhas.R") # segundos: deixa os mapas leves
```

Cada script imprime um resumo no console (número de registros, período,
arquivos salvos). Se terminar sem `Error`, deu certo.

### 2.6 Abrir o painel

```r
shiny::runApp()
```

Ou clique em **Run App** com o `app.R` aberto no RStudio. O painel abre no
navegador.

---

## 3. De onde vêm os dados

| Dado | Origem | Onde fica no projeto | Versionado? |
|---|---|---|---|
| **Notificações de dengue** | SINAN, já geovinculadas a setor censitário (*linkage* geográfico). Banco PostgreSQL `cenarios_ai`, tabela `dados_geolink."DF_geo_Dengue"`. | lido pelo ETL, gera `data/*.parquet` | ❌ Não (sensível) |
| **De-para setor censitário → RA** | Tabela de correspondência entre os setores do Censo 2022 e as 35 RAs do DF. | `data/setores_RAs_DF.csv` | ✅ Sim |
| **Malha de setores censitários** | IBGE — Malha de Setores Censitários, Censo 2022 (site de Geociências do IBGE, recorte do DF). | `data/DF_setores_CD2022.*` (shapefile) | ✅ Sim |
| **Malha das RAs** | Gerada pela união dos setores de cada RA (`data/gerar_geojson.R`). | `data/df_ras_wgs84.geojson` | ✅ Sim |
| **Malha de setores para o mapa de hotspots** | Gerada por `data/etl_hotspots.R` (só código do setor + geometria). | `data/dengue_df_setores_hotspot.geojson` | ✅ Sim |
| **População por RA** | Estimativas fixas no código (ver [Limitações](#11-limitações-conhecidas)). | `data/data_dengue.R` e `config/global.R` | ✅ Sim |

**Colunas lidas da tabela de notificações:**

| Coluna SINAN | Significado |
|---|---|
| `DT_SIN_PRI` | Data de início dos sintomas |
| `SEM_PRI` | Semana epidemiológica dos primeiros sintomas (lida, mas a semana é recalculada a partir de `DT_SIN_PRI`) |
| `CS_SEXO` | Sexo |
| `NU_IDADE_N` | Idade no formato SINAN (1º dígito = unidade, demais = valor) |
| `CLASSI_FIN` | Classificação final |
| `EVOLUCAO` | Evolução do caso (cura, óbito...) |
| `Regiao_adm` | Região Administrativa de residência |
| `Regiao_sau1` | Região de Saúde |
| `CD_SETOR` | Setor censitário (Censo 2022) obtido no geovínculo |

### 3.1 Equipe do projeto

As notificações já estão geocodificadas no banco `cenarios_ai` (tabela
`dados_geolink."DF_geo_Dengue"`). Basta pedir as credenciais à equipe e
seguir o [Início rápido](#2-início-rápido-passo-a-passo).

### 3.2 Usando os seus próprios dados

O banco do projeto **não é público** (contém dados pessoais de saúde). Para
rodar o painel com os dados do seu território:

1. **Obtenha as notificações de dengue do SINAN** junto à vigilância
   epidemiológica local. São dados sensíveis: o uso exige autorização e os
   cuidados da LGPD.
2. **Geocodifique os endereços** com o
   [Sistema de Geocodificação de Dados de Saúde](https://cenarios.unb.br/sistema-de-geocodificacao-de-dados-de-saude/)
   do Projeto CENÁRIOS (UnB) — o mesmo sistema usado para gerar a base deste
   painel. Ele transforma os endereços das notificações em localização
   geográfica, de onde vem o setor censitário (`CD_SETOR`). Metodologia em
   Sanglard, Garcia & Ramalho (2025), *Geospatial Health*,
   [doi:10.4081/gh.2025.1403](https://doi.org/10.4081/gh.2025.1403).
3. **Carregue o resultado numa tabela PostgreSQL** com as colunas da tabela
   acima e aponte o `.Renviron` para o seu banco (seção 2.4). Para conferir
   as colunas da sua tabela:

   ```sql
   SELECT column_name FROM information_schema.columns
   WHERE table_schema = 'seu_schema' AND table_name = 'sua_tabela';
   ```

4. **Ajuste o nome da tabela** na consulta (`FROM dados_geolink."DF_geo_Dengue"`)
   em `data/data_dengue.R` e `data/etl_hotspots.R`, se o seu for diferente.
5. **Fora do DF**, substitua também os insumos territoriais, que são
   específicos do Distrito Federal: o de-para setor → RA
   (`data/setores_RAs_DF.csv`), a malha de setores (`data/DF_setores_CD2022.*`,
   disponível para qualquer UF no site do IBGE) e a tabela de população por
   RA (seção 11).

---

## 4. Ordem de execução dos scripts

| # | Script | Quando rodar | Lê | Gera |
|---|---|---|---|---|
| 0 | criar `.Renviron` | uma vez por máquina | — | credenciais |
| 1 | `data/data_dengue.R` | a cada atualização de dados | PostgreSQL + `setores_RAs_DF.csv` | `dengue_df_processado.parquet`, `_diario`, `_semanal`, `_mensal`, `_canal_endemico`, `_dados_ra` (+ `dengue_df.duckdb`, cópia para consulta avulsa) |
| 2 | `data/etl_hotspots.R` | a cada atualização de dados | PostgreSQL + shapefile de setores | `dengue_df_hotspots_anual.parquet`, `dengue_df_setores_hotspot.geojson` |
| 3 | `data/gerar_geojson.R` | **só** se o de-para setor→RA ou a malha de setores mudar | shapefile + de-para | `df_ras_wgs84.geojson` |
| 4 | `prep/01_simplifica_malhas.R` | **depois** dos passos 2 ou 3 (eles regravam os GeoJSON em tamanho cheio) | GeoJSON | os mesmos GeoJSON, simplificados |
| 5 | `shiny::runApp()` | — | Parquets + GeoJSON | o painel |

Em produção, depois dos passos 1–4 é preciso **reiniciar o app** (ver
[seção 7](#7-publicação-no-shiny-server)), porque parte dos dados é
carregada uma única vez na inicialização.

---

## 5. Metodologia

### 5.1 Tratamento dos dados (ETL — `data/data_dengue.R`)

1. **Leitura** de todas as notificações da tabela `DF_geo_Dengue`.
2. **RA de cada caso**: quando o caso tem setor censitário, a RA vem do
   de-para `setores_RAs_DF.csv` (prevalece sobre a RA digitada no SINAN);
   senão, usa a `Regiao_adm` original. Nomes harmonizados (ex.:
   "SCIA (ESTRUTURAL)" → "SCIA") e em maiúsculas.
3. **Exclusões**:
   - casos sem RA (vazia ou "Não Informado");
   - casos sem data de início de sintomas;
   - casos com início de sintomas **antes de 2021** ou após o ano corrente;
   - casos na **semana epidemiológica 53** (só são mantidas as SE 1–52).
4. **Variáveis derivadas**:
   - **Ano e semana epidemiológica** calculados a partir de `DT_SIN_PRI`
     (`lubridate::epiyear` / `epiweek`, padrão do Ministério da Saúde: semana
     de domingo a sábado);
   - **Faixa etária** a partir de `NU_IDADE_N` (idades em horas, dias ou
     meses contam como menores de 1 ano);
   - **Rótulos** conforme o Dicionário de Dados do SINAN Online:
     `EVOLUCAO` (1 Cura · 2 Óbito pelo agravo · 3 Óbito por outras causas ·
     4 Óbito em investigação · 9 Ignorado) e `CLASSI_FIN` (10 Dengue ·
     11 Com sinais de alarme · 12 Grave · 5 Descartado · 8 Inconclusivo; os
     códigos antigos 1/2/3 são mapeados para as mesmas categorias; demais
     códigos → "Ignorado").
5. **Agregação** pré-calculada por grão (diário, semanal e mensal por RA e
   Região de Saúde), mais uma série "DISTRITO FEDERAL" com o total.

> O painel conta **notificações** que passam pelos filtros acima — não há
> filtro por classificação final (casos descartados não são removidos).

### 5.2 Indicadores

**Incidência** (casos por 100 mil habitantes):

```
incidência = casos / população × 100.000
```

- No mapa e no KPI, para o recorte selecionado. O KPI "Incidência média" é a
  **média simples** da incidência das RAs do recorte.
- Cor do KPI: < 100 claro · 100–299 médio · ≥ 300 escuro (cortes usados pelo
  Ministério da Saúde para baixa / média / alta incidência).
- O mapa usa escala contínua ajustada ao máximo do recorte, porque o recorte
  pode somar de uma semana a vários anos.

**Rt aproximado** (razão de crescimento) — usado no KPI, no gráfico de Rt e
no mapa de tendência:

```
Rt aprox. = casos das últimas 4 semanas / casos das 4 semanas anteriores
```

- \> 1 = crescimento; < 1 = queda. **Não é** o número de reprodução efetivo
  epidemiológico (que exige modelo de intervalo serial); é uma razão simples
  entre janelas.
- No KPI: calculado por RA e depois **média** entre as RAs (Rt não é
  somável). Cor: < 1 claro · 1–1,25 médio · ≥ 1,25 escuro.
- No gráfico temporal: a mesma razão repetida em cada semana da série.

**Curva epidêmica**: casos diários por data de início de sintomas, com
**média móvel centrada de 5 dias**.

### 5.3 Rt × Razão histórica por RA (mapa de tendência)

Gráfico de dispersão com uma bolha por RA, sempre com **todas as RAs**:

- **Eixo X — razão histórica**: casos da RA na janela de 4 semanas atual ÷
  **mediana** dos casos da RA nessa mesma janela de semanas em todos os anos
  disponíveis. X > 1 = acima do normal para a época.
- **Eixo Y — crescimento**: casos da janela atual ÷ casos da janela anterior
  (mesmo ano). Y > 1 = subindo.
- **Cor = quadrante** (abaixo/acima do normal × caindo/subindo); o pior caso
  é "acima do normal e subindo".
- **Forma = incidência do ano**: ▲ < 10 · ◆ 10–50 · ● 50–300 · ■ > 300 por
  100 mil.
- RAs sem casos na janela anterior ficam numa faixa "sem base" (não dá para
  calcular o crescimento).
- A semana de referência é a última semana com pelo menos 10 casos no DF,
  **desconsiderando as 2 semanas mais recentes da base**, cuja notificação
  ainda está incompleta (`se_nao_consolidadas` em `modules/mod_mapa_rt.R`).
  O corte acompanha automaticamente cada atualização dos dados.

### 5.4 Canal endêmico

Calculado no ETL, por RA (e para o DF inteiro), sobre a **incidência
semanal**:

- **Mediana**, **1º quartil (25%)** e **3º quartil (75%)** da incidência de
  cada semana epidemiológica ao longo dos anos de referência.
- **Anos epidêmicos atípicos são excluídos** da referência (hoje: **2024**,
  na variável `anos_excluir_canal` de `data/data_dengue.R`), para que um ano
  excepcional não "normalize" uma epidemia.
- O eixo começa na **SE 27** (início da temporada, fim de junho) e termina na
  SE 26 do ano seguinte, para não cortar ao meio o pico do verão. O seletor
  "Temporada" escolhe qual temporada é comparada com o canal.
- Leitura: abaixo do Q1 = zona de sucesso; entre Q1 e Q3 = zona esperada;
  acima do Q3 = zona de alerta/epidemia.

### 5.5 Hotspots espaciais (aba "Análises" — `data/etl_hotspots.R`)

Método de **Dzul-Manzanilla et al. (2021)**, *The Lancet Planetary Health*,
aplicado aos setores censitários:

1. Mesmo tratamento do ETL principal, mantendo só os casos **com setor
   censitário**.
2. **Casos por setor × ano**; setores sem casos entram com zero.
3. Em cada ano, os casos são **padronizados (escore Z)** entre os setores.
4. **Vizinhança por contiguidade "queen"** (setores que se tocam, inclusive
   por um vértice), com pesos padronizados por linha.
5. Estatística **Getis-Ord Gi\*** local por setor e ano.
6. **Hotspot** = Gi\* ≥ **3,71** (limiar com correção de Bonferroni, mais
   rigoroso que o clássico 1,96, para controlar os milhares de testes
   simultâneos).
7. **Persistência** = número de anos em que o setor foi hotspot. O mapa
   mostra a persistência ou os casos totais; a tabela ranqueia os setores.

---

## 6. Estrutura do projeto

```
dengue_df/
├── app.R                          # Ponto de entrada (carrega config/*.R)
├── .Renviron.example              # Modelo das credenciais (copiar para .Renviron)
├── config/
│   ├── global.R                   # Pacotes, views DuckDB sobre os Parquets, populações, helpers
│   ├── ui.R                       # Interface (bslib::page_navbar)
│   └── server.R                   # Reactives compartilhadas + orquestração dos módulos
├── modules/
│   ├── utils_ui.R                 # Tema visual e componente de card
│   ├── mod_filtros.R              # Recorte: ano / semana / RA / região de saúde
│   ├── mod_kpi_cards.R            # Quadros de indicadores
│   ├── mod_mapa_incidencia.R      # Mapa coroplético por RA
│   ├── mod_mapa_rt.R              # Rt × razão histórica (todas as RAs)
│   ├── mod_rt_temporal.R          # Rt aproximado ao longo do tempo
│   ├── mod_curva_epidemia.R       # Curva epidêmica (MA-5)
│   ├── mod_distribuicao_temporal.R# Série + sazonalidade mês × ano
│   ├── mod_canal_endemico.R       # Canal endêmico
│   ├── mod_perfil_casos.R         # Perfil (sexo, faixa etária)
│   └── mod_analises_hotspot.R     # Aba "Análises" (hotspots)
├── prep/
│   └── 01_simplifica_malhas.R     # Simplifica as malhas GeoJSON
└── data/
    ├── data_dengue.R              # ETL principal
    ├── etl_hotspots.R             # ETL de hotspots
    ├── gerar_geojson.R            # Gera a malha das RAs
    ├── setores_RAs_DF.csv         # De-para setor → RA
    ├── DF_setores_CD2022.*        # Malha de setores (IBGE)
    ├── *.geojson                  # Malhas usadas nos mapas
    └── *.parquet, *.duckdb        # Gerados pelos ETLs — NÃO versionados
```

**Como o painel lê os dados:** na inicialização, `config/global.R` cria
*views* do DuckDB (em memória) sobre cada Parquet. Os filtros viram cláusulas
`WHERE` executadas na leitura do arquivo (*predicate pushdown*); só o
resultado agregado é trazido para o R. Os módulos consomem *reactives*
compartilhados do `server.R` (uma consulta por grão de dado, vários
gráficos). Variável opcional: `DENGUE_DF_DATA_DIR` aponta outra pasta de
dados.

**Personalização:**
- Cores: `PALETA_CHOCOLATE` e `CORES_GRAFICO` em `config/global.R`; tema em
  `modules/utils_ui.R`.
- Novo filtro: `modules/mod_filtros.R` — `aplicar_filtro()` já sabe aplicar
  qualquer campo que exista na view.
- Novo módulo: criar em `modules/` (é carregado automaticamente pelo
  `config/global.R`) e registrar em `config/ui.R` / `config/server.R`.

---

## 7. Publicação no Shiny Server

1. Copie (ou clone) o projeto para a pasta de apps do Shiny Server — no
   servidor atual, `~/shiny-server/dengue_df`.
2. Crie o `.Renviron` nessa pasta (seção 2.4).
3. Instale os pacotes **no R do servidor** (seção 2.3).
4. Rode os scripts da seção 4 a partir dessa pasta.
5. Para o Shiny Server recarregar o app após atualizar dados ou código:

   ```bash
   touch ~/shiny-server/dengue_df/restart.txt
   ```

O endereço do painel segue o nome da pasta (ex.: `.../dengue_df/`).

---

## 8. Atualização automática (cron)

Exemplo para atualizar todo dia às 3h (edite com `crontab -e` no servidor,
ajustando o caminho):

```cron
0 3 * * * cd /home/USUARIO/shiny-server/dengue_df && (Rscript data/data_dengue.R && Rscript data/etl_hotspots.R && Rscript prep/01_simplifica_malhas.R && touch restart.txt) >> /tmp/dengue_df_etl.log 2>&1
```

O `Rscript` lê o `.Renviron` da pasta em que é executado, por isso o `cd`
antes dos scripts é obrigatório.

---

## 9. Solução de problemas

| Sintoma | Causa provável | Solução |
|---|---|---|
| `Parquet nao encontrado em ...` ao abrir o painel | Os ETLs ainda não foram rodados (ou rodaram em outra pasta). | Rode a seção 2.5 com o diretório de trabalho na raiz do projeto. |
| `could not connect to server` / `password authentication failed` | `.Renviron` ausente, incompleto ou R não reiniciado; fora da rede do banco. | Confira `Sys.getenv("PG_HOST")`; reinicie o R; verifique a rede/VPN. |
| `cannot open file 'data/...': No such file` | Diretório de trabalho errado. | `getwd()` deve ser a pasta com `app.R`. |
| Erro ao instalar `sf` | Faltam GDAL/GEOS/PROJ no sistema. | Instale as bibliotecas de sistema (seção 2.3). |
| Mapa ou aba "Análises" demora muito para abrir | Malhas não simplificadas (ou regravadas pelo ETL de hotspots). | Rode `prep/01_simplifica_malhas.R`. |
| Painel não mostra os dados novos no servidor | O app carrega parte dos dados só na inicialização. | `touch restart.txt` na pasta do app. |
| Aviso "RAs sem população cadastrada" no ETL | Nome de RA novo/diferente da tabela de população. | Inclua a RA nas tabelas de população (seção 11). |

---

## 10. Dados e privacidade

- `data/dengue_df_processado.parquet` contém **uma linha por notificação**
  (data, sexo, faixa etária, RA, evolução). Mesmo sem nome, é dado de saúde
  individual: **não compartilhe** os Parquets/DuckDB nem os envie para
  repositórios.
- O `.gitignore` já exclui `data/*.parquet`, `data/*.duckdb` e `.Renviron`.
  Antes de cada commit, confira com `git status` que nenhum desses aparece.
- O painel publicado mostra apenas **agregados** (contagens e taxas).

---

## 11. Limitações conhecidas

- **População por RA** é uma tabela de estimativas aproximadas (valores
  arredondados) escrita **em dois lugares**: `data/data_dengue.R` e
  `config/global.R`. Ao atualizar, mude os dois. RAs ausentes da tabela usam
  10.000 habitantes como valor provisório (o ETL avisa quais).
- **Semana 53** é descartada no ETL.
- **Rt aproximado** é uma razão entre janelas, não o Rt epidemiológico.
- **Atraso de notificação**: as semanas mais recentes tendem a estar
  incompletas; quedas no fim da série devem ser lidas com cautela.

---

## 12. Glossário

| Termo | Significado |
|---|---|
| **SINAN** | Sistema de Informação de Agravos de Notificação, do Ministério da Saúde. |
| **SE** | Semana epidemiológica (domingo a sábado, numeradas de 1 a 52/53 no ano). |
| **RA** | Região Administrativa do Distrito Federal. |
| **Setor censitário** | Menor unidade territorial do Censo do IBGE (algumas centenas de domicílios). |
| **Incidência** | Casos por 100 mil habitantes; permite comparar áreas de tamanhos diferentes. |
| **Rt** | Quantos casos novos cada caso gera; > 1 indica expansão. Aqui, aproximado por razão entre janelas. |
| **Canal endêmico** | Faixa de valores "esperados" para cada semana, calculada com o histórico. |
| **Hotspot (Gi\*)** | Área com concentração de casos significativamente maior que a dos vizinhos. |
| **ETL** | *Extract, Transform, Load*: script que extrai do banco, trata e grava os arquivos do painel. |
| **Parquet / DuckDB** | Formato de arquivo colunar e motor de consulta usados para o painel ler os dados rapidamente. |

---
Desenvolvido com R + Shiny + echarts4r + bslib + DuckDB • Secretaria de Saúde do DF
