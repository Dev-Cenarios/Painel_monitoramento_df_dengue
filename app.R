# =============================================================================
# app.R — Painel de Dengue DF
#
# Caminhos relativos (working directory = raiz do app) em vez de absolutos
# fixos em /srv/shiny-server/dengue_df - mesma convenção do dengue_ride. O
# Shiny Server já define o working directory como a raiz do app ao rodar
# runApp(), então isso funciona igual em produção e localmente
# (shiny::runApp("dengue_df") ou o botão "Run App" do RStudio), o que antes
# não era possível com os caminhos absolutos.
#
# config/global.R agora source() todos os modules/*.R sozinho (mesmo padrão
# do global.R do dengue_ride), então app.R não precisa mais listá-los um a um.
# =============================================================================

options(shiny.sanitize.errors = FALSE)

source("config/global.R")
source("config/ui.R")
source("config/server.R")

shinyApp(ui = ui, server = server)
