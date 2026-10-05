library(sf)
library(dplyr)

depara <- read.csv("data/setores_RAs_DF.csv") %>%
  mutate(CD_SETOR = as.character(CD_SETOR))

st_read("data/DF_setores_CD2022.shp") %>%
  mutate(CD_SETOR = as.character(CD_SETOR)) %>%
  left_join(depara %>% select(CD_SETOR, ra_nome), by = "CD_SETOR") %>%
  filter(!is.na(ra_nome)) %>%
  group_by(ra_nome) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_transform(4326) %>%
  st_write("data/df_ras_wgs84.geojson", delete_dsn = TRUE)

