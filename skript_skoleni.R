library(tidyverse)
library(sf)
library(raster)
library(ggspatial)
library(viridis)
library(igraph)
library(riverconn)
library(elevatr)
library(gridExtra)
library(ggnetwork)
library(lwgeom)
library(gridExtra)
library(corrmorant)
library(RANN)
library(ggpubr)
library(cowplot)

shape_river<-st_read("lucie.zemanova/Ryby_Boris/indexy_povodi/2_04_Bela/Bela_R.shp")

shape_basin<-st_read("lucie.zemanova/Ryby_Boris/gis_vrstvy/povodi_II/povodi_II_2-04.shp")

shape_dams<-st_read("lucie.zemanova/Ryby_Boris/indexy_povodi/2_04_Bela/Bela_bariery_for_R.shp")

shape_river_small <- shape_river

shape_river_small <- shape_river

shape_river_small <- shape_river

# Simplify river shapefile
shape_river_simple <- shape_river_small %>%
  st_as_sf %>%
  st_union()

# Convert shapefile to point object
river_to_points <- shape_river_simple %>%
  st_as_sf %>%
  st_cast("POINT") %>%
  mutate(id = 1:nrow(.))

joins_selection <- river_to_points %>%
  st_equals() %>%
  Filter(function(x){length(x) > 2}, .) %>%
  lapply(., FUN = min) %>%
  unlist() %>%
  unique()

river_joins <- river_to_points %>% 
  filter(id %in% joins_selection)

shape_river_simplified <- lwgeom::st_split(shape_river_simple, river_joins) %>%
  st_collection_extract(.,"LINESTRING") %>%
  data.frame(id = 1:length(.), geometry = .) %>%
  st_as_sf() %>%
  mutate(length = st_length(.))

dams_to_points <- shape_dams %>%
  st_as_sf %>%
  mutate(id = 1:nrow(.))%>% 
  dplyr::select(id,pass_u,pass_d)

ggplot() +
  coord_fixed() +
  theme_minimal() +
  ggspatial::layer_spatial(shape_basin, fill = NA, color = "gray90") +
  ggspatial::layer_spatial(shape_river, color="blue")+
  ggspatial::layer_spatial(dams_to_points, aes(color = factor(pass_u), shape = factor(pass_u)), size=4) + scale_color_manual(values = c("0" = "black", "0.5" = "gray", "1" = "black"), name = "Prostupnost bariér") + scale_shape_manual(values = c("0" = 16, "0.5" = 16, "1" = 21), name = "Prostupnost bariér") +
  theme(legend.position = "bottom") +
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  ggspatial::annotation_north_arrow(location = "br")+
  labs(caption = "Prostupnost bariér")
