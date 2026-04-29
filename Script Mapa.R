# ============================================================
# MAPA PANEL MAPBIOMAS: RÍO, MINERÍA Y SIN VEGETACIÓN
# Optimizado
# ============================================================

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(fs)

# =============================
# 1. RUTAS
# =============================

ruta_puntos <- "C:/Users/GORKY/Downloads/R para Facebbok/06_Area minera en MDD/SHP/Puntos.shp"
dir_out <- "C:/Users/GORKY/Downloads/R para Facebbok/06_Area minera en MDD/SALIDAS"

dir_create(dir_out)

# =============================
# 2. LEER PUNTOS Y CREAR ÁREAS CUADRADAS
# =============================

puntos <- st_read(ruta_puntos, quiet = TRUE) %>%
  st_transform(32719)

lado <- 10000   # 10 km x 10 km = 10,000 ha
mitad <- lado / 2

crear_cuadro <- function(punto, id) {
  
  xy <- st_coordinates(punto)
  
  bb <- st_bbox(
    c(
      xmin = xy[1] - mitad,
      xmax = xy[1] + mitad,
      ymin = xy[2] - mitad,
      ymax = xy[2] + mitad
    ),
    crs = st_crs(puntos)
  )
  
  st_sf(
    area = paste0("Área ", id),
    geometry = st_as_sfc(bb)
  )
}

areas_utm <- map2_dfr(
  split(puntos, seq_len(nrow(puntos))),
  seq_len(nrow(puntos)),
  crear_cuadro
)

areas <- st_transform(areas_utm, 4326)

# Guardar todas las áreas en un solo shapefile
st_write(
  areas,
  file.path(dir_out, "Areas_cuadradas_10000ha.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)

# =============================
# 3. PARÁMETROS MAPBIOMAS
# =============================

years <- 2019:2024

clases_interes <- c(
  "Río" = 33,
  "Minería" = 30,
  "Sin vegetación" = 25
)

paleta <- c(
  "Río" = "#4A90E2",
  "Minería" = "#d00000",
  "Sin vegetación" = "#e85d04"
)

# =============================
# 4. FUNCIÓN PARA DESCARGAR RÁSTER
# =============================

descargar_mapbiomas <- function(year) {
  
  url <- paste0(
    "https://storage.googleapis.com/mapbiomas-public/initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_",
    year, ".tif"
  )
  
  archivo <- file.path(tempdir(), paste0("mapbiomas_", year, ".tif"))
  
  if (!file.exists(archivo)) {
    download.file(url, archivo, mode = "wb", quiet = TRUE)
  }
  
  archivo
}

# Descargar una vez por año
archivos_raster <- setNames(
  map_chr(years, descargar_mapbiomas),
  years
)

# =============================
# 5. FUNCIÓN PARA PROCESAR ÁREA Y AÑO
# =============================

procesar_area_anio <- function(year, area_sf) {
  
  r <- rast(archivos_raster[as.character(year)])
  
  area_r <- area_sf %>%
    st_make_valid() %>%
    st_transform(crs(r))
  
  v <- vect(area_r)
  
  # Verificar cruce espacial de forma segura
  ext_inter <- terra::intersect(ext(r), ext(v))
  
  if (is.null(ext_inter)) {
    message("Sin cruce espacial: ", area_sf$area, " - ", year)
    return(NULL)
  }
  
  r_crop <- crop(r, v)
  r_mask <- mask(r_crop, v)
  
  names(r_mask) <- "clase"
  
  df <- as.data.frame(r_mask, xy = TRUE, na.rm = TRUE)
  
  if (nrow(df) == 0) return(NULL)
  
  df <- df %>%
    filter(clase %in% unname(clases_interes)) %>%
    mutate(
      clase_nom = names(clases_interes)[match(clase, clases_interes)],
      year = year,
      area = area_sf$area
    )
  
  if (nrow(df) == 0) return(NULL)
  
  df
}

# =============================
# 6. PROCESAR TODO
# =============================

df_all <- expand.grid(
  year = years,
  id_area = seq_len(nrow(areas)),
  KEEP.OUT.ATTRS = FALSE
) %>%
  mutate(
    data = map2(
      year,
      id_area,
      ~ procesar_area_anio(.x, areas[.y, ])
    )
  ) %>%
  select(data) %>%
  unnest(data)

# =============================
# 7. NORMALIZAR COORDENADAS PARA PANEL
# =============================

df_plot <- df_all %>%
  group_by(area) %>%
  mutate(
    x_norm = x - min(x, na.rm = TRUE),
    y_norm = y - min(y, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year = factor(year, levels = years),
    area = factor(area, levels = paste0("Área ", seq_len(nrow(areas))))
  )

# =============================
# 8. GRAFICAR
# =============================
nombres_areas <- c(
  "Área 1" = "Dist. Huapetue",
  "Área 2" = "Malinowski",
  "Área 3" = "Alto Huacamayo",
  "Área 4" = "C.P. Azul"
)

df_plot <- df_plot %>%
  mutate(
    area = recode(area, !!!nombres_areas),
    area = factor(
      area,
      levels = c(
        "Dist. Huapetue",
        "Malinowski",
        "Alto Huacamayo",
        "C.P. Azul"
      )
    )
  )

p <- ggplot(df_plot) +
  geom_raster(aes(x = x_norm, y = y_norm, fill = clase_nom)) +
  scale_fill_manual(
    values = paleta,
    name = "Cobertura"
  ) +
  facet_grid(year ~ area, switch = "y") +
  coord_equal() +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(size = 11, face = "bold", angle = 0),
    strip.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.18, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(8, 8, 8, 8)
  )+
  theme(
    strip.text.x = element_text(size = 12, face = "bold", family = "serif"),
    strip.text.y = element_text(size = 11, face = "bold", angle = 0),
  )

print(p)

# =============================
# 9. EXPORTAR
# =============================

ggsave(
  filename = file.path(dir_out, "Mapa_Mineria_Panel_Optimizado.png"),
  plot = p,
  width = 10,
  height = 12,
  dpi = 500,
  bg = "white"
)

