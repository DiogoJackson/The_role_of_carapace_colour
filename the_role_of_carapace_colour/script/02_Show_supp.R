# Grafico da distribuicao simpatrica do LL e LC
# Author: Diogo J. A. Silva
# Wed Mar 20 20:27:15 2024 ------------------------------

# Packages
library(geobr)
library(ggplot2)
library(ggspatial)
library(cowplot)

# Geo data ----
brazil <- read_state(code_state = "all")
rn <- read_state(code_state = "RN")

# Plot map ----
mapa_brasil <- ggplot() +
  geom_sf(data = brazil, fill = "white", color = "black") +
  geom_sf(data = rn, fill = "#d9d99b", color = "black")+
  theme_minimal() +
  labs(title = "Brazil")+
  annotation_north_arrow()
mapa_brasil

mapa_rn <- ggplot() +
  geom_sf(data = rn, fill = "#d9d99b", color = "black") +
  theme_minimal() +
  labs(title = "State of Rio Grande do Norte")+
  annotation_north_arrow()
mapa_rn

#add localization icon
mapa_rn <- ggdraw() + 
  draw_plot(mapa_rn) +
  draw_image(image = "image/localization.png", 
             scale = 0.1,
             x = 0.38, 
             y = 0.1)
mapa_rn

p <- plot_grid(mapa_brasil, mapa_rn, 
               align = "vh",
               labels = "AUTO",
               ncols = 1)

p <- ggdraw() + 
  draw_plot(p) +
  draw_image(image = "image/Suplementar_2.png.", 
             scale = 0.5,
             x = -0.25, 
             y = -0.25)
p

#Save plot ----
ggsave(plot = p, 
       filename = "output/figure/S2.png",
       width = 10, 
       height = 10, 
       dpi = 300)


# THE END -----------------------------------------------------------------
