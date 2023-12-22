#Script to import and show reflectances
#autor: Diogo Silva
#data: Fri Sep  8 21:48:26 2023 ------------------------------

#Last update
# Fri Sep  8 21:48:34 2023 ------------------------------

#packages ----
library(pavo)
library(tidyverse) 
library(cowplot)

library(colorspec)
# to use the function fixpec() you need to install the 'colorspec' package using the code: remotes::install_github("Diogojackson/colorspec/colorspec")
#remotes::install_github("Diogojackson/colorspec/colorspec")

#1. import data ----

#1.1. Importing natural reflectance data from Leptuca leptodactyla CTA ----
yellow <- getspec("data/raw/reflectances/yellow", ext = c('procspec','txt'),lim = c(300, 700), decimal = ',')
yellow <- fixspec(yellow)
plot(yellow)

grey <- getspec("data/raw/reflectances/grey", ext = c('procspec','txt'),lim = c(300, 700), decimal = ',')
grey <- fixspec(grey)
plot(grey)

green <- getspec("data/raw/reflectances/green", ext = c('procspec','txt'),lim = c(300, 700), decimal = ',')
green <- fixspec(green)
plot(green)

white <- getspec("data/raw/reflectances/white", ext = c('procspec','txt'),lim = c(300, 700), decimal = ',')
white <- fixspec(white)
plot(white)

lep_mjo <- read.csv("data/raw/reflectances/leptodactyla_and_mjoebergi.csv")

#2. Plots ----

#2.1. Colors comparation (natural vs paint) ----
p1 <- ggplot(yellow, aes(wl))+
  geom_line(aes(y = yellow_paint, linetype = "Paint"), color = "#ffcf00", linewidth = 0.7)+
  geom_line(aes(y = yellow_natural, linetype = "Natural"), color = "black", linewidth = 0.7)+
  ylim(0, 100)+
  labs(title = expression(bold("Yellow claw")),
        x = "Wavelenght (nm)",
        y = "Reflectance (%)")+
  theme_test(base_size = 8)+
  scale_linetype_manual(values = c("Paint" = "dashed", "Natural" = "solid")) +
  theme(legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.position = c(0.70, 0.8),
        plot.title = element_text(size = 8))+
  guides(linetype = guide_legend(title = NULL))
p1

#2.2. Grey ----
p2 <- ggplot(grey, aes(wl))+
  geom_line(aes(y = grey_natural), color = "black", linewidth = 0.7, linetype = 1)+
  geom_line(aes(y = grey_paint), color = "grey25", linewidth = 0.7, linetype = 2)+
  ylim(0, 100)+
  labs(title = expression(bold("Grey claw")),
       x = "Wavelenght (nm)",
       y = "Reflectance (%)")+
  theme_test(base_size = 8)+
  theme(plot.title = element_text(size = 8))
p2

#2.3. Dark green ----
p3 <- ggplot(green, aes(wl))+
  geom_line(aes(y = green_natural), color = "black", linewidth = 0.7, linetype = 1)+
  geom_line(aes(y = green_paint), color = "#0c6834", linewidth = 0.7, linetype = 2)+
  ylim(0, 100)+
  labs(title = expression(bold("Dark green carapace")),
       x = "Wavelenght (nm)",
       y = "Reflectance (%)")+
  theme_test(base_size = 8)+
  theme(plot.title = element_text(size = 8))
p3

#2.4. White ----
p4 <- ggplot(white, aes(wl))+
  geom_line(aes(y = white_natural), color = "black", linewidth = 0.7, linetype = 1)+
  geom_line(aes(y = white_paint), color = "grey", linewidth = 0.7, linetype = 2)+
  ylim(0, 102)+
  labs(title = expression(bold("White carapace")),
       x = "Wavelenght (nm)",
       y = "Reflectance (%)")+
  theme_test(base_size = 8)+
  theme(plot.title = element_text(size = 8))
p4

#2.5. L. leptodactyla vs A. mjoebergi ----
ll_am <- ggplot(lep_mjo, aes(wl)) +
  geom_line(aes(y = mean_mjoebergi, color = "A. mjoebergi"), linewidth = 1) +
  geom_ribbon(aes(ymin = mean_mjoebergi - sd_mjoebergi, ymax = mean_mjoebergi + sd_mjoebergi, fill = "A. mjoebergi"), alpha = 0.5, show.legend = F) +
  geom_line(aes(y = mean_leptodactyla, color = "L. leptodactyla"), linewidth = 0.7) +
  geom_ribbon(aes(ymin = mean_leptodactyla - sd_leptodactyla, ymax = mean_leptodactyla + sd_leptodactyla, fill = "L. leptodactyla"), alpha = 0.5,show.legend = F) +
  ylim(0, 100) +
  labs(x = "Wavelength (nm)",
       y = "Reflectance (%)",
       color = "Species"
  ) +
  scale_color_manual(
    values = c("A. mjoebergi" = "#ffcf00", "L. leptodactyla" = "#98286f"),
    labels = c(expression(italic("Austruca mjoebergi"),italic("Leptuca leptodactyla"))))+
  scale_fill_manual(
    values = c("A. mjoebergi" = "#fef580", "L. leptodactyla" = "#f5bed9"),
    labels = c("A. mjoebergi", "L. leptodactyla"))+
  theme_test()+
  theme(legend.position = "top")
ll_am


#3. Unite graphs ----
plot <- plot_grid(p1, p2, p3, p4, ncol = 2, 
                     labels = "AUTO", 
                     align = "vh",
                     label_size = 8)
plot

#4. Save ---

#4.1 Paint comparations ----

ggsave(plot = plot, 
       filename = "output/figure/Figure_2.png",
       width = 4, 
       height = 3, 
       dpi = 300)

#4.2. leptodactyla vs mjoebergi comparation ----
ggsave(plot = ll_am, 
       filename = "output/figure/Supplementar_1.png",
       width = 4, 
       height = 3, 
       dpi = 300)

# FIM ---------------------------------------------------------------------
#