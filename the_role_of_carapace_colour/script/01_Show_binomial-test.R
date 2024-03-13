# Script to create graphical representations of results
# Author: Diogo Silva
# Wed Jun  8 17:19:15 2023 ------------------------------
# Last update:
# Fri Oct  6 18:31:04 2023 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)

#1. Import data ----------------------------------------------------------------
data_LNCN_agg <- read.csv("output/table/table_01_data_LNCN_agg.csv")
data_LCCA_agg <- read.csv("output/table/table_01_data_LCCA_agg.csv")
data_LCLA_agg <- read.csv("output/table/table_01_data_LCLA_agg.csv")
data_LACA_agg <- read.csv("output/table/table_01_data_LACA_agg.csv")
data_LPCB_agg <- read.csv("output/table/table_01_data_LPCB_agg.csv")
data_LBCP_agg <- read.csv("output/table/table_01_data_LBCP_agg.csv")

#2. Barplots -------------------------------------------------------------------

#2.1. LNCN. L. leptodactyla vs L. cumulanta (naturals) ----
p1 <- ggplot(data = data_LNCN_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black", stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('leptodactyla','cumulanta'),
                   labels = c(expression(italic('L. leptodactyla'
                                                )), 
                              expression(italic('L. cumulanta'))
                              )) +
  labs(x = "Treatments", y = "Female choice")+
  ylim(0, 63)+
  theme_test(base_size = 18)
p1

p1 <- ggdraw() + 
  draw_plot(p1) +
  draw_image(image = "image/Fiddler-draw_A1.png", 
             scale = 0.21,
             x = -0.114, 
             y = 0.38) +
  draw_image(image = "image/Fiddler-draw_A2.png",
             scale = 0.21,
             x = 0.26,
             y = -0.10)
p1

#2.2. LCCA. L. leptodactyla (grey claw) vs L. cumulanta (yellow claw) ----
p2 <- ggplot(data = data_LCCA_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black", stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LC','CA'),
                   labels = c(expression(italic('L. leptodactyla'
                   )), 
                   expression(italic('L. cumulanta'))
                   )) +
  labs(x = "Treatments", y = "Female choice")+
  ylim(0, 50)+
  theme_test(base_size = 18)
p2

p2 <- ggdraw() + 
  draw_plot(p2) +
  draw_image(image = "image/Fiddler-draw_B1.png", 
             scale = 0.21,
             x = -0.114, 
             y = 0.36) +
  draw_image(image = "image/Fiddler-draw_B2.png",
             scale = 0.21,
             x = 0.26,
             y = 0.1)
p2

#2.3. LCLA. leptodactyla yellow claw vs leptodactyla yellow claw ( both natural white carapaces) ----
p3 <- ggplot(data = data_LCLA_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LC','LA'),
                   labels = c(expression(italic('L. leptodactyla'
                   )), 
                   expression(italic('L. leptodactyla'))
                   )) +
  labs(x = "Treatments", y = "Female choice")+
  ylim(0, 45)+
  theme_test(base_size = 18)
p3

p3<- ggdraw() + 
  draw_plot(p3) +
  draw_image(image = "image/Fiddler-draw_C1.png", 
             scale = 0.21,
             x = -0.114, 
             y = 0.36) +
  draw_image(image = "image/Fiddler-draw_C2.png",
             scale = 0.21,
             x = 0.26,
             y = 0.2) #quanto menor, mais baixo
p3

#2.4. LACA. leptodactyla yellow claw vs cumulanta yellow claw (both natural carapaces) ----
p4 <- ggplot(data = data_LACA_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LA','CA'),
                   labels = c(expression(italic('L. leptodactyla'
                   )), 
                   expression(italic('L. cumulanta'))
                   )) +
  labs(x = "Treatments", y = "Female choice")+
  ylim(0, 40)+
  theme_test(base_size = 18)
p4

p4 <- ggdraw() + 
  draw_plot(p4) +
  draw_image(image = "image/Fiddler-draw_D1.png", 
             scale = 0.21,
             x = -0.114, 
             y = 0.32) +
  draw_image(image = "image/Fiddler-draw_D2.png",
             scale = 0.21,
             x = 0.26,
             y = -0.13)
p4

#2.5. LPCB. leptodactyla dark green painted carapace vs cumulanta white painted carapace (both yellow claw)----
p5 <- ggplot(data = data_LPCB_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LP','CB'),
                   labels = c(expression(italic('L. leptodactyla'
                   )), 
                   expression(italic('L. cumulanta'))
                   )) +
  labs(x = "Treatments", y = "Female choice")+
  ylim(0, 30)+
  theme_test(base_size = 18)
p5

p5<- ggdraw() + 
  draw_plot(p5) +
  draw_image(image = "image/Fiddler-draw_E1.png", 
             scale = 0.21,
             x = -0.114, 
             y = -0.08) +
  draw_image(image = "image/Fiddler-draw_E2.png",
             scale = 0.21,
             x = 0.26,
             y = 0.33)
p5

#2.6. LBCP. leptodactyla white painted carapace vs cumulanta dark green painted carapace (both yellow claw)----
p6 <- ggplot(data = data_LBCP_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LB','CP'),
                   labels = c(expression(italic('L. leptodactyla'
                   )), 
                   expression(italic('L. cumulanta'))
                   )) +
  labs(x = "Treatments", y = "Female choice")+
  ylim(0, 30)+
  theme_test(base_size = 18)
p6

p6 <- ggdraw() + 
  draw_plot(p6) +
  draw_image(image = "image/Fiddler-draw_E2.png", 
             scale = 0.21,
             x = -0.114, 
             y = 0.31) +
  draw_image(image = "image/Fiddler-draw_E1.png",
             scale = 0.21,
             x = 0.26,
             y = -0.06)
p6

#3. Plot grid ------------------------------------------------------------------
p_all <- plot_grid(p1, p2,p3,p4,p5, p6, ncol = 2, 
                   labels = "AUTO", 
                   align = "vh",
                   label_size = 20)
p_all

# 4. Save plot -----------------------------------------------------------------
ggsave(plot = p_all, 
       filename = "output/figure/Figure_3.png",
       width = 10, 
       height = 13, 
       dpi = 300)

# END----

