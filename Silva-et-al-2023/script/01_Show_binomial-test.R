# Script to build the graphics of results
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
  geom_bar(color = "black", stat = "identity", show.legend = F, width = 0.7) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('leptodactyla','cumulanta'),
                   labels = c(expression(italic('L. leptodactyla'
                                                )), 
                              expression(italic('L. cumulanta'))
                              )) +
  labs(x = "Treatments", y = "Female choice", subtitle = 'p < 0.001')+
  ylim(0, 60)+
  theme_test(base_size = 18)
p1

#2.2. LCCA. L. leptodactyla quela Cinza vs L. cumulanta quela amarelo ----
p2 <- ggplot(data = data_LCCA_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black", stat = "identity", show.legend = F, width = 0.7) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LC','CA'),
                   labels = c('LC','CA')) +
  labs(x = "Treatments", y = "Female choice", subtitle = 'p < 0.05')+
  ylim(0, 50)+
  theme_test(base_size = 18)
p2

#2.3. LCLA. leptodactyla quela amarelo vs leptodactyla quela amarelo (carapacas brancos naturais) ----
p3 <- ggplot(data = data_LCLA_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.7) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LC','LA'),
                   labels = c('LC','LA')) +
  labs(x = "Treatments", y = "Female choice", subtitle = 'p > 0.05')+
  ylim(0, 45)+
  theme_test(base_size = 18)
p3

#2.4. LACA. leptodactyla quela amarelo vs cumulanta quela amarelo (carapacas naturais) ----
p4 <- ggplot(data = data_LACA_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.7) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LA','CA'),
                   labels = c('LA','CA')) +
  labs(x = "Treatments", y = "Female choice", subtitle = 'p < 0.001')+
  ylim(0, 40)+
  theme_test(base_size = 18)
p4

#2.5. LPCB. leptodactyla carapaca Preto vs cumulanta branco (quelas pintadas de amarelo)----
p5 <- ggplot(data = data_LPCB_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.7) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LP','CB'),
                   labels = c('LP','CB')) +
  labs(x = "Treatments", y = "Female choice", subtitle = 'p < 0.01')+
  ylim(0, 30)+
  theme_test(base_size = 18)
p5

#2.6. LBCP. leptodactyla carapaca Branco vs cumulanta preto (quelas pintadas de amarelo)----
p6 <- ggplot(data = data_LBCP_agg, aes(x = species, y = female_choice, fill = species)) +
  geom_bar(color = "black",stat = "identity", show.legend = F, width = 0.7) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#e9e6dd", "#e9e6dd")) +
  scale_x_discrete(limits = c('LB','CP'),
                   labels = c('LB','CP')) +
  labs(x = "Treatments", y = "Female choice", subtitle = 'p < 0.01')+
  ylim(0, 30)+
  theme_test(base_size = 18)
p6

#3. Plot grid ------------------------------------------------------------------
p_all <- plot_grid(p1, p2,p3,p4,p5, p6, ncol = 2, 
                   labels = "AUTO", 
                   align = "vh",
                   label_size = 20)
p_all

# 4. Save plot -----------------------------------------------------------------
ggsave(plot = p_all, 
       filename = "output/figure/Fig_02_female-choices.png",
       width = 10, 
       height = 13, 
       dpi = 300)

# END----
