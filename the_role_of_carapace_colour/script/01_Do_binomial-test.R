# Script to analyze processed data
# Author: Diogo Silva
# Wed Jun  7 17:19:15 2023 ------------------------------
# Last update:
# Fri Oct  6 18:17:39 2023 ------------------------------

#Packages
library(tidyverse)
library(sjPlot)

#1. Binomial analysis ----
#1.1 #All ll and lc in natural colorations ----
data_LNCN <- read.csv("data/processed/processed_data_natural.csv")
data_LNCN_agg <- aggregate(female_choice ~ species, data = data_LNCN, FUN = sum)

#51 = Event with the highest observed count.
#60 = Total number of observations.

binomial_1 <- binom.test(51,60, alternative = "greater") 
binomial_1                      

#1.2 ll (Claw painted in grey, and carapace without paint) x lc (Claw painted in yellow, and carapace without paint)
data_LCCA <- read.csv("data/processed/processed_data_LCxCA.csv")
data_LCCA_agg <- aggregate(female_choice ~ species, data = data_LCCA, FUN = sum)
data_LCCA_agg

binomial_2 <- binom.test(39,60, alternative = "greater")
binomial_2                      

#1.3 ll (Claw painted in grey, and carapace without paint) x ll (Claw painted in yellow, and carapace without paint)
data_LCLA <- read.csv("data/processed/processed_data_LCxLA.csv")
data_LCLA_agg <- aggregate(female_choice ~ species, data = data_LCLA, FUN = sum)
data_LCLA_agg

binomial_3 <- binom.test(35,60, alternative = "greater")
binomial_3

#1.4 ll (Claw painted in yellow, and carapace without paint) x lc (Claw painted in yellow, and carapace without paint)
data_LACA <- read.csv("data/processed/processed_data_LAxCA.csv")
data_LACA_agg <- aggregate(female_choice ~ species, data = data_LACA, FUN = sum)
data_LACA_agg

binomial_4 <- binom.test(29,33, alternative = "greater")
binomial_4

#1.5 ll (Claw painted in yellow, and carapace painted dark green) x lc (Claw painted in yellow, and carapace painted white)
data_LPCB <- read.csv("data/processed/processed_data_LPxCB.csv")
data_LPCB_agg <- aggregate(female_choice ~ species, data = data_LPCB, FUN = sum)
data_LPCB_agg

binomial_5 <- binom.test(22,27, alternative = "greater")
binomial_5

#1.6 ll (Claw painted in yellow, and carapace painted white) x lc (Claw painted in yellow, and carapace painted dark green)
data_LBCP <- read.csv("data/processed/processed_data_LBxCP.csv")
data_LBCP_agg <- aggregate(female_choice ~ species, data = data_LBCP, FUN = sum)
data_LBCP_agg

binomial_6 <- binom.test(21,27, alternative = "greater")
binomial_6

# 2. Save tables----------------------------------------------------------------
write.csv(x = data_LNCN_agg, 
          file = "output/table/table_01_data_LNCN_agg.csv",
          row.names = FALSE)

write.csv(x = data_LCCA_agg, 
          file = "output/table/table_01_data_LCCA_agg.csv",
          row.names = FALSE)

write.csv(x = data_LCLA_agg, 
          file = "output/table/table_01_data_LCLA_agg.csv",
          row.names = FALSE)

write.csv(x = data_LACA_agg, 
          file = "output/table/table_01_data_LACA_agg.csv",
          row.names = FALSE)

write.csv(x = data_LPCB_agg, 
          file = "output/table/table_01_data_LPCB_agg.csv",
          row.names = FALSE)

write.csv(x = data_LBCP_agg, 
          file = "output/table/table_01_data_LBCP_agg.csv",
          row.names = FALSE)

#Data test ----
test_data1 <- read.csv("output/table/table_01_data_LNCN_agg.csv")
test_data2 <- read.csv("output/table/table_01_data_LCCA_agg.csv")
test_data3 <- read.csv("output/table/table_01_data_LCLA_agg.csv")
test_data4 <- read.csv("output/table/table_01_data_LACA_agg.csv")
test_data5 <- read.csv("output/table/table_01_data_LPCB_agg.csv")
test_data6 <- read.csv("output/table/table_01_data_LBCP_agg.csv")

test_data1
test_data2
test_data3
test_data4
test_data5
test_data6

#----------------------------END------------------------------------------------
