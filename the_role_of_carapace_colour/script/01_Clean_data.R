# Script to load and clean raw dataset
# Author: Diogo Jackson de Aquino Silva 
# Last update:
# Fri Oct  6 17:45:11 2023 ------------------------------

# Packages ----
library(tidyverse)
library(inspectdf)
library(forcats)
library(readxl)

# Loading data -------------------------------------------------------
# ll = Leptuca leptodactyla
# lc = Leptuca cumulanta

#All ll and lc in natural colorations ----
data_naturais <- read_excel("data/raw/raw_data.xlsx", 
                            sheet = "Naturais") 

#ll (Claw painted in grey, and carapace without paint) x lc (Claw painted in yellow, and carapace without paint)
data_LCxCA <- read_excel("data/raw/raw_data.xlsx", 
                            sheet = "LCxCA")    

#ll (Claw painted in grey, and carapace without paint) x ll (Claw painted in yellow, and carapace without paint)
data_LCxLA <- read_excel("data/raw/raw_data.xlsx", 
                            sheet = "LCxLA")

#ll (Claw painted in yellow, and carapace without paint) x lc (Claw painted in yellow, and carapace without paint)
data_LAxCA <- read_excel("data/raw/raw_data.xlsx", 
                            sheet = "LAxCA")

#ll (Claw painted in yellow, and carapace painted dark green) x lc (Claw painted in yellow, and carapace painted white)
data_LPxCB <- read_excel("data/raw/raw_data.xlsx", 
                            sheet = "LPxCB")

#ll (Claw painted in yellow, and carapace painted white) x lc (Claw painted in yellow, and carapace painted dark green)
data_LBxCP <- read_excel("data/raw/raw_data.xlsx", 
                            sheet = "LBxCP")

# 1. Basic checks ----------------------------------------------------

str(data_naturais)
str(data_LCxCA) 
str(data_LCxLA) 
str(data_LAxCA) 
str(data_LPxCB) 
str(data_LBxCP) 

str(data_naturais)
str(data_LCxCA) 
str(data_LCxLA) 
str(data_LAxCA) 
str(data_LPxCB) 
str(data_LBxCP) 

any(duplicated(data_naturais))
any(duplicated(data_LCxCA))
any(duplicated(data_LCxLA))
any(duplicated(data_LAxCA))
any(duplicated(data_LPxCB))
any(duplicated(data_LBxCP))

any(is.na(data_naturais))
any(is.na(data_LCxCA))
any(is.na(data_LCxLA))
any(is.na(data_LAxCA))
any(is.na(data_LPxCB))
any(is.na(data_LBxCP)) #first choice was not registered, it's ok!

#2. Fix variables ----

# Naturals
data_naturais$pair                <- as.factor(data_naturais$pair)
data_naturais$species             <- as.factor(data_naturais$species)
data_naturais$female_choice       <- as.numeric(data_naturais$female_choice)
data_naturais$first_choice        <- as.numeric(data_naturais$first_choice)
data_naturais$carapace_length_mm  <- as.numeric(data_naturais$carapace_length_mm)
data_naturais$claw_length_mm      <- as.numeric(data_naturais$claw_length_mm)

# LCxCA
data_LCxCA$pair                   <- as.factor(data_LCxCA$pair)
data_LCxCA$species                <- as.factor(data_LCxCA$species)
data_LCxCA$female_choice          <- as.numeric(data_LCxCA$female_choice)
data_LCxCA$first_choice           <- as.numeric(data_LCxCA$first_choice)
data_LCxCA$carapace_length_mm     <- as.numeric(data_LCxCA$carapace_length_mm)
data_LCxCA$claw_length_mm         <- as.numeric(data_LCxCA$claw_length_mm)

# LCxLA
data_LCxLA$pair                   <- as.factor(data_LCxLA$pair)
data_LCxLA$species                <- as.factor(data_LCxLA$species)
data_LCxLA$female_choice          <- as.numeric(data_LCxLA$female_choice)
data_LCxLA$first_choice           <- as.numeric(data_LCxLA$first_choice)
data_LCxLA$carapace_length_mm     <- as.numeric(data_LCxLA$carapace_length_mm)
data_LCxLA$claw_length_mm         <- as.numeric(data_LCxLA$claw_length_mm)

# LAxCA
data_LAxCA$pair                   <- as.factor(data_LAxCA$pair)
data_LAxCA$species                <- as.factor(data_LAxCA$species)
data_LAxCA$female_choice          <- as.numeric(data_LAxCA$female_choice)
data_LAxCA$first_choice           <- as.numeric(data_LAxCA$first_choice)
data_LAxCA$carapace_length_mm     <- as.numeric(data_LAxCA$carapace_length_mm)
data_LAxCA$claw_length_mm         <- as.numeric(data_LAxCA$claw_length_mm)

# LPxCB
data_LPxCB$pair                   <- as.factor(data_LPxCB$pair)
data_LPxCB$species                <- as.factor(data_LPxCB$species)
data_LPxCB$female_choice          <- as.numeric(data_LPxCB$female_choice)
data_LPxCB$first_choice           <- as.numeric(data_LPxCB$first_choice)
data_LPxCB$carapace_length_mm     <- as.numeric(data_LPxCB$carapace_length_mm)
data_LPxCB$claw_length_mm         <- as.numeric(data_LPxCB$claw_length_mm)

# LBxCP
data_LBxCP$pair                   <- as.factor(data_LBxCP$pair)
data_LBxCP$species                <- as.factor(data_LBxCP$species)
data_LBxCP$female_choice          <- as.numeric(data_LBxCP$female_choice)
data_LBxCP$first_choice           <- as.numeric(data_LBxCP$first_choice)
data_LBxCP$carapace_length_mm     <- as.numeric(data_LBxCP$carapace_length_mm)
data_LBxCP$claw_length_mm         <- as.numeric(data_LBxCP$claw_length_mm)

# 3. Rename column names ------------------------------------------------
dat_natural <- dplyr::rename(data_naturais,
                             scape_side = runnaway_side)

dat_LCxCA <- dplyr::rename(data_LCxCA,
                           scape_side = runnaway_side)

dat_LCxLA <- dplyr::rename(data_LCxLA,
                           scape_side = runnaway_side)

dat_LAxCA <- dplyr::rename(data_LAxCA,
                           scape_side = runnaway_side)

dat_LPxCB <- dplyr::rename(data_LPxCB,
                           scape_side = runnaway_side)

dat_LBxCP <- dplyr::rename(data_LBxCP,
                           scape_side = runnaway_side)

# 4. Save processed data ----

#data_naturais
write.csv(x = dat_natural, 
          file = "data/processed/processed_data_natural.csv", 
          row.names = FALSE)

#data_LCxCA
write.csv(x = dat_LCxCA, 
          file = "data/processed/processed_data_LCxCA.csv", 
          row.names = FALSE)

#data_LCxLA
write.csv(x = dat_LCxLA, 
          file = "data/processed/processed_data_LCxLA.csv", 
          row.names = FALSE)

#data_LAxCA
write.csv(x = dat_LAxCA, 
          file = "data/processed/processed_data_LAxCA.csv", 
          row.names = FALSE)

#data_LPxCB
write.csv(x = dat_LPxCB, 
          file = "data/processed/processed_data_LPxCB.csv", 
          row.names = FALSE)

#data_LBxCP
write.csv(x = dat_LBxCP, 
          file = "data/processed/processed_data_LBxCP.csv", 
          row.names = FALSE)

# Test saved data
dat_test1 <- read.csv("data/processed/processed_data_natural.csv")
dat_test2 <- read.csv("data/processed/processed_data_LCxCA.csv")
dat_test3 <- read.csv("data/processed/processed_data_LCxLA.csv")
dat_test4 <- read.csv("data/processed/processed_data_LAxCA.csv")
dat_test5 <- read.csv("data/processed/processed_data_LPxCB.csv")
dat_test6 <- read.csv("data/processed/processed_data_LBxCP.csv")

# FIM ---------------------------------------------------------------------


