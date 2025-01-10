# ******************************************************************************
# Project: PHD thesis og DA10P
# Actions: 
# Author:
# 
# CREATED: 20240611
# UPDATED: 20240611
# ******************************************************************************
# PREP

# libs
library(tidyverse)
library(readxl)
library(stringr)
# install.packages("ggpubr")
# library(gridExtra)
# library(ggpubr)
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# GPT hjælp specifikt for dette: Se noten [[PHAID DDD date notes help]]
  # KONKLUSION: brug min værdierne for hver måned.. vil sammenligne på det billigste grundlag.. for at være fair mod de dyre og de billige.
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# ******************************************************************************

# LOAD -------------------------------------------------------------------------
df_ddd <- read_excel("G:/My Drive/1. Job/1. Phd/16-phdthesis/data/lmpriser_eSundhed_240805.xlsx", sheet = "lmpriser_eSundhed_240805")

# CLEAN ------------------------------------------------------------------------

# Goal: only A10B and the columns i want
df_ddd_A10B <- df_ddd |> 
  filter(
    Indikator == "AUP_pr_DDD", # vil kun have DDD lige nu
    str_detect(ATC,("^A10B"))) |> 
  select(ATC, Indholdsstof, Lægemiddel, starts_with("20")) |>
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Tid",
    values_to = "aupddd") |> 
  mutate(
    Tid = ymd(Tid)) |>
  # group_by(tid) |> 
  # mutate(
  #   hip_ddd = max(aupddd), # hip_ddd = maxpris per ddd
  #   lop_ddd = min(aupddd) # lop_ddd = minpris per ddd
  # ) |> 
  # ungroup() |> 
  filter(
    !is.na(aupddd)
  )

# Make a new var thats floored to months and find the min for each month
df_ddd_clean <- df_ddd_A10B |> 
  mutate(month = floor_date(Tid,"month")) |> 
  group_by(month, ATC, Indholdsstof) |> 
  summarise(
    amin_aupddd = min(aupddd, na.rm = TRUE), # amin is to make it first in the facetwrap later 
    max_aupddd = max(aupddd, na.rm = TRUE)) |> 
  ungroup()

# Lav et mindre med kun metformin, sema, og empa
df_ddd_clean_lite <- df_ddd_clean |> 
  filter(
    str_detect(ATC,"^A10BA") | 
      str_detect(ATC,"^A10BK") | 
      str_detect(ATC,"^A10BJ") |
      str_detect(ATC,"^A10BH"))

# Kun Metformin
df_ddd_clean_lite_m <- df_ddd_clean |> 
  filter(str_detect(Indholdsstof,"Metformin"))

# PLOT ------------------------------------------------------------------------
#
# ggplot(df_ddd_clean_lite_m) +
#   geom_line(aes(x = month, y = amin_aupddd, colour = Indholdsstof)) + # amin is to make it first in the facetwrap later
#   geom_line(aes(x = month, y = max_aupddd, colour = Indholdsstof))
#
# # one more complex one
# ggplot(df_ddd_clean_lite) +
#   geom_line(aes(x = month, y = amin_aupddd, colour = Indholdsstof)) +
#   geom_line(aes(x = month, y = max_aupddd, colour = Indholdsstof))

# kør dette forbi GPT. Lav to akser.. så man kan kende forskel på min og max

# # Two different
# plotmin <- ggplot(df_ddd_clean_lite) +
#   geom_line(aes(x = month, y = min_aupddd, colour = Indholdsstof), linewidth = 0.8) +
#   labs(title = "Minimum AUP per DDD Over Time",
#        x = "",
#        y = "AUP per DDD",
#        colour = "Drug") 
# plotmax <- ggplot(df_ddd_clean_lite) +
#   geom_line(aes(x = month, y = max_aupddd, colour = Indholdsstof), linewidth = 0.8) +
#   labs(title = "Maximum AUP per DDD Over Time",
#        x = "",
#        y = "",
#        colour = "Drug") 
# legend_b <- get_legend(plotmin + theme(legend.position="bottom"))

# lay <- rbind(c(1,1,1,2,2,2,2),
#              c(1,1,1,2,2,2,2),
#              c(1,1,1,2,2,2,2))
# grid.arrange(plotmin, plotmax, nrow = 2, ncol = 2)

# a not good alternative to below graph
ggplot(df_ddd_clean_lite) +
  geom_line(aes(x = month, y = amin_aupddd, colour = Indholdsstof, linetype = "Min Value"), linewidth = 0.8) +
  #geom_point(aes(x = month, y = min_aupddd, colour = Indholdsstof), shape = 16, size = 2) +
  geom_line(aes(x = month, y = max_aupddd, colour = Indholdsstof, linetype = "Max Value"), linewidth = 0.8) +
  #geom_point(aes(x = month, y = max_aupddd, colour = Indholdsstof), shape = 17, size = 2) +
  scale_linetype_manual(name = "Value Type", values = c("Min Value" = "solid", "Max Value" = "dashed")) +
  labs(title = "Minimum and Maximum Values Over Time",
       x = "",
       y = "AUP per DDD",
       colour = "Drug",
       linetype = "Value Type")

# THIS GRAPH --------------------------------------------------------------


# Facet wrap it...
df_ddd_clean_lite_long <- df_ddd_clean_lite |> 
  pivot_longer(
    cols = ends_with("aupddd"),
    names_to = "ddd",
    values_to = "minmax"
  ) |> arrange(minmax)
df_ddd_clean_lite_long

# prep colour scheme for drugs

# sglt2_drugs <- c("Canagliflozin", "Dapagliflozin", "Empagliflozin", "Ertugliflozin")
# glp1_drugs <- c("Dulaglutid", "Liraglutid", "Exenatid", "Lixisenatid", "Semaglutid")
# dpp4_drugs <- c("Linagliptin", "Vildagliptin", "Sitagliptin", "Alogliptin")
# metformin <- c("Metformin")

color_mapping <- c(
  # SGLT2 Inhibitors (Oranges)
  "Canagliflozin" = "orange1",   # Dark Orange
  "Dapagliflozin" = "orange3",   # Orange
  "Empagliflozin" = "tan1",   # Light Orange
  "Ertugliflozin" = "tan4",   # Tomato

  # GLP-1 Receptor Agonists (Blues)
  # "Dulaglutid" = "red1",
  # "Exenatid" = "red3",
  # "Lixisenatid" = "firebrick",
  # "Liraglutid" = "brown1",
  # "Semaglutid" = "darkred",
  "Dulaglutid" = "lightblue",      # Dodger Blue
  "Exenatid" = "steelblue",        # Sky Blue
  "Lixisenatid" = "dodgerblue",     # Steel Blue
  "Liraglutid" = "mediumpurple",      # Deep Sky Blue
  "Semaglutid" = "purple",      # Royal Blue

  # DPP4
  "Linagliptin" = "green1",
  "Vildagliptin" = "green3",
  "Sitagliptin" = "seagreen",
  "Alogliptin" = "seagreen2",

  # Metformin
  "Metformin" = "black"        # Forest Green
)

# color_mapping <- c(
#   dpp4_colours   <- colorRampPalette(c("pink","darkred"))(5) # NB THIS WORKS BECAUSE THE FIRST 5 ARE DPP4, 6 are GLP1, etc etc.
#   glp1_colours   <- colorRampPalette(c("lightblue1","darkblue"))(6)
#   sglt2_colours  <- colorRampPalette(c("yellow","orange4"))(4)
#   met_colour  <- c("darkgreen")
# )
#demo("colors") for at se alle farverne

# Graph
# pdf(file="DDDplot_thesis2.pdf")
ggplot(df_ddd_clean_lite_long, aes(x = month, y = minmax, colour = Indholdsstof)) +
  geom_line() +
  facet_wrap(~ ddd, scales = "free") +
  scale_colour_manual(values = color_mapping) +
  # scale_x_date(
  #   date_breaks = "1 year",
  #   date_labels = "%Y",
  #   expand = c(0, 0)
  # ) +
  scale_x_continuous(breaks = seq.Date(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "year"),
                     labels = format(seq.Date(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "year"), "%Y")) +
  labs(title = "Minimum (left) and Maximum (right) Pharmacy Retail Price per DDD, in DKK",
       x = "",
       y = "",
       colour = "Drug") +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(strip.text = element_blank()) + 
  theme(
    axis.text=element_text(size=15), 
    legend.text = element_text(size=15),
    title = element_text(size=14))

# dev.off()
ggsave("DDDplot_thesis.png",
       bg="white", 
       scale =0.5,
       dpi = 300)

## HELP WITH PRICE DIFFERENCE RATIO ------------------------------------------

pricedif <- df_ddd_clean_lite_long |> 
  group_by(ddd,Indholdsstof) |> 
  summarise(min = min(minmax), max = max(minmax)) |> 
  arrange(min, max) |> 
  ungroup() 

print(pricedif, n = 40)

# Jeg kan godt lide grafen ovenfor. Måske DPP4 skulle med. Måske det skulle være bar graph

# Close log --------------------------------------------------------------------
# End of code ------------------------------------------------------------------
# 
# library(ggplot2)
# library(RColorBrewer)
# 
# # Example data
# data <- data.frame(
#   Class = rep(c("Class 1", "Class 2", "Class 3", "Class 4"), times = c(4, 4, 4, 1)),
#   Drug = c("Drug A1", "Drug A2", "Drug A3", "Drug A4", 
#            "Drug B1", "Drug B2", "Drug B3", "Drug B4",
#            "Drug C1", "Drug C2", "Drug C3", "Drug C4",
#            "Drug D1"),
#   Price = runif(13, 100, 500)
# )
# 
# # Assigning unique colors to each product
# # Here, we're using the 'Set3' palette from RColorBrewer which provides 12 distinct colors
# colors <- brewer.pal(12, "Set3")
# # Adding one more color for the 13th product
# colors <- c(colors, "#66C2A5")  # Adding an extra color for the 13th product
# 
# # Plot
# ggplot(data, aes(x = Drug, y = Price, fill = Drug)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = colors) +
#   theme_minimal() +
#   labs(title = "Drug Prices by Product",
#        x = "Drug",
#        y = "Price") +
#   theme(legend.title = element_blank())
