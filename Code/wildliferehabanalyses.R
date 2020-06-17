## ---------------------------
##
##  Script name: wildliferehabanalyses.R
##
##  Purpose of script: Analysis and figures from
##  "Population-level effects of wildlife rehabilitation and release vary with life-history strategy"
##
##  Authors:  James Paterson, Sue Carstairs, & Christina Davy
##
##  Date Created: 2020-06-17
##
##  Email: james.earle.paterson@gmail.com
##
## ---------------------------
##
## 
##  
## ---------------------------

## ----loadpackages-------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(tidyr)
library(kableExtra)

## ----loadvortexdata-------------------
# Load data (csv file with output summaries for all scenarios)
wr.data <- read.csv(file = "Data/wrvortexdata_clean.csv")
# Species included: Raccoon, Painted Turtle, Snapping Turtle, Blanding's Turtle, Little Brown Bat

# Ordering scenario name (important for colours and legends to properly line-up)
wr.data$scen.name2 <- factor(wr.data$scen.name2, levels = c("base", "+1% harvest",
                                                            "+2% harvest", "+5% harvest",
                                                            "+1% harvest +10% rehab","+1% harvest +25% rehab","+1% harvest +50% rehab",
                                                            "+2% harvest +10% rehab","+2% harvest +25% rehab", "+2% harvest +50% rehab",
                                                            "+5% harvest +10% rehab", "+5% harvest +25% rehab", "+5% harvest +50% rehab"))

## ----mortalityeffects---------------------
# Titles for plots
racc.label <- expression(paste("a) Raccoon"))
patu.label <- expression(paste("b) Painted Turtle"))
sntu.label <- expression(paste("c) Snapping Turtle"))
bltu.label <- expression(paste("d) Blanding's Turtle"))
lbba.label <- expression(paste("e) Little Brown Bat"))

# Load species' outlines for plots
patu.ima <- readPNG("Figures/outlines/patu_outline.png")
patu.g <- rasterGrob(patu.ima, interpolate = TRUE)

bltu.ima <- readPNG("Figures/outlines/leifso_bltu_silhouette.png")
bltu.g <- rasterGrob(bltu.ima, interpolate = TRUE)

sntu.ima <- readPNG("Figures/outlines/sntu_outline_dark.png")
sntu.g <- rasterGrob(sntu.ima, interpolate = TRUE)

racc.ima <- readPNG("Figures/outlines/racc_outline.png")
racc.g <- rasterGrob(racc.ima, interpolate = TRUE)

lbba.ima <- readPNG("Figures/outlines/myotis_outline.png")
lbba.g <- rasterGrob(lbba.ima, interpolate = TRUE)

# Painted Turtle plot: additive mortality from severe injury
figure2.patu.plot <- ggplot(wr.data[wr.data$species == "PATU" & wr.data$rehab == "none",], 
                            aes(Year, Nall)) +
  ggtitle(patu.label) +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_vline(aes(xintercept = 3*11.59), linetype = "dashed", col = "black") +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#000000","#CC79A7", "#E69F00", "#D55E00"),
                      labels = c("None", "+1%", "+2%", "+5%"),
                      name = "Change in adult\nsevere injury rate") +
  theme_classic() +
  annotation_custom(patu.g, xmin = -10, xmax = 40, ymin = 800, ymax = 1000) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1000))

# Snapping Turtle plot: additive mortality from severe injury
figure2.sntu.plot <- ggplot(wr.data[wr.data$species == "SNTU" & wr.data$rehab == "none",], 
                            aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*26.06), linetype = "dashed", col = "black") +
  ggtitle(sntu.label) +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#000000","#CC79A7", "#E69F00", "#D55E00"),
                      labels = c("None", "+1%", "+2%", "+5%"),
                      name = "Change in adult\nfatal injury rate") +
  annotation_custom(sntu.g, xmin = -5, xmax = 55, ymin = 800, ymax = 1000) +
  theme_classic() +
  theme(legend.position = "none") + # c(0.30, 0.77)
  coord_cartesian(ylim = c(0, 1000))
# figure2.sntu.plot

# Blanding's Turtle plot: additive mortality from severe injury
figure2.bltu.plot <- ggplot(wr.data[wr.data$species == "BLTU" & wr.data$rehab == "none",], 
                            aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*35.11), linetype = "dashed", col = "black") +
  ggtitle(bltu.label) +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#000000","#CC79A7", "#E69F00", "#D55E00"),
                      labels = c("None", "+1%", "+2%", "+5%"),
                      name = "Change in adult\nfatal injury rate") +
  theme_classic() +
  annotation_custom(bltu.g, xmin = -5, xmax = 55, ymin = 800, ymax = 1000) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1000))
# figure2.bltu.plot

# Raccoon plot: additive mortality from severe injury
figure2.racc.plot <- ggplot(wr.data[wr.data$species == "Racc" & wr.data$rehab == "none" ,], 
                            aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*2.98), linetype = "dashed", col = "black") +
  ggtitle(racc.label) +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#000000","#CC79A7", "#E69F00", "#D55E00"),
                      labels = c("None", "+1%", "+2%", "+5%"),
                      name = "Change in adult\nfatal injury rate") +
  annotation_custom(racc.g, xmin = 5, xmax = 60, ymax = 1050, ymin = 875) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1000))
# figure2.racc.plot

# Little Brown Bat plot: additive mortality from severe injury
figure2.lbba.plot <- ggplot(wr.data[wr.data$species == "LBBA" & wr.data$rehab == "none",], 
                            aes(Year, Nall)) +
  ggtitle(lbba.label) +
  geom_vline(aes(xintercept = 3*6.67), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#000000","#CC79A7", "#E69F00", "#D55E00"),
                      labels = c("None", "+1%", "+2%", "+5%"),
                      name = "Change in adult\nsevere injury rate") +
  annotation_custom(lbba.g, xmin = 5, xmax = 65, ymin = 800, ymax = 1000) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1000))
# figure2.lbba.plot

# Make a legend grob
legend <-lemon::g_legend(figure2.patu.plot + theme(legend.position='right',
                                                   legend.text=element_text(size=20),
                                                   legend.title=element_text(size=20)))

# Display Figure 2 in plotting window
gridExtra::grid.arrange(figure2.racc.plot, figure2.bltu.plot, 
                        figure2.patu.plot, figure2.lbba.plot,
                        figure2.sntu.plot, legend,
                        ncol = 2, nrow = 3)

# Save
# 1. Open file
png("Figures/Figure2_trajectory_adultmortality.png", width = 17.4, height = 23.4, units = "cm", res = 500)
# 2. Create plot
gridExtra::grid.arrange(figure2.racc.plot, figure2.bltu.plot, 
                        figure2.patu.plot, figure2.lbba.plot,
                        figure2.sntu.plot, legend,# the legend grob here
                        ncol = 2, nrow = 3)
# 3. Close the file
dev.off()

## ----createextinctiontable----
# Make a table with extinction probabilities for each scenario
pext.df <- wr.data %>%
  filter(Year == 200) # Just select the last year for every scenario

pext.df$rehab <- factor(pext.df$rehab, levels = c("none","+10%", "+25%", "+50%"))

# Display the % of simulations that when extinct (with no rehabilitation)
pext.df %>%
  filter(rehab == "none") %>%
  select(species, PExtant, group) %>%
  mutate(PercentExtinct = (1 - PExtant)*100) %>%
  arrange(species, PercentExtinct)

## ----raccrehabgraph-----------------------
# The effects of wildlife rehabilitation on population growth

# Figure 3. 3 plots per species showing effects of rehabilitation on simulations with 1, 2, and 5% severe injury rate
racc.1.plot <- ggplot(wr.data[wr.data$species == "Racc" & 
                                wr.data$group == "Racc +1% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*2.98), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = rev(c("#D55E00", "#0072B2", "#56B4E9", "#009E73")),
                      labels = rev(c("0% rehabbed", "+10% rehabbed", 
                                     "+25% rehabbed", "+50% rehabbed")),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position =  c(0.65, 0.325),
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# racc.1.plot

racc.2.plot <- ggplot(wr.data[wr.data$species == "Racc" & 
                                wr.data$group == "Racc +2% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*2.98), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position =  "hidden",
        axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# racc.2.plot

racc.5.plot <- ggplot(wr.data[wr.data$species == "Racc" & 
                                wr.data$group == "Racc +5% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*2.98), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden") +
  coord_cartesian(ylim = c(0, 1000))
# racc.5.plot

## ----paturehabgraph------
patu.1.plot <- ggplot(wr.data[wr.data$species == "PATU" & 
                                wr.data$group == "PATU +1% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*11.59), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# patu.1.plot

patu.2.plot <- ggplot(wr.data[wr.data$species == "PATU" & 
                                wr.data$group == "PATU +2% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*11.59), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# patu.2.plot

patu.5.plot <- ggplot(wr.data[wr.data$species == "PATU" & 
                                (wr.data$group == "PATU +5% harvest" | wr.data$group == "PATU +5%harvest"),],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*11.59), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# patu.5.plot

## ----blturehabgraph------
bltu.1.plot <- ggplot(wr.data[wr.data$species == "BLTU" & 
                                wr.data$group == "BLTU +1% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*35.11), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# bltu.1.plot

bltu.2.plot <- ggplot(wr.data[wr.data$species == "BLTU" & 
                                wr.data$group == "BLTU +2% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*35.11), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# bltu.2.plot

bltu.5.plot <- ggplot(wr.data[wr.data$species == "BLTU" & 
                                (wr.data$group == "BLTU +5% harvest" | wr.data$group == "BLTU +5%harvest"),],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*35.11), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# bltu.5.plot

## ----snturehabgraph-----------------------
sntu.1.plot <- ggplot(wr.data[wr.data$species == "SNTU" & 
                                wr.data$group == "SNTU +1% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*26.06), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position =  "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# sntu.1.plot

sntu.2.plot <- ggplot(wr.data[wr.data$species == "SNTU" & 
                                wr.data$group == "SNTU +2% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*26.06), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position =  "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# sntu.2.plot

sntu.5.plot <- ggplot(wr.data[wr.data$species == "SNTU" & 
                                wr.data$group == "SNTU +5% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*26.06), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# sntu.5.plot

## ----lbbarehabgraph------------------------
lbba.1.plot <- ggplot(wr.data[wr.data$species == "LBBA" & 
                                wr.data$group == "LBBA +1% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*6.67), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = rehab), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position =  "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# lbba.1.plot

lbba.2.plot <- ggplot(wr.data[wr.data$species == "LBBA" & 
                                wr.data$group == "LBBA +2% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*6.67), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position =  "hidden",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# lbba.2.plot

lbba.5.plot <- ggplot(wr.data[wr.data$species == "LBBA" & 
                                wr.data$group == "LBBA +5% harvest",],
                      aes(Year, Nall)) +
  geom_vline(aes(xintercept = 3*6.67), linetype = "dashed", col = "black") +
  geom_ribbon(aes(ymin = Nall-1.96*SE.Nall., ymax = Nall+1.96*SE.Nall., x = Year, group = scen.name), 
              alpha = 0.3) +
  geom_line(aes(colour = scen.name2)) +
  labs(y = "Population size", colour = "Scenario") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#56B4E9", "#009E73"),
                      labels = c("+1% mortality", "+10% adults rehabbed", 
                                 "+25% adults rehabbed", "+50% adults rehabbed"),
                      name = "Scenario") +
  theme_classic() +
  theme(legend.position = "hidden",
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1000))
# lbba.5.plot

## ----displayrehabplots----
# Scenario labels
scenario1.grob <- textGrob("+1% severe injury", 
                           gp = gpar(fontface="bold"), rot = 90, hjust = 0.5)
scenario2.grob <- textGrob("+2% severe injury", 
                           gp = gpar(fontface="bold"), rot = 90, hjust = 0.5)
scenario5.grob <- textGrob("+5% severe injury", 
                           gp = gpar(fontface="bold"), rot = 90, hjust = 0.5)

# Species labels (and one blank grob)
scenario.grob <- ggplot() +
  xlim(0,1) +
  ylim(0.25,0.75) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

racc.grob <- ggplot()+
  annotate("text", label = "Raccoon", x = 0.25, y = 0.5,
           colour = "black", fontface = 2, hjust = 0,
           size = 4) +
  annotation_custom(racc.g, xmin = 0.55, xmax = 1.05, ymax = 0.75, ymin = 0.25) +
  xlim(0,1) +
  ylim(0.25,0.75) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

patu.grob <- ggplot()+
  annotate("text", label = "Painted Turtle", x = 0.1, y = 0.5,
           colour = "black", fontface = 2, hjust = 0,
           size = 4) +
  annotation_custom(patu.g, xmin = 0.725, xmax = 0.875, ymax = 0.95, ymin = 0.05) +
  xlim(0,1) +
  ylim(0.25,0.75) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

sntu.grob <- ggplot()+
  annotate("text", label = "Snapping Turtle", x = 0.1, y = 0.5,
           colour = "black", fontface = 2, hjust = 0,
           size = 4) +
  annotation_custom(sntu.g, xmin = 0.80, xmax = 1.07, ymax = 1.05, ymin = -0.05) +
  xlim(0,1) +
  ylim(0.25,0.75) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

bltu.grob <- ggplot()+
  annotate("text", label = "Blanding's Turtle", x = 0.1, y = 0.5,
           colour = "black", fontface = 2, hjust = 0,
           size = 4) +
  annotation_custom(bltu.g, xmin = 0.85, xmax = 1.09, ymax = 0.95, ymin = 0.05) +
  xlim(0,1) +
  ylim(0.25,0.75) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

lbba.grob <- ggplot()+
  annotate("text", label = "Little Brown Bat", x = 0.1, y = 0.5,
           colour = "black", fontface = 2, hjust = 0,
           size = 4) +
  annotation_custom(lbba.g, xmin = 0.81, xmax = 1.04, ymax = 1.025, ymin = 0.1) +
  xlim(0,1) +
  ylim(0.25,0.75) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


# Display all plots in a grid for Figure 3
gridExtra::grid.arrange(scenario.grob, racc.grob, patu.grob, sntu.grob, bltu.grob, lbba.grob,
                        scenario1.grob, racc.1.plot, patu.1.plot, sntu.1.plot, bltu.1.plot, lbba.1.plot,
                        scenario2.grob, racc.2.plot, patu.2.plot, sntu.2.plot, bltu.2.plot, lbba.2.plot,
                        scenario5.grob, racc.5.plot, patu.5.plot, sntu.5.plot, bltu.5.plot, lbba.5.plot,
                        ncol = 6,
                        widths = c(0.25,1,1,1,1,1))

# Save
# 1. Open file
png("Figures/Figure3_rehab_trajectory.png", width = 11, height = 8, units = "in", res = 500)
# 2. Create plot
gridExtra::grid.arrange(scenario.grob, racc.grob, patu.grob, sntu.grob, bltu.grob, lbba.grob,
                        scenario1.grob, racc.1.plot, patu.1.plot, sntu.1.plot, bltu.1.plot, lbba.1.plot,
                        scenario2.grob, racc.2.plot, patu.2.plot, sntu.2.plot, bltu.2.plot, lbba.2.plot,
                        scenario5.grob, racc.5.plot, patu.5.plot, sntu.5.plot, bltu.5.plot, lbba.5.plot,
                        ncol = 6,
                        widths = c(0.25,1,1,1,1,1),
                        heights = c(0.25, 1, 1, 1))
# 3. Close the file
dev.off()

## ----persistenceheatmap----
# First need some re-shaping
persistence.table <- pext.df %>%
  mutate(newgroup = paste(species,scen.name2, sep = " "))  %>%
  filter(!(scen.name2 %in% c("+20% harvest", "+30% harvest"))) %>% # Remove 20% and 30% harvest (only simulated for Raccoon populations)
  select(species, scen.name2, PExtant) %>%
  spread(species, PExtant) %>%
  mutate(group = substr(scen.name2, 1, 4),
         rehab = substr(scen.name2, 13, 16)) %>%
  select(group, rehab, Racc, PATU, BLTU, SNTU, LBBA) %>%
  arrange(group, rehab)

persistence.table$rehab[persistence.table$rehab == ""] <- "none"

persistence.table$rehab <- factor(persistence.table$rehab,
                                  levels = c("+50%", "+25%","+10%", "none"))

persistence.table$group <- factor(persistence.table$group, 
                                  levels =  c("base", "+1% ", "+2% ", "+5% "))

persistence.table <- persistence.table %>% 
  arrange(group, rehab)

# Change names
names(persistence.table) <- c("Adult mortality", "Rehabilitation", 
                              "Raccoon", "Painted Turtle", "Blanding's Turtle",
                              "Snapping Turtle", "Little Brown Bat")

# Now rearrange to "long" format
persistence.table3 <- reshape2::melt(persistence.table)
persistence.table3$group <- paste(persistence.table3$'Adult mortality',
                                  persistence.table3$Rehabilitation,
                                  sep = " ")

rehab.labels.df <- data.frame(rehab.labels = rev(c("none",
                                                   "10%",
                                                   "25%",
                                                   "50%",
                                                   "none",
                                                   "10%",
                                                   "25%",
                                                   "50%",
                                                   "none",
                                                   "10%",
                                                   "25%",
                                                   "50%",
                                                   "none")),
                              group = unique(persistence.table3$group),
                              variable = "Raccoon")

mortality.labels.df <- data.frame(mortality.labels= c("none","+1%","","","",
                                                      "+2%","","",
                                                      "","+5%","","",
                                                      ""),
                                  group = unique(persistence.table3$group),
                                  variable = "Raccoon")

persistence.table3 <- left_join(persistence.table3,
                                mortality.labels.df,
                                by = c("group", "variable"))

persistence.table3 <- left_join(persistence.table3,
                                rehab.labels.df,
                                by = c("group", "variable"))

# Order high probability of persistence at top of heat map, low persistence at bottom right of figure
persistence.table3$group <- factor(persistence.table3$group,
                                   levels = c("+5%  none", "+5%  +10%", "+5%  +25%",  "+5%  +50%",
                                              "+2%  none", "+2%  +10%",  "+2%  +25%","+2%  +50%",
                                              "+1%  none", "+1%  +10%", "+1%  +25%",  "+1%  +50%",
                                              "base none"))

# change NA's to ""
persistence.table3$mortality.labels <- as.character(persistence.table3$mortality.labels)
persistence.table3$mortality.labels[is.na(persistence.table3$mortality.labels)] <- ""
persistence.table3$rehab.labels <- as.character(persistence.table3$rehab.labels)
persistence.table3$rehab.labels[is.na(persistence.table3$rehab.labels)] <- ""

# Re-order species
persistence.table3$variable <- factor(persistence.table3$variable,
                                      levels = c( "Raccoon",
                                                  "Painted Turtle",
                                                  "Snapping Turtle", 
                                                  "Blanding's Turtle",
                                                  "Little Brown Bat"))

# Add exintction prob (1 - persistence prob)
persistence.table3$ext.risk <- 1-persistence.table3$value

# Add ranges to help with formatting
x.range <- c(-0.5,5)
y.range <- c(1,14)

# ggplot
heatmap.plot <- ggplot(persistence.table3, aes(x = variable, y = group)) + # plot variable by group
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(label = round(persistence.table3$value, 2))) + # round to reduce long numbers
  scale_fill_gradient2(low = "red1",  mid = "white", high ="blue1", # scale from red (high extinction) to blue (low extinction)
                       midpoint = 0.5, 
                       space = "rgb",
                       guide = guide_colourbar(reverse = TRUE)) + 
  theme(panel.grid.major.x = element_blank(), # no gridlines
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill="white"), 
        axis.text.x = element_text(angle = 25, size = 12, 
                                   face = "bold", colour = "black",
                                   hjust = 0),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title=element_text(face = "bold", size = 12),
        legend.position = "bottom") + 
  geom_hline(yintercept  = 4.5, col = "black") +
  geom_hline(yintercept  = 8.5, col = "black") +
  geom_hline(yintercept  = 12.5, col = "black") +
  
  geom_vline(xintercept = -0.4, col = "black") +
  scale_x_discrete(name = "", position = "top")+
  # scale_y_discrete(name = "", labels = rehab.labels) +
  labs(fill="Probability of persistence") +
  geom_text(x = -1,# Set the position of the text to always be at '-1'
            hjust = 0,
            # size = 12,
            aes(label = mortality.labels)) +
  geom_text(x = 0,# Set the position of the text to always be at '0'
            hjust = 0,
            # size = 12,
            aes(label = rehab.labels)) +
  annotate("text", label = "Severe\ninjury\nrate", x = -0.75, y = 14.5,
           colour = "black", fontface = 2, hjust = 0.5) +
  annotate("text", label = "Rehabilitation", x = 0, y = 14,
           colour = "black", fontface = 2, hjust = 0.25) +
  coord_cartesian(clip = 'off',
                  xlim = x.range, ylim = y.range) +
  theme(plot.margin = unit(c(1,4,1,2), "lines"))
heatmap.plot

# Save plot
ggsave(heatmap.plot, file = "Figures/Figure4_persistenceheatmap.png",
       height = 17.4, width = 17.4, unit = "cm", dpi = 500)

# Save plot (.eps version)
ggsave(heatmap.plot, file = "Figures/Fig4.eps",
       height = 17.4, width = 17.4, unit = "cm", dpi = 500)
