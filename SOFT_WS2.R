# SOFT_WS_2.R
# Brigitte Desharnais, 2022-08-15

# If packages are not installed yet, run the following:
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("extrafont")
install.packages("viridis")
install.packages("ggridges")


# Import a text file (slide 8)
Data <- read.delim("Cook.txt", header = TRUE, sep = "\t")

# Import an Excel file (slide 10)
library(readxl)
Data <- read_excel("Cook.xlsx", sheet = "Dessert")

# Transform (filter) data (slide 12)
library(dplyr)
Data <- as_tibble(Data)
Dessert <- Data %>% filter(Ingredients != "Zucchini")

# Transform (arrange) data (slide 13)
library(tidyr)
Results <- read.delim("Results.txt", header = TRUE, sep = "\t")
Results <- as_tibble(Results)
Cleaned <- Results %>% pivot_wider(names_from = Matrix,
                                   values_from = Concentration)


# Visualizations - note that I am providing the code only so you have an idea of how to build these.
# Since you don't have the data that goes with it you can't run the script.
library(ggplot2)
library(extrafont)
library(viridis)
library(ggridges)

# Slide 15 - line graph
ggplot(data = Data2, aes(x = Year, y = Number.of.samples, color = Drug)) +
  geom_line(size = 2) +
  # A manual color scale is used to highlight only desired drugs.
  scale_color_manual(values = c("lightgrey", "#607ed6", "lightgrey", "#e65a37", "lightgrey", "lightgrey", "#60d0d6", "lightgrey", "lightgrey", "lightgrey")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Number of samples identified") +
  theme_light() +
  theme(axis.title = element_text(size = 20, family = "Century Gothic"),
        axis.text = element_text(size = 20, family = "Century Gothic"),
        legend.title = element_text(size = 20, family = "Century Gothic"),
        legend.text = element_text(size = 20, family = "Century Gothic"),
        panel.grid = element_line(colour = "#bdbdbd", size = 2),
        panel.border = element_rect(colour = "#bdbdbd", size = 2))

# Slide 15 - histogram
ggplot(data = GHB_DUID, aes(x = Conc)) +
  geom_histogram(binwidth = 20, colour="black", fill="#0dc9c9") +
  scale_x_continuous(name = expression("Concentration (mg/L)"), expand = c(0,0), limits = c(0, 450)) +
  scale_y_continuous(name = expression("Number of cases"), expand = c(0,0), limits = c(0, 23)) +
  theme_bw() +
  theme(axis.title = element_text(size = 10, family = "Century Gothic"),
        axis.text = element_text(size = 8, family = "Century Gothic")) +
  # Add a vertical line for the median.
  geom_vline(xintercept = 115, size = 1, color = "black", linetype = "dashed")

# Slide 16 - points & facets
ggplot(Data3, aes(x = Source, y = THC)) +
  # Note that geom_point() is *replaced* by geom_jitter() to provide desired effect.
  geom_jitter(alpha = 0.3, size = 6) +
  facet_grid(.~Type, scale = "free_x", space = "free_x") +
  labs(x = "Blood Source",
       y = "[THC] (ng/mL)",
       face="bold") +
  coord_cartesian(ylim = c(0, 200)) +
  theme_light() +
  theme(axis.title = element_text(size = 20, family = "Century Gothic"),
        axis.text = element_text(size = 20, family = "Century Gothic"),
        axis.text.x = element_text(size = 20),
        legend.title = element_text(size = 20, family = "Century Gothic"),
        legend.text = element_text(size = 20, family = "Century Gothic"),
        strip.text.x = element_text(size = 20, family = "Century Gothic"),
        panel.grid = element_line(colour = "#bdbdbd", size = 2),
        panel.border = element_rect(colour = "#bdbdbd", size = 2),
        panel.grid.major.x = element_blank())

# Slide 17 - heat map
ggplot(Length %>% filter(Batches == 150), aes(Correlation, IS, fill = Length)) + 
  geom_tile() +
  scale_fill_viridis(name = "Length (%)", discrete=FALSE, direction = -1) +
  theme_classic() +
  scale_x_discrete(name = "Correlation threshold", expand = c(0,0)) +
  scale_y_discrete(name = "Internal standards", expand = c(0,0)) +
  theme(axis.title = element_text(size = 26, family = "Century Gothic"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 22, family = "Century Gothic"),
        legend.title = element_text(size = 20, family = "Century Gothic"),
        legend.text = element_text(size = 20, family = "Century Gothic"))

# Slide 17 - ridges
ggplot(Recov, aes(x = Recovery, y = Conditions, fill = Conditions)) +
  geom_density_ridges(alpha = 0.8, bandwidth = 3) +
  theme_ridges() + 
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 115)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(name = "Recovery (%)") +
  theme(axis.title = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 16, family = "Century Gothic"),
        panel.border = element_blank()) +
  geom_vline(xintercept = 80, col = "#f4654b", linetype = "dashed", size = 1)

# Slide 18 - maps
# NB. I haven't quite mastered making maps yet. I was just happy I was able to generate one!
ggplot() +
  geom_polygon(data = raq_fortified, aes(fill = GHBPrev, x = long, y = lat, group = group)) +
  theme_void() +
  # Choose one of the following color scales.
  #scale_fill_viridis(name = "GHB Prevalence in DUID (%)") +
  scale_fill_gradient(name = "GHB Prevalence in DRE (%)",
                      low = "#c7d7f2", high = "#003299",
                      limits = c(0, 40)) +
  geom_path(data = raq_fortified, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.5) +
  geom_polygon(data = water_fortified, aes(x = long, y = lat, group = group), color = NA, fill = "#e6f0f7") +
  coord_map(ylim = c(45, 50), xlim = c(-80, -64)) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.title = element_text(size = 12, family = "Century Gothic"),
        legend.text = element_text(size = 12, family = "Century Gothic"),
        legend.spacing.x = unit(1.0, 'cm'))

# Slide 19 - Endogenous BHB content correction report
# See Correction_Endogene_V2.Rmd

# SLIDE 25.
# Effect of endogenous analyte on calibration curve.
# NB. Export package is not available on CRAN. Follow instructions available here https://github.com/tomwenseleers/export
# This package is SUPER useful if you create visualization to be included in Microsoft documents.
# It exports them in a format manipulable in the Microsoft applications.
library(export)

Concentration <- c(0, 10, 20, 50, 100, 200, 425, 500,
                   0, 10, 20, 50, 100, 200, 425, 500,
                   9.5003, 19.5003, 29.5003, 59.5003, 109.5003, 209.5003, 434.5003, 509.5003)
Measurement <- c(0.05300000, 0.09530000, 0.13550000, 0.27530000, 0.51860000, 0.97800000, 1.93360000, 2.23010000,
                 -0.00914999, 0.03315001, 0.07335001, 0.21315001, 0.45645001, 0.91585001, 1.87145001, 2.16795001,
                 0.05300000, 0.09530000, 0.13550000, 0.27530000, 0.51860000, 0.97800000, 1.93360000, 2.23010000)
Group <- c("W", "W", "W", "W", "W", "W", "W", "W",
           "WO", "WO", "WO", "WO", "WO", "WO", "WO", "WO",
           "C", "C", "C", "C", "C", "C", "C", "C")
Lines <- c("dashed", "dashed", "dashed", "dashed", "dashed", "dashed", "dashed", "dashed",
           "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid",
           "dashed", "dashed", "dashed", "dashed", "dashed", "dashed", "dashed", "dashed")
Data_Endo <- tbl_df(cbind(Concentration, Measurement, Group, Lines))
Data_Endo$Concentration <- as.numeric(Data_Endo$Concentration)
Data_Endo$Measurement <- as.numeric(Data_Endo$Measurement)

# With and without endogenous contribution.
g1 <- ggplot(Data_Endo%>%filter(Group!="C"), aes(x = Concentration, y = Measurement, col = Group)) + 
  geom_point(alpha = 0.6, size = 4) + 
  scale_colour_manual(values = c("#003299", "#e0ba2f")) +
  coord_cartesian(xlim = c(0, 110), ylim = c(0, 0.6)) +
  geom_smooth(method = "lm", se = FALSE, mapping = aes(x = Concentration, y = Measurement, linetype = Lines), show.legend = FALSE, size = 2) +
  scale_x_continuous(name = paste("Concentration (\U00B5","g/mL)", sep = ""), expand = c(0, 0)) +
  scale_y_continuous(name = "Area Ratio (Analyte/IS)", expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20, family = "Calibri Light"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 20, family = "Calibri Light"),
        legend.position = "none") +
  geom_hline(yintercept = 0, col = "#3a3737", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = 0, col = "#3a3737", linetype = "solid", size = 0.5)

print(g1)

graph2ppt(x = g1, file = "~/g1",
          width = 8.26, height = 4)



# SLIDES 26 and 27.
# Endogenous correction process.

g2 <- ggplot(Data_Endo%>%filter(Group=="W"), aes(x = Concentration, y = Measurement)) + 
  geom_point(alpha = 0.6, size = 4, col = c("#003299")) + 
  coord_cartesian(xlim = c(-15, 110), ylim = c(0, 0.6)) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, size = 2, col = "#003299", linetype = "solid") +
  scale_x_continuous(name = paste("Concentration (\U00B5","g/mL)", sep = ""), expand = c(0, 0)) +
  scale_y_continuous(name = "Area Ratio (Analyte/IS)", expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20, family = "Calibri Light"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 20, family = "Calibri Light")) +
  geom_hline(yintercept = 0, col = "#3a3737", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = 0, col = "#3a3737", linetype = "solid", size = 0.5)

print(g2)

graph2ppt(x = g2, file = "~/g2",
          width = 8.26, height = 4)


g3 <- ggplot(Data_Endo%>%filter(Group!="WO"), aes(x = Concentration, y = Measurement, col = Group)) + 
  geom_point(alpha = 0.6, size = 4) + 
  scale_colour_manual(values = c("#e0ba2f", "#003299")) +
  coord_cartesian(xlim = c(-15, 110), ylim = c(0, 0.6)) +
  geom_smooth(method = "lm", se = FALSE, mapping = aes(x = Concentration, y = Measurement, linetype = Lines), show.legend = FALSE, size = 2) +
  scale_x_continuous(name = paste("Concentration (\U00B5","g/mL)", sep = ""), expand = c(0, 0)) +
  scale_y_continuous(name = "Area Ratio (Analyte/IS)", expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20, family = "Calibri Light"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 20, family = "Calibri Light"),
        legend.position = "none") +
  geom_hline(yintercept = 0, col = "#3a3737", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = 0, col = "#3a3737", linetype = "solid", size = 0.5)

print(g3)

graph2ppt(x = g3, file = "~/g3",
          width = 8.26, height = 4)

