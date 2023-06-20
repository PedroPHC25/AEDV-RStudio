setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)

cidades = read.table("Minard/cities.txt", header = T, stringsAsFactors = F)
tropas = read.table("Minard/troops.txt", header = T, stringsAsFactors = F)
temperaturas = read.table("Minard/temps.txt", header = T, stringsAsFactors = F) %>% 
  mutate(dmy(date))

graf_tropas = ggplot() +
  geom_path(data = tropas, aes(x = long, y = lat, group = group, color = direction, linewidth = survivors), lineend = "round") +
  geom_point(data = cidades, aes(x = long, y = lat)) +
  geom_text_repel(data = cidades, aes(x = long, y = lat, label = city), size = 3) +
  scale_linewidth(range = c(0.5, 15)) +
  scale_color_brewer(palette = "Pastel1") +
  theme_light() +
  labs(x = NULL, y = "Latitude (º)", title = "A Trama de Minard de 1812") +
  guides(color = F, linewidth = F) +
  theme(plot.title = element_text(hjust = 0.5, size = 25), axis.text.x = element_blank(), axis.ticks.x = element_blank())

temperaturas_ajustadas = temperaturas %>% mutate(rotulos = paste0(temp, "º , ", month, ". ", day))

graf_temperaturas = ggplot(temperaturas_ajustadas, aes(x = long, y = temp)) +
  geom_line() +
  geom_label(aes(label = rotulos), size = 3) +
  labs(x = "Longitude (º)", y = "Temperatura na recuada (ºC)") +
  scale_x_continuous() +
  scale_y_continuous(position = "left") +
  coord_cartesian(ylim = c(-35, 5), xlim = c(23.95, 37.7)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  theme_light()

ambos = rbind(ggplotGrob(graf_tropas), ggplotGrob(graf_temperaturas))

grid::grid.newpage()
grid::grid.draw(ambos)