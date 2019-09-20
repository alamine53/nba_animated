library(tidyverse)
library(readr)
library(ggmap)
library(gganimate)
library(grid)
library(gtable)
library(gridExtra)
library(magick)

rm(list = ls())
setwd("C:/Users/ralamine/Desktop")

# load data
arenas       <- read_csv("https://raw.githubusercontent.com/alamine53/nba_trademap_final/master/raw_NbaCities_adj.csv")
transactions <- read_csv("https://raw.githubusercontent.com/alamine53/nba_trademap_final/master/IN_Transactions.csv") %>%
  gather(state, team, OTm:NTm, factor_key=TRUE) %>%
  arrange(Rk) %>% group_by(Player) %>% mutate(state = 1:2, show_time = cumsum(state)) %>% ungroup() %>% select(Rk, state, Player, PPG, team, show_time) %>%
  left_join(arenas, by = c("team" = "names")) %>%
  select(-team.y, -east) %>%
  mutate(nframes = 1:n()) %>%
  mutate(PPG2 = ifelse(state == 1, 0, PPG), PPG_total = cumsum(PPG2))

# static map  
nba_map <- ggplot() + 
  theme_void() +
  #guides(size = guide_legend(title.position = "top", title.hjust = 0.5, label.position = "bottom"),colour = "none", fill = "none") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x =  element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = rel(5 / 6), hjust = 0.5),
    plot.title = element_text(size=22, family = "Arial", face = "bold")
  ) +
  labs(title = "NBA Player Movements, Summer 2019", subtitle = "Player", caption = "Creation: Ramzy Al-Amine | © courtsidecoder.home.blog") +
  geom_polygon(data = map_data("usa"), aes(x = long, y = lat, group = group, size = 1), 
               fill = "white", alpha = 0.05, colour = "#dcdcd9", show.legend = FALSE) +
  geom_point(data = arenas, aes(x = long, y = lat), 
             shape = 2, color = "grey90", fill = "white", size = 4) +
  geom_text(data = arenas, aes(x = long, y = lat, label = names), 
            hjust = 0, nudge_x = -1, nudge_y = 1, family = "Arial", fontface = "bold", colour = "grey90") +
  geom_text(data = transactions, aes(x = long, y = lat, label = Player), 
            hjust = 0, nudge_x = -1, nudge_y = 1, family = "Arial", fontface = "bold") +
  geom_path(data = transactions, aes(x = long, y = lat, group = Player),
            size = 1.5, colour = "red", arrow = arrow(length = unit(0.5, "cm")))
nba_map

# static bar chart
nba_bar <- ggplot(data=transactions, aes(x=1, y=PPG_total, colour = Player), show.legend = FALSE) +
  geom_bar(stat = 'identity', width = 0.001, show.legend = FALSE) +
  geom_label(aes(label = round(PPG_total, digits = 1)), show.legend = FALSE) +
  theme_void()

# animate charts
nba_map_anim <- animate(nba_map + transition_reveal(nframes), 
                        width = 800, height = 600,
                        fps = 10, duration = 10)    
#    renderer = file_renderer("map_anim", overwrite = TRUE))

nba_bar_anim <- animate(nba_bar + transition_time(nframes), 
                        width = 75, height = 600,
                        fps = 10, duration = 10)
# renderer = file_renderer("bar_anim", overwrite = TRUE))

a_mgif <- image_read(nba_map_anim)
b_mgif <- image_read(nba_bar_anim)
composite <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  composite <- c(composite, combined)
}

composite
