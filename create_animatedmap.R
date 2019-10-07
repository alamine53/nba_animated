library(tidyverse)
library(readr)
library(ggmap)
library(gganimate)
library(grid)
library(gtable)
library(gridExtra)
library(magick)

rm(list = ls())

setwd("~/Documents/04_Projects/nba_animated")

# load data
arenas       <- read_csv("https://raw.githubusercontent.com/alamine53/nba_animated/master/rawdata/raw_NbaCities_adj.csv")
transactions <- read_csv("https://raw.githubusercontent.com/alamine53/nba_animated/master/rawdata/IN_Transactions.csv") %>%
  gather(state, team, OTm:NTm:Terms, factor_key=TRUE) %>%
  arrange(Rk) %>% group_by(Player) %>% mutate(state = 1:3, show_time = cumsum(state)) %>% ungroup() %>% select(Rk, state, Player, PPG, team, show_time) %>%
  left_join(arenas, by = c("team" = "names")) %>%
  select(-team.y, -east) %>%
  mutate(nframes = 1:n()) %>%
  mutate(Player=replace(Player, state ==3, NA)) %>%
  mutate(PPG2 = ifelse(state == 1, 0, PPG), PPG_total = cumsum(PPG2))

# import and format team logos
source("import_logos.R")
d <- 2
arenas <- arenas %>% mutate(long_min = long - d) %>% mutate(long_max = long + d) %>% mutate(lat_min = lat - d) %>% mutate(lat_max = lat +d)

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
    plot.background=element_rect(fill="#f5f5f2"),
    panel.background=element_rect(fill="#f5f5f2"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(size=22, family = "Arial", face = "bold", hjust = 0.1, vjust = -1),
    plot.subtitle = element_text(size= 12, family = "Arial", hjust = 0.075, vjust = -2)
  ) +
  geom_polygon(data = map_data("usa"), aes(x = long, y = lat, group = group, size = 3), 
               size = 1, fill = "#dcdcd9", colour = "grey60", alpha = 1, show.legend = FALSE) +
  annotation_custom(ATL_logo, xmin= arenas$long_min[1], ymin = arenas$lat_min[1], xmax = arenas$long_max[1], ymax = arenas$lat_max[1]) +
  annotation_custom(BOS_logo, xmin= arenas$long_min[2], ymin = arenas$lat_min[2], xmax = arenas$long_max[2], ymax = arenas$lat_max[2]) +
  annotation_custom(BRK_logo, xmin= arenas$long_min[3], ymin = arenas$lat_min[3], xmax = arenas$long_max[3], ymax = arenas$lat_max[3]) +
  annotation_custom(CHI_logo, xmin= arenas$long_min[4], ymin = arenas$lat_min[4], xmax = arenas$long_max[4], ymax = arenas$lat_max[4]) +
  annotation_custom(CHO_logo, xmin= arenas$long_min[5], ymin = arenas$lat_min[5], xmax = arenas$long_max[5], ymax = arenas$lat_max[5]) +
  annotation_custom(CLE_logo, xmin= arenas$long_min[6], ymin = arenas$lat_min[6], xmax = arenas$long_max[6], ymax = arenas$lat_max[6]) +
  annotation_custom(DAL_logo, xmin= arenas$long_min[7], ymin = arenas$lat_min[7], xmax = arenas$long_max[7], ymax = arenas$lat_max[7]) +
  annotation_custom(DEN_logo, xmin= arenas$long_min[8], ymin = arenas$lat_min[8], xmax = arenas$long_max[8], ymax = arenas$lat_max[8]) +
  annotation_custom(DET_logo, xmin= arenas$long_min[9], ymin = arenas$lat_min[9], xmax = arenas$long_max[9], ymax = arenas$lat_max[9]) +
  annotation_custom(GSW_logo, xmin= arenas$long_min[10], ymin = arenas$lat_min[10], xmax = arenas$long_max[10], ymax = arenas$lat_max[10]) +
  annotation_custom(HOU_logo, xmin= arenas$long_min[11], ymin = arenas$lat_min[11], xmax = arenas$long_max[11], ymax = arenas$lat_max[11]) +
  annotation_custom(IND_logo, xmin= arenas$long_min[12], ymin = arenas$lat_min[12], xmax = arenas$long_max[12], ymax = arenas$lat_max[12]) +
  annotation_custom(LAC_logo, xmin= arenas$long_min[13], ymin = arenas$lat_min[13], xmax = arenas$long_max[13], ymax = arenas$lat_max[13]) +
  annotation_custom(LAL_logo, xmin= arenas$long_min[14], ymin = arenas$lat_min[14], xmax = arenas$long_max[14], ymax = arenas$lat_max[14]) +
  annotation_custom(MEM_logo, xmin= arenas$long_min[15], ymin = arenas$lat_min[15], xmax = arenas$long_max[15], ymax = arenas$lat_max[15]) +
  annotation_custom(MIL_logo, xmin= arenas$long_min[16], ymin = arenas$lat_min[16], xmax = arenas$long_max[16], ymax = arenas$lat_max[16]) +
  annotation_custom(MIA_logo, xmin= arenas$long_min[17], ymin = arenas$lat_min[17], xmax = arenas$long_max[17], ymax = arenas$lat_max[17]) +
  annotation_custom(MIN_logo, xmin= arenas$long_min[18], ymin = arenas$lat_min[18], xmax = arenas$long_max[18], ymax = arenas$lat_max[18]) +
  annotation_custom(NOP_logo, xmin= arenas$long_min[19], ymin = arenas$lat_min[19], xmax = arenas$long_max[19], ymax = arenas$lat_max[19]) +
  annotation_custom(NYK_logo, xmin= arenas$long_min[20], ymin = arenas$lat_min[20], xmax = arenas$long_max[20], ymax = arenas$lat_max[20]) +
  annotation_custom(OKC_logo, xmin= arenas$long_min[21], ymin = arenas$lat_min[21], xmax = arenas$long_max[21], ymax = arenas$lat_max[21]) +
  annotation_custom(ORL_logo, xmin= arenas$long_min[22], ymin = arenas$lat_min[22], xmax = arenas$long_max[22], ymax = arenas$lat_max[22]) +
  annotation_custom(PHI_logo, xmin= arenas$long_min[23], ymin = arenas$lat_min[23], xmax = arenas$long_max[23], ymax = arenas$lat_max[23]) +
  annotation_custom(PHO_logo, xmin= arenas$long_min[24], ymin = arenas$lat_min[24], xmax = arenas$long_max[24], ymax = arenas$lat_max[24]) +
  annotation_custom(POR_logo, xmin= arenas$long_min[25], ymin = arenas$lat_min[25], xmax = arenas$long_max[25], ymax = arenas$lat_max[25]) +
  annotation_custom(SAC_logo, xmin= arenas$long_min[26], ymin = arenas$lat_min[26], xmax = arenas$long_max[26], ymax = arenas$lat_max[26]) +
  annotation_custom(SAS_logo, xmin= arenas$long_min[27], ymin = arenas$lat_min[27], xmax = arenas$long_max[27], ymax = arenas$lat_max[27]) +
  annotation_custom(TOR_logo, xmin= arenas$long_min[28], ymin = arenas$lat_min[28], xmax = arenas$long_max[28], ymax = arenas$lat_max[28]) +
  annotation_custom(UTA_logo, xmin= arenas$long_min[29], ymin = arenas$lat_min[29], xmax = arenas$long_max[29], ymax = arenas$lat_max[29]) +
  annotation_custom(WAS_logo, xmin= arenas$long_min[30], ymin = arenas$lat_min[30], xmax = arenas$long_max[30], ymax = arenas$lat_max[30]) +
  labs(title = "NBA Player Movements, Summer 2019", 
       subtitle = "All the transactions that occurred since June 1st, 2019", 
       caption = "Creation: Ramzy Al-Amine | © ralamine.com") +
geom_path(data = transactions, aes(x = long, y = lat, group = Player),
            size = 3, colour = "#1E90FF", arrow = arrow(length = unit(0.5, "cm")) +
geom_text(data = transactions, aes(x = long, y = lat, label = Player), 
          hjust = 0, nudge_x = -1, nudge_y = 1, size = 10, family = "Arial", fontface = "bold"))
nba_map

# static bar chart
nba_bar <- ggplot(data=transactions, aes(x=1, y=PPG_total, colour = Player), show.legend = FALSE) +
  geom_bar(stat = 'identity', width = 0.0005, fill = "#1E90FF", colour = "grey60", show.legend = FALSE) +
  geom_text(aes(label = round(PPG_total, digits = 0)), 
             colour = "black", size = 10, fontface = "bold", vjust = -1 , show.legend = FALSE) +
  theme_void() +
  theme(plot.background=element_rect(fill="#f5f5f2"),
panel.background=element_rect(fill="#f5f5f2"))

nba_bar
# animate charts
nba_map_anim <- animate(nba_map + transition_reveal(nframes), 
                        width = 800, height = 600,
                        fps = 10, duration = 20)    
#    renderer = file_renderer("map_anim", overwrite = TRUE))

nba_bar_anim <- animate(nba_bar + transition_time(nframes), 
                        width = 70, height = 600,
                        fps = 10, duration = 20)

a_mgif <- image_read(nba_map_anim)
b_mgif <- image_read(nba_bar_anim)
composite <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:200){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  composite <- c(composite, combined)
}

composite
setwd("~/Documents/00_MyWebsite/static/images")
save_animation(composite, "animation.gif")


