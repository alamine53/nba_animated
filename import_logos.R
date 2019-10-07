library(dplyr)
library(magick)
library(png)
library(grid)
library(gridExtra)
setwd("~/Documents/04_Projects/nba_animated/team_logos")

#m <- readPNG(system.file("img", "Rlogo.png", package="png"), FALSE)

teams <- c("ATL","BOS","BRK","CHI","CHO","CLE",
           "DAL", "DEN", "DET", "GSW", "HOU", "IND",
           "LAC", "LAL", "MEM", "MIL", "MIA", "MIN", "NOP",
           "NYK", "OKC", "ORL", "PHI", "PHO", "POR",
           "SAC", "SAS", "TOR", "UTA", "WAS")

img <- list()
for(s in teams) {
  img[[s]] <- image_read(paste0(s,".png")) %>%
    image_scale("50x50!") %>%
    image_blur(0,0) %>%
    image_modulate(brightness = 90) %>%
    image_modulate(saturation = 40) %>%
    image_modulate(hue = 100) %>%
    image_fill('steelblue', fuzz = 15) %>%
    image_colorize(opacity = 40, color = 'white') %>%
  #image_quantize(max = 10, colorspace = 'gray') %>%
# image_quantize(max = 15, colorspace = 'rgb') %>%
   # image_quantize(max = 10, colorspace = 'cmyk') %>%
   image_convert("png")
}

img[[20]]

ATL_logo <- img[[1]] %>%
  rasterGrob(interpolate=TRUE)
BOS_logo <- img[[2]] %>%
  rasterGrob(interpolate=TRUE)
BRK_logo <- img[[3]] %>%
  rasterGrob(interpolate=TRUE)
CHI_logo <- img[[4]] %>%
  rasterGrob(interpolate=TRUE)
CHO_logo <- img[[5]] %>%
  rasterGrob(interpolate=TRUE)
CLE_logo <- img[[6]] %>%
  rasterGrob(interpolate=TRUE)
DAL_logo <- img[[7]] %>%
  rasterGrob(interpolate=TRUE)
DEN_logo <- img[[8]] %>%
  rasterGrob(interpolate=TRUE)
DET_logo <- img[[9]] %>%
  rasterGrob(interpolate=TRUE)
GSW_logo <- img[[10]] %>%
  rasterGrob(interpolate=TRUE)
HOU_logo <- img[[11]] %>%
  rasterGrob(interpolate=TRUE)
IND_logo <- img[[12]] %>%
  rasterGrob(interpolate=TRUE)
LAC_logo <- img[[13]] %>%
  rasterGrob(interpolate=TRUE)
LAL_logo <- img[[14]] %>%
  rasterGrob(interpolate=TRUE)
MEM_logo <- img[[15]] %>%
  rasterGrob(interpolate=TRUE)
MIA_logo <- img[[16]] %>%
  rasterGrob(interpolate=TRUE)
MIL_logo <- img[[17]] %>%
  rasterGrob(interpolate=TRUE)
MIN_logo <- img[[18]] %>%
  rasterGrob(interpolate=TRUE)
NOP_logo <- img[[19]] %>%
  rasterGrob(interpolate=TRUE)
NYK_logo <- img[[20]] %>%
  rasterGrob(interpolate=TRUE)
OKC_logo <- img[[21]] %>%
  rasterGrob(interpolate=TRUE)
ORL_logo <- img[[22]] %>%
  rasterGrob(interpolate=TRUE)
PHI_logo <- img[[23]] %>%
  rasterGrob(interpolate=TRUE)
PHO_logo <- img[[24]] %>%
  rasterGrob(interpolate=TRUE)
POR_logo <- img[[25]] %>%
  rasterGrob(interpolate=TRUE)
SAC_logo <- img[[26]] %>%
  rasterGrob(interpolate=TRUE)
SAS_logo <- img[[27]] %>%
  rasterGrob(interpolate=TRUE)
TOR_logo <- img[[28]] %>%
  rasterGrob(interpolate=TRUE)
UTA_logo <- img[[29]] %>%
  rasterGrob(interpolate=TRUE)
WAS_logo <- img[[30]] %>%
  rasterGrob(interpolate=TRUE)
