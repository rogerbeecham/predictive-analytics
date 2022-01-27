# Use hex-sticker library
library(hexSticker)
library(tidyverse)

d <- tibble(code="GEOG5927M", name="Predictive \n Analytics")
p <- d %>% ggplot()+
  geom_text(aes(x=0,y=0,label=name), size=6, family="Avenir Book", colour="#ffffff")+
  theme_void()
s <- sticker(p, package="GEOG5927M",
        p_size=4, s_x=1, s_y=0.9, s_width = 5, s_height = 5,
        h_color="#ffffff",
        #h_fill="#a50f15",
        h_fill="#252525",
        p_family="Avenir Book",
        u_family="Avenir Book")
ggsave(s, filename="./static/images/geog5927m2.png", width=2, height=2, dpi=300)
