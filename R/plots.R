library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(htmlwidgets)
library(sunburstR)
library(treemap)
library(stringr)
library(grid)
library(gridExtra)
library(beeswarm)


# Base R - Star plots ----
oncat.data %>% 
  mutate_each(funs(as.numeric),-school,-type) %>% 
  group_by(type,school) %>%
  summarize_each(funs(mean), -school,-type) %>% 
  ungroup %>% 
  set_rownames(.$school) %>% 
  select(-type,-school) %>% 
  stars(full = TRUE, lwd=2, key.loc = c(7,2), flip.labels = FALSE, scale = FALSE, len = .3, col.stars = 1:8)

# Sunburst - HTML widget ----

oncat.data %>% 
  filter(type == "University") %>% 
  gather(dimension, value,-school,-type) %>% 
  group_by(dimension,value) %>% 
  unite(category, c(dimension, value), sep="-") %>% 
  count(category) %>% 
  sunburst

# Framework Treemaps ----
vplayout <- function(x, y) viewport(width=25, height=14, layout.pos.row = x, layout.pos.col = y)

framework_tree <- function(df,lvl,x,y) {
  df %>%
    filter(type == lvl) %>% 
    summary %>%
    as.data.frame %>%
    select(-1) %>%
    mutate_each(funs(as.character)) %>%
    filter(!is.na(Freq)) %>%
    slice(-22:-27) %>%
    separate(Freq,c("dimension","value"), sep = ":") %>%
    rename(scale = Var2) %>%
    mutate(dimension = str_trim(dimension, side = c("both")),
           value = as.numeric(str_trim(value, side = c("both")))) %>%
    tbl_df() %>%
    group_by(scale) %>%
    mutate(
      freq = value / sum(value) * 100,
      color = cut(
        freq, breaks = c(0,10,20,30,40,50,60,70,80,90,100), include.lowest = TRUE
      ),
      size  = 1
    ) %>%
    {
      treemap(
        .,
        index = c("scale", "dimension"),
        vSize = "size",
        vColor = "color",
        type = "categorical",
        title = lvl,
        title.legend = "",
        fontsize.labels = c(28,14),
        fontface.labels = c("bold","plain"),
        fontcolor.labels = "#f0f0f0",
        lowerbound.cex.labels = 0.1,
        bg.labels = 0,
        border.col = "#ffffff",
        position.legend = "bottom",
        drop.unused.levels = FALSE,
        align.labels = list(c("left","top"), c("right","bottom")),
        palette = wesanderson::wes_palette("Zissou",10, type = "continuous"),
        vp = vplayout(x,y)
      )
    }
}

framework_maps <- function(df) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1)))
  par(mai=c(0,0,0,0))
  
  framework_tree(df,"University",1,1)
  framework_tree(df,"College",2,1)
  
}

pdf("Framework Maps.pdf", width=11, height=8.5)
framework_maps(oncat.data)
dev.off()

# ONCAT framework bubble ----

oncat.data %>%
  mutate_each(funs(as.numeric),-school, -type, -course) %>%
  gather(
    variable, value, -school, -type, -course, -transfer,-`cognitive process`, -`type of knowledge`
  ) %>%
  filter(!is.na(value)) %>% 
  arrange(value, variable, `cognitive process`, transfer) %>%
  ggplot(aes(x = transfer, y = `cognitive process`)) +
  geom_point(
    aes(fill = factor(value)), size = 10, pch = 21, show.legend = TRUE, position =
      position_jitter()
  ) +
  facet_grid(type~variable, drop = FALSE) +
  xlab("\nTransfer") +
  ylab("Cognitive Process\n") +
  scale_y_discrete(labels = 
                     c(
                       "Remember","Understand","Apply","Analyze","Evaluate","Create"
                     ), limits = c(1:6)) +
  scale_x_discrete(labels =
                     str_wrap(c(
                       "Have Mathematical Knowledge","Apply in the Specific Engineering Context","Apply in Other Engineering Contexts","Apply to Real-World Predictable Situations","Apply to Real-World Unpredictable Situations"
                     ), 15), limits = c(1:5)) +
  guides(fill = guide_legend(title = "Scale Level")) +
  scale_fill_brewer(type = "seq", palette = "YlOrRd") +
  theme_tufte(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0),
        strip.text.y = element_text(angle = 0),
        panel.border = element_rect(fill = NA, color = "grey50"),
        legend.position = "bottom")

ggsave("3x Framework 1.png", width = 26, height = 10, dpi = 300)

# Framework Coxcomb ----
colors<-wesanderson::wes_palette("FantasticFox") %>% 
  readhex(.,class = 'HCL')

oncat.data %>% 
  group_by(type, school) %>% 
  mutate_each(funs(as.numeric), -school, -type) %>% 
  # summarize_each(funs(mean), -school, -type) %>% 
  gather(variable,value,-school,-type,-Application,-`Cognitive process`) %>% 
  group_by(type, variable, value) %>% 
  tally %>% 
  mutate(value = factor(value, levels=c(1,2,3))) %>% 
  mutate(freq = n/sum(n)*100) %>%
  arrange(freq) %>% 
  {
    ggplot(.,aes(x = variable, y=freq)) +
      geom_bar(data = subset(., value==1),
               aes(fill = variable), colour = "#FFFFF3", width = 1
      ) +
      geom_bar(data = subset(., value==2),
               aes(fill = variable), colour = "#FFFFF3", width = 1
      ) +
      geom_bar(data = subset(., value==3),
               aes(fill = variable), colour = "#FFFFF3", width = 1
      ) +
      coord_polar(theta = "x", start = 1)  +
      facet_wrap(~type)
  }    

# Beeswarm ----

data <- oncat.data %>%
  mutate_each(funs(as.numeric),-school, -type, -course) %>%
  gather(
    variable, value, -school, -type, -course, -transfer,-`cognitive process`, -`type of knowledge`
  ) %>%
  filter(!is.na(value)) %>% 
  arrange(value, variable, `cognitive process`, transfer)


beeswarm <- beeswarm(time_survival ~ event_survival, 
data = breast, method = 'swarm', 
pwcol = ER)[, c(1, 2, 4, 6)]