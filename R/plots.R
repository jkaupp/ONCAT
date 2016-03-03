library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(viridis)
library(ggplot2)
library(ggthemes)
library(GGally)
library(treemap)
library(scales)
library(grid)
library(gridExtra)
library(gtable)

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

oncat_framework_scatter <- function(df) {
  
  df %>% 
    mutate_each(funs(as.numeric),-school, -type, -subject) %>%
    gather(
      variable, value, -school, -type, -subject, -rater, -transfer,-`cognitive process`, -`type of knowledge`
    ) %>%
    filter(!is.na(value)) %>% 
    arrange(value, variable, `cognitive process`, transfer) %>%
    ggplot(aes(x = transfer, y = `cognitive process`)) +
    geom_point(
      aes(fill = factor(value)), size = 10, pch = 21, color = "white", show.legend = TRUE, position =
        position_jitter()
    ) +
    facet_grid(type~variable, drop = FALSE) +
    xlab("\nTransfer") +
    ylab("Cognitive Process\n") +
    scale_y_discrete(limits = cognitive_process, labels = function(x) str_wrap(x, width = 10)) +
    scale_x_discrete(limits = transfer, labels = function(x) str_wrap(x, width = 10)) +
    guides(fill = guide_legend(title = "Scale Level")) +
    scale_fill_viridis(labels = depth_knowledge, discrete = TRUE) +
    #scale_fill_brewer(type = "seq", palette = "YlOrRd", labels = depth_knowledge) +
    theme_tufte(base_size = 12) +
    theme(axis.text.x = element_text(angle = 0),
          strip.text.y = element_text(angle = 0),
          panel.border = element_rect(fill = NA, color = "grey50"),
          legend.position = "bottom")
}

calc_1 <- ggplotGrob(rater_data %>%
  filter(subject == "Calculus", rater == 1) %>% 
  oncat_framework_scatter() +
  ggtitle("Rater 1") +
  theme(
        strip.text.y = element_blank()
  ))


calc_2 <- ggplotGrob(rater_data %>%
  filter(subject == "Calculus", rater == 2) %>% 
  oncat_framework_scatter() +
  ggtitle("Rater 2") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_blank()
  ))

calc_3 <- ggplotGrob(
rater_data %>%
  filter(subject == "Calculus", rater == 2) %>% 
  oncat_framework_scatter() +
  ggtitle("Rater 3") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
  ))


physics_1 <- ggplotGrob(rater_data %>%
  filter(subject == "Physics", rater == 1) %>% 
  oncat_framework_scatter() +
  ggtitle("Rater 1") +
  theme(
    strip.text.y = element_blank()
  ))
  
physics_2 <- ggplotGrob(rater_data %>%
  filter(subject == "Physics", rater == 2) %>% 
  oncat_framework_scatter() +
  ggtitle("Rater 2") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_blank()
  ))

physics_3 <- ggplotGrob(rater_data %>%
  filter(subject == "Physics", rater == 3) %>% 
  oncat_framework_scatter() +
  ggtitle("Rater 3") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
  ))

calc <- cbind(calc_1, calc_2, calc_3, size="first")

calc$grobs[[14]] <- zeroGrob()
calc$grobs[[16]] <- zeroGrob()

calc$grobs[[48]] <- zeroGrob()
calc$grobs[[50]] <- zeroGrob()

cairo_pdf("Calculus Raters.pdf", width = 25, height = 12)
grid.newpage()
grid.draw(calc)
dev.off()

physics <- cbind(physics_1,physics_2,physics_3, size="first")

physics$grobs[[14]] <- zeroGrob()
physics$grobs[[16]] <- zeroGrob()

physics$grobs[[48]] <- zeroGrob()
physics$grobs[[50]] <- zeroGrob()

cairo_pdf("Physics Raters.pdf", width = 25, height = 12)
grid.newpage()
grid.draw(physics)
dev.off()

# Framework Radar ----
library(ggradar)

rater_data %>% 
  mutate_each(funs(as.numeric), -school,-subject,-rater,-type) %>% 
  filter(subject == "Calculus") %>% 
  mutate(group = as.character(rater)) %>% 
  select(-rater) %>% 
  select(group, `cognitive process`:interdependence) %>% 
  mutate_each(funs(rescale), -group) %>% 
  group_by(group) %>% 
  summarize_each(funs(mean)) %>% 
  ggradar()
  

ggradar(mtcars_radar) 

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

# Framework Bubble ----

rater_data %>%
# select(-1:-2) %>% 
  mutate_each(funs(as.numeric),-school, -type, -subject) %>%
  gather(
    variable, value, -school, -type, -subject, -rater, -transfer,-`cognitive process`, -`type of knowledge`
  ) %>%
  filter(!is.na(value)) %>% 
  arrange(value, variable, `cognitive process`, transfer) %>% 
  ggplot(aes(x = transfer, y = `cognitive process`)) +
  geom_count(aes(fill = factor(value)), color = "black", pch = 21, position = position_jitter()) +
  #geom_text(stat = "sum", aes(label = ..n..), size = 4, position = position_jitter()) +
  facet_grid(type~variable, drop = FALSE) +
  xlab("\nTransfer") +
  ylab("Cognitive Process\n") +
  scale_y_discrete(limits = cognitive_process, labels = function(x) str_wrap(x, width = 10)) +
  scale_x_discrete(limits = transfer, labels = function(x) str_wrap(x, width = 10)) +
  guides(fill = guide_legend(title = "Scale Level"), size = guide_legend(title = "Size")) +
  scale_fill_brewer(type = "seq", palette = "YlOrRd") +
  scale_size_continuous(range = c(10,20), limits = c(1,200), breaks = c(1,50,100,150,200)) +
  theme_tufte(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0),
        strip.text.y = element_text(angle = 0),
        panel.border = element_rect(fill = NA, color = "grey50"),
        legend.position = "bottom")

ggsave("3x Framework Bubble.png", width = 26, height = 10, dpi = 300)

# Content Histograms ----

oncat.data %>% 
  group_by(type, content) %>% 
  tally %>% 
  ggplot(aes(x = content, y = n)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  facet_wrap(~type) +
  theme_grey(base_size = 20)

ggsave("Content Histograms.png", scale = 1.5, width = 25)

# Parallel Coordinates ----

## By Subject

oncat_pc_subject <- means_by_rater %>% 
  ungroup %>% 
  mutate_each(funs(jitter), -subject:-question) %>% 
  select(subject,type,school,question, everything()) %>% 
  as.data.frame() %>% 
  ggparcoord(columns = c(5:ncol(.)), scale = "globalminmax", alphaLines = 0.2, mapping = aes(color = factor(type)))


oncat_pc_subject$data %<>% 
  mutate(type = factor(type, c(1,2), c("College","University")),
        subject = factor(subject, c(1,2), c("Calculus", "Physics")))

oncat_pc_subject + facet_wrap(~subject) +
  labs(x = "", y = "", title = "") +
  scale_color_manual(name = "", values = c(wesanderson::wes_palette("Zissou",5)[1], wesanderson::wes_palette("Zissou",5)[5])) +
  scale_x_discrete(expand = c(0.1,0), labels = function(x) str_wrap(str_to_title(x), width = 8)) +
  theme_tufte(base_size = 14) +
  theme(axis.ticks.y = element_blank(),
        strip.text = element_text(size = 16),
        strip.text.y = element_text(angle = 180))

ggsave("ONCAT PC - By Subject.png", width = 16, height = 10)

# By Subject and Type

oncat_pc_subject_type <- means_by_rater %>% 
  ungroup %>% 
  mutate_each(funs(jitter), -subject:-question) %>% 
  select(subject,type,school,question, everything()) %>% 
  as.data.frame() %>% 
  ggparcoord(columns = c(5:ncol(.)), scale = "globalminmax", alphaLines = 0.2, mapping = aes(color = factor(type)))

oncat_pc_subject_type$data %<>% 
  mutate(type = factor(type, c(1,2), c("College","University")),
         subject = factor(subject, c(1,2), c("Calculus", "Physics")))

oncat_pc_subject_type + facet_grid(subject ~ type, switch = "y") +
  labs(x = "", y = "", title = "") +
  scale_color_manual(values = c(wesanderson::wes_palette("Zissou",5)[1], wesanderson::wes_palette("Zissou",5)[5])) +
  scale_x_discrete(expand = c(0.1,0), labels = function(x) str_wrap(str_to_title(x), width = 8)) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 16),
        strip.text.y = element_text(angle = 180))

ggsave("ONCAT PC - By Type & Subject.png", width = 16, height = 10)

# Heatmap by Type and Subject faceted by school ----

oncat_type_subject_heatmap <- function(df, v_subject, v_type) {
  
  data <- df %>%
    select(-question) %>% 
    mutate_each(funs(round(.,0)), `cognitive process`:interdependence) %>% 
    gather(variable, value, `type of knowledge`, `depth of knowledge`, `interdependence`) %>% 
    group_by(subject, type, school, `cognitive process`, transfer, variable, value) %>% 
    tally %>% 
    ungroup %>% 
    complete(subject, nesting(type, school), variable, `cognitive process` = 1:6, transfer = 1:5, fill = list(n = NA)) %>% 
    arrange(type,school) %>% 
    filter(subject == v_subject, type == v_type)  %>% 
    mutate(variable = str_wrap(str_to_title(variable), 8))
  

    ggplot(data, aes(y = `cognitive process`, x = transfer, fill = n)) +
    geom_tile(color = "white", size = 0.5) +
    facet_grid(variable ~ school, switch = "y", drop =FALSE) +
    scale_y_continuous(breaks = c(1:6)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis(name = "# of Questions", na.value = "grey95", limits = c(0,80)) +
    labs(x = NULL, y = NULL, title = paste(v_type,v_subject, sep = " - ")) +
    coord_equal() +
    theme_tufte() +
    theme(
      strip.text.y = element_text(angle = 180),
      axis.title.y = element_text(angle = 0),
      axis.ticks=element_blank(),
      axis.text=element_text(size=5),
      panel.border=element_blank(),
      plot.title=element_text(hjust=0),
      strip.text=element_text(hjust=0),
      panel.margin.x=unit(0.5, "cm"),
      panel.margin.y=unit(0.5, "cm"),
      legend.title=element_text(size=6),
      legend.title.align=1,
      legend.text=element_text(size=6),
      legend.position="bottom",
      legend.key.size=unit(0.2, "cm"),
      legend.key.width=unit(0.45, "cm")
    )
}

college_calc <- ggplotGrob(means_by_rater %>% 
  oncat_type_subject_heatmap("Calculus","College") )
  
college_physics <- ggplotGrob(means_by_rater %>% 
  oncat_type_subject_heatmap("Physics","College"))

university_calc <- ggplotGrob(means_by_rater %>% 
  oncat_type_subject_heatmap("Calculus","University") +
    theme(
      strip.text.y = element_blank(),
      axis.text.y = element_blank()))

university_physics <- ggplotGrob(means_by_rater %>% 
  oncat_type_subject_heatmap("Physics","University") +
  theme(
    strip.text.y = element_blank(),
    axis.text.y = element_blank()))


p1 <- cbind(college_calc,university_calc, size = "first")
p2 <- cbind(college_physics,university_physics, size = "first")

pdf("Calculus Heatmap Comparison.pdf", width = 16, height = 10)
grid.newpage()
grid.draw(p1)
dev.off()

pdf("Physics Heatmap Comparison.pdf", width = 16, height = 10)
grid.newpage()
grid.draw(p2)
dev.off()

# Content Heatmap by Subject and School ----

schools <- c("Algonquin", "Mohawk", "Seneca", "Sheridan", "SLC", "Guelph", 
             "McMaster", "Queens", "Ryerson", "UofT", "Waterloo")

oncat_content_heatmap <- function(df, v_subject)  
{
 data <- df %>%
    group_by(subject, school, content) %>% 
    tally %>% 
    ungroup %>% 
    complete(school, nesting(subject, content),  fill = list(n = NA)) %>% 
    mutate(content = ifelse(grepl("\\wC circuit", content), str_replace(content, "(\\w)C circuit", "\\1C Circuit"), str_to_title(content))) %>% 
    filter(subject == v_subject) %>% 
    mutate(school = gdata::reorder.factor(factor(school), new.order = schools))
  
  ggplot(data, aes(y = school, x = content, fill = n)) +
    geom_tile(color = "white", size = 0.1) +
    facet_wrap(~subject, drop = FALSE) +
    scale_fill_viridis(name = "# of Questions in Content Area", na.value = "grey95", limits = c(0,50)) +
    labs(x = NULL, y = NULL, title = NULL) +
    coord_equal() +
    theme_tufte() +
    theme(
      strip.text.y = element_text(angle = 180),
      axis.title.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks=element_blank(),
      axis.text=element_text(size=8),
      panel.border=element_blank(),
      plot.title=element_text(hjust=0),
      strip.text=element_text(hjust=0),
      panel.margin.x=unit(0.5, "cm"),
      panel.margin.y=unit(0.5, "cm"),
      legend.title=element_text(size=6),
      legend.title.align=1,
      legend.text=element_text(size=6),
      legend.position="bottom",
      legend.key.size=unit(0.2, "cm"),
      legend.key.width=unit(0.45, "cm")
    )
}


physics_content_heatmap <- ggplotGrob(rater_data %>% 
  filter(rater ==  1) %>% 
  oncat_content_heatmap("Physics"))
  
calculus_content_heatmap <- ggplotGrob(rater_data %>% 
  filter(rater ==  1) %>% 
  oncat_content_heatmap("Calculus") + 
    theme(axis.text.y = element_blank()))
  
q <- cbind(physics_content_heatmap, calculus_content_heatmap, size = "first")

pdf("Content Area Heatmap by Subject.pdf", width = 16, height = 10)
grid.newpage()
grid.draw(q)
dev.off()

# Heatmap by Subject, faceted by type ----

oncat_subject_heatmap <- function(df, v_subject) {
  
  data <- df %>%
    select(-question) %>% 
    mutate_each(funs(round(.,0)), `cognitive process`:interdependence) %>% 
    gather(variable, value, `type of knowledge`, `depth of knowledge`, `interdependence`) %>% 
    group_by(subject, type, `cognitive process`, transfer, variable, value) %>% 
    tally %>% 
    ungroup %>% 
    complete(nesting(subject,type), variable, `cognitive process` = 1:6, transfer = 1:5, fill = list(n = NA)) %>% 
    mutate(variable = str_wrap(str_to_title(variable), 8)) %>% 
    filter(subject == v_subject)
  
  ggplot(data, aes(y = `cognitive process`, x = transfer, fill = n)) +
    geom_tile(color = "white", size = 0.5) +
    facet_grid(variable ~ type, switch = "y", drop =FALSE) +
    scale_y_continuous(breaks = c(1:6)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis(name = "# of Questions", na.value = "grey95", limits = c(0,165)) +
    labs(x = NULL, y = NULL, title = v_subject) +
    coord_equal() +
    theme_tufte() +
    theme(
      strip.text.y = element_text(angle = 180),
      axis.title.y = element_text(angle = 0),
      axis.ticks=element_blank(),
      axis.text=element_text(size=5),
      panel.border=element_blank(),
      plot.title=element_text(hjust=0),
      strip.text=element_text(hjust=0),
      panel.margin.x=unit(0.5, "cm"),
      panel.margin.y=unit(0.5, "cm"),
      legend.title=element_text(size=6),
      legend.title.align=1,
      legend.text=element_text(size=6),
      legend.position="bottom",
      legend.key.size=unit(0.2, "cm"),
      legend.key.width=unit(0.45, "cm")
    )
}

oncat_subject_physics <- ggplotGrob(means_by_rater %>% 
  oncat_subject_heatmap("Physics"))

oncat_subject_calculus <- ggplotGrob(means_by_rater %>% 
  oncat_subject_heatmap("Calculus") +
  theme(
    strip.text.y = element_blank(),
    axis.text.y = element_blank()
  ))

oncat_s_heatmap <- cbind(oncat_subject_physics, oncat_subject_calculus, size = "first")

pdf("ONCAT Subject Heatmap.pdf", width = 16, height = 10)
grid.newpage()
grid.draw(oncat_s_heatmap)
dev.off()
