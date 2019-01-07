library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(viridis)
library(ggplot2)
library(ggthemes)
library(GGally)
library(ggrepel)
library(treemap)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(jkmisc)

# Parallel Coordinates Rubric Plot----
plot_data <- design_data %>% 
  set_names(tolower(names(.))) %>%
  select(school:contains("design thinking")) %>% 
  mutate_each(funs(jitter), -1:-3) %>% 
  left_join(schools) %>% 
  mutate(alias = ifelse(grepl("Queen",school),"University 2", alias)) %>% 
  select(-school) %>% 
  rename(school = alias) %>% 
  select(school,`course #`, decscription,everything()) %>% 
  as.data.frame() 

 
#para_plot <- ggparcoord(plot_data,  columns = c(4:ncol(plot_data)), scale = "globalminmax", alphaLines = 0.5, mapping = aes(color = factor(school)))
para_plot <- ggparcoord(plot_data,  columns = c(4:ncol(plot_data)), scale = "globalminmax", alphaLines = 0.5)

para_plot$data <- para_plot$data %>% 
   mutate(school = factor(school, c(1:4), c("Technology 4","Engineering 2","Engineering 3","Engineering 4")))
         

disciplinarity  <- c("Disciplinary: Requires knowledge of one specific engineering discipline",
"Non-Disciplinary: Using first principles of mathematical knowledge, natural sciences and engineering sciences",
"Cross-disciplinary: Requires knowledge of different engineering disciplines")


tools <- c("Apply appropriate techniques, resources, and modern engineering and IT tools",
"Select and apply appropriate techniques, resources, and modern engineering and IT tools, including prediction and modelling",
"Create, select and apply appropriate techniques, resources, and modern engineering and IT tools, including prediction and modelling")


design <- c("Identify and select and implement solutions to engineering problems reaching substantiated conclusions",
"Identify, explore, select and implement solutions to engineering problems reaching substantiated conclusions",
"Identify, explore,  select, implement and evaluate solutions to engineering problems reaching substantiated conclusions")


pc_plot <- para_plot +
  facet_wrap(~school, nrow = 1) +
  scale_y_continuous(breaks = seq(1,3,1)) +
  scale_x_discrete(labels = function(x) str_wrap(str_to_title(x),15)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_jk(base_size = 20, base_family = "Calibri") + 
  theme(
    strip.text.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    legend.position = "none")

labels <- data_frame(x=rep(seq(1,3,1),each=3),y=rep(seq(1,3,1),3), label=c(disciplinarity,tools,design))

label_plot <- ggplot(labels, aes(x = x,y = y)) +
  geom_tile(fill = "white", color = "grey20") +
  geom_text(aes(label = str_wrap(label, 40), family = "Lato-Light"), size = 5) +
  scale_x_continuous(breaks = seq(1,3,1), labels = c("Disciplinarity","Use of Tools, Process & Techniques","Design Thinking (Problem Analysis)")) +
  scale_y_continuous(breaks = seq(1,3,1)) +
  expand_limits(x = c(0.5,3.5), y = c(0.5,3.5)) +
  labs(x = NULL, y = NULL, title = "Design Activity Rubric") +
  theme_jk(grid = FALSE,base_size = 20, base_family = "Calibri") 


png("Design Rating Parallel Coordinates.png", width = 1400)
pc_plot
dev.off()


png("Design Rating Results.png", width = 1280, height = 800)
grid.arrange(pc_plot,label_plot)
dev.off()


## htmlwidget
pc<-parcoords(plot_data[c(1,4:6)], 
              brushMode = "1D-axes-multi", 
              rownames = FALSE, 
              reorderable = TRUE,
              margin = list(10,10,10,10),
              # dimensions = list(
              #   disciplinarity = list(
              #     title = "Disciplinarity",
              #     tickValues = seq(1,3,1)
              #   )
              # ),
              color = list(
                colorBy = "school"
                ,colorScale = htmlwidgets::JS(sprintf('
                                                      d3.scale.ordinal()
                                                      .domain(%s)
                                                      .range(%s)
                                                      '
                                                      ,jsonlite::toJSON(c("Queen's","Ryerson","Seneca","Manitoba"))
                                                      ,jsonlite::toJSON(str_sub(viridis(4),1,7))
                ))
              ))


pc$x$options$shadows <- T

pc

# Content Heatmap ----



heat_data <- design_data %>% 
  select(-4:-6) %>% 
  rename(Course = `Course #`) %>% 
  gather(variable,value,-1:-3) %>% 
  set_names(tolower(names(.))) %>%
  mutate(school = str_trim(school)) %>% 
  group_by(school, variable) %>%
  summarize(n = sum(value), count = n()) %>% 
  mutate(percent = n/count) %>% 
  arrange(percent) %>% 
  ungroup %>% 
  mutate(type = "Project") %>% 
  mutate(flag = ifelse(n>0,1,0)) %>% 
  mutate(school = ifelse(school == "Conestoga", "Technology 1", school)) %>% 
  mutate(school = ifelse(school == "Guelph", "Engineering 1", school)) %>% 
  mutate(school = ifelse(school == "Humber", "Technology 2", school)) %>% 
  mutate(school = ifelse(school == "Mohawk", "Technology 3", school)) %>% 
  mutate(school = ifelse(school == "Queen's", "Engineering 2", school)) %>% 
  mutate(school = ifelse(school == "Ryerson", "Engineering 3", school)) %>% 
  mutate(school = ifelse(school == "Seneca", "Technology 4", school)) %>% 
  mutate(school = ifelse(school == "Sheridan", "Technology 5", school)) %>% 
  mutate(school = ifelse(school == "Umanitoba", "Engineering 4", school)) 

  # mutate(variable = factor(variable,variable))

content_heatmap <- ggplot(heat_data, aes(x = school, y = variable, fill = percent)) +
  geom_tile(color = "white", size = 0.5) +
  #facet_wrap(~ school, switch = "y", drop= TRUE, scales = "free_y") +
  #scale_fill_viridis(name = "Percentage", na.value = "grey95", discrete = FALSE, labels = percent, breaks = seq(0,0.15,0.05)) +
  scale_fill_gradient_tableau("Blue", labels = percent) +
  labs(x = NULL, y = NULL, title = "Content Heatmap By Institution", subtitle = "Percentage of total content measured at each institution") +
  coord_equal() +
  theme_jk(base_size = 16, axis_title_just = "l" ) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

ggsave(plot=content_heatmap, "Content Heatmap.png", width = 12)


## Outcomes heatmap

outcomes_heat_data <- design_outcomes_data %>% 
  set_names(tolower(names(.))) %>%
  select(school,topics) %>% 
  mutate(school = str_trim(school)) %>% 
  group_by(school,topics) %>% 
  summarize(n=n()) %>% 
  ungroup %>% 
  complete(school,topics,fill=list(n=0)) %>% 
  group_by(school) %>% 
  mutate(count = sum(n)) %>% 
  mutate(percent = n/count) %>% 
  arrange(percent) %>% 
  mutate(type = "Learning Outcome") %>% 
  rename(variable = topics) %>% 
  mutate(flag = ifelse(n>0,1,0)) %>% 
  ungroup %>% 
  mutate(school = ifelse(school == "Conestoga", "Technology 1", school)) %>% 
  mutate(school = ifelse(school == "Guelph", "Engineering 1", school)) %>% 
  mutate(school = ifelse(school == "Humber", "Technology 2", school)) %>% 
  mutate(school = ifelse(school == "Mohawk", "Technology 3", school)) %>% 
  mutate(school = ifelse(school == "Queen's", "Engineering 2", school)) %>% 
  mutate(school = ifelse(school == "Ryerson", "Engineering 3", school)) %>% 
  mutate(school = ifelse(school == "Seneca", "Technology 4", school)) %>% 
  mutate(school = ifelse(school == "Sheridan", "Technology 5", school)) %>% 
  mutate(school = ifelse(school == "Umanitoba", "Engineering 4", school)) 
# %>% 
#   mutate(topics = factor(topics,topics))
 

outcomes_heatmap <- ggplot(outcomes_heat_data, aes(x = school, y = topics, fill = percent)) +
  geom_tile(color = "white", size = 0.5) +
  #facet_wrap(~ school, switch = "y", drop= TRUE, scales = "free_y") +
  #scale_fill_viridis(name = "Percentage", na.value = "grey95", discrete = FALSE, labels = percent) +
  scale_fill_gradient_tableau("Blue", labels = percent, limits = c(0,1)) +
  labs(x = NULL, y = NULL, title = "Learning Outcomes Heatmap By Institution", subtitle = "Percentage of learning outcomes measured at each institution") +
  coord_equal() +
  theme_jk(base_size = 16, axis_title_just = "l",base_family = "Calibri") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))


ggsave(plot=outcomes_heatmap, "Outcomes Heatmap.png", width = 12)

alignment_map <- bind_rows(heat_data,outcomes_heat_data) %>% 
   select(school,type, variable,flag) %>% 
   complete(school,type, variable, fill = list(flag = NA)) %>% 
   spread(type, flag) %>% 
  mutate(Alignment = ifelse(`Learning Outcome` == 1 & Project == 1, 2, NA)) %>% 
  mutate(Alignment = ifelse(`Learning Outcome` == 1 & Project == 0, 1, Alignment)) %>% 
  mutate(Alignment = ifelse(`Learning Outcome` == 1 & is.na(Project), NA, Alignment)) %>% 
  mutate(Alignment = ifelse(`Learning Outcome` == 0 & Project == 1, 1, Alignment)) %>%
  mutate(Alignment = ifelse(`Learning Outcome` == 0 & Project == 0, 0, Alignment)) %>%
  mutate(Alignment = ifelse(`Learning Outcome` == 0 & is.na(Project), NA, Alignment)) %>% 
  mutate(Alignment = ifelse(is.na(`Learning Outcome`) & is.na(Project), NA, Alignment)) %>%
   gather(type, flag, -school:-variable) %>% 
   mutate(variable = ifelse(str_detect("Cad",variable), "CAD", variable)) %>% 
   mutate(flag = ifelse(is.na(flag), 9, flag))
  
  

component_alignment_map <- filter(alignment_map, type != "Alignment")

component_heatmap <- ggplot(component_alignment_map, aes(x = school, y = variable, fill = factor(flag))) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(name = "", labels = c("Not Observed", "Observed", "No Data"), values = c(wesanderson::wes_palette("Zissou",5)[c(1,3)],"grey95")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  coord_equal() +
  facet_wrap(~type) +
  theme_jk(base_size = 20, axis_title_just = "l",base_family = "Calibri") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        strip.text.x = element_text(size = 20),
        panel.margin.x = unit(28,"pt"),
        legend.position = "bottom")

full_alignment_map <- filter(alignment_map, type == "Alignment")

alignment_heatmap <- ggplot(full_alignment_map, aes(x = school, y = variable, fill = factor(flag))) +
  geom_tile(color = "white", size = 0.5) +
  # scale_fill_manual(name = "", labels = c("Not Observed", "Observed in Project", "Observed in Outcome", "Observed in Project & Outcome","No Data"), breaks = c(seq(1,4,1),9), values = c(wesanderson::wes_palette("Zissou",5)[c(-4)],"grey95")) +
  #scale_fill_manual(name = "", labels = c("No Alignment Observed", "Alignment Observed","No Data"), values = c(wesanderson::wes_palette("Zissou",5)[c(1,3)],"grey95")) +
  scale_fill_manual(name = "", labels = c("Not Observed","Misaligned","Aligned", "Undetermined"), values = c(wesanderson::wes_palette("Zissou",5)[1],"#93B36E",wesanderson::wes_palette("Zissou",5)[3],"grey95")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  coord_equal() +
  facet_wrap(~type) +
  theme_jk(base_size = 20, axis_title_just = "l",base_family = "Calibri") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        strip.text.x = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")

#3B9AB2 <- 0 + 0 "No Alignment"
#97C6D2 <- 0 + NA "Alignment Undertermined "
#EFDF8E <- 1 + NA "Partial Alignment"
#93B36E <- 1 + 0 | 0 + 1 "
#EBCC2A <- 1 + 1

g1 <- ggplotGrob(component_heatmap)

g2 <- ggplotGrob(alignment_heatmap)

plot <- cbind(g1,g2, size = "first")

png("Alignment Plots.png", width = 1400, height = 600)
grid.newpage()
grid.draw(plot)
dev.off()
