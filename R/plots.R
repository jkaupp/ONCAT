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

oncat_framework_scatter <- function(df, selection, save = FALSE, subtitle = NULL, filename = NULL) {

  plot_data <- ungroup(df) %>%
    select(school, type, question, subject, transfer, `cognitive process`, contains(selection)) %>% 
    mutate_each(funs(as.numeric),-question, -school, -type, -subject, -transfer, -`cognitive process`) %>%
    gather(
      variable, value, -school, -type, -question, -subject, -transfer, -`cognitive process` 
    ) %>%
   mutate(variable = str_to_title(variable)) %>% 
   arrange(value, variable, `cognitive process`, transfer)
  
  if (nrow(plot_data) > 0) {
    value_factor <- get(tolower(unique(plot_data$variable)))
    
    plot_data$value <-
      factor(plot_data$value, c(1:length(value_factor)), value_factor)
  } 
  
  if(grepl("Physics",unique(plot_data$subject))){
    transfer_labs <- c(
      "Knowledge of physics",
      "Apply in a disciplinary context",
      "Apply in other engineering contexts",
      "Apply to real-world predictable contexts",
      "Apply to real-world unpredictable contexts"
    )
  } else {
    transfer_labs <- c(
      "Mathematical knowledge",
      "Apply in a disciplinary context",
      "Apply in other engineering contexts",
      "Apply to real-world predictable contexts",
      "Apply to real-world unpredictable contexts"
    )
  }
  
  
  if(is.null(subtitle))
  {
    subtitle <- plot_data$type
  }
  
  if(is.null(filename))
  {
    filename <- paste(plot_data$school, plot_data$subject, plot_data$variable, sep = "_")
  }
 
  
  if(nrow(plot_data) > 0){
    
  of_plot <- ggplot(plot_data, aes(x = transfer, y =`cognitive process`)) +
    geom_point(aes(fill = value), size = 8, pch = 21, color = "white", alpha = 0.7, show.legend = TRUE, position = position_jitter()) + 
    geom_vline(xintercept=seq(1.5, length(transfer)-0.5, 1), lwd=0.1, colour="grey80") +
    geom_hline(yintercept=seq(1.5, length(`cognitive process`)-0.5, 1), lwd=0.1, colour="grey80") +
    labs(x = NULL, y = NULL, title = unique(plot_data$variable), subtitle = subtitle) +
    scale_y_discrete(limits = `cognitive process`, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
    scale_x_discrete(limits = transfer, labels = str_wrap(transfer_labs, width = 15), drop = FALSE) + 
    scale_fill_viridis("Color Key",labels = str_wrap(value_factor,60), discrete = TRUE, drop=FALSE) +
    theme(
      text = element_text(family = "Calibri", size = 20, color = "black"),
      legend.position = c(0.75,0.7),
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white"),
      strip.text.y = element_text(angle = 180),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey80", size = 0.1),
      panel.margin.y = unit(1, "lines"),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      plot.background = element_blank(),
      plot.caption = element_text(size = 10, face = "italic"))
  } else {
    
    plot_data  <- mutate(plot_data, value = ifelse(is.na(value),0,value))
    
    of_plot <- ggplot(plot_data, aes(x = transfer, y =`cognitive process`)) +
      geom_point(alpha = 0) + 
      annotate("text", label = "No Ratings Provided", x =3, y = 3, size = 10, family = "Calibri") +
      geom_vline(xintercept=seq(1.5, length(transfer)-0.5, 1), lwd=0.1, colour="grey80") +
      geom_hline(yintercept=seq(1.5, length(`cognitive process`)-0.5, 1), lwd=0.1, colour="grey80") +
      labs(x = NULL, y = NULL, title = unique(plot_data$variable), subtitle = subtitle) +
      scale_y_discrete(limits = `cognitive process`, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
      scale_x_discrete(limits = transfer, labels = str_wrap(transfer_labs, width = 15), drop = FALSE) + 
      scale_fill_viridis("Color Key",labels = str_wrap(value_factor,60), discrete = TRUE, drop=FALSE) +
      theme(
        text = element_text(family = "Calibri", size = 20, color = "black"),
        legend.position = c(0.75,0.7),
        legend.key = element_blank(),
        legend.background = element_rect(fill = "white"),
        strip.text.y = element_text(angle = 180),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "grey80", size = 0.1),
        panel.margin.y = unit(1, "lines"),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        strip.background = element_blank(),
        plot.background = element_blank(),
        plot.caption = element_text(size = 10, face = "italic"))
    
  }

  
  if(save){
   ggsave(plot = of_plot, filename = paste0(filename,".png"),device = "png",  width = 15, height =12, dpi = 300)}
  else(of_plot)
  
}

merged_plot <- function(x, save = FALSE){
  
  grid.save <- function(x)
  {
    png(file_name, width = 1200, height = 900)
    grid.newpage()
    grid.draw(x)
    dev.off()
  }
  
  
  file_name <- sprintf("%s %s.png", unique(x$subject), unique(x$school))
  
  plots <- list(x$depth_plots + theme(axis.text.x = element_blank()),
                x$novelty_plots + theme(axis.text = element_blank()),
                x$scaffolding_plots,
                x$communication_plots + theme(axis.text.y = element_blank()))
  
  p <- grid.arrange(grobs=plots,nrow=2, left = textGrob("Cognitive Process", rot = 90,gp = gpar(fontsize = 20)), bottom = textGrob("Transfer", gp = gpar(fontsize = 20)))
  
  if(save) {
    grid.save(p)
  }
  
  return(p)
}



# All plots ---
framework_plots <- med_by_rater %>% 
  group_by(type, subject) %>% 
  select(-interdependence) %>% 
  do(plots = oncat_framework_scatter(.,"depth",TRUE,NULL,filename = paste("Depth of Analysis",.$type,.$subject)))

survey_plots <- survey_scatter %>% 
  left_join(schools) %>% 
  rename(school_ =  school,
         school = alias) %>% 
  group_by(subject, school, type) %>% 
  do(depth_plots = oncat_framework_scatter(.,"depth",FALSE,.$school),
     novelty_plots = oncat_framework_scatter(.,"novelty",FALSE,.$school),
     scaffolding_plots = oncat_framework_scatter(.,"scaffolding",FALSE,.$school),
     communication_plots = oncat_framework_scatter(.,"communication",FALSE,.$school)
     ) 


merged_plot <- survey_plots %>% 
  rowwise() %>% 
  do(plots = merged_plot(., save = TRUE))


png("Report Plots - 2x2 Framework College Calculus.png", width = 1200, height = 900)
grid.newpage()
grid.draw(merged_plot$plots[[1]])
dev.off()

png("Report Pots - 2x2 Framework College Physics.png", width = 1200, height = 900)
grid.newpage()
grid.draw(merged_plot$plots[[2]])
dev.off()

png("Report Plots - 2x2 Framework University Calculus.png", width = 1200, height = 900)
grid.newpage()
grid.draw(merged_plot$plots[[3]])
dev.off()

png("Report Pots - 2x2 Framework University Physics.png", width = 1200, height = 900)
grid.newpage()
grid.draw(merged_plot$plots[[4]])
dev.off()



df <- filter(survey_scatter, type == "College", subject == "Calculus", school == "SLC")

oncat_framework_scatter(df,selection)


ggsave("Physics Framework Bubble - Depth of Knowledge.png", width = 15, height = 12, dpi = 300)

# Rater Comparisons ----
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

# Framework Question Bubble ----

df %>%
  mutate_each(funs(as.numeric),-question, -school, -type, -subject, -transfer, -`cognitive process`) %>%
  gather(
    variable, value, -school, -type, -question, -subject, -transfer, -`cognitive process`, -`type of knowledge`
  ) %>%
  filter(!is.na(value)) %>% 
  mutate(variable = str_to_title(variable)) %>% 
  arrange(value, variable, `cognitive process`, transfer) %>%
  ggplot(aes(x = transfer, y =`cognitive process`)) +
  geom_label_repel(aes(label = question, fill = factor(value)), alpha = 0.5, color = "white", show.legend = FALSE, segment.size = 0) +
  #geom_count(aes(fill = factor(value)), color = "black", pch = 21) +
  facet_grid(type~variable) +
  xlab("\nTransfer") +
  ylab("Cognitive Process\n") +
  scale_y_discrete(limits = cognitive_process, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
  scale_x_discrete(limits = transfer, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
  scale_size(breaks = c(1,2,5,10,15)) +
  guides(fill = guide_legend(title = "Type of Knowledge"), size = guide_legend(title = "Number of Questions Rated")) +
  scale_fill_viridis(labels = depth_knowledge, discrete = TRUE, drop=FALSE) +
  #scale_color_viridis(labels = depth_knowledge, discrete = TRUE, drop=FALSE) +
  theme_tufte(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0),
        strip.text.y = element_text(angle = 0),
        panel.border = element_rect(fill = NA, color = "grey50"),
        legend.position = "bottom")
  

ggsave("Seneca Calculus Quesiton Bubble.png", width = 26, height = 10, dpi = 300)

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

oncat_pc_subject <- med_by_rater %>% 
  ungroup %>% 
  mutate_each(funs(as.numeric), -subject:-content) %>% 
  mutate_each(funs(jitter), -subject:-content) %>% 
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

oncat_pc_subject_type <- med_by_rater %>% 
  ungroup %>% 
  mutate_each(funs(as.numeric), -subject:-content) %>% 
  mutate_each(funs(jitter), -subject:-content) %>% 
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

oncat_content_heatmap <- function(df, v_subject)  
{
 data <- df %>%
    group_by(subject, school, content) %>% 
    tally %>% 
    mutate(percent = n/sum(n)) %>% 
    mutate(content = ifelse(grepl("\\wC circuit", content), str_replace(content, "(\\w)C circuit", "\\1C Circuit"), str_to_title(content))) %>% 
    filter(subject == v_subject) %>%
    ungroup %>%
    complete(school = schools$school, subject, content,  fill = list(n = 0, percent = 0)) %>%
    inner_join(pruned_schools, by = "school") 
 
  if (v_subject == "Physics")
  {
    data <- mutate(data, n = ifelse(!(school %in% c("Seneca","SLC","McMaster","Queens","Ryerson","Waterloo")), NA, n))
    
    data <- mutate(data, percent = ifelse(!(school %in% c("Seneca","SLC","McMaster","Queens","Ryerson","Waterloo")), NA, percent))
  }
 
  if (v_subject == "Calculus") 
    {
    data <- mutate(data, n = ifelse(!(school %in% c("Algonquin", "Conestoga", "Mohawk", 
                                                    "Seneca", "Sheridan", "SLC", "Guelph", "McMaster", "Queens", 
                                                    "UofT", "Waterloo")), NA, n))
    
    data <- mutate(data, percent = ifelse(!(school %in% c("Algonquin", "Conestoga", "Mohawk", 
                                                          "Seneca", "Sheridan", "SLC", "Guelph", "McMaster", "Queens", 
                                                          "UofT", "Waterloo")), NA, percent))
  }
 
 #data <- filter(data, !(alias %in% c("College 2", "University 4")))
  
  ggplot(data, aes(y = alias, x = content, fill = percent)) +
    geom_tile(color = "black", size = 0.1) +
    #facet_wrap(~subject, drop = FALSE) +
    scale_fill_gradientn(name = "% of Questions per Exam", na.value = "white", limits = c(0,1), colors = c("grey95", wesanderson::wes_palette("Zissou")), labels = percent) +
    #scale_fill_viridis(name = "% of Questions per Exam", na.value = "grey90", limits = c(0,1), labels = function(x) percent(x)) +
    labs(x = NULL, y = NULL, title = data$subject, subtitle = "White Fill = Not Assessed") +
    scale_y_discrete(limits = pruned_schools$alias) +
    #scale_y_discrete(limits = schools$alias[!grepl("College 2|University 4", schools$alias)]) +
    coord_equal() +
    theme_tufte() +
    theme(text = element_text(size = 22, color = "black", family = "Calibri"),
      strip.text.y = element_text(angle = 180),
      axis.title.y = element_text(angle = 0),
      plot.subtitle = element_text(size = 18, face = "italic"),
      axis.text.x = element_text(angle = -45, hjust = 0),
      axis.ticks=element_blank(),
      axis.text=element_text(size=14),
      panel.border=element_blank(),
      plot.title=element_text(hjust=0),
      strip.text=element_text(hjust=0),
      panel.margin.x=unit(0.5, "cm"),
      panel.margin.y=unit(0.5, "cm"),
      legend.title=element_text(size=14),
      legend.title.align=1,
      legend.text=element_text(size=10),
      legend.position="bottom",
      legend.key.size=unit(0.5, "cm"),
      legend.key.width=unit(0.9, "cm")
    )
}


physics_content_heatmap <- med_by_rater %>%
  oncat_content_heatmap("Physics") 
  
calculus_content_heatmap <- med_by_rater %>%
  oncat_content_heatmap("Calculus")
  
HH::export.eps("Physics Content Heatmap (Publication).eps", width = 16, height = 10)

ggsave(plot = physics_content_heatmap, "Physics Content Heatmap (Publication).png",  dpi = 300, width = 16, height = 10)

ggsave(plot = calculus_content_heatmap, "Calculus Content Heatmap (Publication).png",  dpi = 300, width = 16, height = 10)

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
    gather(variable, value, `type of knowledge`, `depth of analysis`, `interdependence`) %>% 
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
      axis.text=element_text(size=8),
      panel.border=element_blank(),
      plot.title=element_text(hjust=0),
      strip.text=element_text(hjust=0),
      panel.margin.x=unit(0.5, "cm"),
      panel.margin.y=unit(0.1, "cm"),
      legend.title = element_text( size = 8),
      legend.title.align = 0,
      legend.text = element_text(size = 6),
      legend.position ="bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(1, "cm")
    )
}

oncat_subject_physics <- med_by_rater %>% 
  oncat_subject_heatmap("Physics")

oncat_subject_calculus <- means_by_rater %>% 
  oncat_subject_heatmap("Calculus") +
  theme(
    strip.text.y = element_blank(),
    axis.text.y = element_blank()
  )


grid_arrange_shared_legend <- function(...) {
  
  
  strip_guide <- function(x){
    
    keep <- !grepl("guide-box", x$layout$name)
    
    x$grobs <- x$grobs[keep]
    x$layout <- x$layout[keep, ]
    
    return(x)
  }
  
  
  plots <- list(...)
  
  plots <- lapply(plots, ggplotGrob)
  
  id.legend <- grep("guide", plots[[1]]$layout$name)
  
  legend <- plots[[1]][["grobs"]][[id.legend]]
  
  lheight <- max(legend$height)
  
  plots <- lapply(plots, strip_guide)
  
  plots <- do.call(cbind, c(plots,size = "first"))
  
  grid.arrange(plots,
               legend,
               nrow = 2,
               heights = unit.c(unit(1, "npc") - lheight, lheight))
  
}


grid_arrange_shared_legend(oncat_subject_physics,oncat_subject_calculus)

pdf("ONCAT Subject Heatmap.pdf", width = 16, height = 10)
grid.newpage()
grid.draw(oncat_s_heatmap)
dev.off()
