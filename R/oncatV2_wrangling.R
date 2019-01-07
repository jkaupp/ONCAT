library(tidyverse)
library(readxl)
library(stringr)
library(viridis)

schools <- data_frame(school = c("Conestoga","Humber","Mohawk","Seneca","Sheridan","Algonquin","SLC", "Guelph","Queens","Ryerson","Umanitoba","McMaster","UofT","Waterloo"),
                      alias = c("College 1", "College 2", "College 3", "College 4", "College 5","College 6","College 7", "University 1","University 2", "University 3", "University 4", "University 5","University 6","University 7"))

pruned_schools <- data_frame(school = c("Conestoga", "Mohawk","Seneca","Sheridan","Algonquin","SLC", "Guelph","Queens","McMaster","UofT","Waterloo","Ryerson"),
                             alias = c("College 1", "College 2", "College 3", "College 4", "College 5","College 6", "University 1","University 2", "University 3", "University 4", "University 5","University 6"))


`cognitive process` <- c("Remember",
                         "Understand",
                         "Apply",
                         "Analyze",
                         "Evaluate",
                         "Create")


scaffolding <- c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted")

communication <- c("Interpretation","Representation",	"Calculation",	"Application", "Assumptions",	"Communication")


rater_data <- read_excel(list.files("./data/ONCAT V2", pattern = "V3", full.names = TRUE), sheet = "Master") %>% 
  dplyr::select(-nov) %>% 
  setNames(c("school","subject", "content", "question", "rater","cognitive process","solo taxonomy","scaffolding")) %>% 
  mutate(type = case_when(grepl("diploma", school) ~ "College",
                          grepl("degree", school) ~ "University",
                          school %in% c("Seneca","Mohawk","Algonquin","Sheridan","SLC","Humber") ~ "College",
                          TRUE ~ "University")) %>% 
  select(school,type,subject, content, question, rater,everything()) 



oncatV2_framework_scatter <- function(df, school_selection = NULL, save = FALSE, subtitle = NULL, filename = NULL) {
  
  .cognitive_process <- setNames(c(1:6), c("Remember",
                           "Understand",
                           "Apply",
                           "Analyze",
                           "Evaluate",
                           "Create"))
  
  .scaffolding <- setNames(c(1:4), c("Prescribed Problem",
                           "Constrained Problem",
                           "Scaffolded Problem",
                           "Adopted Problem"))
  
  .solo_taxonomy <- setNames(c(1:5), c("Pre-structural",
                                       "Uni-structural",
                                       "Multi-structural",
                                       "Relational",
                                       "Extended Abstract"))
  
  plot_data <- df %>%
    ungroup %>% 
    janitor::clean_names() %>% 
    mutate_each(funs(as.numeric),-question, -school, -type, -subject, -content) %>% 
    arrange(scaffolding, cognitive_process, solo_taxonomy) %>% 
    filter(!is.na(cognitive_process), !is.na(solo_taxonomy), !is.na(scaffolding)) %>% 
    mutate_each(funs(trunc),-question, -school, -type, -subject, -content) %>% 
    mutate(solo_taxonomy = factor(solo_taxonomy, c(1:5), names(.solo_taxonomy)))
    
  
  if (is.null(subtitle))
  {
    subtitle <- case_when(unique(plot_data$type) == "University" ~ "Engineering",
                          TRUE ~ "Technology")
  }
  
  if (is.null(filename))
  {
    filename <- sprintf("%s_%s", case_when(unique(plot_data$type) == "University" ~ "Engineering",
                                           TRUE ~ "Technology"), 
                        unique(plot_data$subject))
  }
  
  if (!is.null(school_selection)) {
    
    plot_data <- mutate(plot_data, school = ifelse(school == school_selection, school_selection, "Other")) 
    
    
  }
  
    of_plot <- ggplot(plot_data, aes(x = scaffolding, y = cognitive_process)) +
      geom_jitter(aes(fill = solo_taxonomy, shape = school), size = 8, color = "white", alpha = 0.7, width = 0.3, show.legend = TRUE) +
      geom_vline(xintercept = seq(1.5, length(.scaffolding) - 0.5, 1), lwd = 0.1, colour = "grey80", show.legend = FALSE) +
      geom_hline(yintercept = seq(1.5, length(.cognitive_process) - 0.5, 1), lwd = 0.1, colour = "grey80", show.legend = FALSE) +
      labs(x = NULL, y = NULL, title = unique(plot_data[["subject"]]), subtitle = subtitle) +
      scale_y_continuous(labels = str_wrap(names(.cognitive_process), 5), breaks = .cognitive_process) +
      scale_x_continuous(labels = str_wrap(names(.scaffolding), 5), breaks = .scaffolding) + 
      expand_limits(x = c(1, 4.5), y = c(1,6.5)) +
      scale_shape_manual("Shape Key", values = setNames(c(23, 21), c(school_selection, "Other")), labels = c(school_selection, "Other Institutions")) +
      scale_fill_viridis("Color Key", discrete = TRUE, drop = FALSE)  +
      guides(fill = guide_legend(override.aes = list(colour = viridis(5))),
             shape = guide_legend(override.aes = list(shape = c(18, 19),
                                                      color = "grey30"))) +
      theme(
        text = element_text(family = "Calibri", size = 20, color = "black"),
        legend.position = "bottom", #c(0.2,0.8),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.box = "vertical",
        strip.text.y = element_text(angle = 180),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "grey80", size = 0.1),
        panel.spacing.y = unit(1, "lines"),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        strip.background = element_blank(),
        plot.background = element_blank(),
        plot.caption = element_text(size = 10, face = "italic"))
 
  
    if (save) {
      ggsave(plot = of_plot, filename = paste0(filename,".png"),device = "png",  width = 15, height = 12, dpi = 300)
    } else { 
        of_plot }
    
}


oncatV2_framework_scatter_p <- function(df, save = FALSE, subtitle = NULL, filename = NULL) {
  
  .cognitive_process <- setNames(c(1:6), c("Remember",
                                           "Understand",
                                           "Apply",
                                           "Analyze",
                                           "Evaluate",
                                           "Create"))
  
  .scaffolding <- setNames(c(1:4), c("Prescribed Problem",
                                     "Constrained Problem",
                                     "Scaffolded Problem",
                                     "Adopted Problem"))
  
  .solo_taxonomy <- setNames(c(1:5), c("Pre-structural",
                                       "Uni-structural",
                                       "Multi-structural",
                                       "Relational",
                                       "Extended Abstract"))
  
  plot_data <- df %>%
    ungroup %>% 
    janitor::clean_names() %>% 
    mutate_each(funs(as.numeric),-question, -school, -type, -subject, -content) %>% 
    arrange(scaffolding, cognitive_process, solo_taxonomy) %>% 
    filter(!is.na(cognitive_process), !is.na(solo_taxonomy), !is.na(scaffolding)) %>% 
    mutate_each(funs(trunc),-question, -school, -type, -subject, -content) %>% 
    mutate(solo_taxonomy = factor(solo_taxonomy, c(1:5), names(.solo_taxonomy))) %>% 
    mutate(type = case_when(type == "University" ~ "Engineering",
                            TRUE ~ "Technology"))
  
  
  if (is.null(filename))
  {
    filename <- sprintf("%s_%s", case_when(unique(plot_data$type) == "University" ~ "Engineering",
                                           TRUE ~ "Technology"), 
                        unique(plot_data$subject))
  }
  
 
  of_plot <- ggplot(plot_data, aes(x = scaffolding, y = cognitive_process)) +
    geom_jitter(aes(fill = solo_taxonomy), shape = 21, size = 8, color = "white", alpha = 0.7, width = 0.3, show.legend = TRUE) +
    geom_vline(xintercept = seq(1.5, length(.scaffolding) - 0.5, 1), lwd = 0.1, colour = "grey80", show.legend = FALSE) +
    geom_hline(yintercept = seq(1.5, length(.cognitive_process) - 0.5, 1), lwd = 0.1, colour = "grey80", show.legend = FALSE) +
    labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
    scale_y_continuous(labels = str_wrap(names(.cognitive_process), 5), breaks = .cognitive_process) +
    scale_x_continuous(labels = str_wrap(names(.scaffolding), 5), breaks = .scaffolding) + 
    expand_limits(x = c(1, 4.5), y = c(1,6.5)) +
    facet_wrap(~type, nrow = 1) +
    scale_fill_viridis("Color Key", discrete = TRUE, drop = FALSE)  +
    guides(fill = guide_legend(override.aes = list(colour = viridis(5)))) +
    theme(
      text = element_text(family = "Calibri", size = 26, color = "black"),
      legend.position = "bottom", #c(0.2,0.8),
      legend.key = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.box = "vertical",
      strip.text.x = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey80", size = 0.1),
      panel.spacing.y = unit(1, "lines"),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      plot.background = element_blank(),
      plot.caption = element_text(size = 10, face = "italic"))
  
  
  if (save) {
    ggsave(plot = of_plot, filename = paste0(filename,".png"),device = "png",  width = 15, height = 12, dpi = 300)
  } else { 
    of_plot }
  
}




# Compute data by groupings
med_by_rater <- rater_data %>% 
  group_by(subject, type, school, question, content) %>% 
  summarize_each(funs(median), -school:-rater)


data <- med_by_rater %>% 
  split(list(.$subject)) %>% 
  map(ungroup)

schools <- map(data, ~.x %>%  ungroup %>% distinct(school) %>% flatten_chr) 


oncatV2_framework_scatter_p(df)

ee_college <- map(schools[[1]], ~oncatV2_framework_scatter_p(data[[1]], subtitle = .x))
me_college <- map(schools[[2]], ~oncatV2_framework_scatter(data[[2]], school_selection = .x, subtitle = .x))
ee_uni <- map(schools[[3]], ~oncatV2_framework_scatter(data[[3]], school_selection = .x, subtitle = .x))
me_uni <- map(schools[[4]], ~oncatV2_framework_scatter(data[[4]], school_selection = .x, subtitle = .x))


walk2(ee_college, schools[[1]], ~ggsave(plot = .x, filename = sprintf("%s Electrical Engineering Comparison.png", .y), dpi = 300, width = 11, height = 8))
walk2(me_college, schools[[2]], ~ggsave(plot = .x, filename = sprintf("%s Mechanical Engineering Comparison.png", .y), dpi = 300, width = 11, height = 8))
walk2(ee_uni, schools[[3]], ~ggsave(plot = .x, filename = sprintf("%s Electrical Engineering Comparison.png", .y), dpi = 300, width = 11, height = 8))
walk2(me_uni, schools[[4]], ~ggsave(plot = .x, filename = sprintf("%s Mechanical Engineering Comparison.png", .y), dpi = 300, width = 11, height = 8))


plots <- med_by_rater %>% 
  mutate(discipline = subject, class = type, institution = school) %>% 
  group_by(discipline, class) %>% 
  nest() %>% 
  mutate(plot = map(data, ~oncatV2_framework_scatter(.x, school_selection = NULL)))
  

plot <- oncatV2_framework_scatter_p(df)
ggsave("ONCAT Trail.png", plot, width = 16, height = 10, dpi = 300)
