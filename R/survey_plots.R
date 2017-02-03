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


Q1 <- c("Every week","At the end of each chapter", "Every month", "One midterm,One final","One final")
Q2 <- c("Once","Two Times","More than two")
Q3 <- c("Within one week","At the end of each chapter",	"Between one week and a month",	"Longer than one month")
Q4 <- c("I don’t know",	"No Confidence","Somewhat confident",	"Neutral", "Confident",	"Very Confident")
Q5 <- c("I don’t know",	"Poor",	"Fair",	"Neutral",	"Good",	"Very Good")
Q6 <- c("Familiar",	"Reorganized",	"New")
Q7 <- c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted")
Q8 <- c("Interpretation",	"Representation",	"Calculation",	"Application",	"Assumptions",	"Communication")

OS_summary_plot <- function(df, selection, save = FALSE) {

  levels <- get(unique(df$question))
  
  plot_data <- filter(df, subject == selection) %>% 
    mutate(value = factor(value,levels)) %>% 
    group_by(subject,value) %>% 
    tally %>% 
    ungroup %>% 
    complete(subject, value, fill = list(n=0)) %>% 
    filter(!is.na(value))
  
  filename <- paste(selection, unique(df$question), sep = "_")
  
   plot <- ggplot(plot_data, aes(x = value, y = n)) +
      geom_bar(width = 0.5, stat="identity") +
      geom_text(aes(label = n), color = "white", hjust = 1.5, family = "Oswald-Light") +
      labs(x = NULL, y = NULL) +
      coord_flip() +
      scale_x_discrete(limits = levels) +
      theme_jk(grid = c("X"),axis = FALSE, base_size =10) +
      theme(strip.text.y = element_text(angle = 0),
          axis.text.x = element_blank())
    
    if(save){
      ggsave(plot = plot, filename = paste0(filename,".png"), device = "png",  width = 4, height = 3, dpi = 300)}
    else(plot)
}  

survey_plots <- survey_data %>% 
  filter(question %in% c("Q1","Q2","Q3","Q4","Q5")) %>% 
  group_by(question) %>% 
  do(p_plots = OS_summary_plot(., "Physics", TRUE),
     c_plots = OS_summary_plot(., "Calculus", TRUE))

survey_scatter %>% 
  group_by(type, subject) %>% 
  do(nov_plots = oncat_framework_scatter(.,"novelty", TRUE, filename = sprintf("%s_%s_%s", "novelty", .$type,.$subject)),
     saff_plots = oncat_framework_scatter(.,"scaffold", TRUE, filename = sprintf("%s_%s_%s", "scaffolding", .$type,.$subject)),
     com_plots = oncat_framework_scatter(.,"communication", TRUE, filename = sprintf("%s_%s_%s", "communication", .$type,.$subject)))


