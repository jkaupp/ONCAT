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

oncat_4by_scatter <- function(df, selection, save = FALSE, subtitle = NULL, filename = NULL) {
  
  plot_data <- df %>%
    filter(!is.na(value)) %>% 
    mutate(variable = str_to_title(variable)) %>% 
    arrange(value, variable, `cognitive process`, transfer)
  
  value_factor <- get(tolower(unique(plot_data$variable)))
  
  plot_data$value <- factor(plot_data$value, c(1:length(value_factor)), value_factor)
  
  if(is.null(subtitle))
  {
    subtitle < plot_data$school
  }
  
  if(is.null(filename))
  {
    filename <- paste(plot_data$school, plot_data$subject, plot_data$variable, sep = "_")
  }
  
  of_plot <- ggplot(plot_data, aes(x = transfer, y =`cognitive process`)) +
    geom_point(aes(fill = value), size = 8, pch = 21, color = "white", alpha = 0.7, show.legend = TRUE, position = position_jitter()) +
    labs(x = "Transfer", y = "Cognitive Process", title = unique(plot_data$variable), subtitle = subtitle) + #, caption = "\nDerived from the Rigour-Relevance Framework: http://www.leadered.com/our-philosophy/rigor-relevance-framework.php") + 
    geom_vline(xintercept=seq(1.5, length(transfer)-0.5, 1), lwd=0.1, colour="grey80") +
    geom_hline(yintercept=seq(1.5, length(`cognitive process`)-0.5, 1), lwd=0.1, colour="grey80") +
    scale_y_discrete(limits = `cognitive process`, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
    scale_x_discrete(limits = transfer, labels = function(x) str_wrap(x, width = 15), drop = FALSE) +
    # facet_wrap(~type, ncol = 1) +
    scale_fill_viridis("Color Key",labels = str_wrap(get(tolower(unique(plot_data$variable))),60), discrete = TRUE, drop=FALSE) +
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
  
  
  if(save){
    ggsave(plot = of_plot, filename = paste0(filename,".png"),device = "png",  width = 15, height =12, dpi = 300)}
  else(of_plot)
  
}


survey_plots <- survey_scatter %>% 
  mutate_each(funs(as.numeric),`depth of analysis`:scaffolding) %>% 
  gather(variable,value,`depth of analysis`:scaffolding) %>% 
 
