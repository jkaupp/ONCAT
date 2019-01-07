library(psych)
library(nFactors)
library(tidyverse)
library(irr)
library(readxl)
library(corrr)
library(janitor)
library(corrr)
library(corrplot)
library(ggbeeswarm)
library(jkmisc)
library(ggforce)
library(viridis)

plot_data <- rater_data %>% 
  clean_names() %>% 
  gather(dimension, rating, cognitive_process:interdependence) %>% 
  spread(rater,rating, sep = "_") %>% 
  group_by(type, subject, question, dimension) %>% 
  dplyr::select(-content, -school) %>% 
  nest() %>% 
  mutate(agree = map_dbl(data, ~agree(as.matrix(.x))$value/100)) %>% 
  group_by(type, subject, dimension) %>% 
  mutate(n = n())

n_labels <- plot_data %>% 
  ungroup %>% 
  distinct(subject, type, dimension, n) %>% 
  filter(grepl("cognitive", dimension))


ggplot(plot_data, aes(y = type, x = agree)) +
  geom_quasirandom(varwidth = TRUE, groupOnX = FALSE, alpha = 0.2) +
  facet_grid(subject~ dimension) +
  theme_jk(grid = "XY")

oncat_labeller <- function(x) {
  
  stringr::str_to_title(gsub("_"," ", x)) 

}

ggplot(plot_data, aes(x = type, y = agree)) +
  geom_violin(aes(fill = type), alpha = 1, scale = "count", show.legend = FALSE) +
  geom_label(data = n_labels, aes(x = type,  label = sprintf("n = %s", n), y = 0), nudge_x = 0.3, inherit.aes = FALSE, family = "Quicksand", size = 3) +
  coord_flip() +
  facet_grid(subject ~ dimension, labeller = as_labeller(oncat_labeller), drop = TRUE) +
  scale_y_continuous(expand = c(0.3,0), limits = c(0,1), breaks = scales::pretty_breaks(), labels = scales::percent) +
  scale_fill_manual(values = wesanderson::wes_palette("Zissou", 5)[c(1,5)]) +
  labs(y = "Total Agreement (%)", x = NULL, title = "Rater Reliability using the ONCAT Learing Outcomes Framework", subtitle =  str_wrap("The total agreement between raters is shown below as a continuous distribution for each dimension of the framework. The width of the distribution is porportional to the number of questions at the specified total agreement rating between the three raters.  Long shapes have a greater range of agreement, while thick shapes have a high number of questions at the specified agreement rating.", 100)) +
  theme_jk(grid = "XY") +
  theme(strip.text.y = element_text(angle = 0),
        plot.subtitle = element_text(family = "Quicksand"))

ggsave("Framework Reliability.png", width = 10, height = 6)
