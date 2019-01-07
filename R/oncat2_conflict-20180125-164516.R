library(tidyverse)
library(readxl)
library(magrittr)
library(purrr)
library(stringr)
library(ggnetwork)
library(ggraph)
library(tidytext)
library(wordcloud)
library(showtext)
library(Cairo)
library(treemap)
library(viridis)
library(jkmisc)
library(gridExtra)
library(grid)
library(gtable)
library(igraph)

gtable_remove_grob <- function(g, pattern = "guide-box") {
  matches <- c(grepl(pattern = pattern, g$layout$name))
  
  g$layout <- g$layout[!matches, , drop = FALSE]
  
  g$grobs <- g$grobs[!matches]
  return(g)
}


files <- list.files("data/ONCAT V2", full.names = TRUE, pattern = "LO_V2")

sheets <- excel_sheets(files)[!grepl("Bloom's",excel_sheets(files))]

bloom_sheet <-  excel_sheets(files)[grepl("Bloom's",excel_sheets(files))]

blooms_factor <- read_excel(files, sheet = bloom_sheet, col_names = FALSE) %>% 
  set_names(c("verb", "bloom_level")) %>% 
  mutate(verb = tolower(verb))

data <- map_df(sheets, ~read_excel(files, sheet = .x) %>% mutate(sheet = .x)) %>% 
  separate(sheet, c("level","discipline"), sep = "-") %>% 
  mutate(level = ifelse(level %in% c("Q","UofT","Con"), "University", "College")) %>% 
  mutate(discipline = ifelse(discipline == "ME", "Mechanical Engineering",
                                ifelse(discipline %in% c("EE","Elec"), "Electrical Engineering",
                                ifelse(discipline == "ElectricalE", "Electrical Engineering",
                                ifelse(discipline == "ElectronicsE",  "Electronics Engineering", discipline))))) %>% 
  set_names(c("outcome","au","concept","verb","level","discipline")) %>% 
  separate_rows(verb, sep = ",") %>% 
  mutate_each(funs(tolower), outcome:concept) %>% 
  mutate_each(funs(trimws)) %>% 
  mutate_each(funs(str_to_title), au, concept, verb)


au.colors <- rev(RColorBrewer::brewer.pal(5, "Set1"))

# Treemap ----



build_treemap <- function(x) {
  
  discipline <- unique(x$discipline)
  level <- unique(x$level)

  filename <- sprintf("./graphics/treemaps/%s - %s AU-Concept-Verb Treemap2.png", level, discipline)
  
  
  tree_data <- x %>%
    mutate(au = ifelse(au == "Complementary", "Complementary Studies", au)) %>%
    mutate(au = factor(au, levels = c("Mathematics","Natural Science","Complementary Studies","Engineering Science","Engineering Design"), labels = c("Mathematics","Natural Science","Complementary Studies","Engineering Science","Engineering Design"))) %>% 
    group_by(level, discipline, au, concept, verb) %>%
    tally 
    
  
  png(filename, width = 1000, height = 1600, units = "px")
  treemap(tree_data,
    index = c("au", "concept", "verb"),
    vSize = "n",
    vColor = "au",
    type = "categorical",
    palette = au.colors,
    title = "",
    title.legend = "Academic Unit (AU) Category",
    fontsize.labels = c(0, 30, 20),
    fontfamily.title = "Lato",
    fontfamily.legend = "Lato",
    fontfamily.labels = "Lato",
    fontcolor.labels = "#f0f0f0",
    inflate.labels = FALSE,
    lowerbound.cex.labels = 1,
    bg.labels = 0,
    position.legend = "bottom",
    border.col = "white",
    border.lwds = c(2, 2, 1),
    align.labels = list(c("left", "top"), c("left", "top"), c("right", "bottom")),
    drop.unused.levels = FALSE,
    aspRatio = NA)
  dev.off()

  
}

data %>% 
  split(list(.$level, .$discipline),drop = TRUE) %>% 
  walk(~build_treemap(.x))


# Slopegraph ----

build_slopegraph <- function(x) {
  
  college <- filter(x, level == "College") 
  
  university <- filter(x, level == "University") 
  
  university_verbs <- university[["verb"]]
  
  college_verbs <- college[["verb"]][(!college[["verb"]] %in% university_verbs)]
  
  plot_data <- jkmisc::slopegraph_offset(x, min_space = 1, target = "percentage")
  
  verb_labels <- tibble(level = c(rep("University", length(university_verbs)), rep("College", length(college_verbs))),
                   verb = c(university_verbs, college_verbs)) %>% 
    inner_join(plot_data)
  
  ggplot(plot_data, aes(x = level, y = spaced)) +
    geom_path(aes(group = verb), show.legend = FALSE, color = "grey20", size = 0.1) +
    geom_point(aes(fill = factor(bloom_level)), shape = 21, color = "white", size = 3) +
    geom_text(aes(label = sprintf("%s (%s)", tools::toTitleCase(verb), scales::percent(percentage))), size = 4, data = filter(verb_labels, level == "University"), nudge_x = 0.1, family = "Scope One", hjust = "left") +
    geom_text(aes(label = sprintf("%s (%s)", tools::toTitleCase(verb), scales::percent(percentage))), size = 4, data = filter(verb_labels, level == "College"), nudge_x = -0.6, family = "Scope One", hjust = "left") +
  labs(y = NULL, x = NULL, title = "Comparison of Verb Frequency in Learning Outcomes between Technology and Engineering Programs", subtitle = stringr::str_wrap("The vertical position indicates the frequency (relative use) of a verb, each verb is color encoded to illustrate its classification according to Bloom's Taxonomy.", 120)) +
    #facet_wrap(~au, scales = "free_y", nrow = 1) +
    scale_x_discrete(breaks = c("College", "University"), labels = c("Technology", "Engineering")) +
    scale_fill_viridis("Bloom's Level", discrete = TRUE, labels = c("Knowledge", "Comprehension", "Application", "Analysis", "Synthesis", "Evaluation"), limits = c(1:6)) +
    theme_jk(grid = "X", base_size = 14) +
    theme(legend.position = "right",
          axis.text.y = element_blank()) 
  
  # grob <- ggplotGrob(plot)
  # 
  # return(grob)
  
}

data %>% 
  count(level, verb) %>% 
  mutate(verb = tolower(trimws(verb))) %>% 
  left_join(blooms_factor, by = "verb") %>% 
  filter(!is.na(bloom_level)) %>% 
  mutate(percentage = n/sum(n)) %>% 
  build_slopegraph()
  #split(.$au) %>% 
  map(~build_slopegraph(.x)) 

# %>% 
#   map_at(c(1:4), gtable_remove_grob)
# 

png("verb comparison slopegraph.png", width = 45, height = 30, units = "in", res = 300)
grid.arrange(grobs = plots, ncol = 5)
dev.off()

ggsave("verb comparison slopegraph.png", width = 10, height = 25, dpi =  300)

# Bipartite Network----'
build_bipartite <- function(x)
  
{
  
  college <- filter(x, level == "College") 
  
  university <- filter(x, level == "University") 
  
  missing <- filter(x, verb %in% setdiff(university$verb, college$verb)) %>% 
    ungroup %>% 
    mutate(level = "College",
           n = 0,
           percentage = 0)

  plot_data <- bind_rows(x, missing)
  
  order <- plot_data %>% 
    arrange(bloom_level) %>% 
    .$verb %>% 
    unique()

  plot <- ggplot(x, aes(x = level, y = verb)) +
    geom_path(aes(group = verb), show.legend = FALSE, color = "grey80") +
    geom_point(aes(fill = factor(bloom_level), size = percentage), shape = 21) +
    labs(y = NULL, x = NULL) +
    facet_wrap(~au, scales = "free_y", nrow = 1) +
    scale_y_discrete(expand = c(0.05, 0), limits = order, breaks = order) +
    scale_x_discrete(breaks = c("College", "University"), labels = c("Technology", "Engineering")) +
    scale_fill_viridis("Bloom's Level", discrete = TRUE, labels = c("Knowledge", "Comprehension", "Application", "Analysis", "Synthesis", "Evaluation"), limits = c(1:6)) +
    scale_size("Frequency of verb", labels = c("1%", "5%", "10%", "25%", "50%"), breaks = c(1, 5,10,25,50)/100, range = c(2,6),   limits = c(0,50)/100) +
    coord_equal() +
    theme_jk(grid = "X") +
    theme(text = element_text(color = "black", family = "Times New Roman", face = "bold"),
          legend.position = "none")
  
  if (unique(plot_data$au == "Natural Science")) {
    
    plot <- plot + theme(legend.position = "right")
  }

  grob <- ggplotGrob(plot)
  
  return(grob)
}

plots <- data %>% 
  count(level, au, verb) %>% 
  mutate(verb = tolower(trimws(verb))) %>% 
  left_join(blooms_factor, by = "verb") %>% 
  filter(!is.na(bloom_level)) %>% 
  mutate(percentage = n/sum(n)) %>% 
  split(.$au) %>% 
  map(~build_bipartite(.x))


png("verb comparison.png", width = 20, height = 7.66, units = "in", res = 200)
grid.arrange(grobs = plots, ncol = 5, widths = c(1,1,1,1,1.4))
dev.off()

ggsave("Verb comparison.png", width = 11, height = 8.5)

