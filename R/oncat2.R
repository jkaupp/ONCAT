library(igraph)
library(tidyverse)
library(readxl)
library(magrittr)
library(purrr)
library(ggnetwork)
library(ggraph)
library(tidytext)
library(wordcloud)
library(showtext)
library(Cairo)
library(treemap)
library(stringr)
library(viridis)
library(jkmisc)
library(gridExtra)
library(grid)
library(gtable)

gtable_remove_grob <- function(g, pattern = "guide-box") {
  matches <- c(grepl(pattern = pattern, g$layout$name))
  
  g$layout <- g$layout[!matches, , drop = FALSE]
  
  g$grobs <- g$grobs[!matches]
  return(g)
}


files <- list.files("./data/", full.names = TRUE, pattern = "ONCAT2")

sheets <- excel_sheets(files)[!grepl("Bloom's",excel_sheets(files))]

bloom_sheet <-  excel_sheets(files)[grepl("Bloom's",excel_sheets(files))]

blooms_factor <- read_excel(files, sheet = bloom_sheet, col_names = FALSE) %>% 
  set_names(c("verb", "bloom_level")) %>% 
  mutate(verb = tolower(verb))


data <- map2_df(files, sheets, ~ read_excel(.x, sheet = .y) %>% 
                  mutate(sheet = .y)) %>% 
  separate(sheet, c("level","discipline"), sep = "-") %>% 
  mutate(level = ifelse(level == "Q", "University", "College")) %>% 
  mutate(discipline = ifelse(discipline == "ME", "Mechanical Engineering",
                                ifelse(discipline == "EE", "Electrical Engineering",
                                ifelse(discipline == "ElectricalE", "Electrical Engineering",
                                ifelse(discipline == "ElectronicsE",  "Electronics Engineering", discipline))))) %>% 
  set_names(c("outcome","au","concept","verb","level","discipline")) %>% 
  separate_rows(verb, sep = ",") %>% 
  mutate_each(funs(tolower), outcome:concept) %>% 
  mutate_each(funs(str_trim)) %>% 
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


# Bipartite Network----'
# 
# 
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
    #scale_x_discrete(expand = c(0.1, 0)) +
    scale_fill_viridis("Bloom's Level", discrete = TRUE, labels = c("Knowledge", "Comprehension", "Application", "Analysis", "Synthesis", "Evaluation"), limits = c(1:6)) +
    scale_size("Frequency of verb", labels = c("1%", "5%", "10%", "25%", "50%"), breaks = c(1, 5,10,25,50)/100, range = c(2,6),   limits = c(0,50)/100) +
    coord_equal() +
    theme_jk(grid = "X") +
    theme(legend.position = "right",
          text = element_text(color = "black")) 
  
  grob <- ggplotGrob(plot)
  
  return(grob)
}

plots <- data %>% 
  group_by(level, au, verb) %>% 
  tally %>% 
  mutate(verb = tolower(verb)) %>% 
  left_join(blooms_factor, by = "verb") %>% 
  filter(!is.na(bloom_level)) %>% 
  mutate(percentage = n/sum(n)) %>% 
  split(.$au) %>% 
  map(~build_bipartite(.x))


plots[1:4] <- lapply(plots[1:4], gtable_remove_grob)


png("verb comparison.png", width = 16, height = 12, units = "in", res = 300)
grid.arrange(grobs = plots, ncol = 5)
dev.off()

ggsave("Verb comparison.png", width = 11, height = 8.5)




# Network ----

au_nodes <- filter(data, discipline == "Mechanical Engineering", level == "University", !is.na(concept)) %>%
  group_by(au) %>% 
  tally %>% 
  mutate(color = au.colors[au]) %>% 
  mutate(label_size = 10) %>% 
  rename(node = au)

concept_nodes <- filter(data, discipline == "Mechanical Engineering", level == "University", !is.na(concept)) %>%
  group_by(au,concept) %>% 
  tally %>% 
  mutate(color = au.colors[au]) %>% 
  ungroup() %>% 
  select(-au) %>% 
  mutate(label_size = 5) %>% 
  rename(node = concept)

nodes <- bind_rows(au_nodes,concept_nodes) %>% 
  mutate(n = 2*as.numeric(cut(n,c(1,5,10,20,50,75,100),include.lowest = TRUE)))

au_concept_edges <- filter(data, discipline == "Mechanical Engineering", level == "University", !is.na(concept)) %>% 
  select(au, concept) %>% 
  rename(from = au, to = concept)


edges <- au_concept_edges 

graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)


## Generate Network Plot

pdf("./network/QU_MECH.pdf", width = 16, height = 10)


ggraph(graph, 'igraph', algorithm = 'kk') +
  geom_edge_arc2(aes(alpha = ..index..)) +
  geom_node_point(aes(size = n, fill = color), alpha = 0.5, color = "white", pch = 21) +
  geom_node_text(aes(size = label_size, colour = color, label = stringr::str_wrap(name,10))) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  theme_blank() +
  theme(legend.position = "none") 


dev.off()


## Wordclouds

font.add.google("Oxygen", "oxygen")
 
filter(data, discipline == "Mechanical Engineering", level == "University", !is.na(concept)) %>%
  distinct(au, concept, outcome) %>% 
  split(.$concept) %>% 
  walk(~generate_wordcloud(.x))

  
  
generate_wordcloud <- function(x){
  
  data("stop_words")
  
  au <- unique(x$au)
  concept <- unique(x$concept)
  
  color <- au.colors[au]
  
  file <- sprintf("./wordcloud/%s_%s.pdf", au, concept)
  
 cloud_data <- unnest_tokens(x, word, outcome) %>% 
    anti_join(stop_words, by = "word") %>% 
    filter(!grepl("apsc151", word)) %>% 
    count(concept, word)
  
 
  pdf(file = file)
  showtext.begin()
  with(cloud_data, wordcloud(word, n, max.words = 10, min.freq = 1, colors = color, rot.per = 0, scale = c(4,1), text = par(family = 'oxygen')))
  showtext.end()
  dev.off()
}


  




