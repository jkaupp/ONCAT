library(readxl)
library(rio)
library(magrittr)
library(dplyr)
library(tidyr)

type_knowledge <- c("Factual",
                      "Conceptual",
                      "Computational",
                      "Math translation",
                      "Investigative")

cognitive_process <- c("Remember",
                        "Understand",
                        "Apply",
                        "Analyze",
                        "Evaluate",
                        "Create")

transfer <- c(
  "Mathematical knowledge",
  "Apply in a disciplinary context",
  "Apply in other engineering contexts",
  "Apply to real-world predictable contexts",
  "Apply to real-world unpredictable contexts"
)

depth_knowledge <- c(
  "Solved by standardized ways",
  "Solved by well-proven analyitcal techniques",
  "Originiality in analysis, no obvious soltions"
)

interdependence <- c(
  "Discrete components",
  "Parts of systems within complex engineering problems",
  "High level problems including many component parts or sub-problems"
)

novelty <- c(
  "Familiar problems",
  "Reorganized problems",
  "New problems"
)


# Calculus and Physics rater data

rater_data <- import(list.files(data.dir, pattern = "ONCAT_", full.names = TRUE), sheet = "Master") %>% 
  set_colnames(c("school","subject", "content", "question", "rater","cognitive process","type of knowledge","transfer","depth of knowledge","interdependence")) %>% 
  mutate(type = ifelse(school %in% c("Seneca","Mohawk","Algonquin","Sheridan","SLC"), "College", "University")) %>% 
  select(school,type,subject, content, question, rater,everything()) 

# Set factor levels for each
rater_data$`cognitive process` %<>%
  factor(c(1:6), cognitive_process)

rater_data$`type of knowledge` %<>%
  factor(c(1:5), type_knowledge)

rater_data$transfer %<>%
  factor(c(1:5), transfer)

rater_data$`depth of knowledge` %<>%
  factor(c(1:3), depth_knowledge)

rater_data$interdependence %<>%
  factor(c(1:3), interdependence)


# Compute data by gropings
means_by_rater <- rater_data %>% 
  group_by(subject, type, school, question) %>% 
  summarize_each(funs(mean), `cognitive process`:`interdependence`)
   
means_by_type <- rater_data %>% 
  group_by(subject, type) %>% 
  summarize_each(funs(mean), `cognitive process`:`interdependence`)

means_by_school <- rater_data %>% 
  group_by(subject, school) %>% 
  summarize_each(funs(mean), `cognitive process`:`interdependence`)

means_by_school_content <- rater_data %>% 
  group_by(subject, school, content) %>% 
  summarize_each(funs(mean), `cognitive process`:`interdependence`)



