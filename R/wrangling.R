library(readxl)
library(rio)
library(magrittr)
library(dplyr)
library(tidyr)

read.files <- function(sheet)
{
  read_excel(data.file, sheet) %>% 
    set_colnames(., tolower(names(.))) %>% 
    mutate(school = sheet)
}

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

data.dir <- "~/ownCloud/Projects/R/Projects/ONCAT/data/"

data.file <- paste0(data.dir,"Calculus Outcomes.xlsx")

sheets <- excel_sheets(data.file)

oncat.data <- lapply(sheets, read.files) %>% 
  set_names(., sheets) %>% 
  bind_rows() %>% 
  mutate(type = ifelse(grepl("Seneca|Mohawk",school),"College","University")) %>% 
  separate(school,c("school","course"), sep = " ")
  

oncat.data$`cognitive process` %<>%
  factor(c(1:6), cognitive_process)

oncat.data$`type of knowledge` %<>%
  factor(c(1:5), type_knowledge)

oncat.data$transfer %<>%
  factor(c(1:5), transfer)

oncat.data$`depth of knowledge` %<>%
  factor(c(1:3), depth_knowledge)

oncat.data$interdependence %<>%
  factor(c(1:3), interdependence)

oncat.data$novelty %<>%
  factor(c(1:3), novelty)



