library(readxl)
library(rio)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)

schools <- data_frame(school = c("Conestoga","Humber","Mohawk","Seneca","Sheridan","Algonquin","SLC", "Guelph","Queens","Ryerson","Umanitoba","McMaster","UofT","Waterloo"),
                      alias = c("College 1", "College 2", "College 3", "College 4", "College 5","College 6","College 7", "University 1","University 2", "University 3", "University 4", "University 5","University 6","University 7"))

pruned_schools <- data_frame(school = c("Conestoga", "Mohawk","Seneca","Sheridan","Algonquin","SLC", "Guelph","Queens","McMaster","UofT","Waterloo","Ryerson"),
                      alias = c("College 1", "College 2", "College 3", "College 4", "College 5","College 6", "University 1","University 2", "University 3", "University 4", "University 5","University 6"))

`type of knowledge` <- c("Factual",
                      "Conceptual",
                      "Computational",
                      "Math translation",
                      "Investigative")

`cognitive process` <- c("Remember",
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

`depth of analysis` <- c(
  "Solved by standardized ways",
  "Solved by well-proven analytical techniques",
  "Originality in analysis, no obvious solutions"
)

interdependence <- c(
  "Discrete components",
  "Parts of systems within complex engineering problems",
  "High level problems including many component parts or sub-problems"
)

novelty <- c("Familiar Problems","Reorganized Problems","New Problems")

scaffolding <- c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted")
communication <- c("Interpretation","Representation",	"Calculation",	"Application", "Assumptions",	"Communication")




# Calculus and Physics rater data
rater_data <- read_excel(list.files("./data/ONCAT V1", pattern = "ONCAT_", full.names = TRUE), sheet = "Master") %>% 
  set_colnames(c("school","subject", "content", "question", "rater","cognitive process","type of knowledge","transfer","depth of analysis","interdependence")) %>% 
  mutate(type = ifelse(school %in% c("Conestoga", "Seneca","Mohawk","Algonquin","Sheridan","SLC"), "College", "University")) %>% 
  select(school,type,subject, content, question, rater,everything()) 

rater_data_V2 <- read_excel(list.files("./data/ONCAT V2", pattern = "V3", full.names = TRUE), sheet = "Master") %>% 
  select(-nov) %>% 
  set_colnames(c("school","subject", "content", "question", "rater","cognitive process","solo taxonomy","scaffolding")) %>% 
  mutate(type = ifelse(school %in% c("Conestoga", "Seneca","Mohawk","Algonquin","Sheridan","SLC","Humber"), "College", "University")) %>% 
  select(school,type,subject, content, question, rater,everything()) 


# Design Data

design_data <- read_excel(list.files("./data/ONCAT V1", pattern = "Design", full.names = TRUE), sheet = "Projects") %>% 
  filter(!is.na(School))

design_outcomes_data <- read_excel(list.files("./data/ONCAT V1", pattern = "Design", full.names = TRUE), sheet = 2) %>% 
  filter(!is.na(School))

# Compute data by gropings
med_by_rater <- rater_data %>% 
  group_by(subject, type, school, question, content) %>% 
  summarize_each(funs(median), -school:-rater)

# summary_list <- means_by_rater$summary %>% 
#   set_names(c("Calculus - College", "Calculus - University", "Physics - College", "Physics - University"))
# 
# write.xlsx(summary_list, "ONCAT Rater Data Summary Statistics.xlsx")
# 
#   mutate_each(funs(round), `cognitive process`:`interdependence`)

   
# means_by_type <- rater_data %>% 
#   group_by(subject, type) %>% 
#   summarize_each(funs(mean), `cognitive process`:`interdependence`)
# 
# means_by_school <- rater_data %>% 
#   group_by(subject, school) %>% 
#   summarize_each(funs(mean), `cognitive process`:`interdependence`)
# 
# means_by_school_content <- rater_data %>% 
#   group_by(subject, school, content) %>% 
#   summarize_each(funs(mean), `cognitive process`:`interdependence`)

# Set factor levels for each
med_by_rater$`cognitive process` %<>%
  factor(c(1:6), `cognitive process`)

med_by_rater$`type of knowledge` %<>%
  factor(c(1:5), `type of knowledge`)

med_by_rater$transfer %<>%
  factor(c(1:5), transfer)

med_by_rater$`depth of analysis` %<>%
  factor(c(1:3), `depth of analysis`)

med_by_rater$interdependence %<>%
  factor(c(1:3), interdependence)


  


# Survey Data
#

survey_key <- read_excel(list.files("./data/ONCAT V1", pattern = "key", full.names = TRUE)) %>%
  separate(school, c("school","course"), sep = "/") %>%
  gather(sub_question,question, -1:-3) %>%
  filter(!is.na(question))

survey_data <- read_excel(list.files("./data/ONCAT V1", pattern = "Survey", full.names = TRUE), sheet = "Data") %>%
  gather(variable, value, -subject:-school) %>%
  mutate(value = ifelse(value == "NA", NA, value)) %>%
  separate(variable,c("question","subquestion"), sep = "\\(") %>%
  select(-subquestion) %>%
  spread(question,value)

survey_data$Q1 <- factor(survey_data$Q1, c(1:5),c("Every week","At the end of each chapter", "Every month", "One midterm,One final","One final"), exclude = NULL)

survey_data$Q2 <- factor(survey_data$Q2, c(1:3),c("Once","Two Times","More than two"), exclude = NULL)

survey_data$Q3 <- factor(survey_data$Q3, c(1:4),c("Within one week","At the end of each chapter",	"Between one week and a month",	"Longer than one month"), exclude = NULL)

survey_data$Q4 <- factor(survey_data$Q4, c(1:6),c("I don’t know",	"No Confidence","Somewhat confident",	"Neutral", "Confident",	"Very Confident"), exclude = NULL)

survey_data$Q5 <- factor(survey_data$Q5, c(1:6),c("I don’t know",	"Poor",	"Fair",	"Neutral",	"Good",	"Very Good"), exclude = NULL)

survey_data$`Q6-a` <- factor(survey_data$`Q6-a`, c(1:3),c("Familiar",	"Reorganized",	"New"), exclude = NULL)
survey_data$`Q6-b` <- factor(survey_data$`Q6-b`, c(1:3),c("Familiar",	"Reorganized",	"New"), exclude = NULL)
survey_data$`Q6-c` <- factor(survey_data$`Q6-c`, c(1:3),c("Familiar",	"Reorganized",	"New"), exclude = NULL)
survey_data$`Q6-d` <- factor(survey_data$`Q6-d`, c(1:3),c("Familiar",	"Reorganized",	"New"), exclude = NULL)
survey_data$`Q6-e` <- factor(survey_data$`Q6-e`, c(1:3),c("Familiar",	"Reorganized",	"New"), exclude = NULL)

survey_data$`Q7-a` <- factor(survey_data$`Q7-a`, c(1:4),c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted"), exclude = NULL)
survey_data$`Q7-b` <- factor(survey_data$`Q7-b`, c(1:4),c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted"), exclude = NULL)
survey_data$`Q7-c` <- factor(survey_data$`Q7-c`, c(1:4),c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted"), exclude = NULL)
survey_data$`Q7-d` <- factor(survey_data$`Q7-d`, c(1:4),c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted"), exclude = NULL)
survey_data$`Q7-e` <- factor(survey_data$`Q7-e`, c(1:4),c("Prescribed",	"Constrained",	"Scaffolded",	"Adopted"), exclude = NULL)

survey_data$`Q8-a` <- factor(survey_data$`Q8-a`, c(1:6),c("Interpretation",	"Representation",	"Calculation",	"Application",	"Assumptions",	"Communication"), exclude = NULL)
survey_data$`Q8-b` <- factor(survey_data$`Q8-b`, c(1:6),c("Interpretation",	"Representation",	"Calculation",	"Application",	"Assumptions",	"Communication"), exclude = NULL)
survey_data$`Q8-c` <- factor(survey_data$`Q8-c`, c(1:6),c("Interpretation",	"Representation",	"Calculation",	"Application",	"Assumptions",	"Communication"), exclude = NULL)
survey_data$`Q8-d` <- factor(survey_data$`Q8-d`, c(1:6),c("Interpretation",	"Representation",	"Calculation",	"Application",	"Assumptions",	"Communication"), exclude = NULL)
survey_data$`Q8-e` <- factor(survey_data$`Q8-e`, c(1:6),c("Interpretation",	"Representation",	"Calculation",	"Application",	"Assumptions",	"Communication"), exclude = NULL)

survey_data <- gather(survey_data, question, value, -subject:-school)

survey_data <- separate(survey_data, question, c("question","sub_question"), sep = "-")



temp<-semi_join(med_by_rater, survey_key, by = c("subject", "school", "question"))

survey_scatter <- filter(survey_data, question %in% c("Q6","Q7","Q8")) %>%
  separate(school, c("school","course"), sep = "/") %>%
  rename(survey_question = question) %>%
  left_join(survey_key) %>%
  # select(subject,type,school,survey_question,question,value) %>%
  mutate(survey_question = str_replace(survey_question,"Q6","novelty")) %>%
  mutate(survey_question = str_replace(survey_question,"Q7","scaffolding")) %>%
  mutate(survey_question = str_replace(survey_question,"Q8","communication")) %>%
  spread(survey_question, value) %>%
  select(-course,-sub_question) %>%
  filter(!is.na(question)) %>%
  left_join(temp,., by = c("subject","type","school","question")) %>%
  select(-interdependence)


survey_scatter$novelty %<>%
  factor(c("Familiar","Reorganized","New"), novelty , exclude = NULL)

survey_scatter$scaffolding %<>%
  factor(scaffolding,scaffolding,exclude = NULL)

survey_scatter$communication %<>%
  factor(communication, communication,exclude = NULL)



