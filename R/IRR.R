library(tidyverse)
library(irr)
library(readxl)

gwet<-function(ratings){
  #First, convert ratings into a r_ik table. 
  # r_ik is the number of raters that voted object i to be of a category k.
  # Some ratings can be missing, so accept NAs in ratings
  ratings<-as.matrix(ratings);
  rLevels<-ratings; dim(rLevels)<-NULL; na.omit(unique(rLevels))->rLevels;
  #If there is only single class, all raters agree
  if(length(rLevels)==1) return(1);
  sapply(rLevels,function(level) rowSums(ratings==level,na.rm=TRUE))->r_ik;
  
  #Next calculate pi_k, vector of mean number of rater votes for each class
  # over objects
  pi_k<-colMeans(r_ik/rowSums(r_ik));
  
  #Then calculate p_e
  p_e<-sum(pi_k*(1-pi_k))/(length(pi_k)-1);
  
  #Now, remove objects that got vote only from a single rater
  r_ik<-r_ik[rowSums(r_ik)>1,];
  
  #Calculate p_a on the cleared set
  rowSums(r_ik)->r_i;
  p_a<-mean(rowSums(r_ik*(r_ik-1))/(r_i*(r_i-1)));
  
  #Calculate gamma_1
  (p_a-p_e)/(1-p_e)
}


file <- list.files("./data", full.names = TRUE, pattern = "AlphaAnalysis")

sheets <- excel_sheets(file)

data <- map2_df(file, sheets, ~read_excel(.x, sheet = .y, col_names = FALSE) %>% mutate(id = .y) %>%  set_names(c("rater_1","rater_2","rater_3", "id"))) %>% 
  separate(id, c("subject", "variable"), "\\_") %>% 
  group_by(subject, variable) %>% 
  nest()

alphas <- data %>% 
  mutate(agree = map(data, ~agree(as.matrix(.x)))) %>% 
  mutate(iccs = map(data, ~icc(as.matrix(.x), "oneway", "consistency"))) %>% 
  mutate(k_alphas = map(data, ~kripp.alpha(t(as.matrix(.x)), "ordinal"))) %>% 
  mutate(k_fl = map(data, ~kappam.fleiss(as.matrix(.x)))) %>% 
  mutate(gAC_1 = map_dbl(data, ~gwet(as.matrix(.x)))) %>% 
  mutate(k_alpha = map_dbl(k_alphas, "value"),
         raters  = map_dbl(k_alphas, "raters"),
         subjects = map_dbl(k_alphas, "subjects"),
         k_fleiss = map_dbl(k_fl, "value"),
         agreement = map_dbl(agree, "value"),
         icc = map_dbl(iccs, "value"))
  

select(alphas, subject, variable, raters, subjects, agreement, k_alpha, k_fleiss, icc, gAC_1) %>% 
  write.csv("IRR.csv")
