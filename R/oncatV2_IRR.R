library(psych)
library(nFactors)
library(tidyverse)
library(irr)
library(readxl)
library(corrr)
library(janitor)
library(corrr)
library(corrplot)
library(tidyverse)


gwet <- function(ratings){
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

source('R/oncatV2_wrangling.R')

safe_spread <- safely(spread)

irr_data <- rater_data %>%
  gather(variable, rating, 7:9) %>% 
  group_by(school, type, subject, content, rater, variable) %>% 
  mutate(question = row_number()) %>% 
  ungroup %>% 
  split(list(.$school,.$subject), drop = TRUE) %>% 
  map_df(~spread(.x, rater, rating, drop = TRUE, sep = "_")) %>% 
  select(subject, variable, matches('rater'))

alphas <- irr_data %>% 
  group_by(subject, variable) %>% 
  nest() %>% 
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

alphas %>% 
  select(subject, variable, raters, subjects, gAC_1, k_alpha, k_fleiss, icc, agreement) %>% 
  write.csv("ONCAT V2 IRR Measures.csv")
