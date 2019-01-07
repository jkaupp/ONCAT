library(psych)
library(nFactors)
library(tidyverse)
library(irr)
library(readxl)
library(corrr)
library(janitor)
library(corrr)
library(corrplot)


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


file <- list.files("./data/ONCAT V1", full.names = TRUE, pattern = "AlphaAnalysis")

sheets <- excel_sheets(file)

data <- map2_df(file, sheets, ~read_excel(.x, sheet = .y, col_names = FALSE) %>% mutate(id = .y) %>%  set_names(c("rater_1","rater_2","rater_3", "id"))) %>% 
  separate(id, c("subject", "variable"), "\\_") %>% 
  mutate_if(is_character, trimws)
  

# Alphas ----
alphas <- data %>% 
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
  

#select(alphas, subject, variable, raters, subjects, agreement, k_alpha, k_fleiss, icc, gAC_1) %>% 
# write.csv("IRR.csv")

# Correlations ----

corr_data <- data %>% 
  rowwise() %>% 
  mutate(median = mean(c(rater_1, rater_2, rater_3))) %>% 
  select(subject, variable, median) %>% 
  group_by(subject, variable) %>% 
  nest() %>% 
  mutate(data = map2(data, variable, ~rename_(.x, .dots = setNames("median", .y) %>% clean_names))) %>% 
  spread(variable, data) %>% 
  unnest() 

corr_by_subject <- corr_data %>% 
  group_by(subject) %>% 
  nest() %>% 
  mutate(corr = map(data, ~correlate(.x)))

overall_corr <- corr_data %>%
  setNames(.,c("Subject", "Cognitive Process","Depth of Analysis","Interdependence","Transfer","Type of Knowledge")) %>% 
  select(-Subject) %>% 
  correlate() %>% 
  fashion() %>% 
  write.csv("corr.csv")

HH::export.eps("Correlations.eps", width = 16, height = 10)

png("Correlations.png", width = 720, height = 480)
corrplot(overall_corr, 
         method = "color",
         addgrid.col = "white",
         diag = TRUE,
         type = "lower",
         p.mat = sig,
         sig.level = 0.05,
         insig = "p-value",
         title = "",
         col = viridis::viridis(10),
         tl.cex = 1,
         tl.col = "grey20",
         cl.cex = 1)
dev.off()


sig <- corr_data %>% 
  select(-subject) %>% 
  psych::corr.test() %>% 
  .$p

overall_corr %>% 
  fashion() %>% write.csv("corr.csv")

#EFA ----
cor_mat <- cor(corr_data[-1])

nS <- nScree(x=cor_mat)

plotnScree(nS)

fa(cor_mat, nfactors = 4,  fm = "pa", SMC = FALSE)


# chi-sq
chi_rater <- med_by_rater %>% 
  janitor::clean_names() %>% 
  ungroup %>% 
  select(subject, type, 6:10) %>% 
  split(.$subject)


chi_test <- function(data, x) {
coin::chisq_test(table(data[c(x, "type")])) 
}


purrr::map(names(chi_rater$Physics[,3:7]), ~fisher_test(chi_rater$Physics, .x)) %>%
  map_df(broom::tidy) %>% 
  bind_cols(data.frame(variable = names(chi_rater$Physics[,3:7]))) %>% 
  mutate(vs = "type") %>% 
  mutate(subject = "physics") %>% 
  select(subject, vs, variable, method, p.value, alternative)


wilcox <- function(data, x) {
  
  new <- data %>% 
    mutate_if(.predicate = is.factor, funs(as.numeric)) %>% 
    mutate(type = factor(type, c("College","University")))  
  
   wilcox.test(eval(as.name(x)) ~ type, data = new, alternative = "greater", exact = TRUE) %>%
     broom::tidy()
  
}


physics <- purrr::map(names(chi_rater$Physics[,3:7]), ~chi_test(chi_rater$Physics, .x))
  
phys_table <- tibble(chi_sq = map(physics, ~statistic(.x)) %>% unlist,
       n = nrow(chi_rater$Physics),
       k = 2,
       p = map(physics, ~pvalue(.x)) %>%  unlist) %>% 
  mutate(V = sqrt(chi_sq/(n*(k-1))),
         variable = names(chi_rater$Physics[,3:7]),
         subject = "Physics") %>% 
  select(subject, variable, n, k, p, V)
  


calculus <- purrr::map(names(chi_rater$Calculus[,3:7]), ~chi_test(chi_rater$Calculus, .x))

calc_table <- tibble(chi_sq = map(calculus, ~statistic(.x)) %>% unlist,
       n = nrow(chi_rater$Calculus),
       k = 2,
       p = map(calculus, ~pvalue(.x)) %>%  unlist) %>% 
  mutate(V = sqrt(chi_sq/(n*(k-1))),
         variable = names(chi_rater$Calculus[,3:7]),
         subject = "Calculus") %>% 
  select(subject, variable, n, k, p, V)
  
output <- bind_rows(phys_table, calc_table) %>% 
  setNames(c("Subject","Variable","n", "k", "p-value", "Cramers V (effect size)")) %>% 
  write.csv("Chi-squared tests.csv")
 