# This file is mostly for analysing the inequality coefficients, and inparticular, for reshaping the data so it can be used to get Gini coeffs / Lorenz curves.

# Hygiene. 
groundhog.date <- '2024-01-01'
library(groundhog)
pkgs <- c('here', 'dineq', 'rsample', 'tidyverse', 'haven')
groundhog.library(pkgs,
                  groundhog.date,
                  tolerate.R.version='4.2.3')

here::i_am('gini.R')

# Helper Functions.
# Function to calculate cumulative shares
lorenz <- function(incomes) {
  sorted_incomes <- sort(incomes)
  cum_income <- cumsum(sorted_incomes)
  total_income <- sum(incomes)
  prop_income <- cum_income / total_income
  population <- seq_along(incomes) / length(incomes)
  return(list(population=population, prop_income=prop_income))
}

# Function to calculate the Gini coefficient
gini <- function(incomes) {
  n <- length(incomes)
  incomes <- sort(incomes)
  G <- (2 * sum((1:n) * incomes)) / (n * sum(incomes)) - (n + 1) / n
  return(G)
}

# Coefficient of variation.
cv <- function(x){
  x <- as.numeric(x)
  sd(x) / mean(x)
}

# standard error of the mean:
se_mean <- function(x){
  se <- sd(x) / sqrt(length(x))
  return(se)
}




# Theoretical distributions. Post Tax. 
anarchy <- c(
  rep(15, 33), # unlucky B
  rep(25, 33), # neutral B
  rep(50, 33), # lucky B
  rep(60, 33), # unlucky A
  rep(100, 33), # neutral A
  rep(200, 33) # lucky A
)

tax <- c(
  rep(25, 33), # unlucky B
  rep(35, 33), # neutral B
  rep(60, 33), # lucky B
  rep(54, 33), # unlucky A
  rep(90, 33), # neutral A
  rep(180, 33) # lucky A
)

ubi <- c(
  rep(21, 33), # unlucky B
  rep(31, 33), # neutral B
  rep(56, 33), # lucky B
  rep(60, 33), # unlucky A
  rep(96, 33), # neutral A
  rep(186, 33) # lucky A
)

# Luck and Effort have the same distribution.
luck <- c(
  rep(21, 100*0.85), # unlucky B
  rep(31, 100*0.85), # neutral B
  rep(56, 100*0.85), # lucky B
  rep(54, 100*0.15), # unlucky B who's mobile
  rep(90, 100*0.15), # neutral B who's mobile
  rep(180, 100*0.15), # lucky B who's mobile
  rep(54, 100), # unlucky A
  rep(90, 100), # neutral A
  rep(180, 100) # lucky A
)


theoretical_gini <- c()
theoretical_theil <- c()

for(x in list(anarchy, tax, ubi, luck)){
  theoretical_gini <- c(theoretical_gini, round(gini(x), 3))
  theoretical_theil <- c(theoretical_theil, round(theil.wtd(x),3))
}

trtmnts <- c("Anarchy", "Tax", "UBI", "Luck/Effort")
theoretical_values <- data.frame(cbind(trtmnts,theoretical_gini, theoretical_theil))



# Getting the realised distributions for i) prior to tax and ii) post tax, but prior to private redistribution.
# CLUSTERED BOOTSTRAP for realised redistributions prior to private transfers

df <- read_stata("replication_set.dta") %>% 
  filter(part == 2) %>% 
  select(c('country', 'treatment', 'income', 'p_income', 'id', 'change', 'income_2', 'income_2_post', 'type', 'p_type'))

bs_gini <- list()
counter <- 1
set.seed(007) # Bond, James Bond
for(cntry in 1:4){
for(trtmnt in 1:5){
  
  tdf <- df %>% 
    filter(country == cntry & treatment == trtmnt) %>% 
    group_by(id) %>% 
    nest()
  
  bs <- bootstraps(tdf, times = 10) # FOR REPLICATION  SPEED, SETTING TO 10, NOT 1000
  bs_gini[[counter]] <- map(bs$splits, ~as_tibble(.) %>% 
                             unnest %>% 
                             summarize(gini_pre = gini(income_2),
                                       gini_post_tax = gini(income_2_post),
                                       theil_pre = theil.wtd(income_2),
                                       theil_post_tax = theil.wtd(income_2_post))) %>% 
    bind_rows
  
  bs_gini[[counter]]$country <- cntry
  bs_gini[[counter]]$treatment <- trtmnt
  
  counter <- counter + 1
}
}

foo <- bind_rows(bs_gini)

# Mean and 90% confidence intervals:
gini_table <- foo %>% 
  group_by(country, treatment) %>% 
  summarise(mean_value_gini_post_tax = mean(gini_post_tax),
            lower_ninety_percent_ci_gini_post_tax = quantile(gini_post_tax, probs = 0.05),
            upper_ninety_percent_ci_gini_post_tax = quantile(gini_post_tax, probs = 0.95),
            mean_value_gini_pre = mean(gini_pre),
            lower_ninety_percent_ci_gini_pre = quantile(gini_pre, probs = 0.05),
            upper_ninety_percent_ci_gini_pre = quantile(gini_pre, probs = 0.95),
            mean_value_theil_pre = mean(theil_pre),
            lower_ninety_percent_ci_theil_pre = quantile(theil_pre, probs = 0.05),
            upper_ninety_percent_ci_theil_pre = quantile(theil_pre, probs = 0.95),
            mean_value_theil_post_tax = mean(theil_post_tax),
            lower_ninety_percent_ci_theil_post_tax = quantile(theil_post_tax, probs = 0.05),
            upper_ninety_percent_ci_theil_post_tax = quantile(theil_post_tax, probs = 0.95),
  )

gini_table$treatment[gini_table$treatment == 1] <- "anarchy"
gini_table$treatment[gini_table$treatment == 2] <- "tax"
gini_table$treatment[gini_table$treatment == 3] <- "ubi"
gini_table$treatment[gini_table$treatment == 4] <- "effort"
gini_table$treatment[gini_table$treatment == 5] <- "luck"
write.csv(gini_table,here("gini_part2_no_private_transfers.csv"))




# Bootstrapping the realised, post private transfers stuff. 
# For anarchy/ubi/tax first, turn the above into a function:
anarchy <- function(test){
  post_pt <- vector() 
  for(i in 1:nrow(test)){
    own_income <- test[i, "income_2_post"][[1]][1]
    p_income <- test[i, "p_income"][[1]][1]
    partner_income <- 0
    
    # draw a random row in the df, until you find one that matches the partner's income.
    while(p_income != partner_income){
      partner <- sample(x = 1:nrow(test), size = 1)
      partner_income <- test[partner, "p_income"][[1]][1]
    }
    
    p_id <- test[partner,"id"][[1]][1]
    p_change <- test[test$id == p_id & test$p_income == own_income, "change"][[1]][1] # This needs to change for luck/effort treatments.
    
    new_income <- own_income + (p_change*2)
    post_pt <- c(post_pt, new_income)
  }
  foo <- data.frame(post_pt)
  foo$country <- cntry
  foo$treatment <- trtmnt
  
  return(foo)
}

df <- read_stata("replication_set.dta") %>% 
  filter(part == 2) %>% 
  select(c('country', 'treatment', 'income', 'p_income', 'id', 'change', 'income_2', 'income_2_post', 'type', 'p_type'))

bs_gini <- list()
counter <- 1
set.seed(007) # Bond, James Bond
for(cntry in 1:4){
  for(trtmnt in 1:3){
    tdf <- df %>% 
      filter(country == cntry & treatment == trtmnt) %>% 
      group_by(id) %>% 
      nest()
    
    bs <- bootstraps(tdf, 
                     times = 10) # the no. of bootstrap iterations. # SHOULD BE 1000, BUT FOR REPLICATION PACKAGE RUN TIME, SETTING TO: 10
    
    bs_gini[[counter]] <- map(bs$splits, ~as_tibble(.) %>% 
                                unnest %>% 
                                anarchy %>% # I could stop here, but that would mean keeping large dfs in memory.
                                summarize(gini = gini(post_pt),
                                          theil = theil.wtd(post_pt))) %>% 
                                bind_rows
    
    bs_gini[[counter]]$country <- cntry
    bs_gini[[counter]]$treatment <- trtmnt
    counter <- counter + 1
    print(counter)
}
}

foo <- bind_rows(bs_gini)


gini_table <- foo %>% 
  group_by(country, treatment) %>% 
  summarise(mean_value_gini = mean(gini),
            lower_ninety_percent_ci_gini = quantile(gini, probs = 0.05),
            upper_ninety_percent_ci_gini = quantile(gini, probs = 0.95),
            mean_value_theil = mean(theil),
            lower_ninety_percent_ci_theil = quantile(theil, probs = 0.05),
            upper_ninety_percent_ci_theil = quantile(theil, probs = 0.95),
  )

gini_table$treatment[gini_table$treatment == 1] <- "anarchy"
gini_table$treatment[gini_table$treatment == 2] <- "tax"
gini_table$treatment[gini_table$treatment == 3] <- "ubi"
gini_table$treatment[gini_table$treatment == 4] <- "effort"
gini_table$treatment[gini_table$treatment == 5] <- "luck"
write.csv(gini_table,here("anarchy_bootstrapped.csv"))


# For social mobility effort/luck

socmob <- function(test){
  post_pt <- vector()
  
  for(i in 1:nrow(test)){
    own_income <- test[i, "income_2_post"][[1]][1]
    own_situation <- test[i, "foo"][[1]][1]
    p_situation <- test[i, "p_foo"][[1]][1]
    partner_situation <- ""
    
    # draw a random row in the df, until you find one that matches the partner's situation.
    while(partner_situation != p_situation){
      partner <- sample(x = 1:nrow(test), size = 1)
      partner_situation <- test[partner, "p_foo"][[1]][1]
    }
    
    p_id <- test[partner,"id"][[1]][1]
    p_change <- test[test$id == p_id & test$p_foo == own_situation, "change"][[1]][1] # This needs to change for luck/effort treatments.
    new_income <- own_income + (p_change*2)
    post_pt <- c(post_pt, new_income)
  }
  
  foo <- data.frame(post_pt)
  foo$country <- cntry
  foo$treatment <- trtmnt
  
  return(foo)
}

bs_gini <- list()
counter <- 1
set.seed(007) # Bond, James Bond
for(cntry in c(1:4)){
  for(trtmnt in c(4,5)){
    test <- df %>% 
      filter(country == cntry & treatment == trtmnt)
    
    test$foo <- "low_neg"
    test$foo[test$income_2_post == 25] <- "low_neutral"
    test$foo[test$income_2_post == 50] <- "low_pos"
    test$foo[test$income_2_post == 54 & test$type == 2] <- "low_up_neg"
    test$foo[test$income_2_post == 54 & test$type == 1] <- "high_neg"
    test$foo[test$income_2_post == 90 & test$type == 2] <- "low_up_neutral"
    test$foo[test$income_2_post == 90 & test$type == 1] <- "high_neutral"
    test$foo[test$income_2_post == 180 & test$type == 2] <- "low_up_pos"
    test$foo[test$income_2_post == 180 & test$type == 1] <- "high_pos"
    
    test$p_foo <- "low_neg"
    test$p_foo[test$p_income == 25] <- "low_neutral"
    test$p_foo[test$p_income == 50] <- "low_pos"
    test$p_foo[test$p_income == 54 & test$p_type == 2] <- "low_up_neg"
    test$p_foo[test$p_income == 54 & test$p_type == 1] <- "high_neg"
    test$p_foo[test$p_income == 90 & test$p_type == 2] <- "low_up_neutral"
    test$p_foo[test$p_income == 90 & test$p_type == 1] <- "high_neutral"
    test$p_foo[test$p_income == 180 & test$p_type == 2] <- "low_up_pos"
    test$p_foo[test$p_income == 180 & test$p_type == 1] <- "high_pos"
    
    tdf <- test %>% 
      group_by(id) %>% 
      nest()

    bs <- bootstraps(tdf, 
                     times = 10) # the no. of bootstrap iterations. # FOR REPLICATION  SPEED, SETTING TO 10, NOT 1000
    
    bs_gini[[counter]] <- map(bs$splits, ~as_tibble(.) %>% 
                                unnest %>% 
                                socmob %>% # I could stop here, but that would mean keeping large dfs in memory.
                                summarize(gini = gini(post_pt),
                                          theil = theil.wtd(post_pt))) %>% 
                                bind_rows
    
    bs_gini[[counter]]$country <- cntry
    bs_gini[[counter]]$treatment <- trtmnt
    counter <- counter + 1
    print(counter)

  }
}

foo <- bind_rows(bs_gini)

gini_table <- foo %>% 
  group_by(country, treatment) %>% 
  summarise(mean_value_gini = mean(gini),
            lower_ninety_percent_ci_gini = quantile(gini, probs = 0.05),
            upper_ninety_percent_ci_gini = quantile(gini, probs = 0.95),
            mean_value_theil = mean(theil),
            lower_ninety_percent_ci_theil = quantile(theil, probs = 0.05),
            upper_ninety_percent_ci_theil = quantile(theil, probs = 0.95)
  )

gini_table$treatment[gini_table$treatment == 4] <- "effort"
gini_table$treatment[gini_table$treatment == 5] <- "luck"
write.csv(gini_table,here("social_mobility_bootstrapped.csv"))