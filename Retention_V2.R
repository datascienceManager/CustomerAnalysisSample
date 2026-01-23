library(dplyr)
library(lubridate)
set.seed(123)

n_customers <- 25
subs_per_customer <- sample(2:6, n_customers, replace = TRUE)

synthetic_data <- lapply(1:n_customers, function(cid) {
  
  n <- subs_per_customer[cid]
  start_date <- as.Date("2024-01-01") + sample(0:200, 1)
  
  data.frame(
    CustomerID = cid,
    Subscription_ID = sample(1000:9999, n),
    Subscription_version = 1:n,
    Subscription_start_date = start_date + cumsum(sample(25:40, n, replace = TRUE)),
    Expiry_date = NA,
    Product_name = sample(c("XYZTotal", "4KTotal", "FootballSeasonal"), n, replace = TRUE),
    Offer_period = sample(c("monthly", "annual"), n, replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Expiry_date = ifelse(
        Offer_period == "monthly",
        Subscription_start_date + 30,
        Subscription_start_date + 365
      ) %>% as.Date(origin = "1970-01-01"),
      
      Subscription_status = ifelse(runif(n) > 0.7, "churned", "active"),
      
      Subscription_type = case_when(
        Subscription_status == "churned" ~ "churned",
        Subscription_version == 1 ~ "new",
        TRUE ~ sample(c("renewal", "winback"), 1, prob = c(0.6, 0.4))
      ),
      
      Subscription_latest = ifelse(Subscription_version == max(Subscription_version), 1, 0)
    )
})

subs <- bind_rows(synthetic_data)
nrow(subs)




# 
# Calculate TRUE Winback Time (Strict Logic)
# Definition
# Churn → next subscription
# Start date after expiry
# Subscription_type = winback


winback_analysis <- subs %>%
  dplyr::filter(.,CustomerID==13)%>%
  # dplyr::mutate(.,Subscription_version=dplyr::if_else(Subscription_version==))
  arrange(CustomerID, Subscription_start_date) %>%
  group_by(CustomerID) %>%
  mutate(
    prev_status = lag(Subscription_status),
    prev_expiry = lag(Expiry_date),
    
    winback_flag = Subscription_type == "winback" &
      prev_status == "churned" &
      Subscription_start_date > prev_expiry,
    
    days_to_winback = ifelse(
      winback_flag,
      as.numeric(Subscription_start_date - prev_expiry),
      NA
    )
  ) %>%
  ungroup()%>% data.frame()


  
  
  
  
  library(dplyr)
  library(lubridate)
  set.seed(42)
  
  n_customers <- 25
  
  synthetic_data <- lapply(1:n_customers, function(cid) {
    
    n_subscriptions <- sample(2:4, 1)
    base_date <- as.Date("2024-01-01") + sample(0:180, 1)
    
    data.frame(
      CustomerID = cid,
      Subscription_ID = sample(1000:9999, n_subscriptions),
      Subscription_start_date = base_date + cumsum(sample(40:90, n_subscriptions)),
      Product_name = sample(c("XYZTotal", "4KTotal", "FootballSeasonal"),
                            n_subscriptions, replace = TRUE),
      Offer_period = sample(c("monthly", "annual"),
                            n_subscriptions, replace = TRUE),
      stringsAsFactors = FALSE) %>%
    mutate(
        Subscription_version = 1,   # resets per new Subscription_ID
        
        Expiry_date = ifelse(
          Offer_period == "monthly",
          Subscription_start_date + 30,
          Subscription_start_date + 365
        ) %>% as.Date(origin = "1970-01-01"),
        
        Subscription_status = sample(
          c("active", "churned"),
          n_subscriptions,
          replace = TRUE,
          prob = c(0.65, 0.35)
        ),
        
        Subscription_type = case_when(
          Subscription_status == "churned" ~ "churned",
          Subscription_start_date == min(Subscription_start_date) ~ "new",
          TRUE ~ "winback"
        ))
  }
    
    subs <- bind_rows(synthetic_data)
    nrow(subs)
    
  
  
  
  
  
  
  
  
  
  
  
  
