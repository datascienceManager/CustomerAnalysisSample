library(dplyr)
library(lubridate)
library(tidyr)


subs <- tibble::tribble(
  ~CustomerID, ~Country, ~Direct_Indirect, ~Product_name, ~Offer_period,
  ~Subscription_ID, ~Subscription_version, ~Subscription_latest,
  ~Subscription_status, ~Subscription_type,
  ~Subscription_start_date, ~Expiry_date,
  
  101,"USA","Direct","XYZTotal","monthly",8889,1,0,"churned","churned","1/18/2025","2/18/2025",
  101,"USA","Direct","FootballSeasonal","monthly",8889,2,1,"active","new","1/18/2025","2/18/2025",
  101,"USA","Direct","XYZTotal","annual",2024,1,1,"active","winback","2/18/2025","2/18/2026",
  102,"India","Direct","FootballSeasonal","annual",6752,1,1,"active","new","2/18/2025","2/18/2026",
  102,"Qatar","Direct","4KTotal","annual",1220,1,1,"active","winback","3/18/2025","4/18/2026",
  103,"USA","Direct","4KTotal","monthly",2390,1,0,"active","new","2/28/2025","3/28/2025",
  103,"USA","Direct","4KTotal","monthly",2390,2,1,"churned","churned","2/28/2025","3/28/2025",
  103,"USA","Direct","4KTotal","monthly",2450,1,1,"active","winback","3/18/2025","4/18/2026",
  103,"USA","Direct","4KTotal","monthly",2670,1,1,"active","winback","2/28/2025","3/29/2025",
  104,"Kenya","Direct","XYZTotal","annual",3498,1,0,"active","new","2/18/2025","3/18/2025",
  104,"Kenya","Direct","XYZTotal","annual",3498,2,1,"churned","churned","2/18/2025","3/18/2025"
) %>%
  mutate(
    Subscription_start_date = mdy(Subscription_start_date),
    Expiry_date = mdy(Expiry_date),
    start_month = floor_date(Subscription_start_date, "month")
  )%>%
  data.frame()


# Current Retained Customers (Top-line KPI)

# Business question:
#   How many customers are currently active?


CurrentRetainedCustomers= subs %>%
  filter(Subscription_latest == 1) %>%
  summarise(
    total_customers = n_distinct(CustomerID),
    active_customers = n_distinct(CustomerID[Subscription_status == "active"]),
    retention_rate = active_customers / total_customers
  )




# Customer-Level Retention Status (Who stayed, who didn’t)


customer_status <- subs %>%
  filter(Subscription_latest == 1) %>%
  select(CustomerID, Product_name, Offer_period, Subscription_status)

customer_status



# 
# Cohort Retention (Gold Standard)
# Step 1: Identify First Subscription (Cohort)

cohort <- subs %>%
  group_by(CustomerID) %>%
  summarise(cohort_month = min(start_month))




# Step 2: Join Back & Calculate Months Since Start


cohort_data <- subs %>%
  left_join(cohort, by = "CustomerID") %>%
  mutate(
    months_since_start =
      interval(cohort_month, start_month) %/% months(1)
  )


# Step 3: Cohort Retention Matrix


cohort_retention <- cohort_data %>%
  filter(Subscription_status == "active") %>%
  group_by(cohort_month, months_since_start) %>%
  summarise(
    retained_customers = n_distinct(CustomerID),
    .groups = "drop"
  )

cohort_retention



offer_retention <- subs %>%
  filter(Subscription_latest == 1) %>%
  group_by(Offer_period) %>%
  summarise(
    customers = n_distinct(CustomerID),
    active = n_distinct(CustomerID[Subscription_status == "active"]),
    retention_rate = active / customers
  )

offer_retention



product_retention <- subs %>%
  filter(Subscription_latest == 1) %>%
  group_by(Product_name) %>%
  summarise(
    customers = n_distinct(CustomerID),
    retained = n_distinct(CustomerID[Subscription_status == "active"]),
    retention_rate = retained / customers
  )

product_retention


# Continuous Renewal vs Break & Rejoin


renewal_behavior <- subs %>%
  group_by(CustomerID) %>%
  summarise(
    subscriptions = n_distinct(Subscription_ID),
    max_version = max(Subscription_version)
  ) %>%
  mutate(
    behavior = case_when(
      subscriptions == 1 & max_version > 1 ~ "continuous_renewal",
      subscriptions > 1 ~ "break_and_rejoin",
      TRUE ~ "single_term"
    )
  )

renewal_behavior




# Winback Rate (Recovery Power)


winback <- subs %>%
  group_by(CustomerID) %>%
  summarise(
    churned = any(Subscription_type == "churned"),
    winback = any(Subscription_type == "winback")
  )

winback %>%
  summarise(
    churned_customers = sum(churned),
    winback_customers = sum(winback),
    winback_rate = winback_customers / churned_customers
  )



# Customer Lifetime (Tenure)


lifetime <- subs %>%
  group_by(CustomerID) %>%
  summarise(
    first_start = min(Subscription_start_date),
    last_expiry = max(Expiry_date),
    lifetime_days = as.numeric(last_expiry - first_start)
  )

lifetime



# Country-Aware Retention (VPN-safe)
# Use dominant country per customer:


dominant_country <- subs %>%
  count(CustomerID, Country) %>%
  group_by(CustomerID) %>%
  slice_max(n, n = 1)


country_retention <- subs %>%
  filter(Subscription_latest == 1) %>%
  left_join(dominant_country, by = c("CustomerID","Country") )%>%
  group_by(Country) %>%
  summarise(
    customers = n_distinct(CustomerID),
    retained = n_distinct(CustomerID[Subscription_status == "active"])
  )

country_retention



# dplyr Code – Step by Step
# Step 0️⃣ Order Events Properly

winback_base <- subs %>%
  arrange(CustomerID, Subscription_start_date, Subscription_version)



# Step 1️⃣ Identify Churn Events


churn_events <- winback_base %>%
  filter(Subscription_status == "churned") %>%
  select(
    CustomerID,
    churn_subscription_id = Subscription_ID,
    churn_date = Expiry_date
  )


# Step 2️⃣ Identify Rejoin / Winback Events


rejoin_events <- winback_base %>%
  filter(Subscription_type == "winback") %>%
  select(
    CustomerID,
    rejoin_subscription_id = Subscription_ID,
    rejoin_date = Subscription_start_date
  )



# Match Each Churn to the Next Rejoin



winback_latency <- churn_events %>%
  inner_join(rejoin_events, by = "CustomerID") %>%
  filter(rejoin_date > churn_date) %>%
  group_by(CustomerID, churn_date) %>%
  slice_min(rejoin_date, n = 1) %>%
  ungroup() %>%
  mutate(
    days_to_winback = as.numeric(rejoin_date - churn_date)
  )



winback_latency_clean <- winback_latency %>%
  filter(days_to_winback >= 0)




winback_latency_clean %>%
  summarise(
    winback_customers = n_distinct(CustomerID),
    avg_days = mean(days_to_winback),
    median_days = median(days_to_winback),
    p75_days = quantile(days_to_winback, 0.75)
  )



winback_latency_clean %>%
  left_join(
    subs %>% select(CustomerID, Product_name),
    by = "CustomerID"
  ) %>%
  group_by(Product_name) %>%
  summarise(
    avg_days = mean(days_to_winback),
    customers = n()
  )


winback_latency_clean %>%
  left_join(
    subs %>% select(CustomerID, Offer_period),
    by = "CustomerID"
  ) %>%
  group_by(Offer_period) %>%
  summarise(
    avg_days = mean(days_to_winback)
  )



winback_latency_clean %>%
  mutate(
    rejoin_type = case_when(
      days_to_winback == 0 ~ "immediate",
      days_to_winback <= 30 ~ "short_gap",
      days_to_winback <= 90 ~ "medium_gap",
      TRUE ~ "long_gap"
    )
  ) %>%
  count(rejoin_type)







library(dplyr)
library(lubridate)



subs <- tibble::tribble(
  ~CustomerID, ~Country, ~Direct_Indirect, ~Product_name, ~Offer_period,
  ~Subscription_ID, ~Subscription_version, ~Subscription_latest,
  ~Subscription_status, ~Subscription_type,
  ~Subscription_start_date, ~Expiry_date,
  
  101,"USA","Direct","XYZTotal","monthly",8889,1,0,"churned","churned","1/18/2025","2/18/2025",
  101,"USA","Direct","FootballSeasonal","monthly",8889,2,1,"active","new","1/18/2025","2/18/2025",
  101,"USA","Direct","XYZTotal","annual",2024,1,1,"active","winback","2/18/2025","2/18/2026",
  102,"India","Direct","FootballSeasonal","annual",6752,1,1,"active","new","2/18/2025","2/18/2026",
  102,"Qatar","Direct","4KTotal","annual",1220,1,1,"active","winback","3/18/2025","4/18/2026",
  103,"USA","Direct","4KTotal","monthly",2390,1,0,"active","new","2/28/2025","3/28/2025",
  103,"USA","Direct","4KTotal","monthly",2390,2,1,"churned","churned","2/28/2025","3/28/2025",
  103,"USA","Direct","4KTotal","monthly",2450,1,1,"active","winback","3/18/2025","4/18/2026",
  103,"USA","Direct","4KTotal","monthly",2670,1,1,"active","winback","2/28/2025","3/29/2025",
  104,"Kenya","Direct","XYZTotal","annual",3498,1,0,"active","new","2/18/2025","3/18/2025",
  104,"Kenya","Direct","XYZTotal","annual",3498,2,1,"churned","churned","2/18/2025","3/18/2025"
) %>%
  data.frame()



subs <- subs %>%
  mutate(
    Subscription_start_date = mdy(Subscription_start_date),
    Expiry_date = mdy(Expiry_date)
  )



churn_events <- subs %>%
  filter(Subscription_status == "churned") %>%
  select(CustomerID, churn_date = Expiry_date)




winback_events <- subs %>%
  filter(Subscription_type == "winback") %>%
  select(CustomerID, rejoin_date = Subscription_start_date)


winback_latency <- churn_events %>%
  inner_join(winback_events, by = "CustomerID") %>%
  filter(rejoin_date > churn_date) %>%   # 🔑 critical condition
  group_by(CustomerID, churn_date) %>%
  slice_min(rejoin_date, n = 1) %>%
  ungroup() %>%
  mutate(days_to_winback = as.numeric(rejoin_date - churn_date))



subs %>%
  filter(Subscription_type == "winback",
         Subscription_start_date < Expiry_date)


subs %>%
  arrange(CustomerID, Subscription_start_date) %>%
  group_by(CustomerID) %>%
  mutate(product_change = Product_name != lag(Product_name))%>%
  data.frame()



subs %>%
  filter(Subscription_type == "winback") %>%
  count(Product_name)



# 
# Correct Mental Model (OTT Reality)
# A customer does NOT churn
# A subscription / entitlement churns
# So winback must be measured at product (or entitlement) level, not customer level.


library(dplyr)
library(lubridate)


subs_clean <- subs %>%
  # mutate(
  #   Subscription_start_date = mdy(Subscription_start_date),
  #   Expiry_date = mdy(Expiry_date)
  # ) %>%
  arrange(CustomerID, Product_name, Subscription_start_date)



# Identify churn per product


churn_events <- subs_clean %>%
  filter(Subscription_status == "churned") %>%
  select(
    CustomerID,
    Product_name,
    churn_date = Expiry_date
  )


# Identify rejoin per product


rejoin_events <- subs_clean %>%
  filter(Subscription_type == "winback") %>%
  select(
    CustomerID,
    Product_name,
    rejoin_date = Subscription_start_date
  )


# Match churn → next rejoin (same product)

product_winback <- churn_events %>%
  inner_join(
    rejoin_events,
    by = c("CustomerID", "Product_name")
  ) %>%
  filter(rejoin_date > churn_date) %>%   # 🔑 real winback
  group_by(CustomerID, Product_name, churn_date) %>%
  slice_min(rejoin_date, n = 1) %>%
  ungroup() %>%
  mutate(days_to_winback = as.numeric(rejoin_date - churn_date))





# Early Renewal Rate (Retention Success)
subs_clean %>%
  filter(
    Subscription_type == "winback",
    Subscription_start_date <= Expiry_date
  ) %>%
  summarise(early_renewals = n())



# Product Save Rate

subs_clean %>%
  group_by(Product_name) %>%
  summarise(
    churns = sum(Subscription_status == "churned"),
    early_renewals = sum(
      Subscription_type == "winback" &
        Subscription_start_date <= Expiry_date
    ),
    save_rate = early_renewals / churns
  )





# Parallel Subscription Penetration (OTT Gold Metric)


subs_clean %>%
  group_by(CustomerID) %>%
  summarise(
    concurrent_products = n_distinct(Product_name)
  ) %>%
  count(concurrent_products)




