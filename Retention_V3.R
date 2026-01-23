# Load & Prepare the Data
library(dplyr)
library(lubridate)



# ========= Sample Data ==========


subs <- tibble::tribble(
  ~CustomerID, ~CusExtID, ~Country, ~Direct_Indirect, ~Product_name, ~Offer_period,
  ~Subscription_ID, ~Subscription_version, ~Subscription_latest,
  ~Subscription_status, ~Subscription_type,
  ~Subscription_start_date, ~Expiry_date, ~Subsc_Cal_date,
  
  1,101,"USA","Direct","XYZTotal","monthly",8889,1,0,"churned","churned","1/18/2025","2/18/2025","2/18/2025",
  1,101,"USA","Direct","FootballSeasonal","monthly",8889,2,1,"active","new","1/18/2025","2/18/2025","1/18/2025",
  1,101,"USA","Direct","XYZTotal","annual",2024,1,1,"active","winback","1/25/2025","2/25/2026","1/25/2025",
  
  2,102,"India","Direct","FootballSeasonal","annual",6752,1,1,"active","new","2/18/2025","2/18/2026","2/18/2025",
  2,102,"Qatar","Direct","4KTotal","annual",1220,1,1,"active","winback","3/18/2025","4/18/2026","3/18/2025",
  
  3,103,"USA","Direct","4KTotal","monthly",2390,1,0,"active","new","2/28/2025","3/28/2025","2/28/2025",
  3,103,"USA","Direct","4KTotal","monthly",2390,2,1,"churned","churned","2/28/2025","3/28/2025","3/28/2025",
  3,103,"USA","Direct","4KTotal","monthly",2450,1,1,"active","winback","3/18/2025","4/18/2026","3/18/2025",
  3,103,"USA","Direct","4KTotal","monthly",2670,1,1,"active","winback","2/28/2025","3/29/2025","2/28/2025",
  
  4,104,"Kenya","Direct","XYZTotal","annual",3498,1,0,"active","new","2/18/2025","3/18/2025","2/18/2025",
  4,104,"Kenya","Direct","XYZTotal","annual",3498,2,1,"churned","churned","2/18/2025","3/18/2025","3/18/2025",
  
  5,105,"USA","Direct","XYZTotal","monthly",8889,2,1,"churned","churned","11/13/2023","12/21/2023","12/21/2023",
  5,105,"USA","Direct","XYZTotal","monthly",8889,1,0,"active","new","11/13/2023","12/21/2023","11/13/2023",
  5,105,"USA","Direct","XYZTotal","monthly",2024,1,1,"active","winback","1/2/2024","1/2/2027","1/2/2024",
  5,105,"USA","Direct","FootballSeasonal","cust",6752,1,0,"active","winback","1/14/2024","2/14/2024","1/14/2024",
  5,105,"USA","Direct","FootballSeasonal","cust",6752,2,1,"churned","churned","1/14/2024","2/14/2024","2/14/2024",
  5,105,"USA","Direct","4KTotal","monthly",2390,1,0,"active","winback","6/14/2024","9/14/2024","6/14/2024",
  5,105,"USA","Direct","4KTotal","monthly",2390,2,1,"churned","churned","6/14/2024","9/14/2024","9/14/2024",
  5,105,"USA","Direct","FootballSeasonal","cust",2450,2,1,"churned","churned","12/26/2025","1/21/2026","1/21/2026",
  5,105,"USA","Direct","FootballSeasonal","cust",2450,1,1,"active","winback","12/26/2025","1/21/2026","12/26/2025"
) %>%
  mutate(
    Subscription_start_date = mdy(Subscription_start_date),
    Expiry_date = mdy(Expiry_date),
    Subsc_Cal_date = mdy(Subsc_Cal_date)
  )%>%
  data.frame()

library(data.table)
fwrite(subs,'retention.csv')


# ============ Perfect Winback Number of Days =========


subs_flagged <- subs %>%
  arrange(CustomerID, Subscription_start_date, Subscription_version) %>%
  group_by(CustomerID) %>%
  mutate(
    prev_sub_id = lag(Subscription_ID),
    prev_status = lag(Subscription_status),
    prev_expiry = lag(Expiry_date),
    
    winback_flag =
      Subscription_ID != prev_sub_id &
      prev_status == "churned" &
      Subscription_start_date > prev_expiry,
    
    days_to_winback = ifelse(
      winback_flag,
      as.numeric(Subscription_start_date - prev_expiry),
      NA
    )
  ) %>%
  ungroup()%>%
  data.frame()



# Actual Winback Time per Customer (RESULT)

winback_results <- subs_flagged %>%
  filter(winback_flag) %>%
  select(
    CustomerID,
    CusExtID,
    prev_expiry,
    Subscription_start_date,
    days_to_winback,
    Product_name
  )

winback_results



# 
# Time Spent in System (Including Overlaps)
# This counts unique active days, not inflated sums.
# library(IRanges)
# 
# lifetime_days <- subs %>%
#   group_by(CustomerID) %>%
#   summarise(
#     total_days_in_system = {
#       ranges <- IRanges(
#         start = as.numeric(Subscription_start_date),
#         end   = as.numeric(Expiry_date)
#       )
#       sum(width(reduce(ranges)))
#     }
#   )
# 
# lifetime_days






# ============= Below is a drop-in replacement that:
# Handles overlapping subscriptions
# Calculates true time spent in system

# Instead of merging ranges with IRanges, we:
#   Sort intervals
# Collapse overlaps manually
# Sum the merged durations


merge_intervals_days <- function(start_dates, end_dates) {
  
  start_dates='2025-01-18'
  end_dates='2025-02-18'
  
  df <- data.frame(
    start = as.Date(start_dates),
    end   = as.Date(end_dates)
  )
  
  df <- df[order(df$start), ]
  
  total_days <- 0
  current_start <- df$start[1]
  current_end   <- df$end[1]
  
  for (i in 2:nrow(df)) {
    
    if (df$start[i] <= current_end) {
      # Overlap → extend end date if needed
      current_end <- max(current_end, df$end[i])
    } else {
      # Gap → close previous interval
      total_days <- total_days + as.numeric(current_end - current_start)
      current_start <- df$start[i]
      current_end   <- df$end[i]
    }
  }
  
  # Add last interval
  total_days <- total_days + as.numeric(current_end - current_start)
  
  return(total_days)
}



lifetime_days <- subs %>%
  group_by(CustomerID) %>%
  summarise(
    total_days_in_system = merge_intervals_days(
      Subscription_start_date,
      Expiry_date
    )
  )



# Final Exec-Ready Metric



final_customer_view <- lifetime_days %>%
  left_join(
    winback_results %>%
      group_by(CustomerID) %>%
      summarise(
        winback_count = n(),
        avg_days_to_winback = mean(days_to_winback)
      ),
    by = "CustomerID"
  )






# What is a Winback Cohort (in OTT terms)
# A winback cohort groups customers based on how long they stayed inactive (gap) before rejoining.




library(dplyr)
library(lubridate)

subs_v1 <- subs %>%
  mutate(
    Subscription_start_date = as.Date(Subscription_start_date),
    Expiry_date = as.Date(Expiry_date)
  ) %>%
  arrange(CustomerID, Subscription_start_date)



# Calculate Churn Gap days 

df_gap <- subs_v1 %>%
  group_by(CustomerID) %>%
  mutate(
    prev_expiry = lag(Expiry_date),
    gap_days = as.numeric(Subscription_start_date - prev_expiry)
  ) %>%
  ungroup()%>%
  data.frame()




# ==== Keep only winback =====

winbacks <- df_gap %>%
  filter(
    Subscription_type == "winback",
    !is.na(gap_days),
    gap_days >= 0
  )


# ====== Assigning Cohort ====


winbacks <- winbacks %>%
  mutate(
    winback_cohort = case_when(
      gap_days == 0                    ~ "Same-day",
      gap_days >= 1  & gap_days <= 7   ~ "1–7 days",
      gap_days >= 8  & gap_days <= 30  ~ "8–30 days",
      gap_days >= 31 & gap_days <= 60  ~ "31–60 days",
      gap_days >= 61 & gap_days <= 90  ~ "61–90 days",
      gap_days >= 91 & gap_days <= 180 ~ "91–180 days",
      gap_days > 180                   ~ "180+ days"
    )
  )


# Winback Cohort Table 



cohort_table <- winbacks %>%
  group_by(winback_cohort) %>%
  summarise(
    winback_customers = n_distinct(CustomerID),
    avg_gap_days = round(mean(gap_days), 1),
    median_gap_days = median(gap_days)
  ) %>%
  arrange(avg_gap_days)




# ======= Usage based Winback =========


weighted_cohort <- winbacks %>%
  group_by(winback_cohort) %>%
  summarise(
    customers = n_distinct(CustomerID),
    total_usage = sum(Usage_minutes, na.rm = TRUE),
    avg_usage_per_user = round(total_usage / customers, 1)
  )



# 
# A winback cohort heatmap shows gap before rejoin (x-axis) vs customer count or engagement (y-axis) — instantly 
# tells management who is coming back, how fast, and how valuable.


# Assuming you already have winbacks from previous step:


library(dplyr)
library(lubridate)
library(ggplot2)

# Recalculate usage_minutes if not present
set.seed(123)
winbacks <- winbacks %>%
  mutate(
    usage_minutes = sample(500:1500, n(), replace = TRUE)
  )





# Summarize for Heatmap
# Two options:
#   A. Count-based



heatmap_data_count <- winbacks %>%
  group_by(winback_cohort) %>%
  summarise(
    customers = n_distinct(CustomerID)
  )

# B. Usage-based (Weighted)


heatmap_data_usage <- winbacks %>%
  group_by(winback_cohort) %>%
  summarise(
    total_usage = sum(usage_minutes),
    avg_usage = mean(usage_minutes)
  )


# 
# Plot Heatmap (ggplot2)
# A. Customer count heatmap


ggplot(heatmap_data_count, aes(x = winback_cohort, y = 1, fill = customers)) +
  geom_tile(color = "white") +
  geom_text(aes(label = customers), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Winback Cohort Heatmap (Customer Count)",
       x = "Winback Cohort (Gap Days)",
       y = "",
       fill = "Customers") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Usage-weighted heatmap


ggplot(heatmap_data_usage, aes(x = winback_cohort, y = 1, fill = total_usage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = total_usage), color = "black", size = 4) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Winback Cohort Heatmap (Total Usage Minutes)",
       x = "Winback Cohort (Gap Days)",
       y = "",
       fill = "Usage Minutes") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())






# Optional: Engagement per Cohort


heatmap_data_usage <- winbacks %>%
  group_by(winback_cohort) %>%
  summarise(
    avg_usage = mean(usage_minutes),
    median_usage = median(usage_minutes)
  )





