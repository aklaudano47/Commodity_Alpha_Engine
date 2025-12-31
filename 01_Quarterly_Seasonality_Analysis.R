# 1. SETUP
rm(list = ls())
library(readr); library(dplyr); library(lubridate); library(here); library(tidyr)

# Helper: Annualized Sharpe
safe_sharpe <- function(x, scale = sqrt(12)) {
  mu <- mean(x, na.rm = TRUE); sig <- sd(x, na.rm = TRUE)
  if (is.na(sig) || sig == 0) return(NA_real_)
  (mu / sig) * scale
}

# 2. EXPANDING WINDOW RECURSIVE FUNCTION
# This uses ALL data prior to the test year
walk_forward_expanding <- function(q_data, min_train_years = 5) {
  years <- sort(unique(q_data$yr))
  if (length(years) <= min_train_years) return(NULL)
  
  oos_list <- list()
  
  # The loop starts after the "burn-in" period
  for (i in (min_train_years + 1):length(years)) {
    # EXPANDING WINDOW: From the very first year to year i-1
    train_yrs <- years[1:(i - 1)] 
    test_yr   <- years[i]
    
    # Selection: Best Quarter based on ALL prior history
    best_q <- q_data %>%
      filter(yr %in% train_yrs) %>%
      group_by(Qtr) %>%
      summarise(M = mean(Ret, na.rm = TRUE), .groups = "drop") %>%
      slice_max(M, n = 1, with_ties = FALSE) %>%
      pull(Qtr)
    
    # Implementation
    test_set <- q_data %>%
      filter(yr == test_yr) %>%
      mutate(Chosen_Q = best_q, 
             Strat_Ret = ifelse(Qtr == best_q, Ret, 0))
    
    oos_list[[as.character(test_yr)]] <- test_set
  }
  
  res <- bind_rows(oos_list) %>%
    mutate(Wealth_Strat = cumprod(1 + Strat_Ret/100),
           Wealth_BH    = cumprod(1 + Ret/100),
           Peak_Strat   = cummax(Wealth_Strat),
           DD_Strat     = (Wealth_Strat - Peak_Strat) / Peak_Strat)
  return(res)
}

# 3. SETTINGS & ENGINE
base_path <- here("Data")
assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE",
            "DOWJONES","BITCOIN","SOYOIL","OATS","GOLD","SUGAR","COCOA","OJ")
seasonal_results <- list()

for (a in assets) {
  file_path <- file.path(base_path, paste0(a, "_Thesis_Data.csv"))
  if (!file.exists(file_path)) next
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Standardize
  if (a == "BITCOIN") {
    df_std <- df %>% transmute(D_COL = End, V_COL = Close)
  } else {
    df_std <- df %>% rename(D_COL = 1, V_COL = 2)
  }
  
  # Clean and Aggregate
  q_data <- df_std %>%
    mutate(D = parse_date_time(as.character(D_COL), orders=c("mdy","ymd","dmy"), quiet=TRUE),
           V = suppressWarnings(as.numeric(gsub("[$,]", "", as.character(V_COL))))) %>%
    filter(!is.na(D), !is.na(V), year(D) < 2024) %>%
    arrange(D) %>%
    group_by(yr = year(D), mn = month(D)) %>%
    summarise(Price = last(V), .groups = "drop") %>%
    mutate(Ret = (Price / lag(Price) - 1) * 100,
           Qtr = case_when(mn %in% 1:3 ~ "Q1", mn %in% 4:6 ~ "Q2", 
                           mn %in% 7:9 ~ "Q3", mn %in% 10:12 ~ "Q4")) %>% 
    filter(!is.na(Ret))
  
  # Significance (Permutation)
  q_stats <- q_data %>% group_by(Qtr) %>% summarise(M = mean(Ret), .groups = "drop")
  obs_spread <- max(q_stats$M) - min(q_stats$M)
  set.seed(123)
  perm_dist <- replicate(2000, {
    null_stats <- q_data %>% mutate(sq = sample(Qtr)) %>%
      group_by(sq) %>% summarise(M = mean(Ret), .groups = "drop")
    max(null_stats$M) - min(null_stats$M)
  })
  p_val <- mean(perm_dist >= obs_spread)
  
  # Run Expanding Recursive Walk-Forward
  wf_res <- walk_forward_expanding(q_data, min_train_years = 5)
  
  if (!is.null(wf_res)) {
    n_yrs <- nrow(wf_res) / 12
    ann_ret_strat <- (tail(wf_res$Wealth_Strat, 1)^(1/n_yrs) - 1) * 100
    ann_ret_bh    <- (tail(wf_res$Wealth_BH, 1)^(1/n_yrs) - 1) * 100
    
    seasonal_results[[a]] <- data.frame(
      Asset = a,
      Ann_Ret_Strat = round(ann_ret_strat, 2),
      Ann_Ret_BH    = round(ann_ret_bh, 2),
      Alpha         = round(ann_ret_strat - ann_ret_bh, 2),
      Sharpe_WF     = round(safe_sharpe(wf_res$Strat_Ret), 2),
      Sharpe_BH     = round(safe_sharpe(wf_res$Ret), 2),
      Max_DD_WF     = round(min(wf_res$DD_Strat, na.rm=TRUE) * 100, 2),
      P_Value       = round(p_val, 4)
    )
  }
}

# 4. FINAL OUTPUT
final_table <- bind_rows(seasonal_results) %>%
  mutate(P_BH = p.adjust(P_Value, method = "BH")) %>%
  arrange(desc(Alpha))

print(final_table)



library(ggplot2)

# Refined Plotting Code
p_final <- ggplot(final_filtered, aes(x = reorder(Asset, Alpha), y = Alpha, fill = Type)) +
  # Use a slimmer bar for a more modern look
  geom_bar(stat = "identity", width = 0.7, color = NA) + 
  coord_flip() +
  # Professional Financial Palette
  scale_fill_manual(values = c("Control (Equities)" = "#7f8c8d", 
                               "Significant (Commodities)" = "#218c53")) +
  theme_minimal(base_family = "sans") + 
  theme(
    panel.grid.major.y = element_blank(), # Remove horizontal lines for cleaner look
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 18, margin = margin(b=10)),
    plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b=20)),
    axis.text = element_text(size = 11, face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Analysis of Seasonal Alpha",
    subtitle = "Annualized Excess Returns: Commodities vs. Equity Control",
    x = NULL,
    y = "Annualized Alpha (%)",
    caption = "Source: Recursive Expanding-Window Model Analysis"
  ) +
  # Better text placement
  geom_text(aes(label = paste0(sprintf("%.2f", Alpha), "%")), 
            hjust = ifelse(final_filtered$Alpha > 0, -0.2, 1.2), 
            size = 4, fontface = "bold", color = "black") +
  # Expand limits so the text doesn't get cut off
  expand_limits(y = c(min(final_filtered$Alpha) - 2, max(final_filtered$Alpha) + 2))

# Save with specific dimensions for better resolution
ggsave("Thesis_Final_Plot.png", p_final, width = 9, height = 6, dpi = 300)

