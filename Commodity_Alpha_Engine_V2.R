# ==============================================================================
# THE COMMODITY ALPHA ENGINE: INSTITUTIONAL EDITION (CORRECTED)
# Path: Market-Efficiency_Research/Data
# ==============================================================================

rm(list = ls())
library(readr); library(dplyr); library(lubridate); library(tidyr); library(ggplot2); library(knitr)

# ------------------------------------------------------------------------------
# 1. SETTINGS & PATHS
# ------------------------------------------------------------------------------
base_path <- "/Users/alexlaudano/Desktop/Market-Efficiency_Research/Data"
assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE","GOLD","BITCOIN","SOYOIL","OATS","OJ")

CFG <- list(
  min_train_yrs = 5,
  friction_bps   = 65,  # 0.65% for Slippage + Roll Costs
  perm_reps      = 1000 # Statistical Rigor
)

# ------------------------------------------------------------------------------
# 2. THE RECURSIVE ENGINE (NO LOOK-AHEAD BIAS)
# ------------------------------------------------------------------------------
run_backtest <- function(q_data, cfg) {
  years <- sort(unique(q_data$yr))
  if (length(years) <= cfg$min_train_yrs) return(NULL)
  
  oos_list <- list()
  
  for (i in (cfg$min_train_yrs + 1):length(years)) {
    train_yrs <- years[1:(i-1)]
    test_yr   <- years[i]
    
    # SIGNAL GENERATION (Using only PAST data)
    signal_logic <- q_data %>%
      filter(yr %in% train_yrs) %>%
      group_by(Qtr) %>%
      summarise(M = mean(Ret, na.rm = TRUE), .groups = "drop") %>%
      slice_max(M, n = 1, with_ties = FALSE)
    
    best_q <- signal_logic$Qtr
    
    # SIGNAL EXECUTION (Applying to UNSEEN FUTURE data)
    test_set <- q_data %>%
      filter(yr == test_yr) %>%
      mutate(
        Chosen_Q  = best_q,
        # We trade only if current quarter matches our best historical quarter
        Trade     = ifelse(Qtr == best_q, 1, 0),
        # Net Return = Gross Return - Friction (only when active)
        Strat_Ret = ifelse(Trade == 1, Ret - (cfg$friction_bps/100), 0)
      )
    
    oos_list[[as.character(test_yr)]] <- test_set
  }
  
  # Wealth Aggregation
  bind_rows(oos_list) %>%
    mutate(
      Wealth_Strat = cumprod(1 + Strat_Ret/100),
      Wealth_BH    = cumprod(1 + Ret/100),
      Peak_Strat   = cummax(Wealth_Strat),
      DD_Strat     = (Wealth_Strat - Peak_Strat) / Peak_Strat
    )
}

# ------------------------------------------------------------------------------
# 3. PRODUCTION LOOP
# ------------------------------------------------------------------------------
results_list <- list()

for (a in assets) {
  # FIX: Correcting the Thesis Data naming convention
  file_path <- file.path(base_path, paste0(a, "_Thesis_Data.csv"))
  if (!file.exists(file_path)) next
  
  message(">>> Processing: ", a)
  
  # Standardize column naming
  q_data <- read_csv(file_path, show_col_types = FALSE) %>%
    rename(D_COL = 1, V_COL = 2) %>%
    mutate(
      D = parse_date_time(as.character(D_COL), orders=c("mdy","ymd","dmy")),
      V = as.numeric(gsub("[$,]", "", as.character(V_COL)))
    ) %>%
    filter(!is.na(D), !is.na(V), year(D) < 2024) %>%
    arrange(D) %>%
    group_by(yr = year(D), mn = month(D)) %>%
    summarise(Price = last(V), .groups = "drop") %>%
    mutate(
      Ret = (Price / lag(Price) - 1) * 100,
      Qtr = case_when(mn %in% 1:3 ~ "Q1", mn %in% 4:6 ~ "Q2", 
                      mn %in% 7:9 ~ "Q3", mn %in% 10:12 ~ "Q4")
    ) %>% 
    filter(!is.na(Ret))
  
  # Permutation Test (Statistical Significance)
  obs_spread <- q_data %>% group_by(Qtr) %>% summarise(M = mean(Ret), .groups="drop") %>% summarise(S = max(M)-min(M)) %>% pull(S)
  set.seed(123)
  perm_dist  <- replicate(CFG$perm_reps, {
    null_stats <- q_data %>% mutate(sq = sample(Qtr)) %>% group_by(sq) %>% summarise(M = mean(Ret), .groups = "drop")
    max(null_stats$M) - min(null_stats$M)
  })
  p_val <- mean(perm_dist >= obs_spread)
  
  # Run Backtest
  wf_res <- run_backtest(q_data, CFG)
  
  if (!is.null(wf_res)) {
    n_yrs <- length(unique(wf_res$yr))
    final_strat_wealth <- tail(wf_res$Wealth_Strat, 1)
    final_bh_wealth    <- tail(wf_res$Wealth_BH, 1)
    
    results_list[[a]] <- data.frame(
      Asset      = a,
      Ann_Return = round((final_strat_wealth^(1/n_yrs) - 1) * 100, 2),
      Alpha      = round(((final_strat_wealth^(1/n_yrs)) - (final_bh_wealth^(1/n_yrs))) * 100, 2),
      Sharpe     = round((mean(wf_res$Strat_Ret)/sd(wf_res$Strat_Ret)) * sqrt(12), 2),
      Max_DD     = round(min(wf_res$DD_Strat) * 100, 2),
      P_Value    = round(p_val, 4)
    )
  }
}

# ------------------------------------------------------------------------------
# 4. FINAL OUTPUT
# ------------------------------------------------------------------------------
if (length(results_list) > 0) {
  final_results <- bind_rows(results_list) %>%
    mutate(Sig = case_when(P_Value <= 0.05 ~ "***", P_Value <= 0.10 ~ "*", TRUE ~ "")) %>%
    arrange(desc(Alpha))
  
  print(kable(final_results, caption = "TABLE 1: RECURSIVE ALPHA (OOS, 65BPS FRICTION)"))
} else {
  message("Check base_path. No files were processed.")
}

# ------------------------------------------------------------------------------
# 5. GENERATE THE "MONEY CHART" (LUMBER EQUITY CURVE)
# ------------------------------------------------------------------------------
# Extract Lumber OOS results specifically
lumber_df <- run_backtest(
  q_data = (read_csv(file.path(base_path, "LUMBER_Thesis_Data.csv"), show_col_types = FALSE) %>%
              rename(D_COL = 1, V_COL = 2) %>%
              mutate(D = parse_date_time(as.character(D_COL), orders=c("mdy","ymd","dmy")),
                     V = as.numeric(gsub("[$,]", "", as.character(V_COL)))) %>%
              filter(!is.na(D), !is.na(V), year(D) < 2024) %>%
              arrange(D) %>%
              group_by(yr = year(D), mn = month(D)) %>%
              summarise(Price = last(V), .groups = "drop") %>%
              mutate(Ret = (Price / lag(Price) - 1) * 100,
                     Qtr = case_when(mn %in% 1:3 ~ "Q1", mn %in% 4:6 ~ "Q2", 
                                     mn %in% 7:9 ~ "Q3", mn %in% 10:12 ~ "Q4")) %>% 
              filter(!is.na(Ret))),
  cfg = CFG
)

# Reshape for ggplot
plot_data <- lumber_df %>%
  select(yr, mn, Wealth_Strat, Wealth_BH) %>%
  mutate(Date = make_date(yr, mn, 1)) %>%
  pivot_longer(cols = c(Wealth_Strat, Wealth_BH), names_to = "Strategy", values_to = "Wealth") %>%
  mutate(Strategy = ifelse(Strategy == "Wealth_Strat", "Seasonal Alpha Engine (Net)", "Buy & Hold (Lumber)"))

# Plot
ggplot(plot_data, aes(x = Date, y = Wealth, color = Strategy)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Seasonal Alpha Engine (Net)" = "#218c53", "Buy & Hold (Lumber)" = "#7f8c8d")) +
  theme_minimal(base_size = 14) +
  labs(title = "Lumber Seasonal Alpha: Performance vs. Benchmark",
       subtitle = "Walk-Forward OOS Results (Net of 65bps Friction)",
       y = "Growth of $1", x = NULL) +
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# 6. THE FINISHING MOVE: MULTI-ASSET ALPHA PORTFOLIO (FIXED)
# ------------------------------------------------------------------------------
# 1. Join and drop any rows where we don't have data for all three (Perfect Alignment)
master_fund <- fund_list %>% 
  reduce(full_join, by = c("yr", "mn")) %>%
  na.omit() %>%  # This kills the NAs that caused the error
  mutate(
    Portfolio_Ret = (LUMBER + WHEAT + CORN) / 3,
    Wealth = cumprod(1 + Portfolio_Ret/100),
    Peak = cummax(Wealth),
    Drawdown = (Wealth - Peak) / Peak
  )

# 2. Calculate Stats based on the actual duration of the aligned data
n_months <- nrow(master_fund)
ann_return_final <- (tail(master_fund$Wealth, 1)^(1/(n_months/12)) - 1) * 100
max_dd_final     <- min(master_fund$Drawdown) * 100

message("--- FINAL MASTER FUND RESULTS ---")
message("Combined Annualized Return: ", round(ann_return_final, 2), "%")
message("Combined Max Drawdown: ", round(max_dd_final, 2), "%")
message("Sharpe Ratio: ", round((mean(master_fund$Portfolio_Ret)/sd(master_fund$Portfolio_Ret)) * sqrt(12), 2))