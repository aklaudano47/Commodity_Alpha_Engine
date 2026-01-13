# ==============================================================================
# PROJECT: Systematic Seasonal Alpha in Physical Commodities
# AUTHOR:  Alexander K. Laudano
# DATE:    January 13, 2026
# ENGINE:  Recursive Walk-Forward OOS (Out-of-Sample)
# ==============================================================================

# 1. LOAD LIBRARIES
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(knitr)
library(purrr)  # Required for the 'reduce' function in the Master Fund

# 2. SETTINGS & CONFIGURATION
base_path <- "/Users/alexlaudano/Desktop/Market-Efficiency_Research/Data"
assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE","GOLD","BITCOIN","SOYOIL","OATS","OJ")

CFG <- list(
  min_train_yrs = 5,
  friction_bps   = 65,   # Institutional Stress Test: 0.65% per trade
  perm_reps      = 1000  # Statistical Rigor for P-Value generation
)

# 3. THE RECURSIVE ENGINE (Eliminates Look-Ahead Bias)
run_backtest <- function(q_data, cfg) {
  years <- sort(unique(q_data$yr))
  if (length(years) <= cfg$min_train_yrs) return(NULL)
  
  oos_list <- list()
  
  for (i in (cfg$min_train_yrs + 1):length(years)) {
    train_yrs <- years[1:(i-1)]
    test_yr   <- years[i]
    
    # SIGNAL GENERATION: Only using PAST data to pick the best quarter
    signal_logic <- q_data %>%
      filter(yr %in% train_yrs) %>%
      group_by(Qtr) %>%
      summarise(M = mean(Ret, na.rm = TRUE), .groups = "drop") %>%
      slice_max(M, n = 1, with_ties = FALSE)
    
    best_q <- signal_logic$Qtr
    
    # SIGNAL EXECUTION: Applying the signal to UNSEEN future data
    test_set <- q_data %>%
      filter(yr == test_yr) %>%
      mutate(
        Chosen_Q  = best_q,
        Trade     = ifelse(Qtr == best_q, 1, 0),
        # Net Return = Gross - 65bps friction (only when active)
        Strat_Ret = ifelse(Trade == 1, Ret - (cfg$friction_bps/100), 0)
      )
    
    oos_list[[as.character(test_yr)]] <- test_set
  }
  
  # Equity Curve Aggregation
  bind_rows(oos_list) %>%
    mutate(
      Wealth_Strat = cumprod(1 + Strat_Ret/100),
      Wealth_BH    = cumprod(1 + Ret/100),
      Peak_Strat   = cummax(Wealth_Strat),
      DD_Strat     = (Wealth_Strat - Peak_Strat) / Peak_Strat
    )
}

# 4. PRODUCTION LOOP (Data Cleaning & Statistical Validation)
results_list <- list()
raw_returns_list <- list() # For the Master Fund assembly

for (a in assets) {
  file_path <- file.path(base_path, paste0(a, "_Thesis_Data.csv"))
  if (!file.exists(file_path)) next
  
  message(">>> Processing: ", a)
  
  # Standardize and Clean Data
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
  
  # Non-Parametric Permutation Test (Monte Carlo Simulation)
  obs_spread <- q_data %>% group_by(Qtr) %>% summarise(M = mean(Ret), .groups="drop") %>% summarise(S = max(M)-min(M)) %>% pull(S)
  set.seed(123)
  perm_dist  <- replicate(CFG$perm_reps, {
    null_stats <- q_data %>% mutate(sq = sample(Qtr)) %>% group_by(sq) %>% summarise(M = mean(Ret), .groups = "drop")
    max(null_stats$M) - min(null_stats$M)
  })
  p_val <- mean(perm_dist >= obs_spread)
  
  # Run the Engine
  wf_res <- run_backtest(q_data, CFG)
  
  if (!is.null(wf_res)) {
    # Store Summary Stats
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
    
    # Store raw OOS returns for "The Big Three" Portfolio
    if (a %in% c("LUMBER", "WHEAT", "CORN")) {
      raw_returns_list[[a]] <- wf_res %>% select(yr, mn, !!a := Strat_Ret)
    }
  }
}

# 5. TABLE 1: RECURSIVE ALPHA RESULTS
if (length(results_list) > 0) {
  final_results <- bind_rows(results_list) %>%
    mutate(Sig = case_when(P_Value <= 0.05 ~ "***", P_Value <= 0.10 ~ "*", TRUE ~ "")) %>%
    arrange(desc(Alpha))
  print(kable(final_results, caption = "TABLE 1: FRICTION-ADJUSTED RECURSIVE ALPHA"))
}

# 6. THE FINISHING MOVE: MULTI-ASSET MASTER FUND PORTFOLIO
if (length(raw_returns_list) >= 3) {
  master_fund <- raw_returns_list %>% 
    reduce(inner_join, by = c("yr", "mn")) %>%
    mutate(
      Portfolio_Ret = (LUMBER + WHEAT + CORN) / 3,
      Wealth = cumprod(1 + Portfolio_Ret/100),
      Peak = cummax(Wealth),
      Drawdown = (Wealth - Peak) / Peak
    )
  
  portfolio_summary <- data.frame(
    Strategy = "Master Fund (Lumber+Wheat+Corn Aggregator)",
    Ann_Return = round(((tail(master_fund$Wealth, 1)^(1/(nrow(master_fund)/12))) - 1) * 100, 2),
    Max_DD = round(min(master_fund$Drawdown) * 100, 2),
    Sharpe = round((mean(master_fund$Portfolio_Ret)/sd(master_fund$Portfolio_Ret)) * sqrt(12), 2)
  )
  print(kable(portfolio_summary, caption = "TABLE 2: PORTFOLIO RISK ENGINEERING"))
}