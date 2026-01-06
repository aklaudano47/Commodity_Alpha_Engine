# ==============================================================================
# THE COMMODITY ALPHA ENGINE: FINAL PRODUCTION VERSION
# Path: Market-Efficiency_Research/Data
# ==============================================================================

# 1. SETUP
rm(list = ls())
library(readr); library(dplyr); library(lubridate); library(tidyr); library(ggplot2)

# Helper: Annualized Sharpe
safe_sharpe <- function(x, scale = sqrt(12)) {
  mu <- mean(x, na.rm = TRUE); sig <- sd(x, na.rm = TRUE)
  if (is.na(sig) || sig == 0 || is.na(mu)) return(NA_real_)
  (mu / sig) * scale
}

# 2. THE RECURSIVE ENGINE (With 0.50% Friction)
walk_forward_expanding <- function(q_data, min_train_years = 5, friction = 0.50) {
  years <- sort(unique(q_data$yr))
  if (length(years) <= min_train_years) return(NULL)
  
  oos_list <- list()
  
  for (i in (min_train_years + 1):length(years)) {
    train_yrs <- years[1:(i - 1)] 
    test_yr   <- years[i]
    
    # Selection: Best Quarter based on Gross history
    best_q <- q_data %>%
      filter(yr %in% train_yrs) %>%
      group_by(Qtr) %>%
      summarise(M = mean(Ret, na.rm = TRUE), .groups = "drop") %>%
      slice_max(M, n = 1, with_ties = FALSE) %>%
      pull(Qtr)
    
    # Implementation: Apply friction penalty when active
    test_set <- q_data %>%
      filter(yr == test_yr) %>%
      mutate(Chosen_Q = best_q, 
             Strat_Ret = ifelse(Qtr == best_q, Ret - friction, 0))
    
    oos_list[[as.character(test_yr)]] <- test_set
  }
  
  res <- bind_rows(oos_list) %>%
    mutate(Wealth_Strat = cumprod(1 + Strat_Ret/100),
           Wealth_BH    = cumprod(1 + Ret/100),
           Peak_Strat   = cummax(Wealth_Strat),
           DD_Strat     = (Wealth_Strat - Peak_Strat) / Peak_Strat)
  return(res)
}

# 3. SETTINGS & PATHS
base_path <- "/Users/alexlaudano/Desktop/Market-Efficiency_Research/Data"
assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE",
            "DOWJONES","BITCOIN","SOYOIL","OATS","GOLD","SUGAR","COCOA","OJ")
seasonal_results <- list()

# Diagnostic: Check folder
if(!dir.exists(base_path)) stop("Folder not found. Check path: ", base_path)
message("Found Data Folder. Starting Engine...")

# 4. THE ENGINE LOOP
for (a in assets) {
  # Auto-detect naming convention
  p1 <- file.path(base_path, paste0(a, "_Thesis_Data.csv"))
  p2 <- file.path(base_path, paste0(a, ".csv"))
  file_path <- if(file.exists(p1)) p1 else if(file.exists(p2)) p2 else NULL
  
  if (is.null(file_path)) next
  
  message("Ingesting: ", a)
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
  
  # Permutation Test (Reduced to 500 for speed)
  obs_spread <- q_data %>% group_by(Qtr) %>% summarise(M = mean(Ret)) %>% summarise(S = max(M)-min(M)) %>% pull(S)
  set.seed(123)
  perm_dist <- replicate(500, {
    null_stats <- q_data %>% mutate(sq = sample(Qtr)) %>% group_by(sq) %>% summarise(M = mean(Ret), .groups = "drop")
    max(null_stats$M) - min(null_stats$M)
  })
  p_val <- mean(perm_dist >= obs_spread)
  
  # Run Walk-Forward
  wf_res <- walk_forward_expanding(q_data, min_train_years = 5, friction = 0.50)
  
  if (!is.null(wf_res)) {
    n_yrs <- length(unique(wf_res$yr))
    ann_ret_strat <- (tail(wf_res$Wealth_Strat, 1)^(1/n_yrs) - 1) * 100
    ann_ret_bh    <- (tail(wf_res$Wealth_BH, 1)^(1/n_yrs) - 1) * 100
    
    seasonal_results[[a]] <- data.frame(
      Asset = a,
      Ann_Ret_Strat = round(ann_ret_strat, 2),
      Ann_Ret_BH    = round(ann_ret_bh, 2),
      Alpha         = round(ann_ret_strat - ann_ret_bh, 2),
      Sharpe_WF     = round(safe_sharpe(wf_res$Strat_Ret), 2),
      Max_DD_WF     = round(min(wf_res$DD_Strat, na.rm=TRUE) * 100, 2),
      P_Value       = round(p_val, 4)
    )
  }
}

# 5. OUTPUT TABLE & PLOT
if (length(seasonal_results) > 0) {
  final_table <- bind_rows(seasonal_results) %>% arrange(desc(Alpha))
  print(final_table)
  
  # Filtering for Plot
  final_filtered <- final_table %>%
    mutate(Type = ifelse(P_Value < 0.10 & !Asset %in% c("DOWJONES", "BITCOIN"), 
                         "Significant (Commodities)", "Control (Equities)"))
  
  p_final <- ggplot(final_filtered, aes(x = reorder(Asset, Alpha), y = Alpha, fill = Type)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Control (Equities)" = "#7f8c8d", "Significant (Commodities)" = "#218c53")) +
    theme_minimal() +
    labs(title = "Net Seasonal Alpha (After 0.50% Friction)",
         subtitle = "Recursive Walk-Forward Model Results",
         x = NULL, y = "Annualized Net Alpha (%)") +
    geom_text(aes(label = paste0(Alpha, "%")), hjust = -0.2, fontface = "bold")
  
  print(p_final)
  ggsave("Thesis_StressTest_Plot.png", p_final, width = 10, height = 7)
} else {
  message("No data processed. Ensure CSVs are in the folder and follow naming conventions.")
}

# ==============================================================================
# FINAL THESIS FORMATTING
# ==============================================================================

# Create the formal table with Significance Stars
thesis_table <- final_table %>%
  mutate(
    # Assign stars based on P-Value
    Significance = case_when(
      P_Value <= 0.01 ~ "***",
      P_Value <= 0.05 ~ "**",
      P_Value <= 0.10 ~ "*",
      TRUE ~ ""
    ),
    # Rename columns for the reader
    Asset = Asset,
    `Strategy Return (%)` = Ann_Ret_Strat,
    `Buy & Hold (%)` = Ann_Ret_BH,
    `Net Alpha (%)` = paste0(Alpha, Significance),
    `Sharpe (WF)` = Sharpe_WF,
    `Max Drawdown (%)` = Max_DD_WF,
    `P-Value` = P_Value
  ) %>%
  select(Asset, `Strategy Return (%)`, `Buy & Hold (%)`, `Net Alpha (%)`, `Sharpe (WF)`, `Max Drawdown (%)`, `P-Value`)

# ==============================================================================
# FINAL ACADEMIC TABLE GENERATOR
# ==============================================================================

# 1. Create the formatted table object
thesis_table <- final_table %>%
  mutate(
    # Assign stars based on P-Value (The "Academic Stamp of Approval")
    Significance = case_when(
      P_Value <= 0.01 ~ "***",
      P_Value <= 0.05 ~ "**",
      P_Value <= 0.10 ~ "*",
      TRUE ~ ""
    ),
    # Combine Alpha and Significance into one string for the paper
    `Net Alpha (%)` = paste0(sprintf("%.2f", Alpha), Significance)
  ) %>%
  select(
    Asset, 
    `Strategy Ret (%)` = Ann_Ret_Strat, 
    `B&H Ret (%)` = Ann_Ret_BH, 
    `Net Alpha (%)`, 
    `Sharpe (WF)` = Sharpe_WF, 
    `Max DD (%)` = Max_DD_WF, 
    `P-Value` = P_Value
  )

# 2. Print it using kable (if you have the knitr library)
if (!require("knitr")) install.packages("knitr")
library(knitr)

print("--- TABLE 1: RECURSIVE WALK-FORWARD PERFORMANCE (NET OF 50BPS FRICTION) ---")
kable(thesis_table, align = "lcccccc")

# 3. Save it so you don't lose it
write.csv(thesis_table, "Final_Thesis_Results_Table.csv", row.names = FALSE)
