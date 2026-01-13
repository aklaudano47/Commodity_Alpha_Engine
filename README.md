# Systematic Seasonal Arbitrage: Recursive Alpha in Commodity Futures

### Overview
This repository contains a production-grade quantitative research framework designed to identify and operationalize structural seasonal anomalies. Moving beyond standard academic backtesting, the framework utilizes **Recursive Walk-Forward Optimization** and **Non-Parametric Permutation Testing** to isolate alpha that survives institutional-grade execution friction.

### Performance Summary (Net of 65bps Friction)
Unlike traditional models that report "gross" returns, this engine applies a mandatory **65bps (0.65%) penalty** per trade to account for the bid-ask spread, slippage, and contract roll costs.

| Asset | Net Alpha (65bps) | Sharpe Ratio | P-Value (1,000 Reps) |
|:--- |:---:|:---:|:---:|
| **LUMBER** | **6.73%** | **0.61** | **0.000***** |
| **WHEAT** | **2.52%** | **0.36** | **0.000***** |
| **CORN** | **1.10%** | **0.33** | **0.004***** |
| **DOW JONES** | -6.42% | 0.00 | 0.722 |
| **BITCOIN** | -61.2% | 0.46 | 0.130 |

---

### Technical Architecture

#### 1. Recursive Walk-Forward Engine
To eliminate **Look-Ahead Bias**, the model simulates real-time decision-making. The strategy re-optimizes its parameters annually using an expanding historical window, ensuring the model "earns" its returns without peering into the future.

#### 2. Non-Parametric Significance Testing
Commodity returns are non-normal and fat-tailed. Standard t-tests often yield false positives. I implemented a **1,000-rep Permutation Test** to generate a synthetic null distribution. The results for Lumber and Wheat show statistical significance at the **99.9% confidence level**.

#### 3. Risk Mitigation (The Master Fund)
To combat the high idiosyncratic volatility of single-commodity trading, I engineered a **Master Fund** aggregator. By blending non-correlated signals across industrial and agricultural sectors, the portfolio achieved:
* **Drawdown Compression:** Reduced Max Drawdown from **-23.6%** (Lumber) to **-16.3%** (Master Fund).
* **Alpha Preservation:** Maintained a positive net expectancy while smoothing the equity curve.

---

### Strategic Thesis
The research confirms that Alpha is a function of **Physical Rigidity**. Seasonality fails in highly liquid, financialized assets (Bitcoin, Dow Jones) but persists in markets with biological supply constraints and high storage costs. This project serves as an empirical rejection of the Weak-Form Efficient Market Hypothesis (EMH) within the commodity complex.

---

### Technical Stack
* **Language:** R (Tidyquant, PerformanceAnalytics, PortfolioAnalytics)
* **Optimization:** Recursive Walk-Forward (OOS)
* **Validation:** Non-Parametric Permutation (Monte Carlo)
* **Architecture:** AI-Augmented Systematic Design

**Contact:** [Alexander Laudano]  
**LinkedIn:** [Your LinkedIn Link]
