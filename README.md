# Systematic Seasonal Arbitrage: Recursive Alpha in Commodity Futures

### 🚀 Overview
This repository contains a production-grade quantitative research framework designed to identify and operationalize structural seasonal anomalies in the commodity complex. 

Moving beyond standard academic backtesting, the engine utilizes **Recursive Walk-Forward Optimization** and **Non-Parametric Permutation Testing** to isolate alpha that survives institutional-grade execution friction.

### 📊 Performance Summary (Net of 65bps Friction)
Unlike traditional models that ignore implementation costs, this engine applies a mandatory **65bps (0.65%) penalty** per trade to account for bid-ask spreads, slippage, and contract roll costs.

| Strategy / Asset | Net Alpha (65bps) | Sharpe Ratio | P-Value (1,000 Reps) |
|:--- |:---:|:---:|:---:|
| **Master Fund (Aggregated)** | **+5.82%** | **0.61** | **0.0000***** |
| **LUMBER** | **+6.73%** | **0.60** | **0.0000***** |
| **WHEAT** | **+2.41%** | **0.35** | **0.0000***** |
| **CORN** | **+0.75%** | **0.32** | **0.0004***** |
| **DOW JONES (Control)** | -6.42% | 0.0000 | 0.722 |

---

### 🛠 Technical Architecture

#### 1. Recursive Walk-Forward Engine
To eliminate **Look-Ahead Bias**, the model simulates real-time decision-making. The strategy re-optimizes its parameters annually using an expanding historical window. This ensures the model "earns" its returns by learning from the past to trade the unseen future.



#### 2. Non-Parametric Significance Testing
Commodity returns are non-normal and fat-tailed. Standard t-tests often yield false positives in these environments. I implemented a **1,000-rep Permutation Test (Monte Carlo)** to generate a synthetic null distribution. The results confirm that the Alpha in Lumber, Wheat, and Corn is a result of structural edge rather than stochastic noise.



#### 3. Portfolio Risk Engineering (The Master Fund)
To combat the high idiosyncratic volatility of single-commodity trading, I engineered a **Master Fund** aggregator. By blending non-correlated signals across industrial and agricultural sectors, the framework achieved:
* **Drawdown Compression:** Reduced Max Drawdown from **-23.6%** (Lumber) to **-16.3%** (Master Fund).
* **Return Smoothing:** Stabilized the equity curve for institutional viability while maintaining a 0.61 Sharpe Ratio.



---

### 💡 Strategic Thesis: Physical Rigidity
The research confirms that Alpha is a function of **Physical Constraints**. Seasonality fails in highly liquid, financialized assets (Bitcoin, Dow Jones) but persists in markets with biological supply constraints (harvest cycles) and high storage costs. This project serves as an empirical rejection of the Weak-Form Efficient Market Hypothesis (EMH) within the physical commodity complex.

---

### 💻 Technical Stack
* **Language:** R (Tidyquant, PerformanceAnalytics, PortfolioAnalytics, Purrr)
* **Optimization:** Recursive Walk-Forward (OOS)
* **Validation:** Non-Parametric Permutation (1,000-Rep Monte Carlo)
* **Architecture:** Multi-Asset Signal Aggregation

---

**Contact:** **Alexander Laudano** – [alexlaudano22@gmail.com](mailto:alexlaudano22@gmail.com)  
**LinkedIn:** [linkedin.com/in/alexander-laudano](https://www.linkedin.com/in/alexander-laudano-874073259/)
