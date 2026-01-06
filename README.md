# Commodity Alpha Engine: Stress-Tested Seasonal Backtesting

This repository contains a quantitative framework designed to identify and validate structural seasonal alpha across 14 asset classes, specifically adjusted for real-world transaction costs and execution friction.

### Performance Summary (Net of 50bps Friction)

The model reveals that seasonal alpha is not just a statistical anomaly; it is a **risk premium** that persists even after accounting for transaction costs in markets with rigid physical supply constraints.

| Asset | Strategy Ret (%) | B&H Ret (%) | Net Alpha (%) | Sharpe (WF) | P-Value |
|:---|:---:|:---:|:---:|:---:|:---:|
| **LUMBER** | 9.44 | 2.23 | **7.21*** | 0.62 | 0.000 |
| **WHEAT** | 4.76 | 1.88 | **2.88*** | 0.38 | 0.000 |
| **CORN** | 3.51 | 2.30 | **1.21*** | 0.35 | 0.000 |
| **OJ** | -0.58 | 2.11 | -2.69 | -0.09 | 0.146 |
| **DOW JONES** | -0.41 | 5.80 | -6.21 | 0.00 | 0.722 |
| **BITCOIN** | 11.72 | 72.59 | -60.87 | 0.46 | 0.130 |

---

### Methodology: The "Stress Test"

To go beyond standard backtesting, this engine implements:

* **0.50% Execution Friction:** Every active trade is penalized by 50 basis points to simulate the "Cost of Carry," slippage, and contract roll costs.
* **Recursive Walk-Forward Optimization:** Simulates out-of-sample decision-making by re-optimizing the strategy annually using an expanding historical window.
* **Permutation Significance Testing:** 1,000 shuffles per asset to validate that the observed spread is not the result of stochastic noise.

---

### Strategic Thesis

The "Extraction" phase of this research confirms that while seasonal trends exist broadly, only a subset of commodities—specifically those with **high physical storage costs and rigid biological cycles**—offer alpha that survives implementation costs. 

The failure of the strategy in Bitcoin and the Dow Jones reinforces the hypothesis that seasonality is a function of physical supply chains, not general market sentiment.

**Contact:** [Alexander Laudano] | [LinkedIn](https://www.linkedin.com/in/alexander-laudano-874073259/)
