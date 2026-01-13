Systematic Seasonal Arbitrage: Recursive Alpha in Commodity Futures
Overview
This repository houses a production-grade quantitative research suite that identifies and operationalizes structural seasonal anomalies. Moving beyond simple backtesting, the framework utilizes Recursive Walk-Forward Optimization and Non-Parametric Permutation Testing to isolate alpha that survives institutional-grade execution friction.

The "Friction-Adjusted" Edge
Most academic models fail in production because they ignore the "bid-ask spread" and "roll costs." This engine applies a mandatory 65bps (0.65%) penalty per trade.

Asset,Net Alpha (65bps),Sharpe Ratio,P-Value (Permutation)
LUMBER,6.73%,0.61,0.000*
WHEAT,2.52%,0.36,0.000*
CORN,1.10%,0.33,0.004*

Key Technical Breakthroughs
1. Recursive Walk-Forward Engine
To eliminate Look-Ahead Bias, the model re-optimizes every year using only historical data. It simulates a "Real-Time" decision-making process where the strategy must "learn" the seasonal ranks dynamically.

2. Non-Parametric Validation
Given that commodity returns are fat-tailed and non-normal, standard t-tests are unreliable. I implemented a 1,000-rep Permutation Test to generate a synthetic null distribution, proving that the Lumber and Wheat signals are statistically significant at the 99.9% confidence level.

3. Portfolio Engineering (The Master Fund)
Recognizing the high idiosyncratic volatility of single-asset commodities, I engineered a Master Fund aggregator. By blending non-correlated seasonal signals (Industrial vs. Agricultural), the framework achieved:

Drawdown Mitigation: Reduced Max Drawdown from -23.6% (Lumber) to -16.3% (Portfolio).

Risk-Adjusted Stability: Stabilized the equity curve for institutional viability.

Strategic Thesis
The research confirms that Alpha is a function of Physical Rigidity. Seasonality fails in highly liquid/financialized assets (Bitcoin, Dow Jones) but persists in markets with biological supply constraints and high storage costs. This project serves as an empirical rejection of the Weak-Form Efficient Market Hypothesis in the commodity complex.

Author: Alexander Laudano Technical Specialty: R-Programming (Tidyquant, PerformanceAnalytics), AI-Augmented Systems Architecture, Risk Mitigation.
