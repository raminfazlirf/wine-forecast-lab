### About this app

**Question it answers:** for each Australian wine varietal, which forecasting model should a planner trust?

**Data.** `AustralianWines.csv`: monthly sales (thousands of litres) of six varietals, Jan 1980 to Dec 1994, from Shmueli & Lichtendahl, *Practical Time Series Forecasting with R*. Rosé has two missing values (Jul and Aug 1994, coded `*`); the app fills them by linear interpolation and marks them with red dots.

**Method.**

1. The CSV is reshaped into a monthly `tsibble` keyed by `Varietal`.
2. Data up to *Training ends* trains the models; later months form the validation window.
3. Three auto-specified `fable` models are fitted per varietal: `TSLM(Sales ~ trend() + season())`, `ETS(Sales)` and `ARIMA(Sales)`. An optional equal-weight **Ensemble** averages them, and an optional log transform stabilises variance.
4. Validation forecasts are scored with `fabletools::accuracy()` (RMSE, MAE, MAPE, MASE). The model with the lowest validation RMSE is recommended.
5. *Future forecast* refits the same model forms on all selected data and forecasts *h* months ahead with prediction intervals.

**Reproduce any figure.** Choose the varietals, date range, training end, models and options listed in the figure caption, click **Fit / update models**, then open the named tab.

**Acknowledgement.** Code drafted with help from Claude (Anthropic), an AI assistant, and reviewed and tested by the author. Methods follow Hyndman & Athanasopoulos, *Forecasting: Principles and Practice* (3rd ed.).
