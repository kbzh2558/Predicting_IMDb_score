# 🎬📈 Predicting IMDb Ratings with Regression Modeling Techniques  
![R](https://img.shields.io/badge/R-4.3.2%2B-blue?logo=R)  
![Regression](https://img.shields.io/badge/Regression-Linear%20Model-orange)  
![Stepwise](https://img.shields.io/badge/Model%20Selection-Stepwise%20BIC-green)  
![Multicollinearity](https://img.shields.io/badge/VIF-Diagnostics-lightblue)  
![Nonlinear](https://img.shields.io/badge/Transformations-Polynomial%20%26%20Spline-red)  

📊 A Statistical Exploration of Movie Characteristics and Their Impact on IMDb Scores  

> **Authors**  
> Kaibo Zhang, Yanfei Wu, Tia Qiu, Wilson Chen, Xuechen Hong  
> **Affiliation**: McGill University  
> **Course**: MGSC401 - Midterm Project  

---

## 📌 Overview  

This project investigates how various movie attributes contribute to IMDb scores using multiple regression modeling techniques. The analysis includes rigorous **exploratory data analysis**, **variable transformation**, **multicollinearity checks**, and **model diagnostics** to ensure accurate and interpretable predictions.  

**Key contributions include:**  
- EDA on budget, duration, media coverage, and genre factors.  
- Stepwise model selection using BIC.  
- Addressing multicollinearity with VIF and eigenvalue decomposition.  
- Implementing nonlinear modeling via Box-Cox transformation, splines, and polynomial terms.  
- Forecasting IMDb scores for 12 unreleased movies in 2025.  

---

## 🎬 Dataset Description  

A cleaned version of the **IMDb Winter 2025 Dataset**, consisting of:
- ~2,000 historical movies
- Detailed features: budget, duration, language, maturity rating, number of articles, IMDbPro rank, genre, etc.

---

## 🔍 Modeling Pipeline  

1. **Data Preprocessing**
   - Removal of high-cardinality categorical variables (e.g., Director).
   - Category merging (e.g., Language → English vs Other).
2. **Initial Model**
   - Baseline regression model: R² = 35.18%
3. **Model Selection**
   - Stepwise regression with BIC for parsimony.
4. **Multicollinearity Analysis**
   - VIF diagnostics and eigenvalue decomposition.
5. **Nonlinear Transformations**
   - Target variable: Box-Cox power transform.
   - Predictors: log transform, polynomial (up to degree 3), quadratic duration, splines on Release Year.
6. **Influence Diagnostics**
   - Leverage, Cook’s distance, DFFITS → 70 outliers removed.
7. **Interaction Effects**
   - Notably: Movie Budget × Action Genre, Movie Budget × Release Year (after mean).

---

## 📊 Model Performance Summary  

| Metric                      | Value     |
|----------------------------|-----------|
| R² (Final Model)           | 59.00%    |
| Adjusted R²                | 58.00%    |
| RMSE (Training)            | 0.80      |
| RMSE (Test)                | 0.95      |
| MAPE (Test Set)            | 12.07%    |

### Cross-Validation Results:
| Method   | RMSE   | MAPE    |
|---------|--------|---------|
| 5-Fold  | 0.76   | 9.96%   |
| 10-Fold | 0.75   | 9.94%   |
| LOOCV   | 0.74   | 9.94%   |

---

## 🎯 Insights and Managerial Implications  

- **Budget ≠ Better Ratings**: Higher production budgets do not guarantee higher IMDb scores.  
- **Genre Influence**: Drama and animation genres score better, action and horror underperform.  
- **Publicity Paradox**: More media coverage boosts ratings up to a point — excessive exposure may backfire.  
- **Runtime Sweet Spot**: Audiences favor movies with balanced durations (90–120 minutes).  
- **Structural Shifts**: Post-2001 films exhibit different patterns — modeled via piecewise splines.  

---

## 🔮 Predictions for 2025 Releases  

| Movie Title                    | Predicted IMDb Score |
|-------------------------------|----------------------|
| The Day the Earth Blew Up     | 6.81                 |
| Novocaine                     | 6.64                 |
| Snow White                   | 6.54                 |
| The Alto Knights             | 6.40                 |
| A Working Man                | 6.38                 |
| Black Bag                    | 6.14                 |
| My Love Will Make You Disappear | 5.55              |
| Locked                       | 5.26                 |
| O'Dessa                      | 5.40                 |
| The Woman in the Yard        | 5.34                 |
| High Rollers                 | 4.88                 |
| Ash                          | 4.89                 |

---

## 📚 References  

[1] IMDb Datasets. https://www.imdb.com/interfaces  


---
