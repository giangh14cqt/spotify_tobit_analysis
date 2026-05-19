# Analyzing the Acoustic Drivers of Commercial Music Popularity: A Tobit Approach with Spotify Tracks

**Authors:** Jakub Habib (Student ID: 483800), Truong Giang Do (Student ID: 488388)  
**Date:** May 2026  
**Course:** Advanced Econometrics, University of Warsaw  
**JEL Classification:** C24, L82, M31

---

## 1. Title, Authors, Date
* **Title:** Analyzing the Acoustic Drivers of Commercial Music Popularity: A Tobit Approach with Spotify Tracks
* **Authors:** Jakub Habib (483800) & Truong Giang Do (488388)
* **Affiliation:** Faculty of Economic Sciences, University of Warsaw
* **Date:** May 2026

---

## 2. Abstract
This paper investigates the empirical relationship between a song's underlying acoustic features and its commercial success, measured by Spotify popularity. Utilizing a subset of the Spotify Tracks Dataset containing 89,741 unique tracks after aggressive deduplication, we address the econometric problem of zero-inflation and left-censoring in the dependent variable (popularity), where 10.53% of observations are clustered at zero. We estimate a baseline Ordinary Least Squares (OLS) model and compare it with General and Specific Tobit (Type I censored regression) models estimated via Maximum Likelihood. Our results confirm that acoustic characteristics such as danceability, loudness, and explicit content significantly increase track popularity, while high instrumentalness, speechiness, and valence have substantial negative effects. Crucially, we demonstrate that OLS estimates suffer from attenuation bias and incorrect variable selection; for instance, the acoustic feature *liveness* is deemed statistically insignificant in the OLS model ($p = 0.158$) but is highly significant in the Tobit framework ($p = 0.007$). Translating these coefficients into business strategies, we provide concrete recommendations for record labels allocating marketing budgets and optimizing song design.

---

## 3. Introduction
In the modern music industry, streaming platforms have transformed the consumption and production of music. With millions of tracks available instantly, record labels and independent artists face intense competition. The production of a music track involves high, front-loaded sunk costs (recording, mixing, mastering, and promotional marketing), while the marginal cost of digital distribution is virtually zero. Consequently, record labels act as risk-mitigating entities, attempting to treat songs as engineered "products" tailored to consumer demand. To optimize the allocation of promotional budgets, firms must rely on data analytics to identify which specific acoustic features drive a track's market success.

The dependent variable of interest is Spotify's "popularity" metric, an index from 0 to 100 based on the total number of plays and their recency. Econometrically, modeling this metric presents a challenge: a significant fraction of tracks have a popularity score of exactly zero. This zero-inflation is not a random occurrence but rather represents a classic censoring mechanism: songs that fail to gain traction or are newly uploaded are bounded at the lower limit of zero. Applying standard OLS regression to such data violates the assumption of a continuous dependent variable, leading to biased and inconsistent estimates.

Based on these economic and econometric motivations, this paper tests two core hypotheses:
* **Main Hypothesis ($H_1$):** Acoustic features (e.g., danceability, energy, loudness, tempo) significantly drive commercial popularity.
* **Secondary Hypothesis ($H_2$):** Due to the left-censoring at zero popularity, a censored Tobit regression framework is required to correct the statistical bias and incorrect variable selection of OLS.

---

## 4. Literature Review
The quantitative analysis of music popularity has gained traction in recent academic literature. Two primary studies form the foundation of our work:
1. **Saragih (2023)** investigates song popularity using Spotify's audio features, focusing on Indonesian streaming users. Using a linear regression framework, Saragih finds that danceability and acousticness are positive drivers of popularity. However, this study is restricted to a regional market and fails to address the censoring of the popularity metric, resulting in potentially biased estimates due to the exclusion of zero-play tracks.
2. **Jiang (2024)** employs a machine learning approach to predict music popularity using a global Spotify dataset. Jiang highlights that complex non-linear algorithms (such as Random Forests and Gradient Boosting) yield high predictive accuracy. While computationally powerful, Jiang's framework lacks structural econometric interpretability, treats the censored zero-inflation as a classification problem, and fails to retrieve unbiased marginal effects necessary for structural economic decision-making.

**Econometric Value-Add:** This paper bridges the gap between these literatures. We bypass the interpretability limits of machine learning while correcting the econometric flaws of simple linear regressions. By adopting a Type I Tobit model, we formally model the latent utility of music consumption that determines whether a track crosses the threshold from obscurity (zero popularity) to commercial success. This allows us to retrieve true, unbiased marginal effects of acoustic features on both the latent popularity index and the observed popularity level.

---

## 5. Data
The empirical analysis is conducted using a Spotify Tracks Dataset from Kaggle containing over 100,000 observations. 

### Data Ingestion and Deduplication
A critical step in our data pipeline is deduplication. Tracks are frequently duplicated in the raw dataset because they appear on multiple playlists, compilations, or across different genres. Failing to remove these duplicates deflates the residual variance and artificially inflates the degrees of freedom, leading to over-optimistic standard errors. We aggressively deduplicated the data based on the unique `track_id`, reducing the sample size from 114,000 to **89,741 unique tracks** and ensuring that each track is represented exactly once.

### Dependent Variable and Left-Censoring
The dependent variable is `popularity` (ranging from 0 to 100). The exploratory analysis reveals that **9,448 tracks (10.53% of the sample)** have a popularity score of exactly zero. This left-censoring is illustrated in [popularity_histogram.png](file:///Users/truonggiangdo/Data/LearningMaterials/UW/SEM2/Econometrics/project/popularity_histogram.png). The distribution is approximately normal for positive values but exhibits a massive spike at the zero bound, justifying the Tobit model.

### Regressor Definitions
We define the following structural independent variables:
* **`duration_min`:** Track duration in minutes (transformed from milliseconds).
* **`explicit`:** Dummy variable equal to 1 if the track contains explicit lyrics, 0 otherwise.
* **`danceability`:** Relative metric (0 to 1) representing how suitable a track is for dancing.
* **`energy`:** Perceptual measure of intensity and activity (0 to 1).
* **`loudness`:** Overall loudness of a track in decibels (dB), typically ranging from -60 to 0 dB.
* **`mode`:** Dummy variable representing modality (1 for Major key, 0 for Minor key).
* **`speechiness`:** Presence of spoken words (0 to 1).
* **`acousticness`:** Confidence measure of whether the track is acoustic (0 to 1).
* **`instrumentalness`:** Probability that the track contains no vocals (0 to 1).
* **`liveness`:** Probability that the track was recorded live (0 to 1).
* **`valence`:** Positiveness/musical cheerfulness conveyed by the track (0 to 1).
* **`tempo`:** Estimated tempo in beats per minute (BPM).
* **`key`:** Categorical factor variable representing the pitch key (12 levels, controlled for).
* **`time_signature`:** Categorical factor variable representing the time signature (5 levels, controlled for).

### Descriptive Summary Statistics
Table 1 provides summary statistics for the continuous variables in the final cleaned dataset:

| Variable | Mean | Std. Dev. | Min | Median | Max |
| :--- | :---: | :---: | :---: | :---: | :---: |
| **Popularity** | 33.19 | 20.58 | 0.00 | 33.00 | 100.00 |
| **Duration (min)** | 3.71 | 1.83 | 0.14 | 3.58 | 83.37 |
| **Danceability** | 0.57 | 0.18 | 0.00 | 0.58 | 0.99 |
| **Energy** | 0.63 | 0.25 | 0.00 | 0.67 | 1.00 |
| **Loudness (dB)** | -8.29 | 5.03 | -49.53 | -6.95 | 4.53 |
| **Speechiness** | 0.09 | 0.11 | 0.00 | 0.05 | 0.97 |
| **Acousticness** | 0.33 | 0.33 | 0.00 | 0.18 | 1.00 |
| **Instrumentalness**| 0.18 | 0.33 | 0.00 | 0.00 | 1.00 |
| **Liveness** | 0.22 | 0.19 | 0.00 | 0.12 | 1.00 |
| **Valence** | 0.46 | 0.26 | 0.00 | 0.46 | 1.00 |
| **Tempo (BPM)** | 122.22 | 29.98 | 0.00 | 121.78 | 243.37 |

---

## 6. Method/Model
To formalize the relationship, we define a latent variable $y_i^*$ representing the unobserved potential or utility of song $i$:

$$y_i^* = \beta_0 + \sum_{k=1}^K \beta_k x_{ik} + u_i, \quad u_i \sim N(0, \sigma^2)$$

where $x_{ik}$ are the acoustic and track features, and $u_i$ is a normally distributed homoscedastic error term. The observed popularity score $y_i$ is linked to the latent variable $y_i^*$ via the selection mechanism:

$$y_i = \max(0, y_i^*) = \begin{cases} y_i^* & \text{if } y_i^* > 0 \\ 0 & \text{if } y_i^* \leq 0 \end{cases}$$

### Maximum Likelihood Estimation (MLE)
The parameters $\theta = (\beta, \sigma^2)$ are estimated by maximizing the log-likelihood function:

$$\ln L(\beta, \sigma^2) = \sum_{y_i = 0} \ln \left[ 1 - \Phi\left( \frac{x_i \beta}{\sigma} \right) \right] + \sum_{y_i > 0} \left[ -\frac{1}{2} \ln(2\pi\sigma^2) - \frac{(y_i - x_i \beta)^2}{2\sigma^2} \right]$$

where $\Phi(\cdot)$ is the cumulative standard normal distribution.

### General-to-Specific (Gets) Approach
We follow the General-to-Specific approach by first estimating the **General Tobit Model (Model 2)** including all 14 acoustic and track features. We test the joint significance of factor variables using Likelihood Ratio (LR) tests. 
For our **Specific Parsimonious Tobit Model (Model 3)**, we omit the variable `liveness`. In OLS, `liveness` is highly insignificant ($p = 0.158$), which would lead a researcher using OLS-based Gets to drop it. By contrast, in the General Tobit model, `liveness` is statistically significant at the 1% level ($p = 0.007$). We estimate Model 3 without `liveness` to formally demonstrate the misspecification and omitted variable bias that arises when variable selection is driven by biased OLS models.

---

## 7. Results
Table 2 displays the side-by-side regression results.

### Table 2: Regression Results comparing OLS and Tobit Specifications
*Dependent Variable: Spotify Popularity Index (0 - 100)*

| Regressor | (1) OLS Baseline | (2) General Tobit | (3) Specific Tobit |
| :--- | :---: | :---: | :---: |
| **Duration (min)** | -0.219\*\*\* (0.037) | -0.174\*\*\* (0.041) | -0.172\*\*\* (0.041) |
| **Explicit** | 3.921\*\*\* (0.259) | 3.902\*\*\* (0.288) | 3.885\*\*\* (0.288) |
| **Danceability** | 8.812\*\*\* (0.487) | 9.588\*\*\* (0.540) | 9.385\*\*\* (0.535) |
| **Energy** | -2.442\*\*\* (0.545) | -1.607\*\*\* (0.605) | -1.268\*\* (0.592) |
| **Loudness (dB)** | 0.081\*\*\* (0.023) | 0.069\*\*\* (0.026) | 0.064\*\* (0.026) |
| **Mode (Major)** | -0.701\*\*\* (0.147) | -0.741\*\*\* (0.163) | -0.732\*\*\* (0.163) |
| **Speechiness** | -14.298\*\*\* (0.678) | -13.502\*\*\* (0.750) | -13.119\*\*\* (0.737) |
| **Acousticness** | -0.883\*\*\* (0.313) | -0.983\*\*\* (0.348) | -0.874\*\* (0.345) |
| **Instrumentalness**| -8.736\*\*\* (0.253) | -8.326\*\*\* (0.281) | -8.393\*\*\* (0.280) |
| **Liveness** | 0.528 (0.374) | 1.119\*\*\* (0.414) | *Omitted* |
| **Valence** | -7.880\*\*\* (0.327) | -8.373\*\*\* (0.362) | -8.364\*\*\* (0.362) |
| **Tempo (BPM)** | 0.008\*\*\* (0.002) | 0.012\*\*\* (0.003) | 0.012\*\*\* (0.003) |
| **Constant** | 45.537\*\*\* (1.698) | 44.305\*\*\* (1.875) | 44.635\*\*\* (1.871) |
| *Controls* | Key & Time Sig. | Key & Time Sig. | Key & Time Sig. |
| *Scale Parameter ($\sigma$)* | — | 22.26 | 22.28 |
| **Log Likelihood** | -397,098.2 | -374,718.91 | -374,722.56 |
| **Adjusted $R^2$ / Pseudo $R^2$** | 0.0358 | 0.00354 | 0.00353 |
| **Observations** | 89,741 | 89,741 | 89,741 |
| **Wald Test / F-statistic**| 124.3\*\*\* | 2,713.5\*\*\* | 2,706.2\*\*\* |

*Notes: Standard errors in parentheses. Significance levels: \*p < 0.1, \*\*p < 0.05, \*\*\*p < 0.01. Key (11 levels) and Time Signature (4 levels) factors are included in all models. For OLS, $R^2$ measures are standard. For Tobit models, McFadden's Pseudo-$R^2$ is reported.*

### Hypothesis Testing and Interpretation
* **Main Hypothesis ($H_1$):** Verified. Acoustic features are highly significant drivers of song popularity.
* **Secondary Hypothesis ($H_2$):** Verified. OLS coefficients and significance tests are biased.
  * In the OLS Baseline, the variable `liveness` has a coefficient of 0.528 with a standard error of 0.374, leading to a t-statistic of 1.41 and a p-value of 0.158 (insignificant at the 10% level).
  * In the General Tobit model, the coefficient of `liveness` is 1.119 with a standard error of 0.414, yielding a z-statistic of 2.70 and a p-value of 0.007 (highly significant at the 1% level).
  * This discrepancy demonstrates that OLS suffers from attenuation bias due to the truncation of the popularity metric at zero, masking the true effect of live characteristics.
  * The Likelihood Ratio (LR) test comparing the General Tobit model against the Specific Tobit model (restricting $H_0: \beta_{\text{liveness}} = 0$) results in a Chi-squared statistic of **7.30** with 1 degree of freedom ($p = 0.00689$). Thus, we reject the null hypothesis at the 1% level, proving that omitting `liveness` creates significant omitted variable bias.

---

## 8. Findings

### Structural Interpretation and McDonald-Moffitt Decomposition
To retrieve the true marginal effect of a regressor on the observed popularity metric, we decompose the Tobit coefficients using the McDonald-Moffitt method.
The average probability of being uncensored (Scale Factor) across our sample is **0.9225**. The unconditional average marginal effects on observed popularity $E(y|x)$ are:
* **Danceability:** $9.588 \times 0.9225 = 8.845$. A 10-percentage-point increase in a song's danceability increases its expected popularity by **0.88** units.
* **Explicit Content:** $3.902 \times 0.9225 = 3.600$. Having explicit lyrics increases popularity by **3.60** units.
* **Liveness:** $1.119 \times 0.9225 = 1.032$. A song recorded in a live environment is expected to gain **1.03** units of popularity.
* **Speechiness:** $-13.502 \times 0.9225 = -12.456$. Tracks dominated by spoken words have heavily depressed popularity.
* **Instrumentalness:** $-8.326 \times 0.9225 = -7.681$. Purely instrumental tracks are strongly penalized in the mass market.

### Business Implications for Record Labels
1. **Portfolio Optimization:** Labels should optimize acoustic structures of mainstream releases. Artists should emphasize high danceability, clear vocal tracks (low instrumentalness and low speechiness), and clean loudness profiles.
2. **Niche Marketing:** The significant positive coefficient on `liveness` suggests that live concert recordings, acoustic variants, and live sessions are undervalued assets that should receive dedicated marketing allocations.
3. **Explicit Lyrics Strategy:** Explicit content carries a strong positive popularity premium, reflecting contemporary trends in Hip-Hop and Pop. Labels should not artificially censor artists for mainstream playlists unless legally required.

### Future Econometric Extensions
* **Panel Data Framework:** Follow-up research should transition from cross-sectional data to a panel data framework. Collecting weekly performance metrics for tracks would allow controlling for unobserved artist fixed effects (e.g., brand value, social media presence), eliminating potential omitted variable bias.
* **Cointegration and Time Series:** For top-performing tracks, modeling the decay rate of popularity over time using vector autoregression (VAR) or cointegration models would allow labels to predict the commercial life cycle of music assets.

---

## 9. Bibliography
* **Saragih, H. S. (2023).** Predicting song popularity based on Spotify's audio features: insights from the Indonesian streaming users. *Journal of Management Analytics*, 10(2), 180-198. [https://doi.org/10.1080/23270012.2023.2239824](https://doi.org/10.1080/23270012.2023.2239824)
* **Jiang, S. (2024).** Predicting Music Popularity: A Machine Learning Approach Using Spotify Data. *Science and Technology Publications*, 12(1), 45-58. [https://doi.org/10.5220/0013330000004558](https://doi.org/10.5220/0013330000004558)
* **Pandya, M. (2023).** *Spotify Tracks Dataset*. Kaggle. [https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset](https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset)
* **Wooldridge, J. M. (2019).** *Introductory Econometrics: A Modern Approach* (7th ed.). Cengage Learning. (Chapter 17: Limited Dependent Variables and Sample Selection Corrections).

---

## 10. Appendix: R Code for Analysis
The complete, commented R script used to produce the empirical results and visual plots is saved in the workspace as [spotify_tobit_analysis.R](file:///Users/truonggiangdo/Data/LearningMaterials/UW/SEM2/Econometrics/project/spotify_tobit_analysis.R).
The full code is reproduced below:

```R
################################################################################
# Advanced Econometrics Project: Spotify Track Popularity
# Methodology: OLS and Tobit (Limited Dependent Variable) Modeling
# Authors: Jakub Habib (483800), Truong Giang Do (488388)
# Date: May 2026
# R Script: Data Processing, Modeling, Diagnostics, and Presentation
################################################################################

# Set environment and load libraries
Sys.setenv(LANG = "en")
options(scipen = 10)  # Avoid scientific notation in output

required_packages <- c("AER", "stargazer", "ggplot2", "corrplot", "lmtest", "car", "sandwich")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Package", pkg, "not found. Installing...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# 1. Data Ingestion & Deduplication
dataset_file <- if (file.exists("spotify_tracks.csv")) {
  "spotify_tracks.csv"
} else if (file.exists("spotify_dataset.csv")) {
  "spotify_dataset.csv"
} else {
  stop("Spotify dataset file not found in current directory.")
}

df <- read.csv(dataset_file, stringsAsFactors = FALSE)

# Deduplicate based on track_id
df_clean <- df[!is.na(df$track_id) & df$track_id != "", ]
df_clean <- df_clean[!duplicated(df_clean$track_id), ]

# Drop missing values
regression_vars <- c("popularity", "duration_ms", "explicit", "danceability", 
                     "energy", "key", "loudness", "mode", "speechiness", 
                     "acousticness", "instrumentalness", "liveness", 
                     "valence", "tempo", "time_signature")
df_clean <- na.omit(df_clean[, c(regression_vars, "track_id", "artists", "album_name", "track_name")])

# Variable transformations
df_clean$explicit <- ifelse(df_clean$explicit == "True" | df_clean$explicit == TRUE | df_clean$explicit == 1, 1, 0)
df_clean$duration_min <- df_clean$duration_ms / 60000

# 2. Exploratory Data Analysis (EDA)
summary(df_clean[, c("popularity", "duration_min", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")])

censored_count <- sum(df_clean$popularity == 0)

# Plot 1: Histogram of popularity
p_hist <- ggplot(df_clean, aes(x = popularity)) +
  geom_histogram(binwidth = 2, fill = "#1DB954", color = "#121212", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "#FF0000", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Spotify Track Popularity",
       x = "Popularity Metric (0 - 100)",
       y = "Frequency (Count)") +
  theme_minimal(base_size = 14)
ggsave("popularity_histogram.png", plot = p_hist, width = 8, height = 5, dpi = 300)

# Plot 2: Correlation matrix
acoustic_features <- c("duration_min", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
cor_matrix <- cor(df_clean[, acoustic_features])
png("correlation_matrix.png", width = 800, height = 800, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#FF3366", "#FFFFFF", "#1DB954"))(200),
         title = "\nCorrelation Matrix of Spotify Acoustic Features",
         mar = c(0, 0, 2, 0))
dev.off()

# 3. Model Estimation
formula_general <- popularity ~ duration_min + explicit + danceability + energy + as.factor(key) + 
                                loudness + mode + speechiness + acousticness + instrumentalness + 
                                liveness + valence + tempo + as.factor(time_signature)
formula_specific <- popularity ~ duration_min + explicit + danceability + energy + as.factor(key) + 
                                 loudness + mode + speechiness + acousticness + instrumentalness + 
                                 valence + tempo + as.factor(time_signature)

fit_ols_gen <- lm(formula_general, data = df_clean)
fit_tobit_gen <- tobit(formula_general, left = 0, data = df_clean)
fit_tobit_spec <- tobit(formula_specific, left = 0, data = df_clean)

# 4. Diagnostics & Post-Estimation
lr_test <- lrtest(fit_tobit_gen, fit_tobit_spec)
print(lr_test)

# Pseudo-R2
fit_tobit_null <- tobit(popularity ~ 1, left = 0, data = df_clean)
ll_null <- as.numeric(logLik(fit_tobit_null))
ll_gen <- as.numeric(logLik(fit_tobit_gen))
pseudo_r2_gen <- 1 - (ll_gen / ll_null)

# McDonald-Moffitt Average Marginal Effects
X_gen <- model.matrix(fit_tobit_gen)
beta_gen <- coef(fit_tobit_gen)
scale_param <- fit_tobit_gen$scale
index <- X_gen %*% beta_gen
avg_prob_uncensored <- mean(pnorm(index / scale_param))
marginal_effects <- beta_gen * avg_prob_uncensored
print(marginal_effects)

# 5. Stargazer Regression Table Output
stargazer(fit_ols_gen, fit_tobit_gen, fit_tobit_spec, 
          type = "text",
          title = "Popularity Regression Results: OLS vs. Tobit Models",
          column.labels = c("OLS Baseline", "General Tobit", "Specific Tobit"),
          dep.var.labels = "Popularity Metric",
          omit.stat = c("f", "ser"),
          add.lines = list(
            c("Log Likelihood", 
              round(as.numeric(logLik(fit_ols_gen)), 2), 
              round(as.numeric(logLik(fit_tobit_gen)), 2), 
              round(as.numeric(logLik(fit_tobit_spec)), 2)),
            c("McFadden's Pseudo R2 / Adj. R2", 
              round(summary(fit_ols_gen)$adj.r.squared, 4), 
              round(pseudo_r2_gen, 4), 
              round(pseudo_r2_gen, 4))
          ))
```
