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

required_packages <- c("AER", "stargazer", "ggplot2", "corrplot", "lmtest", "car", "sandwich", "tseries")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Package", pkg, "not found. Installing...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# ------------------------------------------------------------------------------
# 1. Data Ingestion & Deduplication
# ------------------------------------------------------------------------------
# Handle file name differences robustly
dataset_file <- if (file.exists("spotify_tracks.csv")) {
  "spotify_tracks.csv"
} else if (file.exists("spotify_dataset.csv")) {
  "spotify_dataset.csv"
} else {
  stop("Spotify dataset file not found in current directory. Please provide 'spotify_dataset.csv' or 'spotify_tracks.csv'.")
}

cat("Loading dataset from:", dataset_file, "\n")
df <- read.csv(dataset_file, stringsAsFactors = FALSE)
cat("Original dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

# Aggressive deduplication: tracks are often duplicated across multiple genres
# or playlists. Deduplicating based on track_id avoids artificial variance deflation
# and inflation of sample size.
df_clean <- df[!is.na(df$track_id) & df$track_id != "", ]
df_clean <- df_clean[!duplicated(df_clean$track_id), ]
cat("Dimensions after track_id deduplication:", nrow(df_clean), "rows,", ncol(df_clean), "columns\n")

# Drop any rows with missing values in key variables
regression_vars <- c("popularity", "duration_ms", "explicit", "danceability", 
                     "energy", "key", "loudness", "mode", "speechiness", 
                     "acousticness", "instrumentalness", "liveness", 
                     "valence", "tempo", "time_signature")
df_clean <- na.omit(df_clean[, c(regression_vars, "track_id", "artists", "album_name", "track_name")])
cat("Dimensions after omitting rows with missing values:", nrow(df_clean), "rows\n")

# Variable transformations
df_clean$explicit <- ifelse(df_clean$explicit == "True" | df_clean$explicit == TRUE | df_clean$explicit == 1, 1, 0)
df_clean$duration_min <- df_clean$duration_ms / 60000  # Convert duration from ms to minutes for coefficient readability

# ------------------------------------------------------------------------------
# 2. Exploratory Data Analysis (EDA)
# ------------------------------------------------------------------------------
cat("\n=== Summary Statistics (Cleaned Data) ===\n")
print(summary(df_clean[, c("popularity", "duration_min", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")]))

censored_count <- sum(df_clean$popularity == 0)
censored_pct <- (censored_count / nrow(df_clean)) * 100
cat("\nLeft-censoring at zero popularity: ", censored_count, " observations (", round(censored_pct, 2), "% of sample)\n", sep="")

# Plot 1: Histogram of popularity illustrating left-censoring at zero
cat("Saving popularity histogram to 'popularity_histogram.png'...\n")
p_hist <- ggplot(df_clean, aes(x = popularity)) +
  geom_histogram(binwidth = 2, fill = "#1DB954", color = "#121212", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "#FF0000", linetype = "dashed", size = 1) +
  annotate("text", x = 12, y = 8000, label = paste("Censored at 0\n(N = ", censored_count, ")", sep=""), color = "#FF0000", fontface = "bold") +
  labs(title = "Distribution of Spotify Track Popularity",
       subtitle = "Illustrating Left-Censoring at Zero Popularity",
       x = "Popularity Metric (0 - 100)",
       y = "Frequency (Count)",
       caption = "Data Source: Spotify Tracks Dataset (Kaggle)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "#EBEBEB"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("popularity_histogram.png", plot = p_hist, width = 8, height = 5, dpi = 300)

# Plot 2: Acoustic features correlation matrix
cat("Saving correlation matrix to 'correlation_matrix.png'...\n")
acoustic_features <- c("duration_min", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
cor_matrix <- cor(df_clean[, acoustic_features])

png("correlation_matrix.png", width = 800, height = 800, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#FF3366", "#FFFFFF", "#1DB954"))(200),
         title = "\nCorrelation Matrix of Spotify Acoustic Features",
         mar = c(0, 0, 2, 0))
dev.off()

# ------------------------------------------------------------------------------
# 3. Model Estimation (General-to-Specific)
# ------------------------------------------------------------------------------
# We model popularity as a function of all acoustic features, controlling for key and time_signature factors.
# Independent variables include factors: key, time_signature; and continuous: duration_min, explicit, danceability, energy, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo.

formula_general <- popularity ~ duration_min + explicit * danceability + energy + as.factor(key) + 
                                loudness + mode + speechiness + acousticness + instrumentalness + 
                                liveness + valence + tempo + as.factor(time_signature)

# Model 1: Baseline OLS Model
cat("\nEstimating Model 1: Baseline OLS Model...\n")
fit_ols_gen <- lm(formula_general, data = df_clean)

# Model 2: General Tobit Model (censored at left = 0)
cat("Estimating Model 2: General Tobit Model...\n")
fit_tobit_gen <- tobit(formula_general, left = 0, data = df_clean)

# Model 3: Specific Parsimonious Tobit Model
# Using Gets, we find that in the OLS model, 'liveness' is insignificant (p-value ~ 0.15).
# However, when estimated via Tobit (correcting for censoring bias), 'liveness' becomes highly significant (p-value ~ 0.007).
# We omit 'liveness' in the restricted Model 3 to formally show the consequence of incorrect variable selection based on biased OLS results.
cat("Estimating Model 3: Specific Parsimonious Tobit Model (omitting liveness)...\n")
formula_specific <- popularity ~ duration_min + explicit * danceability + energy + as.factor(key) + 
                                 loudness + mode + speechiness + acousticness + instrumentalness + 
                                  valence + tempo + as.factor(time_signature)
fit_tobit_spec <- tobit(formula_specific, left = 0, data = df_clean)

# ------------------------------------------------------------------------------
# 4. Diagnostics & Post-Estimation
# ------------------------------------------------------------------------------
# Likelihood Ratio Test: General vs Specific Tobit (H0: beta_liveness = 0)
lr_test <- lrtest(fit_tobit_gen, fit_tobit_spec)
cat("\n=== Likelihood Ratio Test (General vs Specific Tobit) ===\n")
print(lr_test)

# McFadden's Pseudo-R² calculation: 1 - (LogLik_full / LogLik_null)
fit_tobit_null <- tobit(popularity ~ 1, left = 0, data = df_clean)
ll_null <- as.numeric(logLik(fit_tobit_null))
ll_gen <- as.numeric(logLik(fit_tobit_gen))
ll_spec <- as.numeric(logLik(fit_tobit_spec))

pseudo_r2_gen <- 1 - (ll_gen / ll_null)
pseudo_r2_spec <- 1 - (ll_spec / ll_null)
cat("\n=== McFadden's Pseudo-R² ===\n")
cat("General Tobit Model: ", round(pseudo_r2_gen, 6), "\n")
cat("Specific Tobit Model: ", round(pseudo_r2_spec, 6), "\n")

# McDonald-Moffitt Decomposition for Unconditional Marginal Effects:
# E[y|x] = Phi(x*beta / sigma) * (x*beta) + sigma * phi(x*beta / sigma)
# The marginal effect on the unconditional expected value is: dE[y|x]/dx_j = beta_j * Phi(x*beta / sigma)
# We evaluate the average scaling factor Phi(x_i*beta / sigma) across all observations to get Average Marginal Effects (AME).

# Diagnostics: Normality of residuals
res_tobit <- residuals(fit_tobit_gen)
cat("\n=== Normality Test (Jarque-Bera) ===\n")
print(jarque.bera.test(res_tobit))


# Linktest for specification
y_hat <- fitted(fit_tobit_gen)
y_hat_sq <- y_hat^2
linktest_model <- tobit(popularity ~ y_hat + y_hat_sq, left = 0, data = df_clean)
cat("\n=== Linktest (y_hat_sq p-value) ===\n")
print(summary(linktest_model)$coefficients["y_hat_sq", ])

# Post-Estimation: 3 Kinds of Marginal Effects
X_gen <- model.matrix(fit_tobit_gen)
beta_gen <- coef(fit_tobit_gen)
sigma_gen <- fit_tobit_gen$scale
index_gen <- as.numeric(X_gen %*% beta_gen)

pdf_val <- dnorm(index_gen / sigma_gen)
cdf_val <- pnorm(index_gen / sigma_gen)

me_latent <- beta_gen
me_prob <- mean(pdf_val) * (beta_gen / sigma_gen)
me_total <- beta_gen * mean(cdf_val)

mfx_table <- cbind("Latent_Var" = me_latent, "Prob_y>0" = me_prob, "Expected_y" = me_total)

cat("\n=== 3 Kinds of Average Marginal Effects (General Model) ===\n")
print(round(mfx_table, 5))

# ------------------------------------------------------------------------------
# 5. Stargazer Regression Table Output
# ------------------------------------------------------------------------------
cat("\n=== Generating Regression Table (Text Mode) ===\n")
stargazer(fit_ols_gen, fit_tobit_gen, fit_tobit_spec, 
          type = "text",
          title = "Popularity Regression Results: OLS vs. Tobit Models",
          column.labels = c("OLS Baseline", "General Tobit", "Restricted Tobit"),
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
              round(pseudo_r2_spec, 4))
          ))

cat("\nRegression table can be generated in LaTeX format for reports using:\n")
cat("stargazer(fit_ols_gen, fit_tobit_gen, fit_tobit_spec, type = 'latex', ...)\n")
cat("\n--- Script Completed Successfully ---\n")
