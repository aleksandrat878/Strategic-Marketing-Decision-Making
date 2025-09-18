## ----include=FALSE------------------------------------------------------------
# install pacman if you don’t have it yet
if (!require("pacman")) install.packages("pacman")

# load all packages at once
pacman::p_load(
  tidyverse,
  scales,
  ggplot2,
  dplyr,
  tinytex,
  kableExtra,
  broom
)

# Global knitr options for clean output
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  fig.width = 7,     # ~ full text width
  fig.height = 4.2,  # comfortable aspect
  out.width = "92%"
)

# A consistent, readable ggplot theme
theme_mm <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.caption = element_text(size = 9, color = "grey30"),
    axis.title.y = element_text(margin = margin(r = 6)),
    axis.title.x = element_text(margin = margin(t = 6)),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )
theme_set(theme_mm)



## ----include=FALSE------------------------------------------------------------
# Read data
stopifnot(file.exists("marketing_mix_assignment.csv"))
data <- readr::read_csv("marketing_mix_assignment.csv", show_col_types = FALSE)

# Standardize names
data <- data %>%
  rename(
    google_ads = `Google Ads`,
    facebook   = Facebook,
    tiktok     = TikTok,
    sales      = Sales
  )

# Parse Date ("mm/dd/yyyy" like "09/10/2025")
data <- data %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Quick quality checks
missing_counts <- colSums(is.na(dplyr::select(data, google_ads, facebook, tiktok, sales)))
neg_counts <- sapply(dplyr::select(data, google_ads, facebook, tiktok, sales), function(x) sum(x < 0, na.rm = TRUE))

tibble::tibble(
  Metric = c("Missing values", "Negative values"),
  Google_Ads = c(missing_counts[["google_ads"]], neg_counts[["google_ads"]]),
  Facebook   = c(missing_counts[["facebook"]],   neg_counts[["facebook"]]),
  TikTok     = c(missing_counts[["tiktok"]],     neg_counts[["tiktok"]]),
  Sales      = c(missing_counts[["sales"]],      neg_counts[["sales"]])
) |>
  kable(caption = "Data Quality Checks") |>
  kable_styling(full_width = FALSE, position = "center")



## -----------------------------------------------------------------------------
# Desired facet order (top → bottom)
order_levels <- c("Sales", "Google Ads", "Facebook", "TikTok")

long <- data %>%
  select(Date, Sales = sales, `Google Ads` = google_ads,
         Facebook = facebook, TikTok = tiktok) %>%
  pivot_longer(-Date, names_to = "series", values_to = "value") %>%
  mutate(
    series = factor(series, levels = order_levels)  # puts Sales first
  )

ggplot(long, aes(Date, value, color = series)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(
    "Sales"      = "orange",
    "Google Ads" = "steelblue",
    "Facebook"   = "firebrick",
    "TikTok"     = "darkgreen"
  )) +
  labs(title = "Sales and Weekly Marketing Spend by Media", x = NULL, y = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = label_dollar(prefix = "$")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")




## -----------------------------------------------------------------------------
# Build time series + log transforms
google_ads <- data$google_ads
facebook   <- data$facebook
tiktok     <- data$tiktok
sales      <- data$sales

ln_google_ads <- log(google_ads + 1)
ln_facebook   <- log(facebook   + 1)
ln_tiktok     <- log(tiktok     + 1)
ln_sales      <- log(sales      + 1)

# One-period lag of ln(Sales)
Lag1_ln_sales <- dplyr::lag(ln_sales, 1)

# Drop first row with NA lag
model_df <- tibble(
  ln_sales = ln_sales,
  Lag1_ln_sales,
  ln_google_ads,
  ln_facebook,
  ln_tiktok
) %>% drop_na()

# Fit model
regression1 <- lm(
  ln_sales ~ Lag1_ln_sales + ln_google_ads + ln_facebook + ln_tiktok,
  data = model_df
)

# Tidy summary table
reg1_tidy <- broom::tidy(regression1, conf.int = TRUE) %>%
  mutate(term = recode(term,
    "(Intercept)"   = "Intercept",
    "Lag1_ln_sales" = "Lagged ln(Sales)",
    "ln_google_ads" = "ln(Google Ads)",
    "ln_facebook"   = "ln(Facebook)",
    "ln_tiktok"     = "ln(TikTok)"
  ))

reg1_glance <- broom::glance(regression1)

reg1_tidy %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high= round(conf.high, 3),
    p.value  = scales::pvalue(p.value)
  ) %>%
  select(Term = term, Estimate = estimate, `95% CI Low` = conf.low, `95% CI High` = conf.high, `p-value` = p.value) %>%
  kable(caption = "Model 1: Log–Log Regression with Carry-Over") %>%
  kable_styling(full_width = FALSE, position = "center")




## ----include=FALSE------------------------------------------------------------
fitted_vals <- fitted(regression1)
actual_vals <- model_df$ln_sales

tibble(
  Index = seq_along(actual_vals),
  `Actual ln(Sales)` = actual_vals,
  `Fitted ln(Sales)` = fitted_vals
) %>%
  pivot_longer(-Index, names_to = "Series", values_to = "Value") %>%
  ggplot(aes(Index, Value, colour = Series, linetype = Series)) +
  geom_line(linewidth = 0.9) +
  labs(x = "Observation", y = "Log Sales") +
  theme(legend.position = "top")



## ----include=FALSE------------------------------------------------------------
# Pull betas
beta_google_ads <- coef(regression1)[["ln_google_ads"]]
beta_facebook   <- coef(regression1)[["ln_facebook"]]
beta_tiktok     <- coef(regression1)[["ln_tiktok"]]

# Baselines
avg_sales    <- mean(sales, na.rm = TRUE)
avg_google   <- mean(google_ads, na.rm = TRUE)
avg_facebook <- mean(facebook,   na.rm = TRUE)
avg_tiktok   <- mean(tiktok,     na.rm = TRUE)

# Unit effects (theta)
theta_google <- beta_google_ads * (avg_sales / avg_google)
theta_fb     <- beta_facebook   * (avg_sales / avg_facebook)
theta_ttk    <- beta_tiktok     * (avg_sales / avg_tiktok)

# Spend totals
google_spend   <- sum(google_ads, na.rm = TRUE)
facebook_spend <- sum(facebook,   na.rm = TRUE)
tiktok_spend   <- sum(tiktok,     na.rm = TRUE)

# Contributions
contrib_google <- theta_google * google_spend
contrib_fb     <- theta_fb     * facebook_spend
contrib_ttk    <- theta_ttk    * tiktok_spend

# ROI (Sales per unit spend)
roi_df <- tibble(
  Channel = c("Google Ads", "Facebook", "TikTok"),
  ROI     = c(contrib_google / google_spend,
              contrib_fb     / facebook_spend,
              contrib_ttk    / tiktok_spend)
) %>%
  mutate(ROI = round(ROI, 2))

roi_df %>%
  kable(caption = "Sales Return on Marketing Investment (SROMI) by Channel") %>%
  kable_styling(full_width = FALSE, position = "center")



## -----------------------------------------------------------------------------
roi_df %>%
  ggplot(aes(reorder(Channel, ROI), ROI)) +
  geom_col(color = "black", fill = c("steelblue", "firebrick", "darkgreen")[rank(roi_df$ROI)]) +
  geom_text(aes(label = ROI), vjust = -0.3, size = 3.6) +
  labs(
    x = NULL, 
    y = NULL,  # remove y-axis label
    title = "Sales Return on Marketing Investment"  # put it on top
  ) +
  coord_cartesian(ylim = c(0, max(roi_df$ROI) * 1.2)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # center & bold
    axis.text.x = element_text(face = "bold")
  )




## -----------------------------------------------------------------------------
# Actual spend distribution
actual_spend <- tibble(
  Channel = c("Google Ads", "Facebook", "TikTok"),
  Spend   = c(google_spend, facebook_spend, tiktok_spend)
) %>% mutate(Share = Spend / sum(Spend),
         Type  = "Current") 

# Elasticity-based optimal share (proportional to betas)
beta_sum <- beta_google_ads + beta_facebook + beta_tiktok
opt_share <- tibble(
  Channel = c("Google Ads", "Facebook", "TikTok"),
  Share   = c(beta_google_ads, beta_facebook, beta_tiktok) / beta_sum,
  Type = "Optimal"
)

bind_rows(
  actual_spend %>% transmute(Channel, Type = "Current", Share),
  opt_share     %>% transmute(Channel, Type = "Optimal", Share)
) %>%
  mutate(SharePct = scales::percent(Share)) %>%
  select(-Share) %>%                                     # drop numeric Share
  pivot_wider(id_cols = Channel,                         # ensure 1 row per Channel
              names_from = Type, values_from = SharePct) %>%
  kable(caption = "Current vs. Model-Recommended Budget Shares") %>%
  kable_styling(full_width = FALSE, position = "center")




## -----------------------------------------------------------------------------
plot_df <- bind_rows(actual_spend, opt_share) %>%
  mutate(
    Channel = factor(Channel, levels = c("Facebook", "Google Ads", "TikTok")),
    Type    = factor(Type, levels = c("Current", "Optimal"))
  )

ggplot(plot_df, aes(x = Channel, y = Share, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("Current" = "#1f77b4", "Optimal" = "#ff7f0e")) +
  labs( title = "Marketing budget: Current vs optimal allocation", x = "Channel", y = "Allocation (%)", fill = "Type" ) +
  coord_cartesian(ylim = c(0, max(plot_df$Share) * 1.2)) +  # ~20% headroom
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),   # centered & bold
    axis.text.x = element_text(face = "bold"),
    legend.position = "right"
  )




## -----------------------------------------------------------------------------
regression2 <- lm(
  ln_sales ~ Lag1_ln_sales + ln_google_ads + ln_facebook + ln_tiktok +
    ln_google_ads:ln_facebook +
    ln_google_ads:ln_tiktok +
    ln_facebook:ln_tiktok,
  data = model_df
)

reg2_tidy <- broom::tidy(regression2) %>%
  mutate(term = recode(term,
    "(Intercept)"                         = "Intercept",
    "Lag1_ln_sales"                       = "Lagged ln(Sales)",
    "ln_google_ads"                       = "ln(Google Ads)",
    "ln_facebook"                         = "ln(Facebook)",
    "ln_tiktok"                           = "ln(TikTok)",
    "ln_google_ads:ln_facebook"           = "ln(Google) × ln(Facebook)",
    "ln_google_ads:ln_tiktok"             = "ln(Google) × ln(TikTok)",
    "ln_facebook:ln_tiktok"               = "ln(Facebook) × ln(TikTok)"
  ))

reg2_tidy %>%
  mutate(estimate = round(estimate, 4),
         p.value  = scales::pvalue(p.value)) %>%
  select(Term = term, Estimate = estimate, `p-value` = p.value) %>%
  kable(caption = "Interaction Terms (Synergies/Antagonisms)") %>%
  kable_styling(full_width = FALSE, position = "center")



## ----echo=FALSE, results='asis'-----------------------------------------------
# Extract the R code from the current Rmd
code_file <- knitr::purl("Assignment1.rmd", quiet = TRUE)
cat("```r\n")
cat(readLines(code_file), sep = "\n")
cat("\n```")

