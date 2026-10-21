# STAT 4050: Are Smaller Companies Riskier?
library(dplyr)
library(ggplot2)

# 1. Read and clean the data
stocks <- read.csv("Data.csv", stringsAsFactors = FALSE)


stocks$DlyCalDt <- as.Date(stocks$DlyCalDt, format = "%Y-%m-%d")
stocks$DlyRet <- suppressWarnings(as.numeric(stocks$DlyRet))
stocks$DlyCap <- suppressWarnings(as.numeric(stocks$DlyCap))

# a return cannot be less than -100%, missing codes become NA
stocks$DlyRet[stocks$DlyRet < -1] <- NA
if (anyNA(stocks$DlyCalDt)) stop("not YYYY-MM-DD format")
if (anyDuplicated(stocks[c("PERMNO", "DlyCalDt")])) stop("duplicate stock or date rows")

# measure company size at the start
# identify shares of U.S. corporations.
baseline <- stocks %>%
  filter(DlyCalDt == as.Date("2023-12-29"),
         ShareType == "NS", SecurityType == "EQTY", SecuritySubType == "COM",
         USIncFlg == "Y", IssuerType %in% c("ACOR", "CORP"),
         !is.na(PERMCO), !is.na(DlyCap), DlyCap > 0) %>%
  group_by(PERMCO) %>%
  
  # DlyCap is in thousands of dollars
  # sum share classes for company size
  mutate(market_cap = sum(DlyCap) * 1000) %>%
  
  # keep one stock per company: its largest share class on the baseline date (If a company’s Class A shares are worth $8 billion and Class B shares are worth $2 billion, we use Class A’s returns to measure volatility. The market_cap remains $10 billion, because the earlier step summed both classes)
  slice_max(DlyCap, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(PERMNO, PERMCO, market_cap)

# calculate one volatility value for each stock during 2024
volatility_data <- stocks %>%
  filter(DlyCalDt >= as.Date("2024-01-01"),
         DlyCalDt <= as.Date("2024-12-31"), !is.na(DlyRet)) %>%
  group_by(PERMNO) %>%
  summarise(days = n(), volatility = sd(DlyRet) * sqrt(252), .groups = "drop") %>%
  filter(days >= 60, !is.na(volatility), volatility > 0)

# combine size and volatility
# each row represents one company.
project_data <- inner_join(baseline, volatility_data, by = "PERMNO") %>%
  mutate(log_size = log(market_cap),
         volatility_percent = volatility * 100,
         size_group = ntile(market_cap, 3))
if (nrow(project_data) < 10) stop("Too few companies. Check dates and export coverage.")
project_data$size_group <- factor(project_data$size_group, levels = 1:3,
  labels = c("Smallest third", "Middle third", "Largest third"))

# summaries to use in the report
summary(project_data[c("market_cap", "volatility_percent")])
group_summary <- project_data %>%
  group_by(size_group) %>%
  summarise(companies = n(), mean_volatility = mean(volatility_percent),
            median_volatility = median(volatility_percent), .groups = "drop")
print(group_summary)

# graphs
# graph 1: company size distribution
graph1 <- ggplot(project_data, aes(x = market_cap)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_log10() + theme_minimal() +
  labs(title = "Distribution of Company Size",
       x = "Market capitalization in dollars (log scale)", y = "Number of companies")
print(graph1)

# graph 2: the relationship we want to study with a fitted regression line
graph2 <- ggplot(project_data, aes(x = log_size, y = volatility_percent)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") + theme_minimal() +
  labs(title = "Company Size and Stock Volatility",
       x = "ln of market capitalization", y = "Annualized volatility (%)")
print(graph2)

# graph 3: compare the volatility distributions of three sample size groups
graph3 <- ggplot(project_data, aes(x = size_group, y = volatility_percent)) +
  geom_boxplot(fill = "lightblue") + theme_minimal() +
  labs(title = "Volatility by Company Size Group", x = "Company size group",
       y = "Annualized volatility (%)")
print(graph3)

# one simple linear regression
model <- lm(volatility_percent ~ log_size, data = project_data)
summary(model)
confint(model)

# for slides and report
# Slope's P-value below 2*10^-16
# 95% interval: [-14.92, -13.59] (entirely negative)
# R^2 = .3113, log company size explains about 31.3% of variation of the 3898 companies
# Really large residuals
# Doubling company size correlates to an approximately 9.88% decrease in annualized volatility: -14.26*ln(2)

# measure doubling of company size in percentage points of volatility
doubling_difference <- coef(model)["log_size"] * log(2)
print(doubling_difference)

# the negative coefficient suggests larger companies have lower volatility
# the association does not prove causation
# these are ordinary OLS intervals

# save your graphs and results in the current folder
# these files will be created only after you supply data and run the script
ggsave("Graph1.png", graph1, width = 7, height = 5)
ggsave("Graph2.png", graph2, width = 7, height = 5)
ggsave("Graph3.png", graph3, width = 7, height = 5)
write.csv(project_data, "Cleaned_Data.csv", row.names = FALSE)
write.csv(group_summary, "Group_Summary.csv", row.names = FALSE)
capture.output(summary(model), confint(model), doubling_difference,
               file = "Regression_Results.txt")
