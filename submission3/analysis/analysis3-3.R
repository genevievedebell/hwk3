# 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.

## Identify years with tax changes 
tax_changes <- final.data %>%
  filter(Year >= 1970, Year <= 1985) %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = tax_state != lag(tax_state, default = first(tax_state))) %>%
  ungroup()

## calculate the proportion of states that had a tax change in each year
change_proportions <- tax_changes %>%
  group_by(Year) %>%
  summarize(proportion = mean(tax_change, na.rm = TRUE))

## plot the results
library(ggplot2)

ggplot(change_proportions, aes(x = Year, y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Proportion of States with Cigarette Tax Changes (1970-1985)",
       x = "Year",
       y = "Proportion of States") +
  theme_minimal()

# 2. Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.

## Get the CPI value for 2012
cpi_2012 <- cpi.data %>% filter(Year == 2012) %>% pull(index)

## Convert tax and price to 2012 dollars
final.data <- final.data %>%
  mutate(price_real = cost_per_pack * (cpi_2012/index),
         tax_real = tax_dollar * (cpi_2012/index))

## Filter for years 1970 to 2018
plot.data <- final.data %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_real, na.rm = TRUE),
            avg_price = mean(price_real, na.rm = TRUE))

## Plot the data
library(ggplot2)

ggplot(plot.data, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax (2012$)"), size = 1.2) +
  geom_line(aes(y = avg_price, color = "Average Price (2012$)"), size = 1.2) +
  labs(title = "Average Tax and Price of Cigarettes (1970-2018, Adjusted to 2012$)",
       x = "Year",
       y = "Dollars (2012 Adjusted)",
       color = "Legend") +
  theme_minimal()

# 3. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

## Ensure 'Year' is an integer
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

## Calculate price difference (2012 dollars)
price_diff <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  select(state, Year, price_real) %>%
  pivot_wider(names_from = Year, values_from = price_real, names_prefix = "Year_") %>%
  mutate(price_increase = Year_2018 - Year_1970) %>%
  arrange(desc(price_increase))

## Identify the top 5 states with the highest price increase
top_5_states <- price_diff %>%
  slice_max(order_by = price_increase, n = 5) %>%
  pull(state)

## Filter data for these 5 states and sales per capita
sales_data <- final.data %>%
  filter(state %in% top_5_states, Year >= 1970 & Year <= 2018) %>%
  select(state, Year, sales_per_capita)

## Plot the sales per capita for these states
ggplot(sales_data, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1.2) +
  labs(title = "Average Packs Sold Per Capita (1970-2018)",
       subtitle = "For the 5 States with the Highest Increase in Cigarette Prices",
       x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

# 4. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

## Ensure 'Year' is an integer
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

## Calculate price difference (2012 dollars)
price_diff <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  select(state, Year, price_real) %>%
  pivot_wider(names_from = Year, values_from = price_real, names_prefix = "Year_") %>%
  mutate(price_increase = Year_2018 - Year_1970) %>%
  arrange(price_increase)  # Sort in ascending order

## Identify the 5 states with the lowest price increase
bottom_5_states <- price_diff %>%
  slice_min(order_by = price_increase, n = 5) %>%
  pull(state)

## Filter data for these 5 states and sales per capita
sales_data <- final.data %>%
  filter(state %in% bottom_5_states, Year >= 1970 & Year <= 2018) %>%
  select(state, Year, sales_per_capita)

## Plot the sales per capita for these states
ggplot(sales_data, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1.2) +
  labs(title = "Average Packs Sold Per Capita (1970-2018)",
       subtitle = "For the 5 States with the Lowest Increase in Cigarette Prices",
       x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()



# 5. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.

## Filter sales data for the top 5 and bottom 5 states
sales_data_top <- final.data %>%
  filter(state %in% top_5_states, Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarize(avg_sales_top = mean(sales_per_capita, na.rm = TRUE))

sales_data_bottom <- final.data %>%
  filter(state %in% bottom_5_states, Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarize(avg_sales_bottom = mean(sales_per_capita, na.rm = TRUE))

## Merge datasets for plotting
sales_comparison <- left_join(sales_data_top, sales_data_bottom, by = "Year")

## Plot the trends for both groups
ggplot(sales_comparison, aes(x = Year)) +
  geom_line(aes(y = avg_sales_top, color = "Top 5 States (Highest Price Increase)"), size = 1.2) +
  geom_line(aes(y = avg_sales_bottom, color = "Bottom 5 States (Lowest Price Increase)"), size = 1.2) +
  labs(title = "Comparison of Cigarette Sales in States with High vs. Low Price Increases",
       subtitle = "Average Packs Sold Per Capita (1970-2018)",
       x = "Year",
       y = "Packs Sold Per Capita",
       color = "State Group") +
  theme_minimal()

# 6. Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the price elasticity of demand over that period. Interpret your results.

## Filter data for 1970-1990 and drop missing values
demand_data <- final.data %>%
  filter(Year >= 1970 & Year <= 1990) %>%
  select(sales_per_capita, price_real) %>%
  drop_na()

## Take natural logs
demand_data <- demand_data %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_real))

## Run the regression
demand_model <- lm(log_sales ~ log_price, data = demand_data)

## Print the summary of the regression
summary(demand_model)

## Interpretation: Based on this regression, we conclude that the demand for cigarettes is inelastic. A 1% increase in price would result in a 0.81% decrease in sales.

# 7. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

library(fixest)

## Create log-transformed variables
iv.data <- final.data %>%
filter(Year >= 1970 & Year <= 1990)%>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price = log(price_cpi),
         ln_total_tax = log(tax_real))

## Run the IV regression using feols() from fixest
ivs <- feols(ln_sales ~ 1 | ln_price ~ ln_total_tax, data = iv.data)

## Display results
summary(ivs)

# 8. Show the first stage and reduced-form results from the instrument.

library(fixest)

## First-Stage Regression: Predicting ln_price using ln_total_tax
first_stage <- feols(ln_price ~ ln_total_tax, data = iv.data)
summary(first_stage)

## Reduced-Form Regression: Direct effect of ln_total_tax on ln_sales
reduced_form <- feols(ln_sales ~ ln_total_tax, data = iv.data)
summary(reduced_form)

# 9. Repeat questions 1-3 focusing on the period from 1991 to 2015.

## Filter data for 1991-2015 and drop missing values
demand_data <- final.data %>%
  filter(Year >= 1991 & Year <= 2015) %>%
  select(sales_per_capita, price_real) %>%
  drop_na()

## Take natural logs
demand_data <- demand_data %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_real))

## Run the regression
demand_model2 <- lm(log_sales ~ log_price, data = demand_data)

## Print the summary of the regression
summary(demand_model)

## Create log-transformed variables
iv.data2 <- final.data %>%
filter(Year >= 1991 & Year <= 2015)%>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price = log(price_cpi),
         ln_total_tax = log(tax_real))

## Run the IV regression using feols() from fixest
ivs2 <- feols(ln_sales ~ 1 | ln_price ~ ln_total_tax, data = iv.data2)

## Display results
summary(ivs)

## First-Stage Regression: Predicting ln_price using ln_total_tax
first_stage2 <- feols(ln_price ~ ln_total_tax, data = iv.data2)
summary(first_stage)

## Reduced-Form Regression: Direct effect of ln_total_tax on ln_sales
reduced_form2 <- feols(ln_sales ~ ln_total_tax, data = iv.data2)
summary(reduced_form)

# Load necessary libraries
library(tibble)
library(kableExtra)

# Create a table calling each regression
results_table <- tibble(
  Metric = c("Estimate", "Reduced Form", "First Stage", "N", "R-squared"),
  
  `1970-1990_OLS` = c(
    coef(demand_model)["log_price"], 
    coef(reduced_form)["ln_total_tax"], 
    coef(first_stage)["ln_total_tax"], 
    nobs(demand_model), 
    summary(demand_model)$r.squared
  ),
  
  `1970-1990_IV` = c(
    coef(ivs)["fit_ln_price"], 
    NA, 
    NA, 
    nobs(ivs), 
    NA  # IV regressions typically don't have R-squared values
  ),
  
  `1991-2015_OLS` = c(
    coef(demand_model2)["log_price"], 
    coef(reduced_form2)["ln_total_tax"], 
    coef(first_stage2)["ln_total_tax"], 
    nobs(demand_model2), 
    summary(demand_model2)$r.squared
  ),
  
  `1991-2015_IV` = c(
    coef(ivs2)["fit_ln_price"], 
    NA, 
    NA, 
    nobs(ivs2), 
    NA  # IV regression does not return R-squared
  )
)

# Format and group columns
kable(results_table, format = "html", caption = "Regression Estimates (OLS & IV)") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 1, "1970-1990" = 2, "1991-2015" = 2)) %>%
  column_spec(2:5, width = "10em")
  
save.image("submission2/Hwk3_workspace.Rdata")

dir.exists("C:/Users/genevievedebell/Documents/GitHub/hwk3/submission_2")
getwd()
