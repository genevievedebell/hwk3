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

##