
# Plotting life expectancy over time:
ggplot(dat_lifeexp, aes(x = year, y = life_expectancy)) + 
  geom_smooth(aes(group = country), size = 0.1, se = FALSE, color = "black") + 
  geom_smooth(size = 2) +
  labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy")


