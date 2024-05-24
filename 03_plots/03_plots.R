
# Plotting life expectancy over time:
ggplot(dat_lifeexp, aes(x = year, y = life_expectancy)) + 
  geom_smooth(aes(group = country), size = 0.1, se = FALSE, color = "black") + 
  geom_smooth(size = 2) +
  labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy")


# Life Expectancy Distribution in 2019
life_exp_2019 <- dat_lifeexp %>% filter(year == 2019)
median_life_exp <- median(life_exp_2019$life_expectancy, na.rm = TRUE)

ggplot(life_exp_2019, aes(x = life_expectancy)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = median_life_exp), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = median_life_exp + 2, y = max(table(life_exp_2019$life_expectancy)), 
           label = paste("Median:", round(median_life_exp, 2)), color = "red") +
  labs(title = "Distribution of Life Expectancy in 2019", x = "Life Expectancy", y = "Frequency") +
  theme_minimal()


# Top and Bottom 5 Countries by Life Expectancy in 2015
top5 <- dat_lifeexp %>% filter(year == 2019) %>% top_n(5, life_expectancy)
bottom5 <- dat_lifeexp %>% filter(year == 2019) %>% top_n(-5, life_expectancy)
top_bottom <- bind_rows(top5, bottom5)

ggplot(top_bottom, aes(x = reorder(country_name, life_expectancy), y = life_expectancy, fill = country_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top and Bottom 5 Countries by Life Expectancy in 2015", x = "Country", y = "Life Expectancy") +
  theme_minimal() +
  theme(legend.position = "none")


# Change in Life Expectancy Over Time for Selected Countries
selected_countries <- c("Italy", "Japan", "United States", "India", "Brazil")

ggplot(dat_lifeexp %>% filter(country_name %in% selected_countries), aes(x = year, y = life_expectancy, color = country_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Change in Life Expectancy Over Time", x = "Year", y = "Life Expectancy", color = "Country") +
  theme_minimal()


