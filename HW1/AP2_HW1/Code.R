library(tidyverse)
library(readxl)
apdata <- read_excel("~/Desktop/Research/HarrisAP/HW1/pset1_q2_2024-3 (1).xlsx")

####### (a) #####

# Identifying countries with any missing value in gdp_pc_growth
countries_with_missing_gdp <- apdata %>%
  group_by(countryname) %>%
  filter(any(is.na(gdp_pc_growth))) %>%
  distinct(countryname)

# Excluding these countries from the analysis
data_filtered <- apdata %>%
  filter(!countryname %in% countries_with_missing_gdp$countryname)


q1_data <- data_filtered %>% 
  group_by(countryname) %>% 
  summarise(average_gdp_growth = mean(gdp_pc_growth), 
            average_autocracy = mean(autocracy_fh))

top_20_countries <- q1_data %>% 
  top_n(20, average_gdp_growth)

top_20_countries$democracy_level <- with(top_20_countries, ifelse(average_autocracy < 0.33, "High", 
                                                                  ifelse(average_autocracy < 0.67, "Medium", "Low")))

# Plotting the bar graph
ggplot(top_20_countries, aes(x=reorder(countryname, average_gdp_growth), y=average_gdp_growth, fill=democracy_level)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values=c("High"="green", "Medium"="orange", "Low"="red")) +
  labs(title="Top 20 Countries by Average GDP Growth Rate",
       x="Country", y="Average GDP Growth Rate") +
  theme_minimal()

## Answer: Most countries with high GDP growth (equal to and more than 5%) are non-democracy

####### (b) #####
bottom_20_countries <- q1_data %>% 
  top_n(-20, average_gdp_growth)

bottom_20_countries$democracy_level <- with(bottom_20_countries, ifelse(average_autocracy < 0.33, "High", 
                                                                  ifelse(average_autocracy < 0.67, "Medium", "Low")))

ggplot(bottom_20_countries, aes(x=reorder(countryname, average_gdp_growth), y=average_gdp_growth, fill=democracy_level)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values=c("High"="green", "Medium"="orange", "Low"="red")) +
  labs(title="Bottom 20 Countries by Average GDP Growth Rate and Democracy Levels",
       x="Country", y="Average GDP Growth Rate") +
  theme_minimal()

# Answer: The GDP growth are around 0 or even negative. Most of low GDP growth countries are also non-democracy.
####### (c) #####

# Filter out the countries with missing values of either GDP or Autocracy score
countries_with_missing_data <- apdata %>%
  group_by(countryname) %>%
  filter(any(is.na(gdp_pc_growth) | is.na(autocracy_fh))) %>%
  distinct(countryname)

# Excluding these countries from the analysis
data_filtered <- apdata %>%
  filter(!countryname %in% countries_with_missing_data$countryname)


q3_data <- data_filtered %>% 
  group_by(countryname) %>% 
  summarise(average_gdp_growth = mean(gdp_pc_growth), 
            average_autocracy = mean(autocracy_fh))

ggplot(q3_data, aes(x = average_autocracy, y = average_gdp_growth)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Adding a linear model line
  labs(title = "Scatter Plot of Average GDP Growth vs Average Autocracy Score",
       x = "Average Autocracy Score",
       y = "Average GDP Growth Rate") +
  theme_minimal()

# Answer: There is marginal positive correlation between GDP growth vs. autocracy score.

# ####### (D) #####

model <- lm(average_gdp_growth ~ average_autocracy, data = q3_data)
summary(model)

# Answer 1: At 0.05 significance level, one unit increase of average autocracy score is related to 1.0236 increase of GDP growth rate, which is significant in terms of the GDP growth.

####################
top2_excluded <- q3_data %>% 
  arrange(desc(average_gdp_growth)) %>% 
  slice(-1:-2)

model_top2_excluded <- lm(average_gdp_growth ~ average_autocracy, data = top2_excluded)
summary(model_top2_excluded)

# Answer 2: Although the magnitude of coefficient is 0.6698, it is not significant at 0.05 level.

#################
top5_excluded <- q3_data %>% 
  arrange(desc(average_gdp_growth)) %>% 
  slice(-1:-5)

model_top5_excluded <- lm(average_gdp_growth ~ average_autocracy, data = top5_excluded)
summary(model_top5_excluded)

# Answer 3: Although the magnitude of coefficient is 0.3347, it is not significant at 0.05 level.

# Conclusion: There is no clear relationship between GDP growth vs Autocracy score.

####### (e) #####

# Answer: We agreed with this point. Whether the authoritarian regime block or allow for the economic reform,
# depends on the the stability of its control of the power. There are some stable authoritarian regime who launch economic reform to make the pie bigger versus
# other unstable regimes afraid of losing powers after the economic boom. Therefore, we see the empirical evidence that both scenario exist so that there is no 
# monotonous relationship between autocracy score and GDP growth.




#######(f)#####


reverse_filtered <- apdata %>%
  filter(countryname %in% countries_with_missing_gdp$countryname)

reverse_filtered <- reverse_filtered %>% 
  group_by(countryname) %>% 
  summarise(average_gdp_growth = mean(gdp_pc_growth, na.rm = TRUE), 
            average_autocracy = mean(autocracy_fh, na.rm = TRUE))

reverse_filtered$democracy_level <- with(reverse_filtered, ifelse(average_autocracy < 0.33, "High", 
                                                                  ifelse(average_autocracy < 0.67, "Medium", "Low")))

reverse_filtered %>% select(countryname, average_gdp_growth, democracy_level) %>% 
  group_by(democracy_level) %>% 
  summarise(average_gdp = mean(average_gdp_growth),
            count = n()) %>% 
  print(.)

# Answer:
# 5 countries fall into high democracy with 2.77 average GDP growth;
# 4 countries fall into high democracy with 2.04 average GDP growth;
# 7 countries fall into high democracy with -0.112 average GDP growth.
# High democracy and medium democracy missing data have much higher GDP growth than low democracy. There are 3 scenarios to interpreate this pattern:
# The high democracy missed some data because they are basically tiny countries with specific high-end industry as the driving engine for GDP
# Like Liechtenstein's pillar industry is finance while Nauru and Palau are depending on tourism and off-shore transactions. They have tiny bureaus to report the data somehow but enjoyed economic well-being
# The medium democracy countries missed some data because they are less established regime and somewhat between liberal democracy and authoritarianism, like Venezuela and Timor-Leste.
# The low democracy countries missed some data because they are basically fail states or not institutionalized due to war (Afghanistan, 	
# Somalia, South Sudan and Yemen) and dictatorship (like Djibouti and Eritrea)