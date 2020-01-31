#readr, dplyr, corrplot, and ggplot2 libraries

# Structure of dataset
str(salesData, give.attr = FALSE)

# Visualization of correlations
salesData %>% select_if(is.numeric) %>%
  select(-id) %>%
  cor() %>%
  corrplot()

# Frequent stores
ggplot(salesData) +
    geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))

# Preferred brand
ggplot(salesData) +
    geom_boxplot(aes(x = preferredBrand, y = salesThisMon))

#We saw that the sales in the last three months are strongly positively correlated with the sales in this month. Hence we will start off including that as an explanatory variable in a linear regression.
