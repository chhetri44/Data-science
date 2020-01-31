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
    geom_boxplot(aes(x = salesThisMon, y = preferredBrand))
