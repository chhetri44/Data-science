## GAPMinder

# Load the gapminder package
library(gapminder)

# Load the dplyr package
library(dplyr)

# Load the ggplot2 package
library(ggplot2)


# Look at the gapminder dataset
gapminder

# Filter the gapminder dataset for the year 1957
gapminder %>%
filter(year == '1957')

# Filter for China in 2002
gapminder %>%
filter(country == "China", year == "2002")

# Sort in ascending order of lifeExp
gapminder %>%
arrange (lifeExp)

  
# Sort in descending order of lifeExp
gapminder %>%
arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
gapminder %>%
filter(year == 1957) %>%
arrange(desc(pop))

# Use mutate to change lifeExp to be in months
gapminder %>%
mutate(lifeExp = lifeExp*12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>%
mutate(lifeExpMonths = lifeExp*12)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>%
filter(year == 2007) %>%
mutate(lifeExpMonths = 12 * lifeExp) %>%
arrange(desc(lifeExpMonths))

# Create gapminder_1952
gapminder_1952 <- gapminder %>%
filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes (x = pop, y = lifeExp)) +
geom_point()

# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()+
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent)) +
geom_point() +
scale_x_log10()

# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
geom_point() +
scale_x_log10() +
facet_wrap(~year)
