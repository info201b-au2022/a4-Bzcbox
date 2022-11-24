library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load dataset
data <- get_data()

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Summary:
# 1. What is the proportion of the black people in jail across all states in 2018?
# 2. What is the proportion of the white people in jail across all states in 2018?
# 3. What is the proportion of the Asian American / Pacific Islander 
#    in jail across all states in 2018?
#----------------------------------------------------------------------------#
# 1. Black people in jail proportion
black_total_pop <- data %>%
  filter(year == 2018) %>%
  select(black_pop_15to64) %>%
  sum(na.rm=TRUE)

black_jail_total_pop <- data %>%
  filter(year == 2018) %>%
  select(black_jail_pop) %>%
  sum(na.rm=TRUE)

black_proportion <- paste(100 * black_jail_total_pop / black_total_pop, "%")

# 2. White people in jail proportion
white_total_pop <- data %>%
  filter(year == 2018) %>%
  select(white_pop_15to64) %>%
  sum(na.rm=TRUE)

white_jail_total_pop <- data %>%
  filter(year == 2018) %>%
  select(white_jail_pop) %>%
  sum(na.rm=TRUE)

white_proportion <- paste(100 * white_jail_total_pop / white_total_pop, "%")

# 3. Asian American / Pacific Islander people in jail proportion
aapi_total_pop <- data %>%
  filter(year == 2018) %>%
  select(aapi_pop_15to64) %>%
  sum(na.rm=TRUE)

aapi_jail_total_pop <- data %>%
  filter(year == 2018) %>%
  select(aapi_jail_pop) %>%
  sum(na.rm=TRUE)

aapi_proportion <- paste(100 * aapi_jail_total_pop / aapi_total_pop, "%")

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function should return a data frame that is suitable for visualization.
# This function takes no parameters.
get_year_jail_pop <- function() {
  year_jail_pop <- data %>%
    group_by(year) %>%
    summarize(jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(year_jail_pop)   
}

# This function return a chart.
# This function takes no parameters and uses `get_year_jail_pop()` function.
plot_jail_pop_for_us <- function()  {
  chart <- ggplot(data = get_year_jail_pop(), aes(x = year, y= jail_pop)) +
    geom_col() +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018)."
    ) +
    scale_y_continuous(labels = scales::comma)
  return(chart)
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#
# This function should return a data frame that is suitable for visualization.
# The parameter `states` should be a vector of states.
get_jail_pop_by_states <- function(states) {
  df <- data %>%
    group_by(state, year) %>%
    filter(state %in% states) %>%
    summarize(jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(df)
}

# This function should return a chart. The parameter `states` should be a
# vector of states. This function calls `get_jail_pop_by_states()` function
plot_jail_pop_by_states <- function(states) {
  chart <- ggplot(data = get_jail_pop_by_states(states),
                  aes(x = year, y = jail_pop, color = state)) +
    geom_line() +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Growth of Jail Population in U.S. by State (1970-2018)",
      caption = "Figure 2. Growth of Jail Population in U.S. by State (1970-2018)."
    ) +
    scale_y_continuous(labels = scales::comma)
  return(chart)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Are some races disproportionally represented in jail
# in different regions?
#----------------------------------------------------------------------------#
# This function should return a data frame that is suitable for visualization.
# This function takes no parameters.
get_jail_prop_by_region_and_race <- function() {
  df <- data %>%
    group_by(region) %>%
    filter(year == 2018) %>%
    summarize(
      Asian = sum(aapi_jail_pop, na.rm = TRUE) / sum(aapi_pop_15to64, na.rm = TRUE),
      Black = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE),
      Latinx = sum(latinx_jail_pop, na.rm = TRUE) / sum(latinx_pop_15to64, na.rm = TRUE),
      Native = sum(native_jail_pop, na.rm = TRUE) / sum(native_pop_15to64, na.rm = TRUE),
      White = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64, na.rm = TRUE)
      )
  df <- df %>% pivot_longer(!region, names_to = "race", values_to = "percentage")
  return(df)
}

# This function should return a chart.
# This function calls `get_jail_pop_by_region_and_race()` function
plot_jail_prop_by_region_and_race <- function() {
  chart <- ggplot(data = get_jail_prop_by_region_and_race(),
                  aes(x = region, y = percentage, fill = race)) +
    geom_bar(position="dodge", stat = "identity") +
    labs(
      x = "Region",
      y = "Percentage",
      title = "Percentage of Jail Population by Race in Different Region in 2018",
      caption = "Figure 3. Percentage of Jail Population by Race in Different Region in 2018"
    )
  return(chart)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# A map that shows how black people population in jail distributed in the U.S.
#----------------------------------------------------------------------------#
# This function should return a data frame that is suitable for visualization.
# This function takes no parameters.
get_black_jail_pop_2018 <- function() {
  df <- data %>%
    group_by(state) %>%
    filter(year == 2018) %>%
    summarize(black_jail_pop = round(sum(black_jail_pop, na.rm = TRUE))) %>%
    rename(abbr = state)
  df <- merge(df, statepop, by = "abbr")
  return(df)
}


# This function should return a map.
# This function calls `get_black_jail_pop_2018()` function
map_black_jail_pop_2018 <- function() {
  map <- plot_usmap(data = get_black_jail_pop_2018(), values = "black_jail_pop", color = "blue") + 
    scale_fill_continuous(
      low = "white",
      high = "blue",
      name = "Population in Jail (2018)",
      label = scales::comma
    ) + theme(legend.position = "right") +
    labs(
      title = "Black People Population in Jail in U.S. (2018)",
      caption = "Figure 4. Black People Population in Jail in U.S. (2018)"
    )
  return(map)
}