#Create Country-Year Variable

#load tidayverse to make it work
library(tidyverse)

#first line is the name of the dataframe followed by %>%

VDem %>%
  # This mutate command below is optional.
  # Basically: the data extend to 2016. If you want to extend it to the most recent year, change it.
  # If you don't need it (i.e. most CoW conflict data end in 2010), leave it alone or comment it out.
  #mutate(endyear = ifelse(endyear == 2016, 2018, endyear)) %>%
  # Prepare the pipe to think rowwise. If you don't, the next mutate command will fail.
  rowwise() %>%
  # Create a list in a tibble that we're going to expand soon.
  mutate(year = list(seq(1789, 2018))) %>%
  # Unnest the list, which will expand the data.
  unnest() %>%
  # Arrange by ccode, year, just to be sure.
  arrange(ccode, year) %>%
  # Select just the ccode, year
  select(ccode, year) %>%
  # Make sure there are no duplicate country-year observations.
  # There shouldnt' be. But be sure.
  # Finally: assign to object.
  distinct(ccode, year) -> cyear