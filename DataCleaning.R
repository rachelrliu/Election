library(dplyr)
library(reshape2)

setwd('/Users/ruiqiliu/Desktop/MSBA/Others/USElection')

#Read in household income data, the data can be downloaded at US Census Bureau:
#https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html
income_raw <- read.csv('income_raw.csv',header = TRUE)

#transform panel data to long format
income_raw_long <- reshape2::melt(income_raw,id.vars = 'State')

#form income dataframe
income <- income_raw_long %>% 
      mutate( year = substr(variable,2,5),
              state_abb = ifelse(!is.na(state.abb[match(State,state.name)]),
                                        state.abb[match(State,state.name)],
                                        as.character(State)),
              year = as.numeric(year)) %>%
      select(state_abb,year,value) 
                        
      
#calculate relative income per state
us <- income %>% filter(state_abb == 'United States')
relative_income <- left_join(income,us,by = 'year') %>% 
      dplyr::rename(state_abb = state_abb.x,
             household_income= value.x ,
             us = state_abb.y,
             national_income = value.y) %>%
      mutate(relative_income = household_income/national_income) %>%
      select(state_abb,year,household_income,national_income,relative_income)



#Read in election data, the data can be found at official Federal Election Commission report
election_raw <- read.csv('election_raw.csv',header = TRUE,stringsAsFactors = FALSE)

#calculate margin and prepare the df ready to be merged with relative_income
election <- election_raw %>%
      mutate(margin = Republican_percent - Democratic_percent,
             number_margin = Republican_number-Democratic_number) %>%
      rename(state_abb = State)



#JOIN election and relative income
output <- left_join(election,relative_income,by = c('state_abb','year' = ))
write.csv(output,'plot_df.csv',row.names = FALSE)

