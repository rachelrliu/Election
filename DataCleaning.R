library(dplyr)
library(reshape2)

setwd('/Users/ruiqiliu/Desktop/MSBA/Others/USElection')

###Read in per capita personal income data, the data can be downloaded from FRED at:
###https://fred.stlouisfed.org/tags/series?t=annual%3Bper+capita%3Bpersonal+income%3Bstate&ob=pv&od=desc
income_raw <- read.csv('pcpi.csv',header = TRUE)

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
             personal_income= value.x ,
             us = state_abb.y,
             national_personal_income = value.y) %>%
      mutate(relative_personal_income = personal_income/national_personal_income) %>%
      select(state_abb,year,personal_income,national_personal_income,relative_personal_income)



###Read in election data, the data can be found at official Federal Election Commission report
election_raw <- read.csv('election.csv',header = TRUE,stringsAsFactors = FALSE)

#calculate margin and prepare the df ready to be merged with relative_income
election <- election_raw %>%
      mutate(margin_percent = Democratic_percent - Republican_percent,
             margin_votes = abs(Republican_votes-Democratic_votes),
             win = ifelse(margin_percent >0,'Democratic','Republican')) %>%
      rename(state_abb = State)



###JOIN election and relative income
output <- left_join(election,relative_income,by = c('state_abb','year'))
write.csv(output,'plot_df.csv',row.names = FALSE)

