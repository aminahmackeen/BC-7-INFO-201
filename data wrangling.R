library(dplyr)
library(stringr)
library(lubridate)

df <- read.csv("WHO-COVID-19-global-data.csv")
fdf <- read.csv("Financials_Full_Data_data.csv")

covid_df <- df[df$Country == "United States of America", ]

# Extracting the year from covid dataset
covid_df$year <- year(ymd(covid_df$Date_reported))

grp_cvd <- group_by(covid_df, year, New_cases)

ccovid_df <- summarize(grp_cvd, year, cum_case = sum(New_cases))

clean_covid_df <- summarize(clean_covid_df, year, cum_case_yr = max(cum_case))

# Extracting only years 2020-2022 in the travel finance dataset
finance_df <- fdf[fdf$Year >= 2020, ]

# Joining datasets
travel_df <- merge(x = clean_covid_df, y = finance_df,
                   by.x = "year", by.y = "Year", 
                   all.x = TRUE)
