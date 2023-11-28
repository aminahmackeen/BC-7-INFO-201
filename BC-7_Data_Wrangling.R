library(dplyr)
library(stringr)
library(lubridate) # Extracting year from date

df <- read.csv("WHO-COVID-19-global-data.csv")
fdf <- read.csv("Financials_Full_Data_data.csv")

covid_df <- df[df$Country == "United States of America", ]

# Extracting the year from covid dataset
covid_df$year <- year(ymd(covid_df$Date_reported))

# Joining data sets
join_df <- merge(x = covid_df, y = fdf,
                 by.x = "year", by.y = "Year", 
                 all.x = TRUE)

join_df <- arrange(join_df, year, Date_reported, Cumulative_cases)

# Creating one new categorical variable

join_df$new_case_or_no <- NA
y_n <- join_df$new_case_or_no

for (num in 1:nrow(join_df)) {
  
  row_df <- join_df[num, ]
  num_case <- row_df$New_cases
  
  if (num_case == '0') {
    y_n[num] = "No"
  } else {
    y_n[num] = "Yes"
  }
}

join_df$new_case_or_no = y_n

# Creating one new continuous/numerical variable

# get avg cases per day
join_df$avg_case = NA
avg = join_df$avg_case

total_case = 0
for (num in 1:nrow(join_df)) {
  
  row_df <- join_df[num, ]
  num_case <- row_df$New_cases
  
  total_case <- total_case + num_case
  avg[num] <- total_case / num
}

join_df$avg_case = avg


# Creating one summarization data frame

cumul_df <- group_by(covid_df, year, New_cases)

covid_cumul_df <- summarize(cumul_df, year, cum_case = sum(New_cases))

covid_cumul_df <- distinct(covid_cumul_df)

# Exporting to CSV
write.csv(join_df, "C:/Users/mai/Downloads/Covid_Travel_Dataset.csv")
