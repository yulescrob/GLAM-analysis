df = read.csv("MergedMidwest501c3.csv")
attach(df)
# Filter the data
cols = c("STATE","FORMATIONORM","TOTAEMPLCNTN","NTEE_CD","TOTREVCURYEA","NAME")

library(dplyr)

filtered_df <- df %>%
  filter(
    df[[cols[[1]]]] %in% c("KY", "OH", "IN","MI","WV","PA"),
    df[[cols[[2]]]] > 2009 & df[[cols[[2]]]] < 2020,
                               df[[cols[[4]]]] %in% c("P19","P60"), #"P190","P192","P193","P197",
    !df[[cols[[6]]]] %in% c("BEYOND HOMELESS INC","LORDS PANTRY AT ANNAS HOUSE INC","EMPOWERMENT PLAN","PUERTO RICO RISE UP INC","SCOUTS HONOR PET FOOD PANTRY LTD","TRIUNE HOUSING CORPORATION","LANDIS COMMUNITIES","DIOCESAN COUNCIL SOCIETY OF ST VINCENT DE PAUL CLEVELAND DIOCESE")
  )
unique_count <- length(unique(filtered_df$NAME))
print(unique_count)

filtered_df$age = filtered_df$TAXYEAR-filtered_df$FORMATIONORM

filtered_df$TOTEXPCURYEA = filtered_df$TOTEXPCURYEA/1000

filtered_df = filtered_df %>%
  filter(filtered_df$age >=1 & filtered_df$age<=10)

glam <- df %>%
  filter(
    df[[cols[[6]]]] == "GIVE LIKE A MOTHER"
  )

glam$age = glam$TAXYEAR-glam$FORMATIONORM
glam$TOTEXPCURYEA = glam$TOTEXPCURYEA/1000

# Define the new rows for ages 5-10 with custom values
new_data_employees <- data.frame(
  age = 4:10,  # Specify the age range from 5 to 10
  forecasted_employees = c(5,5,5,5,5,4,4)  # Custom values for employees
)

new_data_volunteers = data.frame(
  age = 4:10,
  forecasted_volunteers = c(180,190,191,194,200,195,194)  # Custom values for volunteers
)

new_data_expenses = data.frame(
  age=4:10,
  forecasted_expenses = c(356.913,350.000, 450.000, 470.000, 400.000, 360.000, 365.000)  # Custom values for expenses
)

# View the new data
print(new_data)



# Step 2: Create a full range of years (or ages)
age <- filtered_df %>% pull(age) %>% unique()  # Get all unique years in the dataset
age_full <- data.frame(age = sort(age))  # Create a full sequence of years

# Calculate percentiles for the entire dataset
filtered_df <- filtered_df %>%
  group_by(age) %>%
  mutate(
  expenses_percentile = percent_rank(TOTEXPCURYEA),
volunteers_percentile = percent_rank(TOTANBRVVOLU),
employees_percentile = percent_rank(TOTAEMPLCNTN)
) %>%
  ungroup()

# Match and extract the percentile values for the specific organization by year
org_percentiles <- glam %>%
  left_join(filtered_df %>%
              select(age, TOTEXPCURYEA, expenses_percentile, TOTANBRVVOLU, volunteers_percentile, TOTAEMPLCNTN, employees_percentile),
            by = c("age", "TOTEXPCURYEA", "TOTANBRVVOLU", "TOTAEMPLCNTN"))

print(org_percentiles)

# Step 3: Calculate the median and percentiles for expenses, volunteers, and employees by year
df_expenses<- filtered_df %>%
  group_by(age) %>%  # Replace 'Year' with the actual column name representing the year
  summarise(
    # Median and Percentiles for Expenses
    expenses_median = median(TOTEXPCURYEA, na.rm = TRUE),
    expenses_p10 = quantile(TOTEXPCURYEA, probs = 0.1, na.rm = TRUE),
    expenses_p90 = quantile(TOTEXPCURYEA, probs = 0.9, na.rm = TRUE),
    expenses_p25 = quantile(TOTEXPCURYEA, probs = 0.25, na.rm = TRUE),
    expenses_p75 = quantile(TOTEXPCURYEA, probs = 0.75, na.rm = TRUE),
    expenses_p83 = quantile(TOTEXPCURYEA, probs = 0.83, na.rm = TRUE),
    )

df_volunteers = filtered_df %>%
  group_by(age) %>%
  summarise(
# Median and Percentiles for Volunteers
volunteers_median = median(TOTANBRVVOLU, na.rm = TRUE),
volunteers_p25 = quantile(TOTANBRVVOLU, probs = 0.25, na.rm = TRUE),
volunteers_p75 = quantile(TOTANBRVVOLU, probs = 0.75, na.rm = TRUE),
volunteers_p10 = quantile(TOTANBRVVOLU, probs = 0.1, na.rm = TRUE),
volunteers_p90 = quantile(TOTANBRVVOLU, probs = 0.9, na.rm = TRUE),
volunteers_p78 = quantile(TOTANBRVVOLU, probs = 0.78, na.rm = TRUE)
)

df_employees = filtered_df %>%
  group_by(age) %>%
  summarise(
# Median and Percentiles for Employees
employees_median = median(TOTAEMPLCNTN, na.rm = TRUE),
employees_p10 = quantile(TOTAEMPLCNTN, probs = 0.1, na.rm = TRUE),
employees_p90 = quantile(TOTAEMPLCNTN, probs = 0.9, na.rm = TRUE),
employees_p25 = quantile(TOTAEMPLCNTN, probs = 0.25, na.rm = TRUE),
employees_p75 = quantile(TOTAEMPLCNTN, probs = 0.75, na.rm = TRUE),
employees_p93 = quantile(TOTAEMPLCNTN, probs = 0.93, na.rm = TRUE)
)


# Load necessary libraries
library(ggplot2)
library(tidyr)


# Combine actual values from the organization and percentiles
df_combinede <- glam %>%
  select(age, TOTAEMPLCNTN) %>%
  right_join(age_full, by = "age") %>%
  right_join(df_employees, by = "age") %>%  # Join the percentile data by year
  left_join(new_data_employees, by = "age")

# Reshape the combined data to long format for plotting
df_longe <- df_combinede %>%
  pivot_longer(cols = -age, names_to = "Variable", values_to = "Value")

# Create a new column to distinguish between expenses, volunteers, and employees
df_longe <- df_longe %>%
  mutate(
    Metric = case_when(
      grepl("TOTAEMPLCNTN|employees", Variable) ~ "Employees per Year",
      TRUE ~ Variable
    )
  )

# Plot the data, faceting by Metric to create separate graphs

ggplot(df_longe, aes(x = age, y = Value, color = Variable, linetype = Variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +  # Separate plots for each metric
  labs(
    x = "Age",
    y = "Number of Employees",
    color = "Variable"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("employees_p93"="darkgray","forecasted_employees"="red",
    "TOTAEMPLCNTN" = "blue","employees_p10" = "gray","employees_p90"="gray",
    "employees_p25" = "gray", "employees_median" = "gray", "employees_p75" = "gray"
  ),labels=c("TOTAEMPLCNTN"="GLAM","forecasted_employees"="Forecast","employees_median"="median","employees_p10"="10th percentile","employees_p25"="25th percentile","employees_p75"="75th percentile","employees_p90"="90th percentile","employees_p93"="93rd percentile")) +
  scale_linetype_manual(values = c("employees_p93"="dashed","forecasted_employees"="solid",
                                "TOTAEMPLCNTN" = "solid","employees_p10" = "solid","employees_p90"="solid",
                                "employees_p25" = "solid", "employees_median" = "solid", "employees_p75" = "solid"
  ),labels=c("TOTAEMPLCNTN"="GLAM","forecasted_employees"="Forecast","employees_median"="median","employees_p10"="10th percentile","employees_p25"="25th percentile","employees_p75"="75th percentile","employees_p90"="90th percentile","employees_p93"="93rd percentile")) +
  scale_x_continuous(breaks = unique(df_combinede$age))+
  geom_smooth(data = df_longe %>% filter(Variable == "employees_p93"), aes(x = age, y = Value), method = "lm", se = FALSE, color = "red", linetype = "dashed")



# Assuming df_summary contains the summarized statistics from the previous step:
# The df_summary should have columns: Year, expenses_median, expenses_p25, expenses_p75,
# volunteers_median, volunteers_p25, volunteers_p75, employees_median, employees_p25, employees_p75
# Combine actual values from the organization and percentiles
df_combinedv <- glam %>%
  select(age, TOTANBRVVOLU) %>%
  right_join(age_full, by = "age") %>%
  right_join(df_volunteers, by = "age") %>%  # Join the percentile data by year
  left_join(new_data_volunteers, by = "age")

# Reshape the combined data to long format for plotting
df_longv <- df_combinedv %>%
  pivot_longer(cols = -age, names_to = "Variable", values_to = "Value")

# Create a new column to distinguish between expenses, volunteers, and employees
df_longv <- df_longv %>%
  mutate(
    Metric = case_when(
      grepl("TOTANBRVVOLU|volunteers", Variable) ~ "Volunteers per Year",
      TRUE ~ Variable
    )
  )

# Plot the data, faceting by Metric to create separate graphs
ggplot(df_longv, aes(x = age, y = Value, color = Variable, linetype = Variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +  # Separate plots for each metric
  labs(
    x = "Age",
    y = "Number of Volunteers",
    color = "Variable"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("volunteers_p78" = "gray53","forecasted_volunteers"="red",
    "TOTANBRVVOLU" = "blue","volunteers_p10" = "gray","volunteers_p90"="gray",
    "volunteers_p25" = "gray", "volunteers_median" = "gray", "volunteers_p75" = "gray"
  ),labels=c("TOTANBRVVOLU"="GLAM","volunteers_p10"="10th percentile","volunteers_p78"="78th percentile","volunteers_p90"="90th percentile","volunteers_p25"="25th percentile","volunteers_median"="median","volunteers_p75"="75th percentile","forecasted_volunteers"="Forecast")) +
  scale_linetype_manual(values=c("forecasted_volunteers"="solid","volunteers_p78"="dashed","TOTANBRVVOLU" = "solid","volunteers_p10" = "solid","volunteers_p90"="solid",
                                  "volunteers_p25" = "solid", "volunteers_median" = "solid", "volunteers_p75" = "solid"
                                  ),labels=c("volunteers_p10"="10th percentile","TOTANBRVVOLU"="GLAM","volunteers_p78"="78th percentile","volunteers_p90"="90th percentile","volunteers_p25"="25th percentile","volunteers_median"="median","volunteers_p75"="75th percentile","forecasted_volunteers"="Forecast"))+
  scale_x_continuous(breaks = unique(df_combinedv$age))+
  geom_smooth(data = df_longv %>% filter(Variable == "volunteers_p78"), aes(x = age, y = Value), method = "lm", se = FALSE, color = "red", linetype = "dashed")

# Combine actual values from the organization and percentiles
df_combined <- glam %>%
  select(age, TOTEXPCURYEA) %>%
  right_join(age_full, by = "age") %>%
  right_join(df_expenses, by = "age")%>%# Join the percentile data by year
  left_join(new_data_expenses, by = "age")

# Reshape the combined data to long format for plotting
df_long <- df_combined %>%
  pivot_longer(cols = -age, names_to = "Variable", values_to = "Value")

# Create a new column to distinguish between expenses, volunteers, and employees
df_long <- df_long %>%
  mutate(
    Metric = case_when(
      grepl("TOTEXPCURYEA|expenses", Variable) ~ "Expenses per Year",
      TRUE ~ Variable
    )
  )

# Plot the data, faceting by Metric to create separate graphs
ggplot(df_long, aes(x = age, y = Value, color = Variable, linetype = Variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +  # Separate plots for each metric
  labs(
    x = "Age",
    y = "Thousands of Dollars",
    color = "Variable"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("expenses_p83"="gray53","forecasted_expenses"="red",
    "TOTEXPCURYEA" = "blue","expenses_p10" = "gray","expenses_p90"="gray",
    "expenses_p25" = "gray", "expenses_median" = "gray", "expenses_p75" = "gray"
  ),labels = c("expenses_p83"="83rd percentile","expenses_p10"="10th percentile","expenses_p25"="25th percentile","expenses_median"="median","expenses_p75"="75th percentile","expenses_p90"="90th percentile","forecasted_expenses"="Forecast","TOTEXPCURYEA"="GLAM")) +
  scale_linetype_manual(values = c("expenses_p83"="dashed","forecasted_expenses"="solid",
                                "TOTEXPCURYEA" = "solid","expenses_p10" = "solid","expenses_p90"="solid",
                                "expenses_p25" = "solid", "expenses_median" = "solid", "expenses_p75" = "solid"
  ),labels = c("expenses_p83"="83rd percentile","expenses_p10"="10th percentile","expenses_p25"="25th percentile","expenses_median"="median","expenses_p75"="75th percentile","expenses_p90"="90th percentile","forecasted_expenses"="Forecast","TOTEXPCURYEA"="GLAM")) +
  scale_x_continuous(breaks = unique(df_combined$age))+
  scale_y_continuous(breaks = seq(0,600,by=100)) +
  geom_smooth(data = df_long %>% filter(Variable == "expenses_p83"), aes(x = age, y = Value), method = "lm", se = FALSE, color = "red", linetype = "dashed")


# Save the filtered data to a new CSV
write.csv(filtered_df, "/Users/yuliza/Library/CloudStorage/OneDrive-UniversityofCincinnati/filtered_data.csv", row.names = FALSE)
