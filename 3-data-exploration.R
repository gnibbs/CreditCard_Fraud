###################################################################################################
################################### Data Exploration ##############################################

# Summary statistics. ------------------------------------------------------------------------------
summary_statistics <- summary(credit_fraud.df)
print(summary_statistics)


# Top customers and terminals with fraudulent transactions. ----------------------------------------
customer_fraud_count <- credit_fraud.df %>%
  group_by(CUSTOMER_ID) %>%
  summarise(Fraud_Count = sum(TRANS_FRAUD)) %>%
  arrange(desc(Fraud_Count))

terminal_fraud_count <- credit_fraud.df %>%
  group_by(TERMINAL_ID) %>%
  summarise(Fraud_Count = sum(TRANS_FRAUD)) %>%
  arrange(desc(Fraud_Count))


# Plotting top customers and terminals by fraud count.
ggplot(head(customer_fraud_count, 10), aes(x = reorder(CUSTOMER_ID, Fraud_Count), y = Fraud_Count)) +
  geom_col() +
  labs(x = "Customer ID", y = "Number of Frauds", title = "Top Customers by Fraud Count") +
  theme_minimal()

ggplot(head(terminal_fraud_count, 10), aes(x = reorder(TERMINAL_ID, Fraud_Count), y = Fraud_Count)) +
  geom_col() +
  labs(x = "Terminal ID", y = "Number of Frauds", title = "Top Terminals by Fraud Count") +
  theme_minimal()

rm(customer_fraud_count)
rm(terminal_fraud_count)


# Fraudulent transactions by hour. -----------------------------------------------------------------
fraud_transactions_by_hour <- credit_fraud.df %>%
  filter(TRANS_FRAUD == 1) %>%
  group_by(TRANS_HOUR_OF_DAY) %>%
  summarise(Count = n()) %>%
  ungroup()

ggplot(fraud_transactions_by_hour, aes(x = TRANS_HOUR_OF_DAY, y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Number of Fraudulent Transactions by Hour",
       x = "Hour of Day",
       y = "Count of Fraudulent Transactions") +
  theme(plot.title = element_text(hjust = 0.5))

rm(fraud_transactions_by_hour)


# Legit transactions by hour. ----------------------------------------------------------------------
legit_transactions_by_hour <- credit_fraud.df %>%
  filter(TRANS_FRAUD == 0) %>%
  group_by(TRANS_HOUR_OF_DAY) %>%
  summarise(Count = n()) %>%
  ungroup()

ggplot(legit_transactions_by_hour, aes(x = TRANS_HOUR_OF_DAY, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Number of Legit Transactions by Hour",
       x = "Hour of Day",
       y = "Count of Legit Transactions") +
  theme(plot.title = element_text(hjust = 0.5)) 

rm(legit_transactions_by_hour)


# Comparative of Transactions by Hour of Day. ------------------------------------------------------
ggplot(credit_fraud.df, aes(x = TRANS_HOUR_OF_DAY, fill = factor(TRANS_FRAUD))) +
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.6) +
  labs(x = "Hour of Day", fill = "Fraud Status") +
  facet_wrap(~TRANS_FRAUD)


# Relative fraudulent transaction by day of the week. ----------------------------------------------
fraud_day_counts <- credit_fraud.df %>%
  group_by(TRANS_DAY_OF_WEEK) %>%
  summarise(Fraud_Count = sum(TRANS_FRAUD),
            Total_Count = n()) %>%
  mutate(Fraud_Rate = (Fraud_Count / Total_Count) * 100) %>%
  arrange(TRANS_DAY_OF_WEEK) 

ggplot(fraud_day_counts, aes(x = factor(TRANS_DAY_OF_WEEK, levels = c("1", "2", "3", "4", "5", "6", "7")), 
                             y = Fraud_Rate, fill = Fraud_Rate)) +
  geom_col(show.legend = FALSE) + 
  scale_x_discrete(labels = c('1'='Monday', '2'='Tuesday', '3'='Wednesday', '4'='Thursday', 
                              '5'='Friday', '6'='Saturday', '7'='Sunday')) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Relative Number of Fraudulent Transactions by Day of Week",
       x = "Day of the Week", 
       y = "Percentage of Fraudulent Transactions (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

rm(fraud_day_counts)


# Distribution of Transaction Amounts by Fraud Status. ---------------------------------------------
ggplot(credit_fraud.df, aes(x = LOG_TRANS_AMOUNT, fill = factor(TRANS_FRAUD))) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6) +
  labs(x = "Log of Transaction Amount", fill = "Fraud Status") +
  facet_wrap(~TRANS_FRAUD)


# Amount Bins for Fraudulent vs Legitimate Transactions. -------------------------------------------
ggplot(credit_fraud.df, aes(x = TRANS_AMOUNT_BIN, fill = factor(TRANS_FRAUD))) +
  geom_bar(position = 'fill') +
  labs(x = "Transaction Amount Bin", y = "Proportion", fill = "Fraud Status")



# Amount scatter plot with label fraud. ------------------------------------------------------------
ggplot(credit_fraud.df, aes(x = TRANS_ID, y = TRANS_AMOUNT, color = factor(TRANS_FRAUD))) +
  geom_point(alpha = 0.6) +  
  scale_color_manual(values = c('0' = 'blue', '1' = 'red')) +  
  labs(title = "Scatter Plot of Transaction Amount by Fraud Label",
       x = "Transaction ID",
       y = "Transaction Amount",
       color = "Fraud Status") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5))  


# Terminal Clientele Size. -------------------------------------------------------------------------
ggplot(credit_fraud.df, aes(x = TM_CLIENTELE, fill = factor(TRANS_FRAUD))) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6) +
  labs(x = "Number of Unique Customers at Terminal", fill = "Fraud Status") +
  facet_wrap(~TRANS_FRAUD)


# Customer's New Purchase at Terminal Frequency. ---------------------------------------------------
ggplot(credit_fraud.df, aes(x = TM_CLIENTELE, fill = factor(TRANS_FRAUD))) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6) +
  labs(x = "Customer New Terminal Purchase Frequency", fill = "Fraud Status") +
  facet_wrap(~TRANS_FRAUD)


# Fraud rate for first-time and non-first-time terminal users. -------------------------------------
credit_fraud.df <- credit_fraud.df %>%
  mutate(CT_FIRST_TIMER = ifelse(CT_PURCHASE_TM <= 1, "First Timer", "Experienced"))


# Calculate the total and fraudulent transaction counts for first-time -----------------------------
# and non-first-time terminal users. ---------------------------------------------------------------
fraud_rate_first_timers <- credit_fraud.df %>%
  group_by(CT_FIRST_TIMER) %>%
  summarise(Total_Transactions = n(),
            Fraud_Transactions = sum(TRANS_FRAUD == 1),
            Fraud_Percentage = (Fraud_Transactions / Total_Transactions) * 100) %>%
  ungroup()

ggplot(fraud_rate_first_timers, aes(x = CT_FIRST_TIMER, y = Fraud_Percentage, fill = CT_FIRST_TIMER)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("First Timer" = "red", "Experienced" = "green")) +
  labs(title = "Fraud Rate: First Time vs Experienced Terminal Users",
       x = "User Type at Terminal", 
       y = "Percentage of Fraudulent Transactions (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

rm(fraud_rate_first_timers)


# Correlation plot for numerical variables. --------------------------------------------------------
numerical_vars_df <- credit_fraud.df %>%
  select(TRANS_DAY_OF_WEEK, TRANS_HOUR_OF_DAY, TRANS_WEEKEND, TRANS_NIGHT, CT_AVG_DAY_AMOUNT,
         CT_AVG_DAY_TRANS, CT_TOT_DAY_AMOUNT, CT_TOT_DAY_TRANS, CT_FRAUD_HIST, TM_AVG_DAY_AMOUNT,
         TM_AVG_DAY_TRANS, TM_AVG_DAY_TRANS, TM_TOT_DAY_TRANS, TM_FRAUD_HIST, TM_FRAUD_HIST,
         CT_PURCHASE_TM, TRANS_FRAUD)

cor_matrix <- cor(numerical_vars_df, use = "complete.obs")

corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

rm(numerical_vars_df)
rm(cor_matrix)


# Relative percentage of fraudulent transactions ocurring at night. --------------------------------
fraud_night_counts <- credit_fraud.df %>%
  mutate(Night_Day_Label = ifelse(TRANS_NIGHT == 1, "Night", "Day")) %>%
  group_by(Night_Day_Label) %>%
  summarise(Total_Transactions = n(),
            Fraud_Transactions = sum(TRANS_FRAUD),
            Fraud_Percentage = (Fraud_Transactions / Total_Transactions) * 100) %>%
  ungroup()

ggplot(fraud_night_counts, aes(x = Night_Day_Label, y = Fraud_Percentage, fill = Night_Day_Label)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Night" = "darkblue", "Day" = "skyblue")) +
  labs(title = "Relative Percentage of Fraudulent Transactions: Night vs Day",
       x = "Time of Day",
       y = "Percentage of Fraudulent Transactions (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

rm(fraud_night_counts)