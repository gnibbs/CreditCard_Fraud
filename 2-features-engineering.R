####################################################################################################
################################### Features Engineering ###########################################

##### DateTime Focused Features ###################################################################

### Feature #1 - Day of Week ----------------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  mutate(TRANS_DAY_OF_WEEK = wday(DATETIME, label = FALSE))


### Feature #2 - Hour of Day ----------------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  mutate(TRANS_HOUR_OF_DAY = hour(DATETIME))


### Feature #3 - Weekend Flag ---------------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  mutate(TRANS_WEEKEND = if_else(TRANS_DAY_OF_WEEK %in% c(6, 7), 1, 0))


### Feature #4 - Nigth Flag -----------------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  mutate(TRANS_NIGHT = if_else(TRANS_HOUR_OF_DAY >= 20 | TRANS_HOUR_OF_DAY < 6, 1, 0))


##### Amount Focused Features #####################################################################

### Feature #5 - Amount Log Transformation -------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  mutate(LOG_TRANS_AMOUNT = log1p(TRANS_AMOUNT))


### Feature #6 - Amount Level Bins ----------------------------------------------------------------

bin_edges <- quantile(credit_fraud.df$TRANS_AMOUNT, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

credit_fraud.df <- credit_fraud.df %>%
  mutate(TRANS_AMOUNT_BIN = cut(TRANS_AMOUNT,
                                breaks = bin_edges,
                                labels = c("low", "medium", "high", "very_high"),
                                include.lowest = TRUE))

rm(bin_edges)


##### Customer Focused Features ###################################################################

### Feature #7 - CT Average Daily Amount ----------------------------------------------------------

daily_amounts <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(CUSTOMER_ID, DATE) %>%
  summarise(Daily_Total = sum(TRANS_AMOUNT)) %>%
  ungroup()

average_daily_transaction_amount <- daily_amounts %>%
  group_by(CUSTOMER_ID) %>%
  summarise(CT_AVG_DAY_AMOUNT = mean(Daily_Total)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  left_join(average_daily_transaction_amount, by = "CUSTOMER_ID")

rm(average_daily_transaction_amount)
rm(daily_amounts)


### Feature #8 - CT Average Daily Trans -----------------------------------------------------------

daily_transaction_counts <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(CUSTOMER_ID, DATE) %>%
  summarise(Daily_Count = n()) %>%
  ungroup()

average_daily_transaction_count <- daily_transaction_counts %>%
  group_by(CUSTOMER_ID) %>%
  summarise(CT_AVG_DAY_TRANS = mean(Daily_Count)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  left_join(average_daily_transaction_count, by = "CUSTOMER_ID")

rm(average_daily_transaction_count)
rm(daily_transaction_counts)


### Feature #9 - CT Total Daily Amount -------------------------------------------------------------

daily_amounts <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(CUSTOMER_ID, DATE) %>%
  summarise(CT_TOT_DAY_AMOUNT = sum(TRANS_AMOUNT)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  left_join(daily_amounts, by = c("CUSTOMER_ID", "DATE")) %>%
  select(-DATE)

rm(daily_amounts)


### Feature #10 - CT Total Daily Transactions -----------------------------------------------------

daily_transactions <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(CUSTOMER_ID, DATE) %>%
  summarise(CT_TOT_DAY_TRANS = n()) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  left_join(daily_transactions, by = c("CUSTOMER_ID", "DATE")) %>%
  select(-DATE) 

rm(daily_transactions)

### Feature #11 - CT Fraud History  ---------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  arrange(CUSTOMER_ID, DATETIME)

credit_fraud.df <- credit_fraud.df %>%
  group_by(CUSTOMER_ID) %>%
  mutate(CT_FRAUD_HIST = cumsum(TRANS_FRAUD) - TRANS_FRAUD) %>%
  ungroup()


##### Terminal Focused Features ###################################################################

### Feature #12 - TM AVG Daily Trans Amount  ------------------------------------------------------

daily_terminal_amounts <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(TERMINAL_ID, DATE) %>%
  summarise(Terminal_Daily_Total = sum(TRANS_AMOUNT)) %>%
  ungroup()

average_terminal_daily_transaction_amount <- daily_terminal_amounts %>%
  group_by(TERMINAL_ID) %>%
  summarise(TM_AVG_DAY_AMOUNT = mean(Terminal_Daily_Total)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  left_join(average_terminal_daily_transaction_amount, by = "TERMINAL_ID")

rm(daily_terminal_amounts)
rm(average_terminal_daily_transaction_amount)


### Feature #13 - TM AVG Daily Transations  -------------------------------------------------------

daily_terminal_transactions <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(TERMINAL_ID, DATE) %>%
  summarise(Daily_Transaction_Count = n()) %>%
  ungroup()

average_daily_terminal_transaction_count <- daily_terminal_transactions %>%
  group_by(TERMINAL_ID) %>%
  summarise(TM_AVG_DAY_TRANS = mean(Daily_Transaction_Count)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  left_join(average_daily_terminal_transaction_count, by = "TERMINAL_ID")

rm(daily_terminal_transactions)
rm(average_daily_terminal_transaction_count)


### Feature #14 - TM Total Daily Transations Amount ------------------------------------------------

terminal_daily_totals <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  group_by(TERMINAL_ID, DATE) %>%
  summarise(TM_TOT_DAY_AMOUNT = sum(TRANS_AMOUNT)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  mutate(DATE = as.Date(DATETIME)) %>%
  left_join(terminal_daily_totals, by = c("TERMINAL_ID", "DATE"))

rm(terminal_daily_totals)


### Feature #15 - TM Total Daily Transations  ------------------------------------------------------

terminal_daily_transaction_counts <- credit_fraud.df %>%
  group_by(TERMINAL_ID, DATE) %>%
  summarise(TM_TOT_DAY_TRANS = n()) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  left_join(terminal_daily_transaction_counts, by = c("TERMINAL_ID", "DATE"))

rm(terminal_daily_transaction_counts)


### Feature #16 - TM Fraud History  ----------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  arrange(TERMINAL_ID, DATETIME)

credit_fraud.df <- credit_fraud.df %>%
  group_by(TERMINAL_ID) %>%
  mutate(TM_FRAUD_HIST = cumsum(TRANS_FRAUD) - TRANS_FRAUD) %>%
  ungroup()


### Feature #17 - TM Clientele  --------------------------------------------------------------------

terminal_customer_counts <- credit_fraud.df %>%
  group_by(TERMINAL_ID) %>%
  summarise(TM_CLIENTELE = n_distinct(CUSTOMER_ID)) %>%
  ungroup()

credit_fraud.df <- credit_fraud.df %>%
  left_join(terminal_customer_counts, by = "TERMINAL_ID")

rm(terminal_customer_counts)


### Feature #18 - CT New TM Purchase  --------------------------------------------------------------

credit_fraud.df <- credit_fraud.df %>%
  arrange(CUSTOMER_ID, TERMINAL_ID, DATETIME)

credit_fraud.df <- credit_fraud.df %>%
  group_by(CUSTOMER_ID, TERMINAL_ID) %>%
  mutate(CT_PURCHASE_TM = cumsum(!is.na(TRANS_ID)) - 1) %>%
  ungroup() 
