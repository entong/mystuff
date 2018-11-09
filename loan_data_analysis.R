###############################################################################
### 
###    Purpose: Analyze the loan data and uncover insights about loan
###             performance
###    Author: Zhen Wang
###    Date  : 11/05/2018
###
###############################################################################

remove(list = ls())
cat("\014") # this line is to clear the console

library("dplyr")
library("ggplot2")
library("magrittr")
library("lubridate") 
library("aod")

setwd("C:/Users/Zhen/Dropbox/Job Searching/Job Applications/Wealthfront/Problem_Set")

###############################################################################
# Read in the loan data and perform preliminary exploration
loan_data <- read.csv("./raw/loan_data.csv", na = "NA", stringsAsFactors = FALSE)

# drop irrelevant columns
vars_remove <- c("wtd_loans", "interest_rate", "int_rate2", "num_rate",
                 "numrate", "int_rate3")
loan_data <- loan_data %>% select(-one_of(vars_remove))

# Data exploration - check NAs and outliers
head(loan_data)
dim(loan_data)
glimpse(loan_data)
summary(loan_data)

# check if borrower's id uniquely identify each observation
n_distinct(loan_data$id)

# check if funded_amnt is always < loan_amnt
loan_data %$% mean(funded_amnt <= loan_amnt, na.rm = TRUE)



###############################################################################
# Data cleaning

# define loan performance variable based on loan status

loan_data <- loan_data %>%
        mutate(loan_performance = recode(loan_status, "Current"            = 1,
                                                      "Fully Paid"         = 1,
                                                      "Default"            = 0,
                                                      "In Grace Period"    = 0,
                                                      "Late (16-30 days)"  = 0,
                                                      "Late (31-120 days)" = 0))


# drop the loans which don't have a performance classification
loan_data <- loan_data %>% filter(loan_performance == 0 | loan_performance == 1)
table(loan_data$loan_performance)


# extract number of months from the term variable and covert it to categorical variable
loan_data <- loan_data %>% mutate(term = substr(term, start = 1, stop = 2))
loan_data$term <- factor(loan_data$term, levels = c("36", "60"))


# create a variable as percentage of monthly installment to income
loan_data <- loan_data %>% mutate(install_pct = installment/annual_inc * 12)

                                                      
# evaluate home ownership and keep levels "MORTGAGE", "OWN" and "RENT"
table(loan_data$home_ownership)
loan_data <- loan_data %>% filter(home_ownership == "MORTGAGE" | 
                                          home_ownership == "OWN"| home_ownership == "RENT")
loan_data$home_ownership <- factor(loan_data$home_ownership, levels = c("MORTGAGE", "OWN", "RENT"))


# drop rows with months_since_last_delinquent being NA
loan_data <- loan_data %>% filter(is.na(mths_since_last_delinq) == FALSE)

# calculate the length of credit history
loan_data$earliest_cr_line <- strptime(loan_data$earliest_cr_line, "%m/%d/%Y %H:%M")
loan_data$credit_length <- 2018 - year(loan_data$earliest_cr_line)
loan_data <- loan_data %>% select(-earliest_cr_line)



###############################################################################
# Perform logistic regression to model loan performance

# set up training data and test data
set.seed(1)
Training_Pct <- 0.5 # training data percentage
training_rows <- sample(1:nrow(loan_data), Training_Pct * nrow(loan_data)) 

training_data <- loan_data[training_rows, ]  
test_data  <- loan_data[-training_rows, ]  


# model loan performance with a logistic regression with training data

# the data is unbalanced in the sense that only a small proportion of data points representing bad loans
# we need to scale up the proportion of the bad loans in the training data to increase the power of the model
table(training_data$loan_performance)

scale_para <-  round(sum(training_data$loan_performance == 1)/sum(training_data$loan_performance == 0))

training_good_loans <- training_data %>% filter(loan_performance == 1)
training_bad_loans <- training_data %>% filter(loan_performance == 0)
training_bad_loans_scaled <- sample_frac(training_bad_loans, size = scale_para, replace = TRUE)
training_data <- rbind(training_good_loans, training_bad_loans_scaled)



# Run logistic regression on training data
performance_model <- glm(loan_performance ~  funded_amnt + term  + int_rate + install_pct + home_ownership + 
                                  purpose  + dti + delinq_2yrs + credit_length + mths_since_last_delinq + 
                                  open_acc + revol_bal + total_acc + out_prncp + total_pymnt, 
                          data = training_data, family = "binomial")

summary(performance_model)

# check if the estimated coefficients are joinly significant
wald.test(b = coef(performance_model), Sigma = vcov(performance_model), Terms = 2:28)


###############################################################################
# predict loan_performance with test data
test_data$predicted_prob <- predict(performance_model, newdata = test_data, type = "response")

summary(test_data$predicted_prob)
test_data %>% group_by(loan_performance) %>% summarize(mean = mean(predicted_prob, na.rm = TRUE))


# t-test the mean of predicted probability for both good and bad
t.test(predicted_prob ~ loan_performance, data = test_data, var.equal=FALSE, conf.level=0.95)

# Calculate percent correctly predicted
test_data <- test_data %>% mutate(is_good_loan_predicted = predicted_prob > 0.5)
test_data$is_good_loan_predicted <- as.numeric(test_data$is_good_loan_predicted)
pct_correctly_pred <- sum(test_data$loan_performance == test_data$is_good_loan_predicted)/
        length(test_data$loan_performance)
pct_correctly_pred

# plot the predicted default probability vs. actual loan performance
histogram_plot <- ggplot(test_data, aes(x = predicted_prob)) + geom_histogram() + 
        facet_wrap(~loan_performance, scales = "free_y") + 
        labs(title = "Predicted Probability Histogrm by Actual Loan Performance", 
             x = 'Predicted Probability of Being Good Loan')

ggsave("./output/hist_pred_prob.png", histogram_plot, width = 6, height = 5)


box_plot <- ggplot(test_data, aes(factor(loan_performance), predicted_prob)) + geom_boxplot() +
        labs(title = "Predicted Probability Box Plot by Actual Loan Performance",
             y = "Predicted Probabability", x = "")

ggsave("./output/box_pred_prob.png", box_plot, width = 6, height = 5)

# not the end

# The End.

