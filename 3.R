# loading packages
library(tidyverse)
library(lubridate)

# loading .csv dataset
sales <- read.csv("sales2019.csv")

# analyze dataset

# Determine the dimension
dim(sales) # 5000 rows, 5 columns

# Determine the column names
colnames(sales) #"date" "user_submitted_review" "title" "total_purchased" "customer_type"  

# the types of each of the columns
map(sales, typeof)#date:"character"; user_submitted_review:"character"; title:"character"; total_purchased:"integer"; customer_type:"character"

# unique values present in each of the columns
map(sales, unique) #user_submitted_review and total_purchased have NA values

# remove rows have data missing
complete_sales <- sales %>%
  filter(!(is.na(user_submitted_review)))
dim(complete_sales) #456 rows were removed

#calculate the average number of books purchased on an order
purchase_mean <- complete_sales %>%
  filter(!(is.na(total_purchased))) %>%
  pull(total_purchased) %>%
  mean() #mean = 4.0026

#Fill all of the missing values in total_purchased
complete_sales <- complete_sales %>%
  mutate(imputed_purchase = if_else(is.na(total_purchased), 
                                    purchase_mean, 
                                    as.numeric(total_purchased)))

#classify reviews as either positive or negative
classificator <- function(review) {
  review_positive = case_when(
    str_detect(review, "Awesome") ~ "Positive",
    str_detect(review, "OK") ~ "Positive",
    str_detect(review, "Never") ~ "Positive",
    str_detect(review, "a lot") ~ "Positive",
    TRUE ~ "Negative" # The review did not contain any of the above phrases
  )
}
complete_sales <- complete_sales %>% 
  mutate(
    is_positive = unlist(map(user_submitted_review, classificator))
  )

#distinguish between sales that happen before July 1, 2019 and sales that happen after this date.
complete_sales <- complete_sales %>%
  mutate(date_status = if_else(ymd(date) < ymd("2019-07-01"), "Pre", "Post"))

#compares the number of books purchased before July 1, 2019 to after.
complete_sales %>%
  group_by(date_status) %>%
  summarise(book_purchased = sum(imputed_purchase))
#The program did not increase sales

#compares the number of books purchased before July 1, 2019 to after and Title
complete_sales %>%
  group_by(date_status, title) %>%
  summarise(book_purchased = sum(imputed_purchase)) %>%
  arrange(title, date_status)
#R For Dummies and Secrets of R For Advanced Students got more popular.

#compares the number of books purchased before July 1, 2019 to after and Customer Type
complete_sales %>%
  group_by(date_status, customer_type) %>%
  summarise(book_purchased = sum(imputed_purchase)) %>%
  arrange(customer_type, date_status)
#The program was more effective with Business than Individual

#compares the number of positive reviews before and after July 1, 2019.
complete_sales %>%
  group_by(date_status) %>%
  summarise(num_positive_reviews = sum(is_positive == "Positive"))
#review sentiment improved after the program was created

#compares the number of negative reviews before and after July 1, 2019.
complete_sales %>%
  group_by(date_status) %>%
  summarise(num_positive_reviews = sum(is_positive =="Negative"))

#compute the average purchase quantity for each book 
complete_sales %>%
  filter(!(is.na(total_purchased))) %>%
  group_by(title) %>%
  summarise(avg_sales = mean(total_purchased))

imputator <- function(sales, title) {
  if(is.na(sales) && title == "Fundamentals of R For Beginners") {return(3.96)}
  else if (is.na(sales) && title == "R For Dummies") {return(4.05)}
  else if (is.na(sales) && title == "R Made Easy") {return(4.8)}
  else if (is.na(sales) && title == "R vs Python: An Essay") {return(3.99)}
  else if (is.na(sales) && title == "Secrets Of R For Advanced Students") {return(4.06)}
  else if (is.na(sales) && title == "Top 10 Mistakes R Beginners Make") {return(3.89)}
  else {return(sales)}
}

complete_sales <-  complete_sales %>%
  mutate(imputed_purchase2 = unlist(map2(total_purchased, title, imputator)))

#analysis based on the month 
complete_sales <- complete_sales %>%
  mutate(month = month(date))

complete_sales %>%
  group_by(month) %>%
  summarise(sales_per_month = sum(imputed_purchase2)) %>%
  arrange(month)
#books tend to sell more in December because it is Holiday season
  
