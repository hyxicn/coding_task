# This files contains data simulation in the baseline survey and endline survey

## clear the environment ----
rm(list = ls())

library(tidyverse)      # data manipulation and visualization
library(readxl)         # read excel file
library(data.table)     # extend dataframe
library(readstata13)    # read stata file
library(lubridate)      # handle date and time
library(stargazer)      # regression summary 
library(lfe)            # regression with fixed effect
library(statar)         # additional regression
library(haven)          # read dta
library(fastDummies)    # create dummy variables
#install.packages('fastDummies')

## Data Simulation ----

# baseline survey simulation -----

# Set seed for reproducibility
set.seed(123)

# Generate 5,000 sample data
n <- 5000 
treatment_groups <- c("reason", "emotion", "control")
treatment_probabilities <- c(1/3, 1/3, 1/3)

df1 <- data.frame(
  id = 1:n,
  
  # assign treatment
  group = sample(treatment_groups, size = n, replace = TRUE, prob = treatment_probabilities),
  
  ###Vaccination status and intention###
  
  # Have you receive the Covid-19 vaccine?
  # 0-No, 1-Yes/No,but I have an appointment
  uptake = sample(c(0,1),size = n, replace = TRUE, prob = c(0.5,0.5)),
  
  # uptake == 0, How likely are you going to get one more Covid-19 vaccine?
  # 1-extremely unlikely, 2-somewhat unlikely, 3-neither likely or unlikely
  # 4-somewhat likely, 5-extremely likely
  likely = sample.int(5,n,replace = T),
  
  ###Attitudes towards covid-19###
  
  # How much are you worry about getting covid-19?
  # 1-not at all, 3- neutral, 5-extremely worry
  worry = sample(5, n, replace =T),
  # How much do you agree that covid-19 is safe for your body?
  # 1-completely disagree, 3-neutral, 5-completely agree
  safe = sample.int(5, n, replace = T),
  # How much do you agree that getting a covid-19 vaccination can improve your health?
  imp = sample.int(5,n,replace = T),
  # How much do you agree that get a vaccination can protect yourself and family from the disease?
  family = sample.int(5,n,replace = T),
  
  ###Attitudes towards social media###
  # How much do you belief the information about Covid-19 in social media?
  # 1-not at all, 3-neutral, 5-a lot
  belief = sample.int(5,n,replace = T),
  
  # Demographic controls ##
  # What is your political belief?
  republic = sample(c(1,0), n, replace = T, prob = c(0.5,0.5)),
  # What is your gender
  male = sample(c(1, 0), n, replace = TRUE, prob = c(0.5, 0.5)),
  # What is your highest degree so far?
  edu = sample(c("high_school","some_college","bachelor","postgraduate","others"),
               size = n, replace = T,
               prob = c(0.35,0.25,0.15,0.05,0.2)),
  # What is your race?
  race = sample(c("white","black","asian","hispanic","others"),
                size = n, replace = T,
                prob = c(0.65, 0.15, 0.06, 0.10, 0.04)),
  # How old are you?
  age = sample(c("15-24", "25-34", "35-44","45-54","55-64","others"),
               size = n, replace = T,
               prob = c(0.25,0.25,0.15,0.15,0.15,0.05)),
  # What is your occupation?
  occ = sample(c("management_and_professional","service","sales_and_office","construction_extraction_and_maintenance","farming_fishing_and_forestry","others"),
               size = n, replace = T,
               prob = c(0.3, 0.2,0.3,0.1,0.05,0.05)),
  
  ## location controls ##
  # Where do you stay most of the time during a day?
  urban = sample(c(1,0), n, replace = TRUE, prob = c(0.5, 0.5)), # working and living
  # In which way do you work normally?
  work = sample(c("home", "onsite","hybrid"),
                size = n, replace = T,
                prob = c(0.2,0.5,0.3)),
  
  # state fixed effect
  state = sample(1:51,size = n, replace = T),
  
  # 4500 people replied for the endline survey
  endline = sample(c(rep(1, 4500), rep(0, 500)))
)

# create a control column to indicate wether the sample is from control group
df1$treatment <-ifelse(df1$group!= "control", 1, 0)

## endline survey simulation ----

df2 <- df1
# create dummy
df2 <- dummy_cols(df2, select_columns = "group")
df1 <- dummy_cols(df1, select_columns = "group")

# df2 is after treatment
df2$time <- 1
# df1 is before treatment
df1$time <- 0

# Set a seed different from  df1
set.seed(1234)
###Attitudes towards covid-19###
df2$worry <- sample(5, n, replace =T)
df2$safe <- sample.int(5, n, replace = T)
df2$imp <- sample.int(5,n,replace = T)
df2$family <- sample.int(5,n,replace = T)
df2$belief <- sample.int(5,n,replace = T)

# uptake after treatment in endline survey
df2$uptake <- 0.1*df2$group_reason + 0.01*df2$group_emotion+0.01*df2$treatment+0.01*df2$time + 0.2*df2$treatment*df2$time - 0.05*df2$worry + 0.05*df2$safe + 0.05*df2$imp + 0.05*df2$family + 0.1*df2$belief - 0.1*df2$republic + rnorm(n,0.5,1)

# Create a new column 'uptake_integer' based on specified intervals
df2$uptake <- cut(df2$uptake, breaks = c(-Inf, 1,Inf), labels = c(0, 1))
df2$uptake <- as.numeric(as.character(df2$uptake))

# combine data for DID analysis ----

df2 <- df1

# create dummy
df2 <- dummy_cols(df2, select_columns = "group")
df1 <- dummy_cols(df1, select_columns = "group")

# df2 is after treatment
df2$time <- 1

# df1 is before treatment
df1$time <- 0

df1$uptake <- as.numeric(df1$uptake)
df2$uptake <- as.numeric(df2$uptake)
df4 <- rbind(df1,df2)

# conditional logit regression
df4$uptake <- as.numeric(df4$uptake)

# generate dummy variables
df4 <- dummy_cols(df4, select_columns = "edu")
df4 <- dummy_cols(df4, select_columns = "race")
df4 <- dummy_cols(df4, select_columns = "age")
df4 <- dummy_cols(df4, select_columns = "occ")
df4 <- dummy_cols(df4, select_columns = "work")



## Save data----

names(df1)

baseline <- df1 %>%
  select("id","uptake","likely","worry","safe","imp","family","belief",
         "republic","male","edu","race","age","occ","urban","work",
         "state")

treatment_assignment <- df1 %>%
  select("id","group","treatment","group_control","group_emotion",
         "group_reason")

endline <- df2 %>%
  filter(endline == 1) %>%
  select("id","uptake","likely","worry","safe","imp","family","belief",
         "republic","male","edu","race","age","occ","urban","work",
         "state")