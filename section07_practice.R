###############################################################################
# Created by: Raheem Chaudhry
# Last revised by: Gaby Lohner
# Created on: 11 oct 2023
# Last revised on: 18 oct 2024
# Description: Practice. Examine differences in employment outcomes for those
# participating in a safety net program.
###############################################################################/

################################################################################
# Prompt
################################################################################
  
# Suppose we're interested in the effect of safety net receipt on labor 
# market outcomes
#
# This week, I'm sharing data from the March 2023 CPS, restricted to just
# California. I've kept variables related to demographics, participation in
# safety net programs, and labor market outcomes.
#
# Here, I share an example trying to relate receipt of SNAP (food stamps) and 
# employment outcomes

################################################################################
# Setup
################################################################################
 
rm(list=ls())

# Load Libraries
library(dplyr)
library(estimatr)
library(ggplot2)
library(readr)

# Set working directory
setwd("~/Library/CloudStorage/GoogleDrive-jsupeyo@berkeley.edu/My Drive/University of California, Berkeley/1. Goldman School of Public Policy/1. Classes/2. 2024 Fall/PUB POL 249: Statistics for Program Evaluation/Assignments/Code/Statistics4PE")

################################################################################
# Load
################################################################################

## Load March 2023 CPS data (CA only)
cps <- read_csv("section07_data.csv")

## Take a look at the data and explore any variables that seem interesting/confusing
# Here's the codebook: https://www2.census.gov/programs-surveys/cps/techdocs/cpsmar23.pdf

################################################################################
# Tidy
################################################################################

## Generate variables for analysis
# Food stamp receipt
cps <- cps %>% 
  mutate(f_snap = ifelse(f_mv_fs>0, 1, 0))

# Race/ethnic categories
cps <- cps %>% 
  mutate(racecat = case_when(
    prdtrace==1 & pehspnon==2 ~ 1,
    prdtrace==2 & pehspnon==2 ~ 2,
    prdtrace==4 & pehspnon==2 ~ 3,
    pehspnon==1 ~ 4,
    !(prdtrace %in% c(1, 2, 4)) & pehspnon==2 ~ 5
  ))

# Check
cps %>% 
  group_by(racecat) %>% 
  summarize(unweighted_count = n(), weighted_count = sum(marsupwt))

################################################################################
# Exploratory analysis
################################################################################

# Share who work, by SNAP receipt and race
r <- cps %>% 
  group_by(f_snap, racecat) %>% 
  summarize(avg_wkswork = mean(wkswork))
r

# Plot this data
r %>%
  ggplot(aes(x=factor(racecat), fill=factor(f_snap), y=avg_wkswork)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(labels = c("White, NH", "Black, NH", "Asian, NH", "Hispanic", "Other, NH")) +
  labs(title="Weeks Worked by Race and SNAP Participation",
       x="Race/Ethnic Group",
       y="Avg Weeks Worked",
       fill="SNAP") +
  theme_minimal()

################################################################################
# Regression analysis
################################################################################

## Regress employment outcomes on SNAP receipt
ols <- lm_robust(wkswork ~ f_snap, data=cps)
summary(ols)

## Do you want to add any covariates? (You might want to clean some additional
# variables! Make sure you know what you're controlling for)

## Do you want to restrict your sample in any way? Who does it make sense to 
# use as a control group?


# Can you interpret this causally? What if you add covariates?
# What if I told you that SNAP and employment are measured at the same time (e.g., last year)?
# What if I said instead that SNAP is measured for last year, but employment is measured for this year?

# If you want to try working on creating a matched sample (we'll do this next week!) 
install.packages("MatchIt")
library(MatchIt)
? matchit
################################################################################
# End of file
################################################################################