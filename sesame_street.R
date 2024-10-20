### Sesame Street Example 

# Set your working directory (where the data is saved!)
setwd("")

# Read in some packages
  # If these don't work, install them using install.packages("[package name]")
library(tidyverse)
library(AER) # for ivreg function
library(lmtest)
library(estimatr)

# Read in the data
Sesame <- read_csv("Sesame.csv")

Sesame$site <- factor(Sesame$site) # Here 'site' is the blocking variable, meaning
# that people were assigned to the treatment or control within a site.
# don't worry about this too much for now, but this means we need to control
# for the variable site in any regressions we run!

### Breaking our sample into groups based on T (encouraged to watch) and D (actually watched)
# who is in each group?
Sesame %>% count(encouraged, watched)

# how do compliers differ from never-takers in the group assigned to treatment?
Sesame %>% 
  filter(encouraged == 1) %>%
  group_by(watched) %>%
  summarise(
    mean_prelet = mean(prelet, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    share_male = mean(male, na.rm = TRUE)
  )

##### Compute the Wald ratio 
# first stage
first_stage <- lm_robust(watched ~ site + encouraged, data = Sesame)
tidy(first_stage) %>% select(-c(df, outcome))

# ITT
itt <- lm_robust(postlet ~ site + encouraged, data = Sesame)
tidy(itt) %>% select(-c(df, outcome))

3.41/0.348 # Our wald ratio is given by the ITT/(share compliers)

##### By hand
Dhat <- predict(lm(watched ~ site + encouraged, data = Sesame))
lm_robust(postlet ~ site + Dhat, data = Sesame)

##### 2SLS using the IV commands 
  # type ? iv_robust to learn what you need to input to these commands! here's
  # one of the examples of the command: 
  # Instrument for treatment `D` with encouragement `Z`: tidy(iv_robust(Y ~ D + X | Z + X, data = dat))
tidy(iv_robust(postlet ~ site  + watched | 
                 site  + encouraged, 
               data = Sesame)) %>% select(-c(df, outcome))

