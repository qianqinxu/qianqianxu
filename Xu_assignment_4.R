################################################################################
##
## <PROJ> Qianqian Xu
## <FILE> assignment_4
## <INIT> 01 Feb 2022
## <AUTH> qianqinxu (GitHub/Twitter: @DearQianqianxu)
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)


## -----------------------------------------------------------------------------
## Answer assignment 4 questions
## -----------------------------------------------------------------------------


## process
## ---------------------------

dat <- read.csv("hsls_small.csv")

# Q1
# first, calcuate the average test score by region, then join back the origin dataset
# and calcualte the difference, finally calculate the average difference by region.
dat %>% group_by(x1region) %>% summarise(avg_score=mean(x1txmtscor))  %>% inner_join(dat) %>%
  mutate(diff=x1txmtscor-avg_score) %>%
  group_by(x1region) %>% summarise(avg_diff=mean(diff))


# Q2
# group by region and family income level, calculate the average score and join back 
dat %>% group_by(x1region, x1famincome) %>% summarise(avg_score=mean(x1txmtscor)) %>%
  inner_join(dat)


# Q3
# use the method gather to transform data from wide to long.
dat %>% select(stu_id, x1stuedexpct, x1paredexpct, x4evratndclg) %>%
  gather(key=expect_type, value=expectation, x1stuedexpct:x1paredexpct)

## End