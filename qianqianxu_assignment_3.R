
## ---------------------------
## librarie

## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------

data_directory_path <- file.path("..", "data")

## ---------------------------
## settings/macros
## ---------------------------


## ---------------------------
## functions
## ---------------------------


## ---------------------------
## input
## ---------------------------

df <- read_csv(file.path(data_directory_path, "hsls_small.csv")) 

## cleaning the data

colnames(df) <- c("student_id", 
                  "sex", 
                  "race", 
                  "st_dob", 
                  "mat_score", 
                  "parent_educ", 
                  "#_hh_members",
                  "fam_income",
                  "poverty_185",
                  "ses",
                  "stud_ed_expct",
                  "par_ed_expct", 
                  "region",
                  "hs_status",
                  "college_attend",
                  "time_between_HSCol")



## ---------------------------
## process
## ---------------------------

# Question 1

## mutate non-response values to NA, take mean of scores left over (not counting
## NA values)

df<- df %>%
  mutate(mat_score = ifelse(mat_score == -8.0000, NA, mat_score)) 


average_scor <- summarize(df, mat_score = mean(mat_score, na.rm = TRUE))


## The average standardized math score is 51.10957

# Question 2

## create new dataframe, selecting student_id, mat_score and sex variables; mutating
## nonresponse/missing values; create new columns for male scores and female scores
## taking the average of individual columns, excluding NA values

gender_df <- df  %>%
  select(student_id, mat_score, sex) %>%
  mutate(sex = ifelse(sex == -9, NA, sex)) %>%
  mutate(mat_score = ifelse(mat_score == -8.0000, NA, mat_score)) %>%
  mutate(male_scores = ifelse(sex == 1, mat_score, NA)) %>%
  mutate(female_scores = ifelse(sex == 2, mat_score, NA))

male_mean <- mean(gender_df$male_scores, na.rm = TRUE)
female_mean <- mean(gender_df$female_scores, na.rm = TRUE)

## The average score for those identified as male is 51.0726482777625 and the average for 
## those identified as female is 51.147641479587


# Question 3

## create new dataframe from df frame, mutating missing values; arranging DOBs
## in descending order. mutating DOB column to exclude values w/ no month
## and filtering the frame. oldest and youngest are found using max and min functions

df_dob <- mutate(df, st_dob = ifelse(st_dob == -9, NA, st_dob)) %>%
  arrange(desc(st_dob)) %>%
  mutate(st_dob = ifelse(st_dob %in% c(199000, 199100, 199200), NA, st_dob)) %>%
  filter(!is.na(st_dob))

oldest <- min(df_dob$st_dob)
youngest <- max(df_dob$st_dob)

## youngest was born 12/1996 & the oldest was born 01/1992


## Question 4

## create new dataframe from df frame, mutate missing/nonresponse values;
## in descending order. create new column to return one's family income
## value if they are below pov. line. median is measured from that new column
## excluding NA values

income_df <- df %>%
  select(student_id, poverty_185, fam_income) %>%
  mutate(poverty_185 = ifelse(poverty_185 %in% c(-9, -8), NA, poverty_185)) %>%
  mutate(below_185 = ifelse(poverty_185 == 1, fam_income, NA))

median_fam_income <- median(income_df$below_185, na.rm = TRUE)

## Median family income for those under 185% of the federal poverty line is category 
# 2 or 2	Family income > $15,000 and <= $35,000

## Question 5

## create new dataframe from df frame, mutate missing/nonresponse values;
## match GED holders with their region, count all the values in each region
## and group the frame by region, GED status, and previous region count

## get total GED count from frame, then total ppl with HS credentials. divide to
## get % of GED recipients in this sample

## get GED % by region by identifying values that match in region & GED value; 
## if they do, divide total region count by count of GED in the region to get percentages
## by region

HS_cred_df <- df %>%
  select(student_id, hs_status, region) %>%
  mutate(hs_status = ifelse(hs_status %in% c(-8, 3), NA, hs_status)) %>%
  mutate(GED = ifelse(hs_status == 1, NA, region)) %>%
  filter(!is.na(hs_status)) %>%
  add_count(region) %>%
  group_by(region, GED, n) 

GED_count <- length(which(HS_cred_df$hs_status == 2))
total_hs_count <- length(which(HS_cred_df$hs_status %in% c(1, 2)))
perc_GED <- GED_count / total_hs_count

sums <- count(HS_cred_df) %>%
  mutate(region_avg = ifelse(region == 1 & GED == 1, nn/n, NA)) %>%
  mutate(region_avg = ifelse(region == 2 & GED == 2 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 3 & GED == 3 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 4 & GED == 4 & is.na(region_avg), nn/n, region_avg))

region_1_ged <- sums$region_avg[1]
region_2_ged <- sums$region_avg[3]
region_3_ged <- sums$region_avg[5]
region_4_ged <- sums$region_avg[7]

## The percentage of students with a HS credential that earned a GED is 0.0421166958305678
## or about 4%. By region: Region 1: 0.02727272727273 or about 3%, 
## Region 2: 0.03406814 or about 3%, Region 3: 0.04947526 or 5%
## Region 4: 0.05158730 or about 5%

## Question 6

## get college count from dividing total filt_df count by # of college attendees.

## create new dataframe, mutate missing/nonresponse values for college_attend and
## fam_income columns. categorize fam incomes values from codebook as below or above
## 35k. count the individuals in the region and the college attendees, group by 
## region, below/above 35k, and previous count. filter the frame excluding 
## NA values for college_attend column

## get below/above 35k by region by identifying values that match in region & below/above 35k value; 
## if they do, divide total region count by count of below/above 35k in the region to get percentages
## by region

filt_df <- df %>%
  mutate(college_attend = ifelse(college_attend == -8, NA, college_attend)) %>%
  filter(!is.na(college_attend))

college_count <- length(which(df$college_attend == 1)) / count(filt_df)

college_df <- df %>%
  select(student_id, college_attend, region, fam_income) %>%
  mutate(college_attend = ifelse(college_attend %in% c(-8, 0), NA, college_attend)) %>%
  mutate(fam_income = ifelse(fam_income %in% c(-9,-8), NA, fam_income)) %>%
  mutate(below_35 = ifelse(fam_income %in% c(1, 2), region, NA)) %>%
  mutate(above_35 = ifelse(fam_income %in% c(3:13), region, NA)) %>%
  filter(!is.na(college_attend), !is.na(fam_income)) %>%
  add_count(region, college_attend) %>%
  group_by(region, below_35, above_35, n)


college_df_region <- count(college_df) %>%
  mutate(region_avg = ifelse(region == 1 & below_35 == 1, nn/n, NA)) %>%
  mutate(region_avg = ifelse(region == 2 & below_35 == 2 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 3 & below_35 == 3 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 4 & below_35 == 4 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 1 & above_35 == 1 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 2 & above_35 == 2 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 3 & above_35 == 3 & is.na(region_avg), nn/n, region_avg)) %>%
  mutate(region_avg = ifelse(region == 4 & above_35 == 4 & is.na(region_avg), nn/n, region_avg))

region_1_below35 <- college_df_region$region_avg[1]
region_2_below35 <- college_df_region$region_avg[3]
region_3_below35 <- college_df_region$region_avg[5]
region_4_below35 <- college_df_region$region_avg[7]

region_1_above35 <- college_df_region$region_avg[2]
region_2_above35 <- college_df_region$region_avg[4]
region_3_above35 <- college_df_region$region_avg[6]
region_4_above35 <- college_df_region$region_avg[8]

## Out of the entire student sample that answered the postsecondary q, 0.753043 or about 75% attended a postsecondary
## institution by Feb. 2016.

## For the students who ever attended a postsecondary institution in Region 1, 0.1906125
## or about 19% had a reported family income that was at or below $35k and 0.8093875 or 81%
## had a reported family income that was above $35k.

## For the students who ever attended a postsecondary institution in Region 2, 0.1977067
## or about 20% had a reported family income that was at or below $35k and 0.8022933 or 80%
## had a reported family income that was above $35k. 

## For the students who ever attended a postsecondary institution in Region 3, 0.2119565
## or about 21% had a reported family income that was at or below $35k and 0.7880435 or 79%
## had a reported family income that was above $35k. 

## For the students who ever attended a postsecondary institution in Region 4, 0.2067511
## or about 21% had a reported family income that was at or below $35k and 0.7932489 or 79%
## had a reported family income that was above $35k. 

## ---------------------------
## output
## ---------------------------

#1. 
average_scor

#2.
male_mean
female_mean

#3.
oldest
youngest

#4.
median_fam_income

#5.
perc_GED
region_1_ged
region_2_ged
region_3_ged
region_4_ged

#6.
college_count

region_1_below35
region_2_below35 
region_3_below35 
region_4_below35 

region_1_above35 
region_2_above35 
region_3_above35  
region_4_above35  

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------