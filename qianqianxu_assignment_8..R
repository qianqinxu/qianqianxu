################################################################################
##
## <PROJ> EDH7916
## <FILE> QianqianXu
## <INIT> 28 Feb 2022
## <AUTH> Qianqinxu (GitHub/Twitter: @DearQianqianxu)
##
################################################################################

## Q1
library(tidyverse)
files <- list.files("../data/sch_test/by_school")
files <- files[grepl("bend|niagara", files)]
lst <- list()
for(i in 1:length(files)){
  path <- paste0("../data/sch_test/by_school/", files[i])
  data <- read.csv(path)
  data$relative_path <- path
  lst[[i]] <- data
}
dat <- bind_rows(lst)

## Q2
dat <- map_df(files, function(file){
  path <- paste0("../data/sch_test/by_school/", file)
  data <- read.csv(path)
  data$relative_path <- path
  data
})

## Q3
# 1
df <- read.csv("../data/hsls_small.csv")
fix_missing <- function(x, miss_val) {
  ## use ifelse(< test >, < do this if TRUE >, < do that if FALSE >)
  x <- ifelse(x %in% miss_val, # is x == any value in miss_val?
              NA, # TRUE: replace with NA
              x) # FALSE: return original value as is
  ## return corrected x
  return(x)
}
df$x1ses <- fix_missing(df$x1ses, -8)

# 2
test_scr <- df %>%
  filter(row_number() <= 50) %>%
  pull(x1txmtscor)
for(i in 1:length(test_scr)){
  if(test_scr[i]==-8){
    print(i)
  }
}

for(i in 1:length(test_scr)){
  if(test_scr[i]==-8){
    cat("Missing at ", i, "\n")
  }else{
    cat("Non-missing at ", i, "\n")
  }
}


for(i in 1:length(test_scr)){
  if(test_scr[i]==-8){
    cat("Flag: missing value\n")
  }else if(test_scr[i]<40){
    cat("Flag: low score\n")
  }else{
    cat("Non-missing at ", i, "\n")
  }
}

# 3
return_higher <- function(x, y){
  v <- c()
  for(i in 1:length(x)){
    if(is.na(x[i]) & is.na(y[i])){
      v[i]=NA
    }else if(is.na(x[i]) & !is.na(y[i])){
      v[i] = y[i]
    }else if(!is.na(x[i]) & is.na(y[i])){
      v[i] = x[i]
    }else if(x[i]>y[i]){
      v[i] = x[i]
    }else if(x[i]<=y[i]){
      v[i] = y[i]
    }
    v
  }
  
}

df <- df %>% mutate(high_expct=return_higher(x1stuedexpct, x1paredexpct))
df

## =============================================================================
## END SCRIPT
################################################################################
