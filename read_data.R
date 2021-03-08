library(tidyverse)

# read data 
df <- read_csv("SEDAC_CSI_All_Collections_v1-1.csv")

# drop the `Country_EPI` var b/c similar to `Country_Standard`
country_vars <- df %>% 
  select(Country_Standard, Country_EPI)
df <- df %>% 
  select(-Country_EPI)

# get the number of na values by row
na_rows <- rowSums(is.na(df))

# T/F for rows that have more than 300 NA vals (52 rows)
na_more_300 <- na_rows > 300

# rows/countries of `df` that have more than 300 NA vals
countries_with_na <- df %>% 
  select(Country_Standard) %>% 
  filter(na_more_300)
  # I noticed that the countries with a lot of missing values tend to be very
  # small islands so I will just drop those observations

df <- df[na_rows < 300,]

# make binary matrix, each element is TRUE if the element of df is NA, FALSE otherwise
na_mat <- is.na(df)
# check total proportion of missing data - it is almost 30%  --> **NOW 16.5%**
sum(na_mat)/(nrow(df)*ncol(df))
# look at the proportion of missingness for each variable 
col_prop <- colMeans(na_mat)
hist(col_prop)
# now, remove all variables that have 75% or less of their observations
df_sm_var <- df[,which(colMeans(na_mat)<.25)]
  # **Before there were 203 variables, now there are 393 variables**

# now, we have a dataset with 185 observations, but only 393 variables 
sum(is.na(df_sm_var))/(nrow(df_sm_var)*ncol(df_sm_var))
# it now has 9% missingness 
write.csv(df_sm_var, "data_fewer_vars.csv")

# use the list of col names to choose which variables I want
col_names <- names(df_sm_var)

# variables to keep
keep_vars <- c(6:11, 28:43, 45:50, 72:76, 78:90, 92:102, 105:112, 115, 116, 118:121,
               123:126, 176:216, 219:223, 225:229, 234:236, 242, 244, 245, 247, 251,
               253:256, 269:272, 317:325, 327, 332:335, 391:393)
# create smaller dataset w/ variables I want (159) => `keep_df`
keep_df <- df_sm_var[, keep_vars]

# check total proportion of missing data ~ 9.16%
na_val <- is.na(keep_df)
sum(na_val)/(nrow(keep_df)*ncol(keep_df))

keep_df[["Country_Standard"]] <- df_sm_var$Country_Standard # add country names back to last column

####################################### Imputation Process #############################################
# reference index & name of var using this list to categorize the variables
list_of_var <- names(keep_df)

# A list of small countries `sm_most_na` (found by looking for rows with most
# missing data)
na_by_row <- rowSums(is.na(keep_df))
small_country_rows <- na_by_row > 30 # if country has > 30 NA values => small country
countries_most_na <- keep_df %>% 
  select(Country_Standard) %>% 
  filter(small_country_rows) 

# find a model small country `model_sm_country` to impute data with
no_missing_countries <- na_by_row == 0
model_countries <- keep_df %>% # countries w/ no NAs
  select(Country_Standard) %>% 
  filter(no_missing_countries)
model_sm_country <- "Jamaica"
model_sm_row_index <- which(keep_df$Country_Standard == model_sm_country) # index*

# includes, Taiwan & Hong Kong & Singapore => remove Singapore from list
sm_most_na <- countries_most_na[-25,]

# general function for replacing NA values (mean)
replace_mean <- function(col_index_list) {
  for (column_index in col_index_list) {
    column <- keep_df[[column_index]]
    if (!all(!is.na(column))) { # if there exists a missing value (if not all of them are not NA)
      loop_range <- c(1:length(column))
      na_for_column <- loop_range[is.na(column)]
      for (row_index in na_for_column) {
        if (any(sm_most_na == (keep_df[1,] %>% pull(Country_Standard)))) {
          # we know the missing value for the column is associated with a small country
          column[row_index] <- column[model_sm_row_index]
        } else {
          # we know the missing value is for a general country => impute average
          column[row_index] <- mean(column, na.rm = T)
        }
      }
    }
    # replace the column in keep_df with the imputed column
    keep_df[column_index] <- column
  }
  keep_df
}

# general function for imputing NA values (median)
replace_median <- function(col_index_list) {
  for (column_index in col_index_list) {
    column <- keep_df[[column_index]]
    if (!all(!is.na(column))) { # if there exists a missing value (if not all of them are not NA)
      loop_range <- c(1:length(column))
      na_for_column <- loop_range[is.na(column)]
      for (row_index in na_for_column) {
        if (any(sm_most_na == (keep_df[1,] %>% pull(Country_Standard)))) {
          # we know the missing value for the column is associated with a small country
          column[row_index] <- column[model_sm_row_index]
        } else {
          # we know the missing value is for a general country => impute average
          column[row_index] <- median(column, na.rm = T)
        }
      }
    }
    # replace the column in keep_df with the imputed column
    keep_df[column_index] <- column
  }
  keep_df
}

# numerical variables (general ratio) [[DONE]]
general_index <- c(7:22, 36:42, 45:46, 51, 54:55, 57, 63, 65, 70:71, 76:89, 91:98,
                   101, 104:107, 109:112, 114:117, 119, 122, 124, 140:141, 153:154,
                   157:159)
keep_df <- replace_mean(general_index)


# ordinal variables that range from 1-100 [[DONE]]
range_100_index <- c(1:6, 23:28, 68, 127:138, 155:156)
keep_df <- replace_mean(range_100_index)

# variables that have percentage units [[DONE]]
percent_index <- c(29:35, 43:44, 47, 49, 52:53, 56, 64, 66:67, 90, 99:100, 102:103,
                   108, 113, 118, 120:121, 123, 125:126, 139, 142:152)
keep_df <- replace_mean(percent_index)

# ordinal variables that range from 1-7 [[DONE]]
range_7_index <- c(48, 60, 73:75)
keep_df <- replace_median(range_7_index)

# variables that are scores between 0 & 1 [[DONE]]
small_range_index <- c(69)
keep_df <- replace_median(small_range_index) 

# z-score (standardized scale) variables [[DONE]]
z_index <- c(58,59,61)
keep_df <- replace_median(z_index)

# `AGSUB` indicator is on a scale from 1-8 with 0 = NA
keep_df[[50]][keep_df[[50]] == 0] <- NA # replace 0s w/ NA
# noticed there are a lot of NA values -> remove this variable last **

# `POLITY` indicator (62) scores -> democracy score (contains negative values) [[DONE]]
keep_df <- replace_mean(62)

#  `EVI indicator` (72) scale for scores is between 174 & 450 [[DONE]]
keep_df <- replace_mean(72)

ordinal_df <- keep_df[,c(160, range_100_index, range_7_index, small_range_index, z_index, 62, 72)]
ratio_df <- keep_df[, c(160, general_index, percent_index)]
sm_ordinal_df <- keep_df[,c(160, range_7_index, small_range_index, z_index, 62)]
all_df <- keep_df[, c(160, general_index, range_100_index, percent_index, range_7_index,
                      small_range_index, z_index, 62, 72)] # removed the `AGSUB` indicator that had too many NA values 
write.csv(ordinal_df,"ordinal_df.csv")
write.csv(ratio_df, "ratio_df.csv")
write.csv(sm_ordinal_df, "sm_ordinal_df.csv")
write.csv(all_df, "all_df.csv")
# Check to make sure data has no NA values --> PASSED
sum(is.na(all_df))

######### Original code for general_index imputation:
# for (column_index in general_index) {
#   column <- keep_df[[column_index]]
#   if (!all(!is.na(column))) { # if there exists a missing value (if not all of them are not NA)
#     loop_range <- c(1:length(column))
#     na_for_column <- loop_range[is.na(column)]
#     for (row_index in na_for_column) {
#       if (any(sm_most_na == (keep_df[1,] %>% pull(Country_Standard)))) {
#         # we know the missing value for the column is associated with a small country
#         column[row_index] <- column[model_sm_row_index]
#       } else {
#         # we know the missing value is for a general country => impute average
#         column[row_index] <- mean(column, na.rm = T)
#       }
#     }
#   }
#   # replace the column in keep_df with the imputed column
#   keep_df[column_index] <- column
# }
