require(readxl)
require(tidyverse)
library(doMC)


registerDoMC(cores = 2)

library(caret)


# Copied from https://github_com/OpenIntroOrg/oilabs-r-package/blob/master/R/rep_sample_n_R
# no oilabs package in cran

#' Repeating sampling.
#' 
#' @param tbl tbl of data.
#' @param size The number of rows to select.
#' @param replace Sample with or without replacement?
#' @param reps The number of samples to collect.
#' @return A tbl_df that aggregates all created samples, with the addition of a \code{replicate} column that the tbl_df is also grouped by
#' @export

rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1)
{
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))
  
  rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])
  
  dplyr::group_by(rep_tbl, replicate)
}



# List containing all the necessary metadata of the files
tulokset_meta = list(
  "2013"   = list(
    file="../2013/Mekaniikka 2013 tulokset_analysis.xlsx", 
    scales_et = rep(24, 10), scales_lh = rep(5,10), scale_matlab = 5,
    range = c(1:30, 48) # Do not use midterm exam maximum points
  ),
  "2014"   = list(
    file="../2014/Arvostelu_20150105.xlsx", 
    scales_et = rep(c(4,2), 5), scales_lh = c(12, rep(c(3,6),4), 3), scale_matlab = 15,
    range = c(1:32, 40) # Do not use midterm exam maximum points
  ),
  "2015"   = list(
    file="../2015/Valikokeet/Arvostelu.xlsx",
    scales_et = rep(c(4,2), 5), scales_lh = rep(4, 10), scale_matlab = 15,
    range = c(1:30, 34, 38, 42, 59, 63) # Do not use midterm exam maximum points
  ),
  "2016"   = list(
    file="../2016/ELEC-A3110_2016_tulokset.xlsx",
    scales_et = rep(3, 10), scales_lh = rep(4, 10), scale_matlab = 21,
    range = c(1:33, 41, 44)
  )
)

# Read all of the files at once, preprocess the data and massage in into a list of tibbles
tulokset = map(
  tulokset_meta, 
  function (x) {
    scale <- c(x$scales_et, x$scales_lh, x$scale_matlab)
    
    read_excel(x$file, sheet = 1, range = cell_cols(x$range)) %>%
      # Unify column names. Stupid me for not having done this years ago when creating the files
      # Attempt to follow the principles of reproducible research, so make no changes into the files
      # themselves!
      rename_all(
        funs(
          stringr::str_replace_all(., " ", "_") %>%
          tolower(.) %>%
          # These probably should be refactored into a map2 call for easier maintenance
          stringr::str_replace_all(., "id_number", "opnro") %>%
          stringr::str_replace_all(., "first_name", "etunimi") %>%
          stringr::str_replace_all(., "surname", "sukunimi") %>%
          stringr::str_replace_all(., "email_address", "email") %>%
          stringr::str_replace_all(., "lh0", "lh") %>%
          stringr::str_replace_all(., ".yht", "") %>%
          stringr::str_replace_all(., ".sum", ".totals") %>%
          stringr::str_replace_all(., "^matlab$", "matlab.1") %>%
          stringr::str_replace_all(., "matlab([12])", "matlab.\\1") %>%
          stringr::str_replace_all(., "kurssipalaute", "palautepiste") %>%
          stringr::str_replace_all(., "as", "arvosana")
        )
      ) %>%
      # Select only distinct values
      distinct() %>%
      #
      # Here follows an ugly(-ish) hack! 
      #
      # Nest the numeric data for easier scaling. A bit slow...involves creating many lists. 
      #
      # This would benefit from a refactor, but I do not know how to do
      # scale the data using dplyr pipelines, when the scaling depends on the variable being scaled. 
      nest(et1:et10, lh1:lh10, matlab.1) %>% 
        # Scale the scores to range 0-1, as the score ranges have changed over the years
        # This would be perfect spot to perform other computations, like fitting individual models 
        # or such-like
        mutate(data=map(data, ~ . / scale)) %>% 
      # Un-nest when we are done
      unnest(data) %>% 
      # Add column for the year. This is obtained from the file path. WILL BREAK if the files are fetched 
      # from somewhere else
      mutate(vuosi = as.numeric(unlist(strsplit(x$file, "/"))[2])) %>%
      # Convert all NA's to zeros for modeling
      mutate_if(is.numeric, function(x) {if_else(is.na(x), 0, x)}) %>%
      # Form sums
      mutate(
        et_sum = rowSums(select(., et1:et10), na.rm=T),
        lh_sum = rowSums(select(., lh1:lh10), na.rm=T)
      )
  }
)

# Find common columns and form one data frame for all data 
tulokset_all <- bind_rows(
  map(
    tulokset, 
    function(x) {
      x %>% 
        select(!!Reduce(intersect, map(tulokset, colnames)))
    }
  )
)


# Now we have all the data in one huge data frame. Lovely. Start analyzing. 
# Remember that the data still contains students' names so it's not anonymous

# Manual analysis:
# check if there is a correlation between students accumulated points from the first 
# few weeks and their final score
#
tulokset_analyysi <- tulokset_all %>% 
  mutate(
    puolet_tehty = if_else(rowSums(select(., et1:et3,lh1:lh3), na.rm=T) >= .5 * 6, "Kyllä", "Ei"), 
    vk1_paikalla = if_else(vk1 > 0, "Ei", "Kyllä"),
    kurssi_lapi = if_else(arvosana > 0, "Kyllä", "Ei")
  ) %>% 
  # Filter out ghost students, i.e., those who have done nothing in the course
  #
  # They represent unnecessary bias for this analysis
  #
  filter(et_sum > 0 & lh_sum > 0) %>% 
  select(puolet_tehty, vk1_paikalla, kurssi_lapi)

# now the data is anonymous

lapaisyprosentit <- tulokset_analyysi %>% 
  group_by(puolet_tehty) %>% 
  summarize(mean(kurssi_lapi == "Kyllä"))

# Check if the seen difference in average number of students passing the course
# when they have obtained > 50 % of first week's points is due to random chance
# or not.

# Shuffle data to obtain random permutations of the data
tulokset_analyysi_perm <- tulokset_analyysi %>%
  rep_sample_n(size = nrow(tulokset_analyysi), reps = 20000) %>%
  mutate(kurssi_lapi_perm = sample(kurssi_lapi)) %>%
  group_by(replicate, puolet_tehty) %>%
  summarize(prop_kurssi_lapi_perm = mean(kurssi_lapi_perm == "Kyllä"),
            prop_kurssi_lapi = mean(kurssi_lapi == "Kyllä")) %>%
  summarize(diff_perm = diff(prop_kurssi_lapi_perm),
            diff_orig = diff(prop_kurssi_lapi))  # treatment - control

# Calculate P-value = the probability of null hypothesis 
# (failing while not having done at least 50 % is due to random variation)
p_val <- tulokset_analyysi_perm %>%
  summarize(mean(diff_perm >= diff_orig))

print(p_val)


### NEXT, ATTTEMPT MACHINE LEARNING ###

## This is really just for me to teach myself machine learning


#
# Fit a GLMNET model on the data
#

# Prepare dataset for classification
tulokset.glmnet <- tulokset_all %>%
  # Add classification variable -- did the student pass the course or not?
  mutate(lapi = if_else(arvosana > 0, "Kyllä", "Ei")) %>% 
  select(lapi, et1:et3, lh1:lh3, vk1) 
  

# Custom training control
# 10 times cross validation
#
# Would it make more sense to split the data so that each year is represented equally well?
# Would it bias the data otherwise?
#
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T,
  verboseIter = TRUE
)


# Train the model. 
model <- train(
  lapi ~ ., 
  tulokset.glmnet, 
  metric = "ROC", 
  method="glmnet", 
  trControl = myControl
)


# Model done. Now, predict.