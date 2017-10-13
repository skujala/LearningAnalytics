require(readxl)
require(tidyverse)


# List containing all the necessary metadata of the files
tulokset.meta = list(
  "2013"   = list(
    file="../2013/Mekaniikka 2013 tulokset_analysis.xlsx", 
    scales.et = rep(24, 10), scales.lh = rep(5,10), scale.matlab = 5,
    range = c(1:30, 48) # Do not use midterm exam maximum points
  ),
  "2014"   = list(
    file="../2014/Arvostelu_20150105.xlsx", 
    scales.et = rep(c(4,2), 5), scales.lh = c(12, rep(c(3,6),4), 3), scale.matlab = 15,
    range = c(1:32, 40) # Do not use midterm exam maximum points
  ),
  "2015"   = list(
    file="../2015/Valikokeet/Arvostelu.xlsx",
    scales.et = rep(c(4,2), 5), scales.lh = rep(4, 10), scale.matlab = 15,
    range = c(1:30, 34, 38, 42, 59, 63) # Do not use midterm exam maximum points
  ),
  "2016"   = list(
    file="../2016/ELEC-A3110_2016_tulokset.xlsx",
    scales.et = rep(3, 10), scales.lh = rep(4, 10), scale.matlab = 21,
    range = c(1:33, 41, 44)
  )
)

# Read all of the files at once, preprocess the data and massage in into a list of tibbles
tulokset = map(
  tulokset.meta, 
  function (x) {
    scale <- c(x$scales.et, x$scales.lh, x$scale.matlab)
    
    read_excel(x$file, sheet = 1, range = cell_cols(x$range)) %>%
      # Unify column names. Stupid me for not having done this years ago when creating the files
      # Attempt to follow the principles of reproducible research, so make no changes into the files
      # themselves!
      rename_all(
        funs(
          stringr::str_replace_all(., " ", "_") %>%
          tolower(.) %>%
          # These probably should be refactored into a map2 call for easier maintenance
          stringr::str_replace_all(., "id.number", "opnro") %>%
          stringr::str_replace_all(., "first.name", "etunimi") %>%
          stringr::str_replace_all(., "surname", "sukunimi") %>%
          stringr::str_replace_all(., "email.address", "email") %>%
          stringr::str_replace_all(., "lh0", "lh") %>%
          stringr::str_replace_all(., ".yht", "") %>%
          stringr::str_replace_all(., ".sum", ".totals") %>%
          stringr::str_replace_all(., "^matlab$", "matlab.1") %>%
          stringr::str_replace_all(., "matlab([12])", "matlab.\\1") %>%
          stringr::str_replace_all(., "kurssipalaute", "palautepiste") %>%
          stringr::str_replace_all(., "as", "arvosana")
        )
      ) %>%
      # Convert midterm results with zero points to NA's to signify student not being present
      mutate(vk1 = ifelse(vk1 == 0, NA, vk1),
             vk2 = ifelse(vk2 == 0, NA, vk2),
             vk3 = ifelse(vk3 == 0, NA, vk3)) %>%
      # Ugly(-ish) hack! 
      #
      # Nest the numeric data for easier scaling. A bit slow...involves creating many lists. 
      #
      # This would benefit from a refactor, but I do not know how to do
      # scale the data using dplyr pipelines, when the scaling depends on the variable being scaled. 
      nest(et1:et10, lh1:lh10, matlab.1) %>% 
        # Scale the scores to range 0-1, as the score ranges have changed over the years
        # This would be perfect spot to perform other computations, like fitting models 
        # or such-like
        mutate(data=map(data, ~ . / scale)) %>% 
      # Un-nest when we are done
      unnest(data) %>% 
      # Add column for the year. This is obtained from the file path. WILL BREAK if the files are fetched 
      # from somewhere else
      mutate(vuosi = as.numeric(unlist(strsplit(x$file, "/"))[2]))
  }
)

# Find common columns and form one data frame for all data 
tulokset.all <- bind_rows(
  map(
    tulokset, 
    function(x) {
      x %>% 
        select(!!Reduce(intersect, map(tulokset, colnames)))
    }
  )
)

# Now we have all the data in one huge data frame. Lovely. Start analyzing. 
# Remember that the data still students' names so it's not anonymous

