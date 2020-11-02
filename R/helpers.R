# Helper functions

clean_csv_col_names <- function(df) {
  # Clean AOI column names
  x <- purrr::set_names(~ str_to_lower(df) %>%
                     str_replace_all("AOI|aoi", "") %>%
                     str_replace_all("\\[", "") %>%
                     str_replace_all("\\]", "") %>%
                     str_replace_all("[Hh]it", ""))
  
  
}

make_narrow_long <- function(df) {
  require(tidyverse)
  x <- tidyr::pivot_longer(
    cols = `1c-wide-swing`:`32x-wide-all_1`,
    names_pattern = "([0-9]+)([abcx])\\-([a-z]+)\\-([a-z_1]+)",
    names_to = c("trial", "category", "layout", "img"),
    values_to = "count"
  )
  x <- dplyr::filter(x, count > 0)
}
