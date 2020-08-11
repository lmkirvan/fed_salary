library(httr)
library(jsonlite)
library(dplyr)
library(progress)

get_data <- function(year, page_length = 50000, with_progress = TRUE){
  
  get_response <- function(url = "https://www.fedsdatacenter.com/federal-pay-rates/output.php", 
                           start = 0, 
                           length = page_length, 
                           as = "text", 
                           year = year,
                           with_progress = with_progress){
    
    #if(with_progress){
   #   pb$tick()
   # }
    
    resp <- 
      GET(url, query = list(
        y = year
        ,sEcho = 1
        ,iColumns = 9
        ,sColumns = ",,,,,,,,"
        ,iDisplayStart = start
        ,iDisplayLength = length
        ,iSortCol_0 = 0
        ,sSortDir_0 = "asc"
        ,iSortingCols = 1
      )
      )
    content(resp, as = as)
  }
  
  safe_response <- purrr::safely(get_response, otherwise =  "try again")
  
  initial_response <- 
    safe_response(start = 0, length  = 0, year = year, with_progress =FALSE)
  
  total_records <- 
    jsonlite::fromJSON(initial_response$result)$iTotalDisplayRecords %>% 
    as.integer()
  
  pages <- seq(0, total_records, page_length) 
  diff <- total_records - dplyr::last(pages)
  lens <- c(rep(page_length, length(pages) - 1), diff )
  pages <- pages %>% as.integer() %>% as.character()
  
 # pb <- progress::progress_bar$new(total = length(pages))
  
  purrr::map2(
    pages, lens, ~safe_response(start = .x, length = .y, year = year))
  
  }

get_tibble <- function(json_string){
  mat <- jsonlite::fromJSON(json_string)$aaData 
  colnames(mat) <- c("name"
                  , "grade"
                  , "pay plan"
                  , "salary"
                  , "bonus"
                  , "agency"
                  , "location"
                  , "occupation"
                  , "fy")  
  
  as_tibble(mat)
}

data_files <- list.files(pattern = "*.Rdata")

load("data_2015.Rdata")
load("data_2016.Rdata")
load("data_2017.Rdata")
load("data_2018.Rdata")
results_2019 <- read_rds("results_2019.rds")

results_2015 <- purrr::map(data_2015, ~ .$result) %>% 
  purrr::map_df(get_tibble)

results_2016 <- purrr::map(data_2016, ~ .$result) %>% 
  purrr::map_df(get_tibble)

results_2017 <- purrr::map(data_2017, ~ .$result) %>% 
  purrr::map_df(get_tibble)

results_2018 <- purrr::map(data_2018, ~ .$result) %>% 
  purrr::map_df(get_tibble)

results_2019 <- purrr::map(results_2019, ~ .$result) %>% 
  purrr::map_df(get_tibble)

df <- bind_rows(
  results_2015,
  results_2016,
  results_2017,
  results_2018,
  results_2019
)

#this is slow, should probably just do the whole thing with two regexes
cash_to_int <- function(cash_vec){
  
  remove_cents <- function(cash_vec) {
    stringr::str_sub(cash_vec, end = -4)
  }
  
  has_cents <- function(cash_vec) {
    stringr::str_detect(cash_vec, ".+\\.[0-9][0-9]")
  }
  
  result <-  map2_chr(has_cents(cash_vec), cash_vec, ~if(.x){remove_cents(.y)} )
  
  stringr::str_remove_all(string = result , pattern = "\\$|\\,") %>% 
    as.integer()
}

df$salary_int <- cash_to_int(df$salary)

readr::write_rds(df, "fed_data.rds")

