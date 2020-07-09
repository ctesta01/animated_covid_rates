
#' Load USAFACTS COVID-19 Data
load_usafacts_data <- function(export = T) {

  filenames <- paste0("usafacts/", 
    c(
      'covid_confirmed_usafacts.csv',
      'covid_county_population_usafacts.csv',
      'covid_deaths_usafacts.csv'
    ))

  usafacts <- list(
    confirmed = readr::read_csv(filenames[[1]]),
    population = readr::read_csv(filenames[[2]]),
    deaths = readr::read_csv(filenames[[3]])
  )

  # convert confirmed & deaths into a tidy format
  usafacts %<>% clean_usafacts()

  if (export) {
    usafacts <<- usafacts
  }

  return(invisible(usafacts))
}

#' Clean USAFACTS
clean_usafacts <- function(usafacts) {

  usafacts$confirmed %<>% 
    pivot_longer(
      cols = colnames(usafacts$confirmed)[5:ncol(usafacts$confirmed)],
      names_to = "date",
      values_to = 'confirmed') %>% 
    mutate(
      date = lubridate::mdy(gsub("X", "", date)))

  usafacts$deaths %<>% 
    pivot_longer(
      cols = colnames(usafacts$deaths)[5:ncol(usafacts$deaths)],
      names_to = "date",
      values_to = 'deaths') %>% 
    mutate(
      date = lubridate::mdy(gsub("X", "", date)))

  return(invisible(usafacts))
}
