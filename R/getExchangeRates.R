getExchangeRates <- function(api_key, base = "USD") {
  url <- paste0("https://openexchangerates.org/api/latest.json?app_id=",api_key,"&base=", base)

  httr::GET(url) %>%
    httr::content(., as = "text") %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("rates")
}

getUSDExchangeRate <- function(api_key, currency = "ZMW") {

  #Getting rates via USD seems to be more reliable
  rates <- getExchangeRates(api_key, base = "USD")

  ex_rate <- rates %>% purrr::pluck(currency)

  return(1/ex_rate)
}
