library(tidytuesdayR)
library(tidyverse)
library(glue)
library(extrafont)
library(countrycode)
library(lubridate)
library(priceR)
library(tidymodels)

font_import()
loadfonts(device = "win")


tuesdat <- tidytuesdayR::tt_load(2021,week = 2)
df <- tuesdat$transit_cost
df <- df %>% filter(!is.na(year), !is.na(currency))
df <- df %>% filter(year >= 2000)

bigmac <- tt_load(2020,week = 52)$`big-mac`

bigmac <- bigmac %>% mutate(year = year(date), month = month(date)) %>%
  select(year, month, date, iso_a3, currency_code, name, dollar_ex, usd_adjusted, usd_raw, gdp_dollar) %>% 
  group_by(year, iso_a3, currency_code, name) %>% summarise(usd_exch = median(dollar_ex), usd_adj = median(usd_adjusted), 
                                                            usd_raw = median(usd_raw), 
                                                            gdp_dollar = median(gdp_dollar, na.omit = TRUE)) %>% 
  rename("currency" = "currency_code") %>% 
  mutate(country = countrycode(iso_a3, origin = "iso3c", destination = "iso2c"))

df <- left_join(df, bigmac, by = c("year", "currency")) 

# function to get the exchange rate for the remainder who does not have any exchange rate from the bigmac 
#' @param year the year to query
#' @param currency currency id as 3 letter abbreviations
get_exchange <- function(year, currency){
  if (currency == "USD"){
    return(1)
  } else {
    if (year >= 2021){
      start <- "2021-01-01"
    } else {
      start <- paste0(year, "-06-01")
    }
    rate <- historical_exchange_rates(from = "USD", to = currency, start_date = start, end_date = start)[,2]
    return(rate)  
  }
}

# Only get those that do not have exchange rates 
exch_rate <- vector(length = nrow(df))
for (i in 1:nrow(df)){
  if (!is.na(df$usd_exch[i])){
    exch_rate[i] <- df$usd_exch[i]
  } else {
    exch_rate[i] <- get_exchange(year = df$year[i], currency = df$currency[i])
  }
}

df$usd_exch <- exch_rate
df <- df %>% mutate(usd_cost = cost/usd_exch) %>% select(-c(reference, source1, country.y, source2)) %>% 
  mutate(usd_cost_km = usd_cost/length) %>% mutate(tunnel_prop = tunnel/length) %>% 
  mutate()

# simple linear model to get the conditional expectation  
mod <- lm(data = df, formula = usd_cost_km ~ tunnel_prop + stations + length)

summary(mod)

reduced_df <- df %>% select(ppp_unadj_km, tunnel_prop, gdp_dollar, stations, length)


model <- lm(data = reduced_df, formula = ppp_unadj_km ~ tunnel_prop + gdp_dollar + stations + length)

df %>% mutate(pred_price = predict(model, newdata = df)) %>% ggplot(aes(pred_price, cost_km_millions)) + geom_point()

df %>% mutate(yr_diff = case_when(
  str_ends(start_year, "years") == TRUE ~ as.numeric(strsplit(start_year, " ")[[1]][1]),
  TRUE ~ as.numeric(end_year) - as.numeric(start_year)
)) %>% slice(254) %>% View()

case_when(
  str_ends(df$start_year, "years") ~ as.numeric(strsplit(df$start_year, " ")[[1]][1])
)

strsplit(df$start_year[254], " ")[[1]][1] %>% as.numeric()

