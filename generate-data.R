library("countrycode")
library("dplyr")

data(codelist_panel)

dat <- codelist_panel %>% select(continent, wb_api3c, iso3c, country.name.en) %>% distinct() %>% rowwise() %>%
  mutate(basic.systems = sample(c("Less than basic", "Basic", "Essential", "Full", "Advanced", "No data"), 1)) %>%
  mutate(provision = sample(c("Less than basic", "Basic", "Essential", "Full", "Advanced", "No data"), 1, prob = c(1:6))) %>%
  mutate(overall = sample(c("Less than basic", "Basic", "Essential", "Full", "Advanced", "No data"), 1, prob = c(6:1))) %>%
  mutate(governance = sample(c("Yes","No", "No data"), 1, prob = c(69,14,17))) %>%
  mutate(income = sample(c("High income", "SIDS", "LDCs", "Low income"), 1)) %>%
  mutate(quantity = rnorm(1))


write.table(dat, file = "../data/simulated/data.csv", col.names = TRUE, row.names = FALSE, sep=",")

