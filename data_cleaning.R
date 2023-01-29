setwd("~/Documents/rshiny/example1/MyApp/")
data <- read_csv("data/1310081701_databaseLoadingData.csv")

data <- data %>%
  filter(
    `Selected Sociodemographic characteristics` %in% c(
      "Total population",
      "Visible minority",
      "Highest level of education (ages 25 and older), post-secondary certificate/diploma or university degree"
    )
  ) %>%
  mutate(
    `Selected Sociodemographic characteristics` = if_else(
      `Selected Sociodemographic characteristics` == "Highest level of education (ages 25 and older), post-secondary certificate/diploma or university degree",
      "Post-secondary education",
      `Selected Sociodemographic characteristics`
    )
  ) %>%
  filter(`Sexual orientation` %in% c("Heterosexual", "Lesbian or gay")) %>%
  select(GEO, `Selected Sociodemographic characteristics`, VALUE, `Sexual orientation`) %>%
  rename(Demographic = `Selected Sociodemographic characteristics`) %>%
  mutate(thousands = VALUE / 1000)
