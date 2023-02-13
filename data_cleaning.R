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


filter_data <- function(data, geo_names, demo_var){
  
  if (is.null(geo_names)) {
    geo_select <- c("Canada",
                    "British Columbia",
                    "Prairie provinces",
                    "Ontario",
                    "Quebec")
  } else{
    geo_select <- geo_names
  }

  demo_select <- case_when(demo_var == "Race" ~ "Visible minority",
                           demo_var == "Education" ~ "Post-secondary education")
  
  
  data <- data %>%
    filter(Demographic %in% c("Total population", demo_select)) %>% 
    filter(GEO %in% geo_select) %>%
    mutate(
      GEO = fct_relevel(
        GEO,
        "Canada",
        "British Columbia",
        "Prairie provinces",
        "Ontario",
        "Quebec"
      ),
      Demographic = fct_relevel(Demographic, "Visible minority", "Post-secondary education", "Total population"),
      `Sexual orientation` = fct_relevel(`Sexual orientation`, "Lesbian or gay", "Heterosexual")
    ) %>%
    mutate() %>%
    drop_na()
  
  return(data)
}

summary_vars <- c("Race", "Education")
regions <- c("Canada",
             "British Columbia",
             "Prairie provinces",
             "Ontario",
             "Quebec")