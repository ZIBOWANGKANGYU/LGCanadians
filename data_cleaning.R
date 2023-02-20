setwd("~/Documents/rshiny/example1/MyApp/")
data_raw <- read_csv("data/13100817.csv")
  
data <- data_raw %>%
  filter(
    `Selected Sociodemographic characteristics` %in% c(
      "Total population",
      "Visible minority",
      "Highest level of education (ages 25 and older), post-secondary certificate/diploma or university degree"
    ),
   Characteristics  == "Number of persons",
   Sex == "Both sexes",
   `Age group` == "Total, 15 years and over"
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

data_by_age <- 
  data_raw %>%
  filter(
    `Selected Sociodemographic characteristics` == "Total population",
    Characteristics  == "Number of persons",
    Sex == "Both sexes",
    GEO == "Canada"
  )

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
  
  data_percent <- data %>%
    select(GEO, Demographic, VALUE, `Sexual orientation`) %>%
    group_by(GEO, Demographic) %>%
    mutate(TOTAL_VALUE = sum(VALUE)) %>%
    ungroup() %>%
    filter(`Sexual orientation` == "Lesbian or gay") %>%
    mutate(lg_proportion = VALUE / TOTAL_VALUE) %>%
    mutate(lg_percent = scales::percent(lg_proportion, 0.1)) %>%
    select(GEO, Demographic, lg_percent)
    
  data_x_lim <- data %>%
    group_by(Demographic) %>%
    summarise(max_thousand = max(thousands))
  
  data_x_lim_dict <- set_names(data_x_lim[["max_thousand"]], data_x_lim[["Demographic"]])
  
  return(list(data_bars = data, data_percent = data_percent, data_x_lim_dict = data_x_lim_dict))
}

set_title <- \(demo_var)if_else(demo_var == "Race", "Proportion of Lesbian and Gay Canadians by Minority Status", "Proportion of Lesbian and Gay Canadians by Education")

summary_vars <- c("Race", "Education")
regions <- c("Canada",
             "British Columbia",
             "Prairie provinces",
             "Ontario",
             "Quebec")