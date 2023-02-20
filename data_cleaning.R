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
    `Selected Sociodemographic characteristics` %in% c(
      "Total population",
      "Visible minority"
      ),
    Characteristics  == "Number of persons",
    Sex == "Both sexes",
    GEO == "Canada"
  ) %>%
  rename(Demographic = `Selected Sociodemographic characteristics`) %>%
  filter(`Sexual orientation` %in% c("Heterosexual", "Lesbian or gay")) %>%
  mutate(`Sexual orientation` = fct_relevel(`Sexual orientation`, "Heterosexual", "Lesbian or gay")) %>%
  select(GEO, `Sexual orientation`, Demographic, `Age group`, VALUE)

data_by_age_all <- data_by_age %>%
  filter(`Age group` == "Total, 15 years and over") %>%
  rename(VALUE_ALL = VALUE) %>%
  select(-`Age group`)

data_by_age <- data_by_age %>%
  left_join(data_by_age_all, by = c("GEO", "Sexual orientation", "Demographic")) %>%
  filter(`Age group` != "Total, 15 years and over") %>%
  mutate(`Age group` = fct_relevel(`Age group`, "15 to 24 years", "25 to 64 years", "65 years and over"),
         age_prop = VALUE / VALUE_ALL)
  

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
      `Sexual orientation` = fct_relevel(`Sexual orientation`, "Lesbian or gay", "Heterosexual"),
      Population = paste0(thousands, "k")
    ) %>%
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

filter_data_by_age <- function(data_by_age, demo_var){
 if (demo_var == "Race"){
   data_by_age <- data_by_age %>%
     mutate(`Sexual orientation` = fct_relevel(`Sexual orientation`, "Lesbian or gay", "Heterosexual"),
            Population = paste0(round(100 * age_prop, 0), "%"))
   return(data_by_age)
 } else {
   return(NULL)
 }
}

set_title <- \(demo_var)if_else(demo_var == "Race", "Proportion of Lesbian and Gay Canadians by Minority Status", "Proportion of Lesbian and Gay Canadians by Education")

summary_vars <- c("Race", "Education")
regions <- c("Canada",
             "British Columbia",
             "Prairie provinces",
             "Ontario",
             "Quebec")