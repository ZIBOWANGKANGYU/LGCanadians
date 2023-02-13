
filter_data <- function(data, geo_names, demo_var){
  
  geo_select <- if_esle(is.null(geo_names), c("Canada",
                                              "British Columbia",
                                              "Prairie provinces",
                                              "Ontario",
                                              "Quebec"), geo_names)
  
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

chart_race <- data %>%
  ggplot(aes(y = GEO, x = thousands)) +
  geom_col(aes(fill = `Sexual orientation`), position = position_dodge()) +
  facet_grid(cols = vars(Demographic), scales = "free_x") + 
  labs(title = "Lesbian and Gay Canadians") +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())

chart_education <- data %>%
  ggplot(aes(y = GEO, x = thousands)) +
  geom_col(aes(fill = `Sexual orientation`), position = position_dodge()) +
  facet_grid(cols = vars(Demographic), scales = "free_x") + 
  labs(title = "Lesbian and Gay Canadians") +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())
