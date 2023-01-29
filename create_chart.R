summary_vars <- c("Race", "Education")

chart_race <- data %>%
  filter(Demographic %in% c("Visible minority", "Total population")) %>% 
  filter(!(GEO %in% c(
    "Canada", "Atlantic provinces", "Territories"
  ))) %>%
  mutate(
    GEO = fct_relevel(
      GEO,
      "British Columbia",
      "Prairie provinces",
      "Ontario",
      "Quebec"
    ),
    Demographic = fct_relevel(Demographic, "Visible minority", "Total population")
  ) %>%
  mutate() %>%
  drop_na() %>%
  ggplot(aes(y = GEO, x = thousands)) +
  facet_grid(cols = vars(`Sexual orientation`), scales = "free_x") + 
  geom_col(aes(fill = Demographic), position = position_dodge()) +
  labs(title = "Lesbian and Gay Canadians") +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())

chart_education <- data %>%
  filter(Demographic %in% c("Post-secondary education", "Total population")) %>%
  filter(!(GEO %in% c(
    "Canada", "Atlantic provinces", "Territories"
  ))) %>%
  mutate(
    thousands = VALUE / 1000,
    GEO = fct_relevel(
      GEO,
      "British Columbia",
      "Prairie provinces",
      "Ontario",
      "Quebec"
    ),
    Demographic = fct_relevel(Demographic, "Post-secondary education", "Total population")
  ) %>%
  mutate() %>%
  drop_na() %>%
  ggplot(aes(y = GEO, x = thousands)) +
  geom_col(aes(fill = Demographic), position = position_dodge()) +
  facet_grid(cols = vars(`Sexual orientation`), scales = "free_x") + 
  labs(title = "Lesbian and Gay Canadians") +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())