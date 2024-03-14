library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

# metadata
pubs <- read_csv("/Users/andrewtyner/Documents/GitHub/SCORE-P2/metadata/CR_metadata.csv")
fields <- read_csv("/Users/andrewtyner/Documents/GitHub/SCORE-P2/team_resources/data/publications.csv") %>% 
  mutate(field = COS_pub_category) %>% 
  mutate(
    field = ifelse(COS_pub_category %in% c("marketing/org behavior", "management"), "business", field),
    field = ifelse(COS_pub_category == "criminology", "sociology", field),
    field = ifelse(COS_pub_category == "public administration", "political science", field),
    field = ifelse(COS_pub_category == "health", "psychology", field)
  )

pr_raw <- read_csv("PR_data_final_with_metadata.csv")
pr <- pr_raw %>% filter(!covid) %>% 
  select(paper_id, year, pr_data_available, pr_code_available) %>% 
  rename(d = pr_data_available, code = pr_code_available) %>%
  mutate(
    across(
      .cols = c(d, code),
      .fns = function(x) str_detect(x, "Yes")
    )
  ) %>% 
  left_join(pubs %>% select(paper_id, publication_standard), by = "paper_id") %>% 
  left_join(fields %>% select(publication_standard, field), by = "publication_standard") %>% 
  select(-publication_standard) %>% 
  mutate(field = str_to_sentence(field)) %>% 
  mutate(
    avail = case_when(
      d & !code ~ "data",
      !d & code ~ "code",
      d & code ~ "both",
      !d & !code ~ "neither"
    )
  ) %>% 
  mutate(avail = as_factor(avail) %>% fct_relevel(., "code", "data", "both", "neither")) %>% 
  group_by(field) %>% mutate(pr_ord = mean(avail == "both")) %>% 
  ungroup() %>% group_by(avail != "neither") %>% 
  arrange(avail) %>% ungroup() %>% select(-`avail != "neither"`)


font_add_google("Open sans", "open sans")
showtext_auto()

cats <- pr %>% 
  group_by(field, avail != "neither") %>% 
  mutate(rn = row_number()) %>% 
  mutate(cat = percent_rank(rn)) %>% 
  ungroup() %>% select(-`avail != "neither"`) %>% 
  replace_na(list(cat = 1)) %>% 
  mutate(field = as_factor(field) %>% fct_reorder(., pr_ord) %>% fct_rev()) %>% 
  mutate(cat = ifelse(field == "Sociology" & cat == 0, 0.79, cat)) %>% 
  mutate(cat = ifelse(field == "Economics" & avail == "neither" & cat == 0, 0.79, cat))

pr %>% 
  ggplot() +
  geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.8), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = -0.26)) +
  geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.6 & cat < 0.8), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = -0.13)) + 
  geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.4 & cat < 0.6), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = 0)) +
  geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.2 & cat < 0.4), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = 0.13)) +
  geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat < 0.2), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = 0.26)) +
  geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.8), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = -0.26)) +
  geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.6 & cat < 0.8), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = -0.13)) +
  geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.4 & cat < 0.6), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = 0)) +
  geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.2 & cat < 0.4), 
               aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = 0.13)) +
  geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat < 0.2), 
               aes(reorder(field, -pr_ord), fill = avail), binwidth = 1/3, dotsize = 0.4,
               stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = 0.26)) +
  geom_text(data = pr, aes(x = field, y = 1, label = glue("{(pr_ord %>% round(2))*100}%")), 
            size = 30, inherit.aes = F, family = "open sans") +
  scale_fill_manual(
    limits = c("neither", "code", "data", "both"),
    values = c("tomato3", "seagreen3", "khaki3", "deepskyblue4"),
    labels = c("Neither", "Code", "Data", "Code + data")
  ) +
  coord_flip() +
  labs(
    x = "",
    y = "",
    title = "",
    fill = "",
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 100, family = "open sans", margin = margin(r = 300), hjust = 0.5),
    #axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 100),
    text = element_text(family="open sans")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 100)))