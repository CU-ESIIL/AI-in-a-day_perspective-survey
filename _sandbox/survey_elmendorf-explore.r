## -------------------------------------------- ##
# SCE's Exploration of the Survey Data
## -------------------------------------------- ##
# Purpose
## Check out parts of the survey data that feel interesting but might not be interesting beyond myself.

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR, lme4, RColorBrewer, patchwork, ggplot2, maps)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

# Load any custom functions
purrr::walk(.x = dir(path = file.path("tools"), 
    pattern = "*.r", full.names = TRUE),
  .f = ~ source(file = .x))

real_data = TRUE
# Read in data
if (real_data) {
svy_v01 <- read.csv(file.path("data", "01_tidied-responses.csv")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .)))
    graph_path = 'graphs'
} else{
    svy_v01 <- read.csv(file.path("data", "broken-row-survey-data.csv")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .)),
  ResponseId = row_number())
  graph_path = 'graphs_fake'
}

# Check structure
# dplyr::glimpse(svy_v01)

# Read in survey question lookup table too
lkup <- read.csv(file.path("data", "01_question-lookup-table.csv"))
# dplyr::glimpse(lkup)

## -------------------------------------------- ##
# Geography ----
## -------------------------------------------- ##
library()

# Get world polygon geometry
world_sf <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
  left_join(., (svy_v01 %>%
                  mutate(Country = 
                           case_when(Country ==  'United States of America' ~ 'USA',
                                     Country ==  'United Kingdom' ~ 'UK',
                                     Country ==  'U.S. Virgin Is.' ~ 'Virgin Islands, US',
                                     TRUE ~ Country
                           )
                  ) %>%
                 group_by(Country)%>%
              dplyr::summarise(total_n = dplyr::n())), by = c('ID' = 'Country'))

ggplot(world_sf) +
  geom_sf(aes(fill = total_n), colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(trans = "log10",option = "turbo", na.value = "grey90", 
                       breaks = c(1, 10, 100, 400)) +
  labs(title = "Survey respondents by country",
       fill = "Total") +
  supportR::theme_lyon(title_size = 20, text_size = 16)+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=16))


# Export locally
ggsave(file.path(graph_path, "respondents_by_country.png"),
       height = 15, width = 15, units = "in")

## -------------------------------------------- ##
# Training Received vs Training Desired ----
## -------------------------------------------- ##

# Prep both 'training received' and 'training desired' dataframes
trainrec_df <- prep_select_all(df = svy_v01, q = "Training_Received", summarize = FALSE) %>% 
  dplyr::rename_with(.fn = ~ paste0("had_", .), .cols = -value) %>%
  mutate(response = "Yes", type = 'had')%>%
  rename(ResponseId = had_ResponseId)
traindes_df <- prep_select_all(df = svy_v01, q = "Training_Desired", summarize = FALSE) %>% 
  dplyr::rename_with(.fn = ~ paste0("want_", .), .cols = -value)%>%
  mutate(response = "Yes", type = 'want')%>%
  rename(ResponseId = want_ResponseId)

prof_df <- prep_select_all(df = svy_v01, q = "Prof_Role", summarize = FALSE) 

non_students <-prof_df %>%
  filter(value == 'Student')%>%
  select(ResponseId) %>%
  anti_join(prof_df, ., by = 'ResponseId')

# fill out the NO's by difference
train_binary <- bind_rows(trainrec_df, traindes_df)

not_response = expand_grid(ResponseId = unique(train_binary$ResponseId),
value  = unique(train_binary$value),
type = unique(train_binary$type)) %>%
  anti_join(., train_binary, by = c("ResponseId", "value", "type")) %>%
  mutate(response = "No")

# Join the selected and inferred not selected
train_binary <- bind_rows(train_binary, not_response) %>%
  rename(learning_mode = value) %>%
  mutate(response_numeric = ifelse(response == "Yes", 1, 0))

# Check structure
dplyr::glimpse(train_binary)

#relevel so the intercepts reflect what people WANT to learn
train_binary$type <- relevel(factor(train_binary$type), ref = "want")

# random intercepts as subjects not distinct
m_no_int_want_ref_baseline <- glmer(
  response_numeric ~ 0 + learning_mode + learning_mode:type + (1 | ResponseId),
  family = binomial,
  data = train_binary,
  #may not need this funkiness if working with real data where the ranefs should be more estimable
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# this one is probably more correct if it's not singular;
# would assume some people just overall WANT more training and/or HAD
# more training, but not that the same people want/have more training
m_no_int_want_ref_random <- glmer(
  response_numeric ~ 0 + learning_mode + learning_mode:type + (1 + type | ResponseId),
  family = binomial,
  data = train_binary,
  #may not need this funkiness if working with real data where the ranefs should be more estimable
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

anova(m_no_int_want_ref_baseline, m_no_int_want_ref_random)

coefs <- summary(m_no_int_want_ref_random)$coefficients
# coefs

###### Aggregate for plotting
# Prep both 'training received' and 'training desired' dataframes
trainrec_df <- prep_select_all(df = svy_v01, q = "Training_Received", summarize = TRUE) %>% 
  dplyr::rename_with(.fn = ~ paste0("had_", .), .cols = -value)
traindes_df <- prep_select_all(df = svy_v01, q = "Training_Desired", summarize = TRUE) %>% 
  dplyr::rename_with(.fn = ~ paste0("want_", .), .cols = -value)

# Join the two questions' prepared dfs and calculate difference
train_v01 <- dplyr::full_join(x = trainrec_df, y = traindes_df, by = "value") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40))

# Check structure
dplyr::glimpse(train_v01)

# Prepare this for graphing of want/had
train_v02 <- train_v01 %>% 
  dplyr::select(value, dplyr::ends_with("percent")) %>% 
  tidyr::pivot_longer(cols = -value, values_to = "percent_responses") %>% 
  dplyr::mutate(status = ifelse(name == "had_percent",
    yes = "received", no = "desires"))

# Check structure
dplyr::glimpse(train_v02)

# Extract stats from model
p_labels <- as.data.frame(coefs) |>
  tibble::rownames_to_column("term") |>
  # pull just the interaction terms
  dplyr::filter(grepl(":typehad", term)) |>
  dplyr::mutate(
    value = stringr::str_remove(term, "^learning_mode"),
    value = stringr::str_remove(value, ":typehad$"),
    value = stringr::str_wrap(value, width = 40),
    p_label = ifelse(`Pr(>|z|)` < 0.05, "*", "")
  ) |>
  dplyr::select(value, p_label)

# Join to plotting data and make max percent per mode for label placement
train_plot <- train_v02 |>
  dplyr::left_join((p_labels %>% mutate(name = 'had_percent')), by = c("name", "value")) |>
  dplyr::group_by(value) |>
  dplyr::mutate(label_x = max(percent_responses) + 2) |>
  dplyr::ungroup()

# Reorder value factor by percent_responses for 'desires' status
train_plot <- train_plot |>
  mutate(value = forcats::fct_reorder(value, percent_responses, 
                                       .fun = function(x) mean(train_plot$percent_responses[train_plot$status == "desires"])))

# Plot
ggplot(train_plot, aes(x = percent_responses, y = value, fill = status)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(x = label_x, label = p_label), 
            position = position_dodge(width = 0.9),
            hjust = 0, size = 3.5, na.rm = TRUE) +
  scale_fill_manual(values = c("received" = "#1b9e77", "desires" = "#d95f02")) +
  labs(x = "Percent Respondents (%)", y = "") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.95)
  )

# Export locally
ggsave(file.path(graph_path, "lyon-explore_training-difference_w_stats.png"),
  height = 15, width = 15, units = "in")

# Figure option 2
# Build side-by-side pie charts with checkmarks after text in legends for
# over/under-represented categories
# Extract significance flags from the no-intercept mixed model
coef_results <- summary(m_no_int_want_ref)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  dplyr::filter(grepl(":typehad", term)) |>
  dplyr::mutate(
    value = stringr::str_remove(term, "^learning_mode"),
    value = stringr::str_remove(value, ":typehad$"),
    value = stringr::str_wrap(value, width = 40),
    # With 'want' as reference, a negative typehad coefficient means desired > received
    want_gt_had = Estimate < 0 & `Pr(>|z|)` < 0.05,
    # A positive typehad coefficient means received > desired
    had_gt_want = Estimate > 0 & `Pr(>|z|)` < 0.05
  )

# Ensure consistent color palette and ordering ("Other" at the bottom)
mode_order <- c(setdiff(unique(train_v02$value), "Other"), "Other")
base_colors <- RColorBrewer::brewer.pal(n = length(mode_order), name = "Set3")
mode_palette <- setNames(base_colors, mode_order)

# Prepare legend labels with percentages before text and checkmarks after
# Create a lookup for percentages by value and status
pct_lookup <- train_v02 |>
  select(value, status, percent_responses) |>
  pivot_wider(names_from = status, values_from = percent_responses) |>
  mutate(value = factor(value, levels = mode_order)) |>
  arrange(value)

# Match mode_order to the lookup table rows
legend_labels_want <- sapply(1:length(mode_order), function(i) {
  pct <- round(pct_lookup$desires[i], 1)
  label <- mode_order[i]
  sig <- mode_order[i] %in% coef_results$value[coef_results$want_gt_had]
  if (sig) {
    paste0(pct, "% ", label, " \u2713")
  } else {
    paste0(pct, "% ", label)
  }
})
names(legend_labels_want) <- mode_order

legend_labels_had <- sapply(1:length(mode_order), function(i) {
  pct <- round(pct_lookup$received[i], 1)
  label <- mode_order[i]
  sig <- mode_order[i] %in% coef_results$value[coef_results$had_gt_want]
  if (sig) {
    paste0(pct, "% ", label, " \u2713")
  } else {
    paste0(pct, "% ", label)
  }
})
names(legend_labels_had) <- mode_order

# Prepare data for each pie chart, ordered consistently
pie_had <- train_v02 |>
  dplyr::filter(status == "received") |>
  dplyr::mutate(value = factor(value, levels = mode_order)) |>
  dplyr::arrange(value)

pie_want <- train_v02 |>
  dplyr::filter(status == "desires") |>
  dplyr::mutate(value = factor(value, levels = mode_order)) |>
  dplyr::arrange(value)

# Pie chart of training received - clean pie with legend showing percentages
p_pie_had <- ggplot(pie_had, aes(x = "", y = percent_responses, fill = value)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = mode_palette[mode_order], labels = legend_labels_had[mode_order]) +
  labs(title = "Training Received", x = NULL, y = NULL) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  guides(fill = guide_legend(title = ""))

# Pie chart of training desired
p_pie_want <- ggplot(pie_want, aes(x = "", y = percent_responses, fill = value)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = mode_palette[mode_order], labels = legend_labels_want[mode_order]) +
  labs(title = "Training Desired", x = NULL, y = NULL) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  guides(fill = guide_legend(title = ""))

# Display side by side
p_pie_had + p_pie_want

# Export locally
ggsave(file.path(graph_path, "training-received-vs-desired-pies.png"),
  plot = p_pie_had + p_pie_want,
  height = 10, width = 16, units = "in")
