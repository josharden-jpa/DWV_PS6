# ------------------------------------------------------------
# DWV Assignment 6
# Author: Josh Arden
# ------------------------------------------------------------

# load relevant packages and read data file

library(haven)
library(tidyverse)   # tidyverse already loads ggplot2
mydata <- read_dta("gss7224_r1.dta")


# ============================================================
# PART 1: Years 1976-1998
# ============================================================

# ---- Figure 1 ----

# Subset the 15 columns used in the figure (plus year)

my_15_vars <- c("year", "spkath", "colath", "libath", "spkrac", "colrac", "librac",
                "spkcom", "colcom", "libcom", "spkmil", "colmil", "libmil",
                "spkhomo", "colhomo", "libhomo")
filtered_data <- mydata[, my_15_vars]

# Create new object containing only years 1976-1998 for filtered data

new_filtered_data <- filtered_data |> filter(year %in% 1976:1998)

# Check how each question is coded before recoding

print(table(new_filtered_data$spkath))
print(table(new_filtered_data$colath))
print(table(new_filtered_data$libath))

# Recode each intolerance question answer as a 1 or 0, 1 being intolerant

new_filtered_data <- new_filtered_data |>
  mutate(
    spkath_r  = case_when(spkath  == 2 ~ 1, spkath  == 1 ~ 0),
    spkrac_r  = case_when(spkrac  == 2 ~ 1, spkrac  == 1 ~ 0),
    spkcom_r  = case_when(spkcom  == 2 ~ 1, spkcom  == 1 ~ 0),
    spkmil_r  = case_when(spkmil  == 2 ~ 1, spkmil  == 1 ~ 0),
    spkhomo_r = case_when(spkhomo == 2 ~ 1, spkhomo == 1 ~ 0),

    colath_r  = case_when(colath  == 5 ~ 1, colath  == 4 ~ 0),
    colrac_r  = case_when(colrac  == 5 ~ 1, colrac  == 4 ~ 0),
    colmil_r  = case_when(colmil  == 5 ~ 1, colmil  == 4 ~ 0),
    colhomo_r = case_when(colhomo == 5 ~ 1, colhomo == 4 ~ 0),

    colcom_r  = case_when(colcom  == 4 ~ 1, colcom  == 5 ~ 0),

    libath_r  = case_when(libath  == 1 ~ 1, libath  == 2 ~ 0),
    librac_r  = case_when(librac  == 1 ~ 1, librac  == 2 ~ 0),
    libcom_r  = case_when(libcom  == 1 ~ 1, libcom  == 2 ~ 0),
    libmil_r  = case_when(libmil  == 1 ~ 1, libmil  == 2 ~ 0),
    libhomo_r = case_when(libhomo == 1 ~ 1, libhomo == 2 ~ 0)
  )

# Sum intolerance response scores per person from 0-15 (high scores = intolerant)
# and drop anyone missing any of the 15 items

new_filtered_data <- new_filtered_data |>
  mutate(intol = rowSums(across(ends_with("_r")))) |>
  filter(!is.na(intol))

# Group intolerance scores by year, finding mean score of each year

yearly <- new_filtered_data |>
  group_by(year) |>
  summarise(mean_intol = mean(intol))

# Line plot of change in mean intolerance score over time

fig1 <- ggplot(yearly, aes(x = year, y = mean_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 15)) +
  theme_classic()
print(fig1)


# ---- Figure 2 ----

# Subset intolerance towards racists and homosexuals columns, 1976-1998

my_rac_vars <- c("year", "spkrac", "colrac", "librac")
filtered_data2rac <- mydata[, my_rac_vars]

my_homo_vars <- c("year", "spkhomo", "colhomo", "libhomo")
filtered_data2homo <- mydata[, my_homo_vars]

new_filtered_data2rac  <- filtered_data2rac  |> filter(year %in% 1976:1998)
new_filtered_data2homo <- filtered_data2homo |> filter(year %in% 1976:1998)

# Recode as before (Fig 1)

new_filtered_data2rac <- new_filtered_data2rac |>
  mutate(
    spkrac_r = case_when(spkrac == 2 ~ 1, spkrac == 1 ~ 0),
    colrac_r = case_when(colrac == 5 ~ 1, colrac == 4 ~ 0),
    librac_r = case_when(librac == 1 ~ 1, librac == 2 ~ 0)
  )

new_filtered_data2homo <- new_filtered_data2homo |>
  mutate(
    spkhomo_r = case_when(spkhomo == 2 ~ 1, spkhomo == 1 ~ 0),
    colhomo_r = case_when(colhomo == 5 ~ 1, colhomo == 4 ~ 0),
    libhomo_r = case_when(libhomo == 1 ~ 1, libhomo == 2 ~ 0)
  )

# Sum intolerance scores (0-3), group by year and find mean per year

new_filtered_data2rac <- new_filtered_data2rac |>
  mutate(intol = rowSums(across(ends_with("rac_r")))) |>
  filter(!is.na(intol))

yearly_rac <- new_filtered_data2rac |>
  group_by(year) |>
  summarise(mean_intol = mean(intol))

fig2_rac <- ggplot(yearly_rac, aes(x = year, y = mean_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 3)) +
  theme_classic()
print(fig2_rac)

new_filtered_data2homo <- new_filtered_data2homo |>
  mutate(intol = rowSums(across(ends_with("homo_r")))) |>
  filter(!is.na(intol))

yearly_homo <- new_filtered_data2homo |>
  group_by(year) |>
  summarise(mean_intol = mean(intol))

fig2_homo <- ggplot(yearly_homo, aes(x = year, y = mean_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 3)) +
  theme_classic()
print(fig2_homo)

# Plot both lines together (blue = intol towards racists, red = intol towards homosexuals)

fig2 <- ggplot() +
  geom_line(data = yearly_rac,  aes(x = year, y = mean_intol), color = "blue") +
  geom_line(data = yearly_homo, aes(x = year, y = mean_intol), color = "red") +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = NULL, y = "Mean intolerance (0-3)") +
  theme_classic()
print(fig2)


# ---- Figure 5 ----

# Only those who score 0/15 are considered tolerant. Any other score gets
# recoded as a 1. Find and plot the proportion of intolerant respondents by year.

yearly5 <- new_filtered_data |>
  mutate(intolerant = if_else(intol > 0, 1, 0)) |>
  group_by(year) |>
  summarise(prop_intol = mean(intolerant))

fig5 <- ggplot(yearly5, aes(x = year, y = prop_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = "Proportion Intolerant") +
  theme_classic()
print(fig5)


# ============================================================
# PART 2: Same figures using ALL years (no 1976-1998 filter)
# ============================================================

# ---- Figure 1 (all years) ----

all_data <- mydata[, my_15_vars] |>
  mutate(
    spkath_r  = case_when(spkath  == 2 ~ 1, spkath  == 1 ~ 0),
    spkrac_r  = case_when(spkrac  == 2 ~ 1, spkrac  == 1 ~ 0),
    spkcom_r  = case_when(spkcom  == 2 ~ 1, spkcom  == 1 ~ 0),
    spkmil_r  = case_when(spkmil  == 2 ~ 1, spkmil  == 1 ~ 0),
    spkhomo_r = case_when(spkhomo == 2 ~ 1, spkhomo == 1 ~ 0),

    colath_r  = case_when(colath  == 5 ~ 1, colath  == 4 ~ 0),
    colrac_r  = case_when(colrac  == 5 ~ 1, colrac  == 4 ~ 0),
    colmil_r  = case_when(colmil  == 5 ~ 1, colmil  == 4 ~ 0),
    colhomo_r = case_when(colhomo == 5 ~ 1, colhomo == 4 ~ 0),

    colcom_r  = case_when(colcom  == 4 ~ 1, colcom  == 5 ~ 0),

    libath_r  = case_when(libath  == 1 ~ 1, libath  == 2 ~ 0),
    librac_r  = case_when(librac  == 1 ~ 1, librac  == 2 ~ 0),
    libcom_r  = case_when(libcom  == 1 ~ 1, libcom  == 2 ~ 0),
    libmil_r  = case_when(libmil  == 1 ~ 1, libmil  == 2 ~ 0),
    libhomo_r = case_when(libhomo == 1 ~ 1, libhomo == 2 ~ 0)
  ) |>
  mutate(intol = rowSums(across(ends_with("_r")))) |>
  filter(!is.na(intol))

yearly_all <- all_data |>
  group_by(year) |>
  summarise(mean_intol = mean(intol))

fig1_all <- ggplot(yearly_all, aes(x = year, y = mean_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 15)) +
  theme_classic()
print(fig1_all)


# ---- Figure 2 (all years) ----

all_rac <- mydata[, my_rac_vars] |>
  mutate(
    spkrac_r = case_when(spkrac == 2 ~ 1, spkrac == 1 ~ 0),
    colrac_r = case_when(colrac == 5 ~ 1, colrac == 4 ~ 0),
    librac_r = case_when(librac == 1 ~ 1, librac == 2 ~ 0)
  ) |>
  mutate(intol = rowSums(across(ends_with("rac_r")))) |>
  filter(!is.na(intol))

all_homo <- mydata[, my_homo_vars] |>
  mutate(
    spkhomo_r = case_when(spkhomo == 2 ~ 1, spkhomo == 1 ~ 0),
    colhomo_r = case_when(colhomo == 5 ~ 1, colhomo == 4 ~ 0),
    libhomo_r = case_when(libhomo == 1 ~ 1, libhomo == 2 ~ 0)
  ) |>
  mutate(intol = rowSums(across(ends_with("homo_r")))) |>
  filter(!is.na(intol))

yearly_rac_all <- all_rac |>
  group_by(year) |>
  summarise(mean_intol = mean(intol))

yearly_homo_all <- all_homo |>
  group_by(year) |>
  summarise(mean_intol = mean(intol))

fig2_rac_all <- ggplot(yearly_rac_all, aes(x = year, y = mean_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 3)) +
  theme_classic()
print(fig2_rac_all)

fig2_homo_all <- ggplot(yearly_homo_all, aes(x = year, y = mean_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 3)) +
  theme_classic()
print(fig2_homo_all)

fig2_all <- ggplot() +
  geom_line(data = yearly_rac_all,  aes(x = year, y = mean_intol), color = "blue") +
  geom_line(data = yearly_homo_all, aes(x = year, y = mean_intol), color = "red") +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = NULL, y = "Mean intolerance (0-3)") +
  theme_classic()
print(fig2_all)


# ---- Figure 5 (all years) ----

yearly5_all <- all_data |>
  mutate(intolerant = if_else(intol > 0, 1, 0)) |>
  group_by(year) |>
  summarise(prop_intol = mean(intolerant))

fig5_all <- ggplot(yearly5_all, aes(x = year, y = prop_intol)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = "Proportion Intolerant") +
  theme_classic()
print(fig5_all)

# ============================================================
# ORIGINAL WORK: TV hours, 2024 GSS (univariate) -- AI ASSISTED
# ============================================================

theme_mondak <- theme_classic(base_size = 11) +
  theme(
    plot.title            = element_text(face = "bold", size = 13),
    plot.subtitle         = element_text(color = "grey30"),
    plot.caption          = element_text(color = "grey40", hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y    = element_line(color = "grey92"),
    legend.position       = "none"
  )

gss_caption <- "Source: General Social Survey (GSS) cumulative file, 1972-2024."

tv <- mydata |>
  filter(year == 2024) |>
  select(tvhours) |>
  mutate(tvhours = as.numeric(tvhours)) |>   # drop haven labels
  filter(!is.na(tvhours))

# Summary numbers for the writeup
summary(tv$tvhours)     # median, mean, quartiles, max
sd(tv$tvhours)          # spread
nrow(tv)                # should be about 2,152 (matches codebook)

fig_tv <- ggplot(tv, aes(x = tvhours)) +
  geom_histogram(binwidth = 1, fill = "#2a78d6", color = "white") +
  geom_boxplot(aes(y = -40), width = 40) +                       # box plot under the bars
  geom_vline(xintercept = mean(tv$tvhours), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  labs(title    = "Daily Television Viewing, 2024",
       subtitle = "Bars show the number of respondents; box shows median and middle 50%; dashed line = mean",
       x = "Hours of TV on an average day",
       y = "Number of respondents",
       caption = gss_caption) +
  theme_mondak
print(fig_tv)


# ============================================================
# ORIGINAL WORK: SOCBAR (going to a bar or tavern), 2024 GSS -- AI ASSISTED
# ============================================================

bar_labels <- c("Almost every day", "Once or twice a week", "Several times a month",
                "About once a month", "Several times a year", "About once a year", "Never")

bar <- mydata |>
  filter(year == 2024) |>
  select(socbar) |>
  mutate(socbar = as.numeric(socbar)) |>   # 1-7 codes, drop haven labels
  filter(!is.na(socbar))

nrow(bar)            # should be 2,172 (matches codebook p. 105)
table(bar$socbar)    # compare counts to the codebook

# Center and spread, using the 1-7 codes
# (type = 1 keeps the quartiles on actual categories instead of in between them)
bar_q <- quantile(bar$socbar, c(0.25, 0.5, 0.75), type = 1)
bar_labels[bar_q]    # 25th percentile, median, 75th percentile as text

# Percentage giving each answer
bar_dist <- bar |>
  count(socbar) |>
  mutate(pct       = n / sum(n),
         answer    = factor(bar_labels[socbar], levels = bar_labels),
         is_median = socbar == bar_q[2])

fig_bar <- ggplot(bar_dist, aes(x = answer, y = pct, fill = is_median)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("FALSE" = "#9ec5f4", "TRUE" = "#2a78d6")) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "How Often Americans Go to a Bar or Tavern, 2024",
       subtitle = paste0("Dark bar = median answer (", bar_labels[bar_q[2]], "); ",
                         "middle 50% range from '", bar_labels[bar_q[1]],
                         "' to '", bar_labels[bar_q[3]], "'"),
       x = NULL, y = "Percent of respondents",
       caption = gss_caption) +
  theme_mondak +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(fig_bar)