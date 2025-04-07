  gss_data %>% filter(gender == "men") %>% summarise(wtd.mean = wtd.mean(earnings, weights = wgt, na.rm = TRUE))
  gss_data %>% filter(gender == "women") %>% summarise(wtd.mean = wtd.mean(earnings, weights = wgt, na.rm = TRUE))

gss_data %>% filter(gender == "men", child == 0) %>% summarise(wtd.mean = wtd.mean(earnings, weights = wgt, na.rm = TRUE))
gss_data %>% filter(gender == "women", child == 0) %>% summarise(wtd.mean = wtd.mean(earnings, weights = wgt, na.rm = TRUE))

gss_data %>% filter(gender == "men", child == 1) %>% summarise(wtd.mean = wtd.mean(earnings, weights = wgt, na.rm = TRUE))

gss_data %>% filter(gender == "women", child == 1) %>% summarise(wtd.mean = wtd.mean(earnings, weights = wgt, na.rm = TRUE))

gss_data %>%
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth",
       x = "Years around Birth of First Child",
       y = "Average Earnings",
       color = "Gender") +
  geom_vline(xintercept = 0)

reg <- lm(formula = earnings ~ child*gender, data = gss_data)
summary(reg)

reg <- lm(formula = earnings ~ child*gender, data = gss_data)

gss_data %>%
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 0.85, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

reg <- gss_data %>% 
  filter(child_birth_year == 1980) %>% 
  lm(formula = employed ~ child*gender)

gss_data %>%
  filter(child_birth_year == 1980) %>%
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_employed = wtd.mean(employed, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_employed, color = gender)) +
  geom_line() + 
  labs(title = "Employment Rate around Year of Child Birth - for Parents of children born in 1980",
       x = "Years around Birth of First Child",
       y = "Average Employment Rate",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 0.85, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

  reg <- gss_data %>% 
  filter(child_birth_year == 2015) %>% 
  lm(formula = employed ~ child*gender)

gss_data %>%
  filter(child_birth_year == 2015) %>%
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_employed = wtd.mean(employed, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_employed, color = gender)) +
  geom_line() + 
  labs(title = "Employment Rate around Year of Child Birth - for Parents of children born in 2015",
       x = "Years around Birth of First Child",
       y = "Average Employment Rate",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 0.85, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

child_penalty_df %>%  
  ggplot(aes(year, child_penalty)) +
  geom_point() + 
  labs(title = "Child Penalty as a function of Time",
       x = "Time",
       y = "Child Penalty")

reg <- gss_data %>% 
  filter(marital_status == 3) %>% 
  lm(formula = earnings ~ child*gender)

gss_data %>%
  filter(marital_status == 3) %>% 
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth - for married parents with present spouses ",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 10000, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

reg <- gss_data %>% 
  filter(marital_status == 2) %>% 
  lm(formula = earnings ~ child*gender)

gss_data %>%
  filter(marital_status == 2) %>% 
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth - for married parents with absent spouses ",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 10000, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

reg <- gss_data %>% 
  filter(edlevel == 1) %>% 
  lm(formula = earnings ~ child*gender)

gss_data %>%
  filter(edlevel == 1) %>% 
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth - for parents with education level of below high school  ",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 2000, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

reg <- gss_data %>% 
  filter(edlevel == 4) %>% 
  lm(formula = earnings ~ child*gender)

gss_data %>%
  filter(edlevel == 4) %>% 
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth - for parents with education level of college or higher ",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 20000, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

reg <- gss_data %>% 
  filter(immigrant == 0) %>% 
  lm(formula = earnings ~ child*gender)

gss_data %>%
  filter(immigrant == 0) %>% 
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth - for US-born parents ",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 20000, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))

reg <- gss_data %>% 
  filter(immigrant == 1) %>% 
  lm(formula = earnings ~ child*gender)

gss_data %>%
  filter(immigrant == 1) %>% 
  group_by(year_around_birth, gender) %>%
  dplyr::summarize(avg_earnings = wtd.mean(earnings, weights = wgt, na.rm = TRUE)) %>%
  ggplot(aes(year_around_birth, avg_earnings, color = gender)) +
  geom_line() + 
  labs(title = "Earnings around Year of Child Birth - for immigrant parents ",
       x = "Years around Birth of First Child",
       y = "Average Earnings ($)",
       color = "Gender") +
  geom_vline(xintercept = 0) +
  annotate(geom="text", x = 2.5, y = 15000, label = 
             paste("Estimate:", signif(summary(reg)[["coefficients"]][4], 4), "\n", 
                   "SE:",  signif(summary(reg)[["coefficients"]][8], 4)))