#laina emmons 4/10/2021 
#regression analysis on States & Counties dataset
#dot-and-whisker visualization of regression tables

setwd("/Users/lainaemmons/Documents/senior_thesis")
getwd()

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stringr)
library(broom)

data <- read.csv("Data/states_and_counties.csv")
#colnames(data)

data = subset(data, select = -c(inf_mar, inf_apr, inf_may, inf_jun, inf_jul, inf_aug, Demographics,
                                inf_sep_13, death_mar, death_apr, death_may, death_jun, death_jul, 
                                death_aug, death_sep, med_income, num_polls, Voting, Election,
                                difference_turnout_2020_minus_2016, difference_trump_perc_2020_minus_2016,
                                difference_gop_gov_perc_2020_minus_2016, X2020_registered, X2016_registered))
data$inf_by_pop <- as.numeric(as.character(data$inf_by_pop))
data$death_rate_by_pop <- as.numeric(as.character(data$death_rate_by_pop))
data$perc_non_white <-as.numeric(as.character(data$perc_non_white))
data$perc_below_poverty_level <- as.numeric(as.character(data$perc_below_poverty_level))
data$unemployment <- as.numeric(as.character(data$unemployment))
data$polls_per_person <- as.numeric(as.character(data$polls_per_person))
data$early_vote_duration <- as.numeric(as.character(data$early_vote_duration))
data$perc_trump_2016 <- as.numeric(as.character(data$perc_trump_2016))
data$perc_trump_2020 <- as.numeric(as.character(data$perc_trump_2020))
data$perc_rep_gov_2016 <- as.numeric(as.character(data$perc_rep_gov_2016))
data$perc_rep_gov_2020 <- as.numeric(as.character(data$perc_trump_2020))
data$X2016_turnout <- as.numeric(as.character(data$X2016_turnout))
data$X2020_turnout <- as.numeric(as.character(data$X2020_turnout))

colnames(data)
data <- data %>% 
  filter(!death_rate_by_pop == "NA")
data <- data %>%
  filter(!inf_by_pop == "NA")
data <- data %>%
  filter(!unemployment == "NA")
data <- data %>% 
  filter(!gov_rep_incumbent_2020 == 0) 
summary(data)

###### Turnout?
turnout <- data %>%
  filter(gov_rep_incumbent_2020 == 1) 
turnout <- lm(X2020_turnout ~  death_rate_by_pop + inf_by_pop + perc_non_white +
                unemployment + polls_per_person + early_vote_duration + same_day_reg +
                vra_provision + X2016_turnout, data = turnout)
summary(turnout)
turnout

dwplot(turnout, show_intercept = FALSE, ci = .95) %>%
  relabel_predictors(c(inf_by_pop = "Infection Rate",
                       perc_non_white = "% Population Non-White",
                       unemployment = "Unemployment Rate",
                       polls_per_person = "Polls per Capita",
                       early_vote_duration = "Duration of Early Vote Period",
                       same_day_reg = "Offers Same-Day Registration",
                       vra_provision = "Subject to VRA Provision",
                       perc_trump_2016 = "Trump 2016 %",
                       perc_rep_gov_2016 = "GOP Govornor 2016 %")) +
  ylab("Predictors") + xlab("Coefficient Estimate") + 
  ggtitle("Regression Analysis on 2020 Election Models") + 
  theme_bw() +
  theme(axis.text = element_text(color = "black", face = "bold")) +
  labs(subtitle = "by Laina Emmons") +
  scale_color_brewer(palette = "Set1", 
                     breaks = c("Trump Share of Vote", "GOP Gov Share of Vote")) +
  theme(plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face = "bold"),
        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

###### deaths only dotwhisker regression plots
library(dotwhisker)

gub_inc <- data %>%
  filter(gov_rep_incumbent_2020 == 1) 
gub_inc <- lm(perc_rep_gov_2020 ~  death_rate_by_pop +  perc_non_white +
       unemployment + polls_per_person + early_vote_duration + same_day_reg +
       vra_provision + perc_rep_gov_2016, data = gub_inc)
summary(gub_inc)

trump_inc <- data %>%
  filter(gov_rep_incumbent_2020 == 1) 
trump_inc <- lm(perc_trump_2020 ~ death_rate_by_pop + perc_non_white +
                  unemployment + polls_per_person + early_vote_duration + same_day_reg +
                  vra_provision + perc_trump_2016, data = trump_inc)
summary(trump_inc)

death_model <- bind_rows(
  tidy(gub_inc) %>% mutate(model = "GOP Gov Share of Vote"),
  tidy(trump_inc) %>% mutate(model = "Trump Share of Vote"))

death_model$model <- factor(death_model$model,
                            levels = c("Trump Share of Vote", "GOP Gov Share of Vote"),
                            labels = c("Trump Share of Vote", "GOP Gov Share of Vote")
)

dwplot(death_model, show_intercept = FALSE, ci = .95) %>%
  relabel_predictors(c(death_rate_by_pop = "Death Rate",
                       perc_non_white = "% Population Non-White",
                       unemployment = "Unemployment Rate",
                       polls_per_person = "Polls per Capita",
                       early_vote_duration = "Duration of Early Vote Period",
                       same_day_reg = "Offers Same-Day Registration",
                       vra_provision = "Subject to VRA Provision",
                       perc_trump_2016 = "Trump 2016 %",
                       perc_rep_gov_2016 = "GOP Govornor 2016 %")) +
  ylab("Predictors") + xlab("Coefficient Estimate") + 
  ggtitle("Regression Analysis on 2020 Election Models") +
  theme_bw() +
  labs(subtitle = "       This is a dot-and-whisker plot, the dots represent the regression coefficient estimate and the whiskers represent confidence interval. 
       This plot shows that the variable is statistically relevant if the whisker does not cross 0. 
       If the coefficient estimate > 0, the variable positively impacted the share of votes, and vice versa.
       
       by Laina Emmons") +
  scale_color_brewer(palette = "Set1", 
                     breaks = c("Trump Share of Vote", "GOP Gov Share of Vote")) +
  theme(plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face = "bold"),
        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

##### infections only dotwhisker

gub_inc_gov <- data %>%
  filter(gov_rep_incumbent_2020 == 1) 
gub_inc_gov <- lm(perc_rep_gov_2020 ~  inf_by_pop + perc_non_white + unemployment + 
                polls_per_person + early_vote_duration + same_day_reg +
                vra_provision + perc_rep_gov_2016, data = gub_inc_gov)
summary(gub_inc_gov)

trump_inc_gov <- data %>%
  filter(gov_rep_incumbent_2020 == 1) 
trump_inc_gov <- lm(perc_trump_2020 ~ inf_by_pop + perc_non_white +
                  unemployment + polls_per_person + early_vote_duration + same_day_reg +
                  vra_provision + perc_trump_2016, data = trump_inc_gov)
summary(trump_inc_gov)

inf_model <- bind_rows(
  tidy(gub_inc_gov) %>% mutate(model = "GOP Gov Share of Vote"),
  tidy(trump_inc_gov) %>% mutate(model = "Trump Share of Vote"))

inf_model$model <- factor(inf_model$model,
                          levels = c("Trump Share of Vote", "GOP Gov Share of Vote"),
                          labels = c("Trump Share of Vote", "GOP Gov Share of Vote"))

dwplot(inf_model, show_intercept = FALSE, ci = .95) %>%
  relabel_predictors(c(inf_by_pop = "Infection Rate",
                       perc_non_white = "% Population Non-White",
                       unemployment = "Unemployment Rate",
                       polls_per_person = "Polls per Capita",
                       early_vote_duration = "Duration of Early Vote Period",
                       same_day_reg = "Offers Same-Day Registration",
                       vra_provision = "Subject to VRA Provision",
                       perc_trump_2016 = "Trump 2016 %",
                       perc_rep_gov_2016 = "GOP Govornor 2016 %")) +
  ylab("Predictors") + xlab("Coefficient Estimate") + 
  ggtitle("Regression Analysis on 2020 Election Models") +
  theme_bw() +
  labs(subtitle = "       This is a dot-and-whisker plot, the dots represent the regression coefficient estimate and the whiskers represent confidence interval. 
       This plot shows that the variable is statistically relevant if the whisker does not cross 0. 
       If the coefficient estimate > 0, the variable positively impacted the share of votes, and vice versa.
       
       by Laina Emmons") +
  scale_color_brewer(palette = "Set1", 
                     breaks = c("Trump Share of Vote", "GOP Gov Share of Vote")) +
  theme(plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face = "bold"),
        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

