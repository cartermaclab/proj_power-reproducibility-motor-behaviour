#> -------------------------------------------
#> ON THE REPRODUCIBILITY OF POWER ANALYSES
#> -- McKay, Bacelar, and Carter
#>
#> Required libraries
#>
#> Authors:
#>   Brad McKay
#>
#> Last update: July 31 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> SCRIPT SETUP ----
#>
#> Load required libraries
source("scripts/libraries.R")

#> Load data file
dat <- read_csv(file = "data/dat.csv")


#> ANALYSIS ----
#>
#> Proportion of power analyses that could be reproduced...
#> ... with information provided:
reproducible_w_info <- length(which(dat$rep_w_info_provided == "yes"))
not_reproducible_w_info <- length(which(dat$rep_w_info_provided == "no"))
proportion_reproducible <- reproducible_w_info /
  (reproducible_w_info + not_reproducible_w_info)

#> ... only after making assumptions for missing parameters:
conditionally_reproducible <- length(which(
  dat$conditionally_reproducible == "yes"))
proportion_conditionally_reproducible <- conditionally_reproducible / 84

#> Proportion of power analyses that could be reproduced
#> with our without assumptions
total_reproducible <- conditionally_reproducible + reproducible_w_info
proportion_total_reproducible <- total_reproducible / 84

#> View proportions
proportion_conditionally_reproducible
proportion_reproducible
proportion_total_reproducible

#> Proportion of power analyses that matched statistical analysis...
#> ... out of all power analyses in the sample:
matches_design <- length(which(dat$matches_design == "yes"))
dont_match_design <- length(which(dat$matches_design == "no"))
proportion_match_design_total <- matches_design / 84

#> ... out of all power analyses with enough information to judge:
proportion_match_design_of_known <- matches_design /
  (matches_design + dont_match_design)

#> View proportions of power analyses that matched the statistical analysis
proportion_match_design_total
proportion_match_design_of_known

#> Proportion of power analyses that matched a hypothesis...
#> ... out of all power analyses in the sample:
matches_one_hypothesis <- length(
  which(dat$matches_primary_hypotheses == "match_one"))
matches_hypothesis <- length(
  which(dat$matches_primary_hypotheses == "yes" |
        dat$matches_primary_hypotheses == "match_one"))
dont_match_hypothesis <- length(
  which(dat$matches_primary_hypotheses == "no"))
proportion_matches_hypothesis_total <- matches_hypothesis / 84

#> ... out of all power analyses with enough information to judge:
proportion_matches_hypothesis_of_known <- matches_hypothesis /
  (matches_hypothesis + dont_match_hypothesis)

#> View proportions of power analyses that matched a hypothesis
proportion_matches_hypothesis_total
proportion_matches_hypothesis_of_known

#> Proportion of power analyses with default or SPSS eta-squared setting
#> out of all power analyses that used eta-squared as the effect size
spss <- length(which(dat$eta_squared_setting == "spss"))
default <- length(which(dat$eta_squared_setting == "default"))
not_reproducible <- length(
  which(dat$eta_squared_setting == "not_reproducible"))
proportion_as_in_spss <- spss / (spss + default + not_reproducible)
proportion_default <- default / (default + spss + not_reproducible)

#> View proportions that used default and as in SPSS setting
proportion_as_in_spss
proportion_default


#> Proportion of power analyses conducted on appropriate software..
#> ... out of all power analyses in the sample:
appropriate_software <- length(which(dat$appropriate_software == "yes"))
not_appropriate_software <- length(which(dat$appropriate_software == "no"))
proportion_not_appropriate_total <- not_appropriate_software / 84

#> ... out of all power analyses with enough info to judge:
proportion_not_appropriate_of_known <- not_appropriate_software /
  (not_appropriate_software + appropriate_software)

#> View proportion of power analyses conducted with appropriate software
proportion_not_appropriate_total
proportion_not_appropriate_of_known

#> Proportion of power analyses with software reported
gpower <- length(which(dat$software == "gpower"))
not_reported <- length(which(dat$software == "not_reported"))
proportion_gpower <- gpower / 84

#> View proportion
proportion_gpower

#> Reproducible, matches design, matches hypothesis, appropriate software,
#> eta-squared as in spss or not_app.
all_good <- length(
  which(
    dat$rep_w_info_provided == "yes" & dat$appropriate_software == "yes" &
    (dat$matches_primary_hypotheses == "yes" |
       dat$matches_primary_hypotheses == "match_one") &
      dat$matches_design == "yes"))

proportion_all_good <- all_good / 84

#> Impact of using default eta-squared settings if effect size was
#> calculated in SPSS

#> Tibble of sample size calculations with each setting
n_with_default <- c(26, 28, 7, 14, 23, 84, 36, 22)
n_with_spss <- c(76, 108, 17, 22, 60, 324, 104, 82)
setting_df <- tibble(n_with_default, n_with_spss)

#> Increase in sample size when using as in SPSS setting compared to default
more_N_w_spss <- as_tibble(setting_df$n_with_spss - setting_df$n_with_default)
mean(more_N_w_spss$value)

#> View individual differences
more_N_w_spss

#> Count unique statistical analyses used in the power analysis
count_per_design <- count(dat, design)
count_per_design

#> Count unique effect size types
count_per_effect_size <- count(dat, effect_size_type)
count_per_effect_size

#> Count unique effect size values
count_per_effect_value <- count(dat, effect_size_value)
count_per_effect_value

#> Count unique power targets
count_per_power <- count(dat, power)
count_per_power
