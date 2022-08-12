#> -------------------------------------------
#> ON THE REPRODUCIBILITY OF POWER ANALYSES
#> -- McKay, Bacelar, and Carter
#>
#> Required libraries
#>
#> Authors:
#>   Brad McKay
#>   Mike Carter
#>
#> Last update: Aug 2 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> SCRIPT SETUP ----
#>
#> Load required libraries
source("scripts/analyses.R")


#> FIGURES IN MANUSCRIPT ----
#>
#> Figure 1
#> Create tibbles for Fig 1a (Reproducible power analysis with info provided),
#> and for Fig 1b (Identify the conditionally reproducible ones)
#>
fig1a_tib <- tibble(
  category = c("Fully reproducible",
               "Not fully reproducible"),
  n = c(6, 78)
)

fig1b_tib <- tibble(
  category = c("Fully reproducible",
               "Conditionally reproducible",
               "Not reproducible"),
  n = c(reproducible_w_info,
        conditionally_reproducible,
        84 - total_reproducible)
)

#> Create the subplots for multipanel Figure 1 using the created
#> tibbles from above
fig1a <- waffle(
  fig1a_tib,
  rows = 4,
  size = .5,
  colors = c("#6a3d9a","#cab2d6"),
  flip = "FALSE", reverse = "TRUE"
) +
  theme_enhance_waffle() +
  theme(legend.text = element_text(size = 14))
fig1a

fig1b <- waffle(
  fig1b_tib,
  rows = 4,
  size = .5,
  colors = c("#6a3d9a", "#ae6cd6","#cab2d6"),
  flip = "FALSE", reverse = "TRUE"
) +
  theme_enhance_waffle() +
  theme(legend.text = element_text(size = 14))
fig1b

#> Combine Figure 1 subplots into multipanel Figure 1
fig1 <- fig1a + fig1b +
  plot_layout(nrow = 2,
              guides = "collect") +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1


#> Figure 2
#> Create tibbles for Fig 2a (Matched analysis?)
#> and for Fig 2b (Matched hypothesis?)
fig2a_tib <- tibble(
  category = c("Matched analysis",
               "Did not match analysis",
               "Unknown"),
  n = c(matches_design,
        dont_match_design,
        84 - (matches_design + dont_match_design))
)

fig2b_tib <- tibble(
  category = c("Matched hypothesis",
               "Did not match hypothesis",
               "Unknown"),
  n = c(matches_hypothesis,
        dont_match_hypothesis,
        84 - (matches_hypothesis + dont_match_hypothesis))
)

#> Create the subplots for multipanel Figure 2 using the created
#> tibbles from above
fig2a <- waffle(
  fig2a_tib,
  rows = 4,
  size = .5,
  colors = c("#6a3d9a", "#ae6cd6","#cab2d6"), # #88c0d0, #81a1c1, #5e81ac
  flip = "FALSE", reverse = "TRUE"
) +
  theme_enhance_waffle() +
  theme(legend.text = element_text(size = 14))
fig2a

fig2b <- waffle(
  fig2b_tib,
  rows = 4,
  size = .5,
  colors = c("#6a3d9a", "#ae6cd6","#cab2d6"),
  flip = "FALSE", reverse = "TRUE"
) +
  theme_enhance_waffle() +
  theme(legend.text = element_text(size = 14))
fig2b

#> Combine Figure 2 subplots into multipanel Figure 2
fig2 <- fig2a + fig2b + plot_layout(nrow = 2,
                                    guides = "collect") +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2

#> Figure 3
#> Create tibble for Fig3a (Eta-squared as effect size) and
#> for Fig3b (Eta-squared and sample size comparison)
fig3a_tib <- tibble(
  category = c("As in SPSS",
               "Default",
               "Not reproducible",
               "Not applicable"),
  n = c(spss,
        default,
        not_reproducible,
        84 - (spss + default + not_reproducible))
)

setting_df$"As in SPSS" <- setting_df$n_with_spss - setting_df$n_with_default
setting_df <- setting_df |>
  rename(Default = n_with_default)
setting_df$id <- seq(1:nrow(setting_df))
setting_df <- data.frame(setting_df)
setting_df <- setting_df[order(setting_df$Default),]
setting_df <- setting_df |> pivot_longer(
  cols = !c(id, n_with_spss),
  names_to = "category",
  values_to = "values")
setting_df <- arrange(
  setting_df,
  desc("values"),
  group_by = "n_with_default")
setting_df$study <- rep(1:8, each = 2)
fig3b_tib <- setting_df

#> Create the subplots for multipanel Figure 3 using the created
#> tibbles from above
fig3a <- waffle(
  fig3a_tib,
  rows = 4,
  size = .5,
  colors = c("#6a3d9a", "#ae6cd6","#cab2d6", "#1e002a"),
  flip = "FALSE", reverse = "TRUE"
) +
  theme_enhance_waffle() +
  theme(legend.text = element_text(size = 14))
fig3a

fig3b <- fig3b_tib |>
  ggplot(aes(x = study, y = values, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_cartesian(expand = FALSE) +
  theme_minimal() +
  labs(x = "Study",
       y = "Sample size",
       fill = "Setting") +
  scale_fill_manual(labels = c("As in SPSS",
                               "Default"),
                    values = rep(c("#6a3d9a","#cab2d6"), 8)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(axis.text = element_text(colour = "black", size = 13))
fig3b

#> Combine Figure 3 subplots into multipanel Figure
fig3 <- fig3a + fig3b +
  plot_layout(nrow = 2) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3


#> Figure 4 (Accurate power analysis out of the entire sample of articles
#> from the three journals surveyed)
#> Create tibble
fig4_tib <- tibble(
  category = c("Accurate Power Analysis",
               "Unconfirmed"),
  n = c(all_good, 632)
)

fig4 <- waffle(
  fig4_tib,
  rows = 15,
  size = .5,
  colors = c("#6a3d9a", "#cab2d6"),
  flip = "FALSE", reverse = "TRUE"
) +
  theme_enhance_waffle() +
  theme(legend.text = element_text(size = 14))
fig4


#> Misc results
#> 6 unreported one-tailed tests, 4 possible within-subjects Ns used
#> as between-subjects n.
#>
#> 10 additional studies with designs that wouldn't work in g*power
#> (2 scored yes for appropriate software bc PA different)
#>
#> 66% (51/77) of the not reproducible studies were missing a description
#> of the design
#>
#> (13/77) did not report the effect size used in the power calculation
