#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have a package downloaded, start by running:
#install.packages("ggplot2")

library(ggplot2)
library(scales)
library(dplyr)
library(maps)

#######################################################################
### Read in workshop dataset ##########################################
#######################################################################

countries <- read.delim("https://raw.githubusercontent.com/seaneff/ghs-designing-data-viz/main/country_workforce_data.tsv")

#######################################################################
### Reformat data as needed ###########################################
#######################################################################

countries$income_group <- factor(countries$income_group,
                                 levels = c("Low income", "Lower middle income",
                                            "Upper middle income", "High income"))

#######################################################################
### Explore a dataset generally #######################################
#######################################################################

## What are the columns/fields?
names(countries)

#######################################################################
### Start recording plots (for GIF) ###################################
#######################################################################

gg_record(
  dir = file.path("figure_versions"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

###############################################################################
### Relationship between childhood mortality and MDs per population ###########
###############################################################################

countries$scatterplot_label <- ""
countries[which(countries$country == "Niger"),]$scatterplot_label <- "Niger"
countries[which(countries$country == "Nigeria"),]$scatterplot_label <- "Nigeria"
countries[which(countries$country == "Pakistan"),]$scatterplot_label <- "Pakistan"
countries[which(countries$country == "Cuba"),]$scatterplot_label <- "Cuba"
countries[which(countries$country == "Sweden"),]$scatterplot_label <- "Sweden"
countries[which(countries$country == "Turkmenistan"),]$scatterplot_label <- "Turkmenistan"
countries[which(countries$country == "Mali"),]$scatterplot_label <- "Mali"
countries[which(countries$country == "Greece"),]$scatterplot_label <- "Greece"
countries[which(countries$country == "Somalia"),]$scatterplot_label <- "Somalia"
countries[which(countries$country == "Eswatini"),]$scatterplot_label <- "Eswatini"
countries[which(countries$country == "Chad"),]$scatterplot_label <- "Chad"

countries$alpha_value <- 0.55
countries$alpha_value[which(countries$scatterplot_label != "")] <- 1

workforce_morality_scatter <- countries %>%
  filter(income_group != "") %>%
  filter(complete.cases(most_recent_medical_doctor_per10000)) %>%
  filter(complete.cases(under_5_mortality_per1000)) %>%
  ggplot(aes(x = most_recent_medical_doctor_per10000, 
             y = under_5_mortality_per1000,
             size = total_population,
             alpha = alpha_value,
             fill = income_group,
             label = scatterplot_label)) +
  geom_point(colour = "gray10", pch = 21, stroke = .1) +
  geom_text_repel(size = 2, min.segment.length = 0, family = "Barlow",
                  colour = "grey10", seed = 12, 
                  segment.size  = .2, segment.color = "gray10",
                  max.overlaps = 10, force_pull = .5) +
  labs(x = "Medical doctors\nper 10,000 population",
       y = "Under 5 mortality rate\nper 1,000 population",
       fill = "",
       size = "Population size",
       title = "Childhood mortality is highest in\nareas with low health workforce density",
       caption = "Workforce data from WHO Global Health Workforce statistics database\nChildhood mortality estimates from World Bank Data Bank\nVisualization by Steph Eaneff as part of designing data visualizations workshop") +
  ## this specifies the colors for the fill values
  scale_fill_manual(values = c( "#193665", "#00688f", "#339ba9", "#7fcdbb")) +
  scale_size_continuous(range = c(.45, 6), 
                        breaks = c(100000000, 800000000, 1400000000),
                        labels = c("100M", "800M", "1.4B")) +
  scale_alpha_identity() +
  guides(fill = guide_legend(position = "top", ncol = 4,
                             override.aes = list(size = 2)),
         size = guide_legend(position = "top", ncol = 3),
         alpha = "none") +
  theme_minimal() +
  theme(panel.grid.major = element_line(linewidth = 0.35),
        panel.grid.minor = element_line(linewidth = 0.2),
        legend.box = "vertical", 
        legend.position = c(.7, .7), 
        text = element_text(colour = "grey10", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.1)), 
        plot.subtitle = element_text(hjust = 0.5, size = rel(1)),
        axis.text.x = element_markdown(size = rel(1)),
        axis.text.y = element_markdown(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.75)),
        legend.title = element_text(size = rel(0.75)),
        legend.key.size = unit(.4, "lines"),
        legend.spacing.y = unit(-.6, 'lines'),
        plot.caption = element_text(size = rel(0.4))) 
  
workforce_morality_scatter

###############################################################################
### World map with nurses per capita overlaid #################################
###############################################################################

countries %>%
  filter(income_group != "") %>%
  filter(complete.cases(most_recent_dentist_per10000)) %>%
  ggplot(aes(x = most_recent_dentist_per10000, y = region, 
             size = total_population,
             label = scatterplot_label)) +
  geom_line(aes(group = region), size = 0.75) +
  geom_point(aes(color = income_group)) +
  geom_text_repel(size = 2, min.segment.length = 0, family = "Barlow",
                  colour = "grey10", seed = 12, 
                  segment.size  = .2, segment.color = "gray10",
                  max.overlaps = 500, force_pull = .5) +
  scale_color_manual(values = c( "#193665", "#00688f", "#339ba9", "#7fcdbb")) +
  scale_size_continuous(range = c(.45, 6), 
                        breaks = c(100000000, 800000000, 1400000000),
                        labels = c("100M", "800M", "1.4B")) 
  

  labs(
    title = "COVID-19 impact on poverty rates of informal workers",
    caption = "Source: International Labor Organization\n© UNHCR, The UN Refugee Agency"
  ) +
  scale_x_continuous(
    limit = c(0, 1),
    label = label_percent(),
    breaks = c(0, 1)
  ) 

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = workforce_morality_scatter,
       filename = "mortality_vs_mds.png", 
       dpi = 350, height = 3.5, width = 5, units = "in",
       bg = 'white')

#######################################################################
### Stop recording and save GIF #######################################
#######################################################################

gg_stop_recording()

gg_playback(
  name = file.path("figure_versions/figure_versions.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)


