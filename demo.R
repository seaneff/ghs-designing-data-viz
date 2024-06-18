#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have a package downloaded, start by running:
#install.packages("ggplot2")

library(ggplot2)
library(scales)
library(tidyr)
library(forcats)
library(maps)
library(ggrepel)
library(ggtext)
library(sf)

#######################################################################
### Read in workshop dataset ##########################################
#######################################################################

## load data directly from github
countries <- read.delim("https://github.com/seaneff/ghs-designing-data-viz/raw/main/country_workforce_data.tsv")

#######################################################################
### Reformat data as needed ###########################################
#######################################################################

## treat income group as a factor so that R knows it's an ordinal variable
## this is needed to have data show in the "correct" order in plots, legends, etc
countries$income_group <- factor(countries$income_group,
                                 levels = c("Low income", "Lower middle income",
                                            "Upper middle income", "High income"))

## do the same thing for region
## it's not ordinal, but I want it plotted in a specific order later
countries$region <- factor(countries$region, 
                           levels = rev(c("Sub-Saharan Africa",
                                          "Middle East & North Africa",
                                          "Europe & Central Asia",
                                          "East Asia & Pacific",
                                          "South America",
                                          "North America")))

#######################################################################
### Explore a dataset generally #######################################
#######################################################################

## What are the columns/fields?
names(countries)

## print first five rows
head(countries)

###############################################################################
### Relationship between childhood mortality and MDs per population ###########
###############################################################################

## specify which countries will get labeled in the scatterplot
## could do this more elegantly, but I'm figuring it out as I go, 
## one country at a time
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

## by default, make all points partially transparent (alpha = 0.5)
## make the specifically labeled points above not transparent at all
countries$alpha_value <- 0.55
countries$alpha_value[which(countries$scatterplot_label != "")] <- 1

workforce_morality_scatter <- countries %>%
  ## only look at complete data points
  filter(income_group != "") %>%
  filter(complete.cases(most_recent_medical_doctor_per10000)) %>%
  filter(complete.cases(under_5_mortality_per1000)) %>%
  ## specify aesthetics -- map data elements to parts of the plot
  ggplot(aes(x = most_recent_medical_doctor_per10000, 
             y = under_5_mortality_per1000,
             size = total_population,
             alpha = alpha_value,
             fill = income_group,
             label = scatterplot_label)) +
  ## add points, specify that they should be thinly outlined in dark gray
  geom_point(colour = "gray10", pch = 21, stroke = .1) +
  ## add text to specific labels
  geom_text_repel(size = 2, min.segment.length = 0, family = "Barlow",
                  colour = "grey10", seed = 12, 
                  segment.size  = .2, segment.color = "gray10",
                  max.overlaps = 10, force_pull = .5) +
  ## add axis labels and titles
  labs(x = "Medical doctors\nper 10,000 population",
       y = "Under 5 mortality rate\nper 1,000 population",
       fill = "",
       size = "Population size",
       title = "Childhood mortality is highest in\nareas with low health workforce density",
       caption = "Workforce data from WHO Global Health Workforce statistics database\nChildhood mortality estimates from World Bank Data Bank\nVisualization by Steph Eaneff as part of designing data visualizations workshop") +
  ## this specifies the colors for the fill values
  scale_fill_manual(values = rev(c( "#663267", "#63699B", "#4D9C9F", "#73CA81"))) +
  ## this specifies hwo the bubbles showing population size shoudl work
  scale_size_continuous(range = c(.45, 6), 
                        breaks = c(100000000, 800000000, 1400000000),
                        labels = c("100M", "800M", "1.4B")) +
  ## tell R to pay attention to the alpha values we input and use
  ## them directly to style the plot
  scale_alpha_identity() +
  ## put our legends on the top of the plot
  guides(fill = guide_legend(position = "top", ncol = 4,
                             override.aes = list(size = 2)),
         size = guide_legend(position = "top", ncol = 3),
         alpha = "none") +
  ## use a specific R theme (minimal) then make some updates below
  theme_minimal() +
  theme(## make background plot lines/grid lighter
        panel.grid.major = element_line(linewidth = 0.35),
        panel.grid.minor = element_line(linewidth = 0.2),
        legend.box = "vertical", 
        ## make text Barlow font in dark gray
        text = element_text(colour = "grey10", family = "Barlow"),
        ## adjust sizing and alignment of text
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
 
ggsave(plot = workforce_morality_scatter,
       filename = "figure-examples/mortality_vs_mds.png", 
       dpi = 350, height = 3.5, width = 5, units = "in",
       bg = 'white')

###############################################################################
### Line map showing dentists per capita by region ############################
###############################################################################

## specify which countries will get labeled in the plot
## could do this more elegantly, but I'm figuring it out as I go, 
## one country at a time
countries$horizontal_line_plot <- ""
countries[which(countries$country == "Canada"),]$horizontal_line_plot <- "Canada"
countries[which(countries$country == "United States"),]$horizontal_line_plot <- "United States"
countries[which(countries$country == "Seychelles"),]$horizontal_line_plot <- "Seychelles"
countries[which(countries$country == "Mauritius"),]$horizontal_line_plot <- "Mauritius"
countries[which(countries$country == "Lebanon"),]$horizontal_line_plot <- "Lebanon"
countries[which(countries$country == "Israel"),]$horizontal_line_plot <- "Israel"
countries[which(countries$country == "Djibouti"),]$horizontal_line_plot <- "Djibouti"
countries[which(countries$country == "Algeria"),]$horizontal_line_plot <- "Algeria"
countries[which(countries$country == "Algeria"),]$horizontal_line_plot <- "Algeria"
countries[which(countries$country == "Montenegro"),]$horizontal_line_plot <- "Montenegro"
countries[which(countries$country == "Sweden"),]$horizontal_line_plot <- "Sweden"
countries[which(countries$country == "United Kingdom"),]$horizontal_line_plot <- "United Kingdom"
countries[which(countries$country == "Vietnam"),]$horizontal_line_plot <- "Vietnam"
countries[which(countries$country == "Sudan"),]$horizontal_line_plot <- "Sudan"
countries[which(countries$country == "Jordan"),]$horizontal_line_plot <- "Jordan"
countries[which(countries$country == "Iraq"),]$horizontal_line_plot <- "Iraq"
countries[which(countries$country == "Japan"),]$horizontal_line_plot <- "Japan"
countries[which(countries$country == "Australia"),]$horizontal_line_plot <- "Australia"
countries[which(countries$country == "Vanuatu"),]$horizontal_line_plot <- "Vanuatu"

dentist_line <- countries %>%
  filter(complete.cases(most_recent_dentist_per10000)) %>%
  filter(complete.cases(region)) %>%
  ggplot(aes(x = most_recent_dentist_per10000, y = region, 
             size = total_population,
             label = horizontal_line_plot)) +
  geom_hline(yintercept = c(1:7), size = 0.3, color = "#c4cdd3") +
  geom_jitter(color = "#3C5B6F", alpha = 0.9,
              width = 0, height = 0.2) +
  geom_text_repel(size = 2, min.segment.length = 0, family = "Barlow",
                  colour = "grey10", seed = 12, 
                  segment.size  = .2, segment.color = "gray10",
                  max.overlaps = 500, force_pull = .5) +
  scale_color_manual(values = c( "#193665", "#00688f", "#339ba9", "#7fcdbb")) +
  scale_size_continuous(range = c(.45, 6), 
                        breaks = c(100000000, 800000000, 1500000000),
                        labels = c("100M", "800M", "1.4B")) +
  labs(x = "Dentists per 10,000 population",
       y = "",
       color = "Income group",
       size = "Country population",
       title = "National dentistry workforce by region",
       caption = "Workforce data from WHO Global Health Workforce statistics database\nVisualization by Steph Eaneff as part of designing data visualizations workshop") +
  theme_minimal() +
  theme(panel.grid.major = element_line(linewidth = 0),
        panel.grid.minor = element_line(linewidth = 0),
        legend.box = "vertical", 
        text = element_text(colour = "grey10", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.1)), 
        plot.subtitle = element_text(hjust = 0.5, size = rel(1)),
        axis.text.x = element_markdown(size = rel(1)),
        axis.text.y = element_markdown(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.75)),
        legend.title = element_text(size = rel(0.75)),
        #legend.key.size = unit(.4, "lines"),
        #legend.spacing.y = unit(-.6, 'lines'),
        plot.caption = element_text(size = rel(0.4))) 

dentist_line

ggsave(plot = dentist_line,
       filename = "figure-examples/dentistry_workforce.png", 
       dpi = 350, height = 3.5, width = 7, units = "in",
       bg = 'white')


####################################################################
### Boxplot showing dentists by region ############################
###################################################################

countries$barchart_region <- factor(countries$region,
                                    levels = rev(c("Europe & Central Asia",
                                               "North America",
                                               "Middle East & North Africa",
                                               "East Asia & Pacific",
                                               "Sub-Saharan Africa")))
dentist_box <- countries %>%
  filter(complete.cases(most_recent_dentist_per10000)) %>%
  filter(complete.cases(barchart_region)) %>%
  ggplot(aes(x = most_recent_dentist_per10000, y = barchart_region)) +
  geom_boxplot(fill = "#78ABA8",
               color = "gray10",
               linewidth = .3,
               width = .5) +
  labs(x = "Dentists per 10,000 population",
       y = "",
       color = "Income group",
       title = "Dentistry Workforce by Region",
       caption = "Workforce data from WHO Global Health Workforce statistics database\nVisualization by Steph Eaneff as part of designing data visualizations workshop") +
  theme_minimal() +
  theme(text = element_text(colour = "grey10", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.1)), 
        axis.text.x = element_markdown(size = rel(1)),
        axis.text.y = element_markdown(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.75)),
        legend.title = element_text(size = rel(0.75)),
        plot.caption = element_text(size = rel(0.4))) 

dentist_box

ggsave(plot = dentist_box,
       filename = "figure-examples/dentistry_workforce_box.png", 
       dpi = 350, height = 3.5, width = 5, units = "in",
       bg = 'white')

#######################################################################
### Barchart describing health workforce in Australia #################
#######################################################################

## craete new dataset and reformat it with data just from Australia
au_workforce <- countries %>%
  filter(country == "Australia") %>%
  select(country, most_recent_medical_doctor_per10000,
         most_recent_nurse_midwives_per10000,
         most_recent_pharmacists_per10000,
         most_recent_dentist_per10000,
         most_recent_chw_per10000,
         most_recent_physiotherapists_per10000) %>%
  pivot_longer(cols = most_recent_medical_doctor_per10000:most_recent_physiotherapists_per10000) 

au_workforce$name[which(au_workforce$name == "most_recent_medical_doctor_per10000")] <- "medical doctors"
au_workforce$name[which(au_workforce$name == "most_recent_nurse_midwives_per10000")] <- "nurses and midwives"
au_workforce$name[which(au_workforce$name == "most_recent_pharmacists_per10000")] <- "pharmacists"
au_workforce$name[which(au_workforce$name == "most_recent_dentist_per10000")] <- "dentists"
au_workforce$name[which(au_workforce$name == "most_recent_chw_per10000")] <- "community health workers"
au_workforce$name[which(au_workforce$name == "most_recent_physiotherapists_per10000")] <- "physiotherapists"

## order factor
au_workforce$name <- factor(au_workforce$name, 
                            levels = au_workforce$name[order(au_workforce$value)])

au_workforce_bar <- ggplot(au_workforce, aes(x = name, y = value, 
                           label = round(value, 2))) +
  geom_bar(fill = "#344C64", width = 0.8, stat = "identity", 
           color = "black", size = 0.1) + 
  labs(x = "",
       y = "Health workers per 10,000 population",
       title = "Australia's Health Workforce as of 2021",
       caption = "Workforce data from WHO Global Health Workforce statistics database\nVisualization by Steph Eaneff as part of designing data visualizations workshop") +
  theme_minimal() +
  theme(legend.box = "vertical", 
        ## make background plot lines/grid lighter
        panel.grid.major = element_line(linewidth = 0.35),
        panel.grid.minor = element_line(linewidth = 0.2),
        text = element_text(colour = "grey10", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.1)), 
        axis.text.x = element_markdown(size = rel(1)),
        axis.text.y = element_markdown(size = rel(1.1)),
        axis.title = element_text(size = rel(0.9)),
        plot.caption = element_text(size = rel(0.4)))  +
    scale_y_continuous(limits = c(0, 150)) +
    geom_text(hjust = -0.3, size = 6 / .pt,
              family = "Barlow", colour = "grey10") +
    coord_flip()

au_workforce_bar

ggsave(plot = au_workforce_bar,
       filename = "figure-examples/au_workforce.png", 
       dpi = 350, height = 3.5, width = 5, units = "in",
       bg = 'white')

#######################################################################
### Choropleth showing nursing workforce per capita ###################
#######################################################################

# Load data, using geospatial data from UNHCR
poly_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_polygons_simplified.json"
line_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_lines_simplified.json"

# add polygon info to countries dataset
poly <- read_sf(poly_url) %>%
  st_set_crs(4326) %>%
  left_join(countries, by = c("iso3" = "iso_code")) %>%
  mutate(legend = case_when(
    most_recent_nurse_midwives_per10000 < 10 ~ "less than 10",
    most_recent_nurse_midwives_per10000 <= 30 ~ "10-30",
    most_recent_nurse_midwives_per10000 <= 50 ~ "30-50",
    most_recent_nurse_midwives_per10000 > 50 ~ "more than 50",
    is.na(most_recent_nurse_midwives_per10000) ~ "no data"
  )) %>%
  mutate(legend = as_factor(legend) |> 
           fct_relevel("less than 10", "10-30", "30-50", "more than 50", "no data"))

# structure lines for map
line <- read_sf(line_url) %>%
  mutate(type = as_factor(type) %>% fct_relevel("solid", "dashed", "dotted", "dashed-dot")) %>%
  st_set_crs(4326)

## generate plot
nursing_map <- ggplot() +
  geom_sf(data = poly, aes(fill = legend), color = "transparent") +
  geom_sf(data = line, aes(linetype = type), color = "white",
          linewidth = .1, show.legend = FALSE) +
  coord_sf(crs = st_crs('ESRI:54030'), expand = TRUE) +
  scale_fill_manual(values = rev(c("gray70", "#7a0177", "#ae017e", "#f768a1", "#fbb4b9"))) +
  labs(x = "",
       y = "",
       title = "Global Nursing and Midwifery Workforce",
       fill = "Nurses and Midwives per 10,000 capita",
       caption = "Workforce data from WHO Global Health Workforce statistics database\nVisualization by Steph Eaneff as part of designing data visualizations workshop") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(colour = "grey10", family = "Barlow"),
        legend.position = "bottom",
        legend.title.align = 0.5) +
  guides(fill = guide_legend(title.position = "top"))

nursing_map

ggsave(plot = nursing_map,
       filename = "figure-examples/nursing_map.png", 
       dpi = 350, height = 3.5, width = 7, units = "in",
       bg = 'white')

