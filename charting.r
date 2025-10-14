# Recreating, so far as possible, the figure 3 in the MBIE report Overview of the technology research landscape in New Zealand 2024
# report available in repo, sourced from: https://www.mbie.govt.nz/dmsdocument/30369-overview-of-the-technology-research-landscape-in-new-zealand-pdf

# Olivia Burge
# 14 October 2025

# packages
require(tidyverse)
require(tibble)
require(grid)
require(gridExtra)
require(cowplot)

# downloaded all years, all countries, for which info was available, just govt spend.
tab <- read.csv("input/v2OECD.STI.STP,DSD_RDS_GERD@DF_GERD_SEO,1.0+AUS+AUT+BEL+CHL+COL+DNK+EST+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+USA+ARG+BGR+HRV+ROU+RUS+ZAF+TWN.A..GOV.....NABS11+NABS10+NABS09+NABS12_13+NABS04+_.csv")
#data query: https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_RDS_GERD@DF_GERD_SEO,1.0/AUS+AUT+BEL+CHL+COL+DNK+EST+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+USA+ARG+BGR+HRV+ROU+RUS+ZAF+TWN.A..GOV.....NABS11+NABS10+NABS09+NABS12_13+NABS04+_TX14+NABS01+NABS02+NABS03+NABS05+NABS06+NABS07+NABS08+NABS14+_T.XDC.?startPeriod=2015&dimensionAtObservation=AllDimensions
# structure query: https://sdmx.oecd.org/public/rest/dataflow/OECD.STI.STP/DSD_RDS_GERD@DF_GERD_SEO/1.0?references=all

unique(tab$Socio.economic.objectives)

# the report places some categories in an "other" column
matching <- tribble(~new, ~Socio.economic.objectives,
        "Environment", "Environment", 
        "Agriculture", "Agriculture",
        "Industrial production and technology", "Industrial production and technology",
        "Health", "Health",
        "Energy", "Energy", 
        "Defence", "Defence",
        "Exploitation of the Earth", "Exploration and exploitation of the Earth",
        "Exploitation of space",  "Exploration and exploitation of space",
        "Other", "Transport, telecommunication and other infrastructures",
        "Other", "General advancement of knowledge",
        "Other", "Education",
        "Other", "Political and social systems, structures and processes",
        "Other", "Culture, recreation, religion and mass media" )


# It looks like most country data comes from 2021, but some countries did not report in 2021, and therefore I assume
# that the most recent "complete" data were used
# here we take the most recent year (if not 2021) for which 5 or fewer indicators were NA
# NB I can't figure out which year they were using for Latvia

mostRecent <- tab %>%
  filter(TIME_PERIOD < 2022) %>%
  group_by(Reference.area, TIME_PERIOD, Socio.economic.objectives, isPres = is.na(OBS_VALUE)) %>% summarise()  %>%
  group_by(Reference.area, TIME_PERIOD) %>%
  summarise(sumNAs = sum(isPres)) %>%
  filter(! sumNAs > 5)%>%
  dplyr::select(Reference.area, TIME_PERIOD) %>%
  group_by(Reference.area) %>%
  arrange(desc(TIME_PERIOD)) %>%
  slice(1) %>%
  data.frame(.)


# get the "total" spend. This controls for teh fact that some spend is not reported as "confidential" - 
# e.g. NZ does not report "Exploitation of teh Earth" as confidential

total <- tab %>% 
  filter(Socio.economic.objectives == "Total") %>%
  left_join(mostRecent,.) %>%
  group_by(Reference.area, totalSpent = OBS_VALUE, Socio.economic.objectives, TIME_PERIOD) %>%
  summarise(.groups = "drop") %>%
  dplyr::select(-Socio.economic.objectives)

# here we get the most recent year (in this case, prior to 2022)
# and sum (because of the "other" col) 
# and caluculate all spend as a percent of the total

short <- tab %>% filter(!Socio.economic.objectives == "Total") %>%
  #filter(TIME_PERIOD == ourYear) %>%
  left_join(mostRecent, .) %>%
  left_join(., matching) %>%
  group_by(Reference.area, OBS_VALUE, TIME_PERIOD, new) %>%
  summarise(sumObs = sum(OBS_VALUE), .groups = "drop") %>%
  left_join(., total) %>%
  mutate(percentTotal = (100 * sumObs) / totalSpent)

# factor with same levels as original Figure 3
short$new2 <- factor(short$new, levels = rev(c("Environment", "Agriculture", "Industrial production and technology", "Health", "Energy", "Defence", "Exploitation of the Earth",
                                           "Exploitation of space", "Other")))

# bar width to match figure
bar_width = .8
orig <- ggplot(data = short, aes(x = Reference.area, y = percentTotal)) + 
  geom_bar(aes(fill = new2), 
           stat = "identity",
           position=position_stack(), width = bar_width) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(
    limits = c("Environment", "Agriculture", "Industrial production and technology", "Health", "Energy", "Defence", "Exploitation of the Earth",
               "Exploitation of space", "Other"),
    values = c(
      "#00B050",
      "#92D050",
      "#FFC000",
      "#00B0F0",
      "#FFFF00",
      "#FF9999",
      "#C00000",
      "#0070C0",
      "#BFBFBF"
    )
  ) + 
  scale_x_discrete(limits = c("New Zealand", "Switzerland", "Ireland", "Israel", "Latvia", "Colombia", "Argentina", 
                              "Romania", "Slovak Republic", "Portugal", 
                              "Poland", "South Africa", "Lithuania",
                              "Türkiye", "Spain", "Italy", "Japan", "Chinese Taipei", "Korea", "Greece", "Estonia", "Hungary", 
                              "Germany", "Russia", "Austria", "France", "Sweden", "Denmark")) + 
  labs(x = "Country", y = "Percentage of GOVRED") +
  ggtitle(paste("Percentage of government expenditure on R&D\nper socio-economic area, as at", "~2021")) +
  theme(panel.grid.minor.x =  element_blank(),
        panel.grid.major.x =  element_blank()) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), expand = c(0,0)) +
  labs(fill = "")
orig


# now we do the same thing, but for the most recent data
# here, getting anything from 2023, but dropping back to less recent years if needed
mostRecent2 <- tab %>%
  # filter(TIME_PERIOD < 2022) %>%
  group_by(Reference.area, TIME_PERIOD, Socio.economic.objectives, isPres = is.na(OBS_VALUE)) %>% summarise()  %>%
  group_by(Reference.area, TIME_PERIOD) %>%
  summarise(sumNAs = sum(isPres)) %>%
  filter(! sumNAs > 5)%>%
  dplyr::select(Reference.area, TIME_PERIOD) %>%
  group_by(Reference.area) %>%
  arrange(desc(TIME_PERIOD)) %>%
  slice(1) %>%
  data.frame(.)

# calculate totals for 'most recent'
total2 <- tab %>% 
  filter(Socio.economic.objectives == "Total") %>%
  # filter(TIME_PERIOD == 2023) %>%
  left_join(mostRecent2,.) %>%
  group_by(Reference.area, totalSpent = OBS_VALUE, Socio.economic.objectives, TIME_PERIOD) %>%
  summarise(.groups = "drop") %>%
  dplyr::select(-Socio.economic.objectives)

# here we get the most recent year 
# and sum (because of the "other" col) 
# and caluculate all spend as a percent of the total
short2 <- tab %>% filter(!Socio.economic.objectives == "Total") %>%
  # filter(TIME_PERIOD == 2023) %>%
  left_join(mostRecent2, .) %>%
  left_join(., matching) %>%
  group_by(Reference.area, OBS_VALUE, TIME_PERIOD, new) %>%
  summarise(sumObs = sum(OBS_VALUE), .groups = "drop") %>%
  left_join(., total2) %>%
  mutate(percentTotal = (100 * sumObs) / totalSpent)


short2$new2 <- factor(short2$new, levels = rev(c("Environment", "Agriculture", "Industrial production and technology", "Health", "Energy", "Defence", "Exploitation of the Earth",
                                               "Exploitation of space", "Other")))
updated <- ggplot(data = short2, aes(x = Reference.area, y = percentTotal)) + 
  geom_bar(aes(fill = new2), 
           stat = "identity",
           position=position_stack(), width = bar_width) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(
    limits = c("Environment", "Agriculture", "Industrial production and technology", "Health", "Energy", "Defence", "Exploitation of the Earth",
               "Exploitation of space", "Other"),
    values = c(
      "#00B050",
      "#92D050",
      "#FFC000",
      "#00B0F0",
      "#FFFF00",
      "#FF9999",
      "#C00000",
      "#0070C0",
      "#BFBFBF"
    )
  ) + 
  scale_x_discrete(limits = c("New Zealand", "Switzerland", "Ireland", "Israel", "Latvia", "Colombia", "Argentina", 
                              "Romania", "Slovak Republic", "Portugal", 
                              "Poland", "South Africa", "Lithuania",
                              "Türkiye", "Spain", "Italy", "Japan", "Chinese Taipei", "Korea", "Greece", "Estonia", "Hungary", 
                              "Germany", "Russia", "Austria", "France", "Sweden", "Denmark")) + 
  labs(x = "Country", y = "Percentage of GOVRED") +
  ggtitle(paste("Percentage of government expenditure on R&D\nper socio-economic area, as at", "most recent measure (~2023)")) +
  theme(panel.grid.minor.x =  element_blank(),
        panel.grid.major.x =  element_blank()) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), expand = c(0,0)) +
  labs(fill = "")

updated

#leg <- get_legend(orig + theme(legend.position = 'bottom'))
leg <- get_legend(orig + theme(legend.text = element_text(size = rel(.7))))
# ourPlots <- plot_grid(plot_grid(orig + theme(legend.position = 'right'), updated  + theme(legend.position = 'right'), 
#           nrow = 2), leg, nrow = 1, rel_widths = c(2, 1.6))


ourPlots <- plot_grid(orig + theme(legend.position = 'right'), updated  + theme(legend.position = 'right'), 
                                nrow = 2)


ggsave(ourPlots, file = "output/revised_figure3.png", width = 140, height = 210, units = "mm",
       scale = 1.5)

# New Zealand through time ----
# e.g. NZ does not report "Exploitation of teh Earth" as confidential
# why has environment dropped so much in 2023?

totalTime <- tab %>% 
  filter(Socio.economic.objectives == "Total" & Reference.area == "New Zealand") %>%
  group_by(Reference.area, totalSpent = OBS_VALUE, Socio.economic.objectives, TIME_PERIOD) %>%
  summarise(.groups = "drop") %>%
  dplyr::select(-Socio.economic.objectives)

short3 <- tab %>% filter(!Socio.economic.objectives == "Total") %>%
  left_join(totalTime, .) %>%
  left_join(., matching) %>%
  group_by(Reference.area, TIME_PERIOD, totalSpent, new) %>%
  summarise(sumObs = sum(OBS_VALUE), .groups = "drop") %>%
  mutate(percentTotal = (100 * sumObs) / totalSpent)

# just showing agriculture and environment
# others are more messy
nzPlot <- ggplot(short3 %>% filter(new %in% c("Agriculture", "Environment")), 
                 aes(x = TIME_PERIOD, y = percentTotal)) +
  geom_line(aes(colour = new, linetype = new)) +
  geom_point(aes(colour = new)) +
  theme(legend.position = "bottom") + 
  scale_colour_manual("", values = c("black", "grey50")) +
  scale_linetype_manual("", values = c("dashed", "solid")) +
  labs(x = "Year", y = "Percentage of GOVRED",
       title = "Percentage of government expenditure on R&D:\nagriculture and environment over time")

nzPlot

ggsave(nzPlot,
       file = "output/nz_over_time.png",
       width = 210, height = 140, units = "mm",
       dpi = 600)

# plotting the raw numbers
nzPlot_raw <- ggplot(tab %>% filter(Socio.economic.objectives %in% c("Agriculture", "Environment") & Reference.area == "New Zealand"), 
                 aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line(aes(colour = Socio.economic.objectives, linetype = Socio.economic.objectives)) +
  geom_point(aes(colour = Socio.economic.objectives)) +
  theme(legend.position = "bottom") + 
  scale_colour_manual("", values = c("black", "grey50")) +
  scale_linetype_manual("", values = c("dashed", "solid")) +
  labs(x = "Year", y = "Spend (millions)",
       title = "Percentage of government expenditure on R&D:\nagriculture and environment over time")

nzPlot_raw

ggsave(nzPlot_raw,
       file = "output/nz_over_time_raw.png",
       width = 210, height = 140, units = "mm",
       dpi = 600)
