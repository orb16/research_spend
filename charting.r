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
# I think I used this one: https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C0%7CScience%252C%20technology%20and%20innovation%23INT%23&fs[1]=Topic%2C1%7CScience%252C%20technology%20and%20innovation%23INT%23%7CResearch%20and%20development%20%28R%26D%29%23INT_RD%23&pg=0&fc=Topic&snb=19&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_RDS_GERD%40DF_GERD_SEO&df[ag]=OECD.STI.STP&df[vs]=1.0&dq=AUS%2BAUT%2BBEL%2BCHL%2BCOL%2BDNK%2BEST%2BFRA%2BDEU%2BGRC%2BHUN%2BIRL%2BISR%2BITA%2BJPN%2BKOR%2BLVA%2BLTU%2BLUX%2BNZL%2BNOR%2BPOL%2BPRT%2BSVK%2BSVN%2BESP%2BSWE%2BCHE%2BTUR%2BUSA%2BARG%2BBGR%2BHRV%2BROU%2BRUS%2BZAF%2BTWN.A..GOV....._TX14%2BNABS01%2BNABS02%2BNABS03%2BNABS04%2BNABS05%2BNABS06%2BNABS07%2BNABS08%2BNABS09%2BNABS10%2BNABS11%2BNABS14%2BNABS12_13%2B_T.XDC.&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb
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
# here we take the most recent year (if not 2021) for which 3 or 
# more indicators were present (sumNAs > 3; NB naming misinformative), 
# note that an indicator will be counted if zero spend (because not NA), hence Latvia
# NB I can't figure out which year they were using for Latvia, South Africa, Lithuania


getRecents <- function(data, period = NULL){
  if(!is.null(period)){
    setup <- data %>% 
      filter(TIME_PERIOD < period) 
  } else {
    setup <- data
  }
  
  ret <- setup %>% 
    group_by(Reference.area, TIME_PERIOD, Socio.economic.objectives, isPres = !is.na(OBS_VALUE)) %>% summarise()  %>%
    group_by(Reference.area, TIME_PERIOD) %>%
    summarise(sumNAs = sum(isPres)) %>%
    filter(sumNAs > 3)%>%
    dplyr::select(Reference.area, TIME_PERIOD) %>%
    group_by(Reference.area) %>%
    arrange(desc(TIME_PERIOD)) %>%
    slice(1) %>%
    data.frame(.)
  return(ret)
  
}


mostRecent <- getRecents(tab, period = 2022)


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
  group_by(Reference.area, TIME_PERIOD, new) %>%
  summarise(sumObs = sum(OBS_VALUE), .groups = "drop") %>%
  left_join(., total) %>%
  mutate(percentTotal = (100 * sumObs) / totalSpent)

# ordered countries 
ourCountries <- c("New Zealand", "Switzerland", "Ireland", "Israel", "Latvia", "Colombia", "Argentina", 
                  "Romania", "Slovak Republic", "Portugal", 
                  "Poland", "South Africa", "Lithuania", "Luxembourg", 
                  "Türkiye", "Spain", "Italy", "Japan", "Chinese Taipei", "Korea", "Greece", "Estonia", "Hungary", 
                  "Germany", "Russia", "Austria", "France", "Sweden", "Denmark")

ourTypes <- c("Environment", "Agriculture", "Industrial production and technology", "Health", "Energy", "Defence", "Exploitation of the Earth",
              "Exploitation of space", "Other")

# matched to our types, hex codes obtained from report
ourCols <- c(
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

# factor with same levels as original Figure 3
short$new2 <- factor(short$new, levels = rev(ourTypes))

# bar width to match figure
bar_width = .8

orig <- ggplot(data = short, aes(x = Reference.area, y = percentTotal)) + 
  geom_bar(aes(fill = new2), 
           stat = "identity",
           position=position_stack(), width = bar_width) +
  scale_fill_manual(
    limits = ourTypes,
    values = ourCols
  ) + 
  scale_x_discrete(limits = ourCountries) + 
  labs(x = "Country", y = "Percentage of GOVRED") +
  ggtitle(paste("Percentage of government expenditure on R&D\nper socio-economic area, as at", "~2021")) +
  theme_minimal()+
  theme(panel.grid.minor.x =  element_blank(),
        panel.grid.major.x =  element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), expand = c(0,0)) +
  labs(fill = "")
orig


# now we do the same thing, but for the most recent data
# here, getting anything from 2023, but dropping back to less recent years if needed

mostRecent2 <- getRecents(tab)

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
  group_by(Reference.area, TIME_PERIOD, new) %>%
  summarise(sumObs = sum(OBS_VALUE), .groups = "drop") %>%
  left_join(., total2) %>%
  mutate(percentTotal = (100 * sumObs) / totalSpent)


short2$new2 <- factor(short2$new, levels = rev(ourTypes))
updated <- ggplot(data = short2, aes(x = Reference.area, y = percentTotal)) + 
  geom_bar(aes(fill = new2), 
           stat = "identity",
           position=position_stack(), width = bar_width) +
  scale_fill_manual(
    limits = ourTypes,
    values = ourCols
  ) + 
  scale_x_discrete(limits = ourCountries) + 
  labs(x = "Country", y = "Percentage of GOVRED") +
  ggtitle(paste("Percentage of government expenditure on R&D\nper socio-economic area, as at", "most recent measure (~2023)")) +
  theme_minimal() + 
  theme(panel.grid.minor.x =  element_blank(),
        panel.grid.major.x =  element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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
  theme_minimal()  +
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
  theme_minimal()  +
  theme(legend.position = "bottom") + 
  scale_colour_manual("", values = c("black", "grey50")) +
  scale_linetype_manual("", values = c("dashed", "solid")) +
  labs(x = "Year", y = "Spend (millions)",
       title = "Government expenditure on R&D:\nagriculture and environment over time") 

nzPlot_raw

ggsave(nzPlot_raw,
       file = "output/nz_over_time_raw.png",
       width = 210, height = 140, units = "mm",
       dpi = 600)
