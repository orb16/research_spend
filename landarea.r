# land area and gdp

# Olivia Burge
# 15 October 2025

# packages
require(tidyverse)
require(tibble)
require(grid)
require(gridExtra)
require(cowplot)
require(readxl)
theme_set(theme_classic())


# science funding in US dollars
#https://data-explorer.oecd.org/vis?lc=en&tm=DF_TABLE1_EXPENDITURE_HCPC&pg=0&snb=1&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_NAMAIN10%40DF_TABLE1_EXPENDITURE_HCPC&df[ag]=OECD.SDD.NAD&df[vs]=&pd=%2C&dq=A.AUS%2BAUT%2BBEL%2BCAN%2BCHL%2BCOL%2BCRI%2BCZE%2BDNK%2BEST%2BFIN%2BFRA%2BDEU%2BGRC%2BHUN%2BISL%2BIRL%2BISR%2BITA%2BJPN%2BKOR%2BLVA%2BLTU%2BLUX%2BMEX%2BNLD%2BNZL%2BNOR%2BPOL%2BPRT%2BSVK%2BSVN%2BESP%2BSWE%2BCHE%2BTUR%2BGBR%2BUSA...B1GQ_POP.......&to[TIME_PERIOD]=false&vw=tb
funding <- read.csv("input/OECD.STI.STP,DSD_RDS_GERD@DF_GERD_SEO,1.0+AUS+AUT+BEL+CHL+COL+DNK+EST+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+USA+ARG+BGR+HRV+ROU+RUS+ZAF+TWN.A..GOV....._TX14+NABS01+NABS02+NABS03+NABS04+NABS0.csv")

# agricultural area (world bank)
# https://data.worldbank.org/indicator/AG.LND.AGRI.ZS

ag <- read_csv("input/API_AG.LND.AGRI.ZS_DS2_en_csv_v2_5884/API_AG.LND.AGRI.ZS_DS2_en_csv_v2_5884.csv", skip = 4) %>%
  dplyr::select(-`...70`)%>%
  dplyr::select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, `2014`:`2024`) %>%
  gather(Year, agArea, `2014`:`2024`)

# terrestrial protected area (wrold bank)
#https://data.worldbank.org/indicator/ER.LND.PTLD.ZS
proc <- read_csv("input/API_ER.LND.PTLD.ZS_DS2_en_csv_v2_6158/API_ER.LND.PTLD.ZS_DS2_en_csv_v2_6158.csv",skip = 4) %>%
  dplyr::select(-`...70`) %>%
  dplyr::select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, `2014`:`2024`) %>%
  gather(Year, protectedArea, `2014`:`2024`)

land <- read_csv("input/API_AG.LND.TOTL.K2_DS2_en_csv_v2_22546/API_AG.LND.TOTL.K2_DS2_en_csv_v2_22546.csv",skip = 4)%>%
  dplyr::select(-`...70`) %>%
  dplyr::select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, `2014`:`2024`) %>%
  gather(Year, landArea, `2014`:`2024`) %>%
  dplyr::select(-`Indicator Name`) %>% 
  dplyr::select(-`Indicator Code`)

# get everything in 2021

envAgFunding2021 <- funding %>% 
  filter(TIME_PERIOD == 2021 & Socio.economic.objectives %in%  c("Environment", "Agriculture") & Price.base == "Constant prices") %>%
  # price base has two optiosn but they don't appear to differ so much
  dplyr::select(TIME_PERIOD, Reference.area, Socio.economic.objectives, REF_AREA, OBS_VALUE, Unit.of.measure, Unit.multiplier) 
nrow(envAgFunding2021) # should stay like this through the jions

fund2021Total <- funding %>% filter(Socio.economic.objectives == "Total" & TIME_PERIOD == 2021 & Price.base == "Constant prices") %>%
  dplyr::select(TIME_PERIOD, Reference.area, TotalSpent = OBS_VALUE, REF_AREA)

proc2021 <- proc %>% rename(REF_AREA = `Country Code`, TIME_PERIOD = Year) %>%
  filter(TIME_PERIOD == 2021)

ag2021 <- ag %>% rename(REF_AREA = `Country Code`, TIME_PERIOD = Year) %>%
  filter(TIME_PERIOD == 2021)

land2021 <- land %>% rename(REF_AREA = `Country Code`, TIME_PERIOD = Year) %>%
  filter(TIME_PERIOD == 2021) 

join21b <- left_join(envAgFunding2021, proc2021%>% mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>% dplyr::select(-`Indicator Name`) %>% dplyr::select(-`Indicator Code`)) %>%
  left_join(., ag2021 %>% mutate(TIME_PERIOD = as.numeric(TIME_PERIOD))%>% dplyr::select(-`Indicator Name`) %>% dplyr::select(-`Indicator Code`))
nrow(join21b)


join21 <- left_join(join21b, fund2021Total %>% mutate(TIME_PERIOD = as.numeric(TIME_PERIOD))) %>%
  left_join(., land2021%>% mutate(TIME_PERIOD = as.numeric(TIME_PERIOD))) %>%
  mutate(isNZ = ifelse(Reference.area == "New Zealand", TRUE, FALSE))

# this is total spend by % area, which is not exctly what we want
ggplot(join21 %>% filter(Socio.economic.objectives=="Environment"), aes(x = protectedArea, y = OBS_VALUE)) +
  geom_point() +
  geom_text(aes(label = `Country Name`), hjust =0, nudge_x = .5, 
            size =3) +
  labs(x = "% of country in protected land", y = "Govt investment in\nenvironmental science (USD; millions)")

ggplot(join21 %>% filter(Socio.economic.objectives=="Agriculture"), aes(x = agArea, y = OBS_VALUE)) +
  geom_point() +
  geom_text(aes(label = `Country Name`), hjust =0, nudge_x = .5, 
            size =3) +
  labs(x = "% of country in agricultural land", y = "Govt investment in\nagricultural science (USD; millions)")


# these do % vs %, but the problem is, they don't control for the very small spend by NZ
ggplot(join21 %>% filter(Socio.economic.objectives=="Environment"), aes(x = protectedArea, y = 100* OBS_VALUE/TotalSpent)) +
  geom_point() +
  geom_text(aes(label = `Country Name`), hjust =0, nudge_x = .5, 
            size =3) +
  labs(x = "% of country in protected land", y = "Govt investment in\nenvironmental science (%)")

ggplot(join21 %>% filter(Socio.economic.objectives=="Agriculture"), aes(x = agArea, y = 100* OBS_VALUE / TotalSpent)) +
  geom_point() +
  geom_text(aes(label = `Country Name`), hjust =0, nudge_x = .5, 
            size =3) +
  labs(x = "% of country in agricultural land", y = "Govt investment in\nagricultural science (%)")

# now by actual land area and actual spend ----
join21$protectedAreaRaw <- with(join21, ((protectedArea/100) * landArea))
join21 %>% filter(Socio.economic.objectives == "Agriculture" & Reference.area == "New Zealand")
protectedPlot <- ggplot(join21 %>% filter(Socio.economic.objectives=="Environment"), aes(x =protectedAreaRaw, y = OBS_VALUE)) +
  geom_point(aes(colour = isNZ), show.legend = FALSE) +
  geom_text(aes(label = `Country Name`,
                colour = isNZ), 
            hjust =0, nudge_x = 2000,
            size =3, show.legend = FALSE) +
  labs(x = bquote("Area of country in protected land (km"^2*")"), y = "Govt investment in\nenvironmental science (USD; millions)")+
  ggtitle("Environment R+D spend (2021)") +
  scale_colour_manual(values = c("#00B050", "black")) +
  scale_x_continuous(labels = scales::label_comma())
protectedPlot
join21$agAreaRaw <- with(join21, ((agArea/100) * landArea))
join21 %>% filter(Socio.economic.objectives == "Agriculture" & Reference.area == "New Zealand")

agriculturalPlot <- ggplot(join21 %>% filter(Socio.economic.objectives=="Agriculture"), aes(x = agAreaRaw, y = OBS_VALUE)) +
  geom_point(aes(colour = isNZ), show.legend = FALSE) +
  geom_text(aes(label = `Country Name`,
                colour = isNZ), 
            hjust =0, nudge_x = 10000,
            size =3, show.legend = FALSE) +
  labs(x = bquote("Area of country in agricultural land (km"^2*")"), y = "Govt investment in\nagricultural science (USD; millions)") +
  ggtitle("Agriculture R+D spend (2021)") +
  scale_colour_manual(values = c("#92D050", "black")) +
  scale_x_continuous(labels = scales::label_comma()) 
agriculturalPlot


doublePlot <- cowplot::plot_grid(protectedPlot, agriculturalPlot, labels = "auto", nrow = 2)

ggsave(doublePlot,
       file = "output/spend_per_km2.png",
         width = 140, height = 210, units = "mm", 
       dpi = 500)

# just NZ
jnz <- join21 %>% filter(Reference.area =="New Zealand")
jnz

# calculate the spend in USD, multiply by one million to get real USD 

# spend on ag per km2 of ag
agnz <- jnz %>% filter(Socio.economic.objectives == "Agriculture")
with(agnz, (OBS_VALUE * 1000000) / agAreaRaw)
# $1745 USD per km2 of ag land
# or, per ha,
with(agnz, (OBS_VALUE * 1000000) / (agAreaRaw * 100))
# 17 USD per ha

# spend on ag per km2 of env
envnz <- jnz %>% filter(Socio.economic.objectives == "Environment")
with(envnz, (OBS_VALUE * 1000000) / protectedAreaRaw)
# $2738 USD per km2 of protected land
# or, per ha, 
with(envnz, (OBS_VALUE * 1000000) / (protectedAreaRaw * 100))
# $27 USD per ha of protected land


