## This script produces final visualisations of global, GN, and GS charts with respect to the three boundaries,
# starting in 1960 (main) and 1850 (SI).

#Read in datafiles from previous scripts (if needed)
#myData20 <- read_csv("./myData/07_myDataCO2convergenceWithGroups_v3.csv")

#overshoot1 <- read_csv("./myData/11_myDataOvershootGDP_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double()))

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#-------------------------------------------------------------------------------------------------------------------
#		PREP NORMALISED VALUES
#-------------------------------------------------------------------------------------------------------------------

overshoot2 <- overshoot1 %>%
	rowwise() %>%
	mutate(overshootFairShare350 = fairShare350 / fairShare15C,
	  overshootFairShare15C = fairShare15C / fairShare15C,
	  overshootFairShare2C = fairShare2C / fairShare15C) %>%
	ungroup()

rm(overshoot1)

#---------------------------------------------------------------------------------------------------------------------------
#		make world, Global South and global North figures
#---------------------------------------------------------------------------------------------------------------------------
#prep data
global <- overshoot2 %>%
	filter(iso3c != "WLD", scenario %in% c("Historical", "BAU", "NetZero")) %>%
	select(startDate, scenario, iso3c, date, cumCO2, cumCO2_lwr, cumCO2_upr, fairShare350, fairShare15C, fairShare2C,
		overshoot350, overshoot350lwr, overshoot350upr, overshootFairShare350, overshootFairShare15C, overshootFairShare2C) %>%
	group_by(startDate, scenario, date) %>%
	summarise(cumCO2 = sum(cumCO2, na.rm=T),
		cumCO2_lwr = sum(cumCO2_lwr, na.rm=T),
		cumCO2_upr = sum(cumCO2_upr, na.rm=T),
		fairShare350 = sum(fairShare350, na.rm=T),
		fairShare15C = sum(fairShare15C, na.rm=T),
		fairShare2C = sum(fairShare2C, na.rm=T)) %>%
	ungroup() %>%
	rowwise() %>%
	mutate(overshoot15C = round(cumCO2 / fairShare15C, digits=2),
		overshoot15Clwr = round(cumCO2_lwr / fairShare15C, digits=2),
		overshoot15Cupr = round(cumCO2_upr / fairShare15C, digits=2),
		overshootFairShare350 = fairShare350 / fairShare15C,
		overshootFairShare15C = fairShare15C / fairShare15C,
		overshootFairShare2C = fairShare2C / fairShare15C) %>%
	ungroup() %>%
	add_column(northSouth = "WLD") %>%
	relocate(northSouth, .before=date)


northSouth <- overshoot2 %>%
	filter(iso3c != "WLD", scenario %in% c("Historical", "BAU", "NetZero")) %>%
	group_by(startDate, scenario, northSouth, date) %>%
	summarise(cumCO2 = sum(cumCO2, na.rm=T),
		cumCO2_lwr = sum(cumCO2_lwr, na.rm=T),
		cumCO2_upr = sum(cumCO2_upr, na.rm=T),
		fairShare350 = sum(fairShare350, na.rm=T),
		fairShare15C = sum(fairShare15C, na.rm=T),
		fairShare2C = sum(fairShare2C, na.rm=T)) %>%
	ungroup() %>%
	rowwise() %>%
	mutate(overshoot15C = round(cumCO2 / fairShare15C, digits=2),
		overshoot15Clwr = round(cumCO2_lwr / fairShare15C, digits=2),
		overshoot15Cupr = round(cumCO2_upr / fairShare15C, digits=2),
		overshootFairShare350 = fairShare350 / fairShare15C,
		overshootFairShare15C = fairShare15C / fairShare15C,
		overshootFairShare2C = fairShare2C / fairShare15C) %>%
	ungroup()

globalNS <- rbind(northSouth, global)
rm(global, northSouth)

globalNS1 <- globalNS %>%
	rowwise() %>%
	mutate(labelNS = ifelse(northSouth == "NRT", "Global North", 
	  ifelse(northSouth == "STH", "Global South", "World"))) %>%
	ungroup() %>%
	mutate(northSouth = factor(northSouth, levels=c("WLD", "STH", "NRT")),
		labelNS = factor(labelNS, levels=c("World", "Global South", "Global North"))) %>%
	relocate(labelNS, .before=northSouth)

rm(globalNS)
#----------------------------------------------------------------------------------------------------
#		Figure 1. WORLD, GLOBAL SOUTH, AND GLOBAL NORTH CHART WITH BAU AND NET ZERO (1960-2050)
#----------------------------------------------------------------------------------------------------
countries <- globalNS1 

#labeller function
myLabels <- function(x) {
  val <- round(x / 1000000, digits=0)
  return(val) 
}

#VISUALISE
#ggsave("./Figures/Figure1_GN-GS-WLDcumulativeCO2-fairShares_v4.pdf", 
  ggplot(data = countries %>% filter(startDate == 1960), aes(x = date)) + 
	geom_area(data = countries %>% filter(startDate == 1960, scenario %in% c("Historical", "NetZero")),
	  aes(y = overshootFairShare350), fill= "#a1d99b", alpha=0.3) + #, 
	geom_ribbon(data = countries %>% filter(startDate == 1960, scenario != "NetZero"),
	  aes(ymin = overshootFairShare350, ymax = overshootFairShare15C), fill= "#ffeda0", alpha=0.3) +
	geom_ribbon(data = countries %>% filter(startDate == 1960, scenario != "NetZero"),
	  aes(ymin = overshootFairShare15C, ymax = overshootFairShare2C), fill= "#fc4e2a", alpha=0.3) +
	geom_ribbon(data = countries %>% filter(startDate == 1960, scenario != "NetZero"),
	  aes(ymin = overshootFairShare2C, ymax = 4.5), fill= "#ce2029", alpha=0.5) + ##ef3b2c #e31a1c 
	geom_area(data = countries %>% filter(startDate == 1960, scenario == "Historical"),
	  aes(y = overshoot15C), fill="black") +
	geom_line(data = countries %>% filter(startDate == 1960, scenario != "NetZero"),
	  aes(y = overshootFairShare350), size=0.9, colour = "#387f30") +
	geom_line(data = countries %>% filter(startDate == 1960, scenario != "NetZero"),
	  aes(y = overshootFairShare15C), size=0.9, colour = "#feb24c") + #a1d99b"
	geom_line(data = countries %>% filter(startDate == 1960, scenario != "NetZero"),
	  aes(y = overshootFairShare2C), size=0.9, colour = "#e31a1c") + #de970b
	geom_ribbon(data = countries %>% filter(startDate == 1960, scenario == "BAU"), 
	  aes(ymin = overshoot15Clwr, ymax = overshoot15Cupr), alpha=0.3, fill="#525252") + #969696
	geom_line(data = countries %>% filter(startDate == 1960, scenario == "BAU"),
	  aes(y = overshoot15C), size=0.9, colour = "black", linetype="dashed") +
	geom_line(data = countries %>% filter(startDate == 1960, scenario == "NetZero"),
	  aes(y = overshoot15C), size=0.9, colour = "#0000a5") +
	scale_y_continuous(limits=c(0,4.5), breaks=seq(0,4.5,0.5), expand=c(0,0)) +
	scale_x_continuous(expand=c(0,0)) +
	facet_wrap(~ labelNS) +
	labs(x="Year", y="Cumulative CO2 emissions (1 = fair share of 1.5°C budget)") +
	theme_chart_SMALLM +
	theme(legend.title=element_blank(),
	  legend.position="none",
	  plot.margin = unit(c(0.5,6,0.5,0.5), "lines"),
	  panel.grid = element_blank(),
	  panel.spacing.x = unit(1.25, "lines"),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=8, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=8, family="sans", color="#636363"),
	  axis.text = element_text(colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363")),
width = 183, height = 80, units = "mm", device = "pdf")

#----------------------------------------------------------------------------------------------------
#		Figure S1. WORLD, GLOBAL SOUTH, AND GLOBAL NORTH CHART WITH BAU AND NET ZERO (1850-2050)
#----------------------------------------------------------------------------------------------------
countries <- globalNS1 

#labeller function
myLabels <- function(x) {
  val <- round(x / 1000000, digits=0)
  return(val) 
}

#VISUALISE
#ggsave("./Figures/FigureS1_GN-GS-WLDcumulativeCO2-fairShares_v4.pdf", 
  ggplot(data = countries %>% filter(startDate == 1960), aes(x = date)) + 
	geom_area(data = countries %>% filter(startDate == 1850, scenario %in% c("Historical", "NetZero")),
	  aes(y = overshootFairShare350), fill= "#a1d99b", alpha=0.3) + #, 
	geom_ribbon(data = countries %>% filter(startDate == 1850, scenario != "NetZero"),
	  aes(ymin = overshootFairShare350, ymax = overshootFairShare15C), fill= "#ffeda0", alpha=0.3) +
	geom_ribbon(data = countries %>% filter(startDate == 1850, scenario != "NetZero"),
	  aes(ymin = overshootFairShare15C, ymax = overshootFairShare2C), fill= "#fc4e2a", alpha=0.3) +
	geom_ribbon(data = countries %>% filter(startDate == 1850, scenario != "NetZero"),
	  aes(ymin = overshootFairShare2C, ymax = 3.75), size=1.2, fill= "#ce2029", alpha=0.5) +
	geom_area(data = countries %>% filter(startDate == 1850, scenario == "Historical"),
	  aes(y = overshoot15C), fill="black") +
	geom_line(data = countries %>% filter(startDate == 1850, scenario != "NetZero"),
	  aes(y = overshootFairShare350), size=0.9, colour = "#387f30") +
	geom_line(data = countries %>% filter(startDate == 1850, scenario != "NetZero"),
	  aes(y = overshootFairShare15C), size=0.9, colour = "#feb24c") + #a1d99b"
	geom_line(data = countries %>% filter(startDate == 1850, scenario != "NetZero"),
	  aes(y = overshootFairShare2C), size=0.9, colour = "#e31a1c") + #de970b
	geom_ribbon(data = countries %>% filter(startDate == 1850, scenario == "BAU"), 
	  aes(ymin = overshoot15Clwr, ymax = overshoot15Cupr), alpha=0.3, fill="#525252") + #969696
	geom_line(data = countries %>% filter(startDate == 1850, scenario == "BAU"),
	  aes(y = overshoot15C), size=0.9, colour = "black", linetype="dashed") +
	geom_line(data = countries %>% filter(startDate == 1850, scenario == "NetZero"),
	  aes(y = overshoot15C), size=0.9, colour = "#0000a5") +
	facet_wrap(~ labelNS) +
	scale_y_continuous(limits=c(0,3.75), breaks=seq(0,3.75,0.5), expand=c(0,0)) +
	scale_x_continuous(breaks=seq(1850,2050,50), expand=c(0,0)) +
	labs(x="Year", y="Cumulative CO2 emissions (1 = fair share of 1.5°C budget)") +
	theme_chart_SMALLM +
	theme(legend.title=element_blank(),
	  legend.position="none",
	  plot.margin = unit(c(0.5,6,0.5,0.5), "lines"),
	  panel.grid = element_blank(),
	  panel.spacing.x = unit(1.25, "lines"),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=8, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=8, family="sans", color="#636363"),
	  axis.text = element_text(colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363")),
width = 183, height = 80, units = "mm", device = "pdf")


#----------------------------------------------------------------------------------------------------
#		Figure S2. WORLD, GLOBAL SOUTH, AND GLOBAL NORTH CHART WITH BAU AND NET ZERO (1992-2050)
#----------------------------------------------------------------------------------------------------
countries <- globalNS1 

#labeller function
myLabels <- function(x) {
  val <- round(x / 1000000, digits=0)
  return(val) 
}

#VISUALISE
#ggsave("./Figures/FigureS2_GN-GS-WLDcumulativeCO2-fairShares_v4.pdf", 
  ggplot(data = countries %>% filter(startDate == 1992), aes(x = date)) + 
	geom_area(data = countries %>% filter(startDate == 1992, scenario %in% c("Historical", "NetZero")),
	  aes(y = overshootFairShare350), fill= "#a1d99b", alpha=0.3) + #, 
	geom_ribbon(data = countries %>% filter(startDate == 1992, scenario != "NetZero"),
	  aes(ymin = overshootFairShare350, ymax = overshootFairShare15C), fill= "#ffeda0", alpha=0.3) +
	geom_ribbon(data = countries %>% filter(startDate == 1992, scenario != "NetZero"),
	  aes(ymin = overshootFairShare15C, ymax = overshootFairShare2C), fill= "#fc4e2a", alpha=0.3) +
	geom_ribbon(data = countries %>% filter(startDate == 1992, scenario != "NetZero"),
	  aes(ymin = overshootFairShare2C, ymax = 5), fill= "#ce2029", alpha=0.5) +
	geom_area(data = countries %>% filter(startDate == 1992, scenario == "Historical"),
	  aes(y = overshoot15C), fill="black") +
	geom_line(data = countries %>% filter(startDate == 1992, scenario != "NetZero"),
	  aes(y = overshootFairShare350), size=0.9, colour = "#387f30") +
	geom_line(data = countries %>% filter(startDate == 1992, scenario != "NetZero"),
	  aes(y = overshootFairShare15C), size=0.9, colour = "#feb24c") + #a1d99b"
	geom_line(data = countries %>% filter(startDate == 1992, scenario != "NetZero"),
	  aes(y = overshootFairShare2C), size=0.9, colour = "#e31a1c") + #de970b
	geom_ribbon(data = countries %>% filter(startDate == 1992, scenario == "BAU"), 
	  aes(ymin = overshoot15Clwr, ymax = overshoot15Cupr), alpha=0.3, fill="#525252") + #969696
	geom_line(data = countries %>% filter(startDate == 1992, scenario == "BAU"),
	  aes(y = overshoot15C), size=0.9, colour = "black", linetype="dashed") +
	geom_line(data = countries %>% filter(startDate == 1992, scenario == "NetZero"),
	  aes(y = overshoot15C), size=0.9, colour = "#0000a5") +
	facet_wrap(~ labelNS) +
	scale_y_continuous(limits=c(0,5), breaks=seq(0,5,0.5), expand=c(0,0)) +
	scale_x_continuous(limits=c(1992,2050), breaks=seq(2000,2050,10), expand=c(0,0)) +
	#scale_fill_manual(values = c("#525252","#969696"), labels=c("Global North", "Global South")) +
	labs(x="Year", y="Cumulative CO2 emissions (1 = fair share of 1.5°C budget)") +
	theme_chart_SMALLM +
	theme(legend.title=element_blank(),
	  legend.position="none",
	  plot.margin = unit(c(0.5,6,0.5,0.5), "lines"),
	  panel.grid = element_blank(),
	  panel.spacing.x = unit(1.25, "lines"),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=8, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=8, family="sans", color="#636363"),
	  axis.text = element_text(colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363")),
width = 183, height = 80, units = "mm", device = "pdf")

#-------------------------------------------------------------------------------------
#		WRITE CHART DATA TO FILE.
#-------------------------------------------------------------------------------------
#write_csv(overshoot2, "./myData/12_myDataOvershootGDPnormalised_v4.csv")
#write_csv(globalNS1, "./Figures/12_worldGSandGN_v4.csv")



