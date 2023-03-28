## This script visualises final historical cumulative GDP and CO2 emissions (with respect to fair shares), 1960-2018.

#Read in datafiles from previous scripts (if needed)
#myData20 <- read_csv("./myData/07_myDataCO2convergenceWithGroups_v3.csv")

#overshoot2 <- read_csv("./myData/12_myDataOvershootGDPnormalised_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#co2gdp2 <- read_csv("./myData/11_cumulativeGDPandCO2_v4.csv", col_types=list(col_double(), col_character(), col_character(),
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))

#rank3 <- read_csv("./myData/14_countryOvershootShare-1850-2050_v4.csv")


#---------------------------------------------------------------------
#			PREP DATA
#----------------------------------------------------------------------

#remove gdp NAs from tibble (N = 151 plus WORLD)
co2gdp3 <- co2gdp2 %>% 
	drop_na(gdp)

#add country groups in 2018
countryGrps <- rank3 %>%
	filter(startDate == 1960, date == 2018) %>%
	select(country, iso3c, debtGroup15C, credGroup15C) %>%
	rowwise() %>%
	mutate(countryGrp15C = ifelse(is.na(debtGroup15C), credGroup15C, debtGroup15C)) %>%
	ungroup() %>%
	select(-debtGroup15C, -credGroup15C)

co2gdp4 <- left_join(co2gdp3, countryGrps, by=c("country", "iso3c"))

#1.5C
bubbleData <- co2gdp4 %>%
	filter(date == 2018, !iso3c %in% c("WLD")) %>%
	select(-gdp, -population, -overshoot350, -overshoot2C) %>%
	rowwise() %>%
	mutate(cumGDPperCap = cumGDP / cumPop,
	  countryGrp15C = ifelse(countryGrp15C == "Rest of global South overshoot", "Global South overshoot", countryGrp15C)) %>%
	ungroup() %>%
	arrange(-cumPop)

colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e

#------------------------------------------------------------------------------------------------------
# 		VISUALISE
#------------------------------------------------------------------------------------------------------
bubbleData1 <- bubbleData %>%
	filter(startDate == 1960)

fit <- lm(cumGDPperCap ~ overshoot15C, data = bubbleData1)
summary(fit)

ggplot() +
  geom_vline(xintercept = 1, colour = "#bdbdbd", size=0.5, linetype="dashed") +
  geom_point(data = bubbleData1, aes(x=overshoot15C, y=cumGDPperCap, 
    size=cumPop, fill=countryGrp15C), shape = 21, colour = "black", alpha=0.7) +  #, fill = "#41b6c4"
  geom_text_repel(data=bubbleData1, aes(x=overshoot15C, y=cumGDPperCap, label=country,
		hjust=0), size=2.5, max.overlaps=10, show.legend=F) +
  #geom_smooth(data = bubbleData1, aes(x=overshoot15C, y=cumGDPperCap), method = "lm", alpha=0.15) +
  #facet_wrap(~ overshootGrp) +
  scale_size(range = c(2,20)) +  #range = c(0.5,10)
  scale_y_continuous(labels=scales::comma, limits = c(0,53000), breaks = seq(0,50000,10000), expand=c(0.1,0)) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1), expand=c(0,0.14)) +
  scale_fill_manual(values=colours, limits=c("USA", "EU and UK", "Rest of global North", "Global South overshoot", 
	  "India", "China", "Sub-Sah. Africa", "Rest of global South")) +
  #scale_color_viridis(option="magma") + #discrete=TRUE, 
  #scale_colour_manual(values = brewerCols, aesthetics = c("fill", "colour")) +
  labs(x="Cumulative CO2 emissions with respect to 1.5 ºC fair shares, 1960–2018 (1 = fair share)",
    y="Cumulative GDP per capita, 1960–2018 (constant 2010 $)") +
	theme_chart_SMALLM + 
	theme(#legend.position = "none",
		panel.border = element_blank(),
		panel.grid=element_blank(),
		#plot.margin = unit(c(1.5,2.5,0.5,0.5), "lines"),
		legend.title=element_blank(),
		legend.position= c(0.89, 0.2),
		legend.direction="horizontal",
		legend.text = element_text(size=6, colour = "#636363"),
		legend.key.size = unit(0.5, "line"),
		legend.key = element_blank(),
		legend.background=element_blank(),
		legend.box.background = element_rect(colour = "#636363"),
		axis.line = element_line(colour = "#636363", size = 0.7),
		axis.title = element_text(size=7, family="sans", color="#636363"),
		axis.text = element_text(size=6, colour = "#636363"),
		axis.ticks = element_line(colour = "#636363")) +
	guides(fill = guide_legend(ncol=1), size = "none")
		
#ggsave("./Figures/Figure3_overshootVSgdp_v5.pdf", width = 183, height = 90, units = "mm", device = "pdf")
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# 		WRITE DATA TO FILE
#------------------------------------------------------------------------------------------------------
#chart data
#write_csv(bubbleData1, "./Figures/15_cumulativeCO2-GDPchartData_v4.csv")

#full dataset
#write_csv(co2gdp4, "./myData/15_cumulativeCO2-GDP-POP_v4.csv")

rm(bubbleData, bubbleData1, co2gdp2, co2gdp3)

