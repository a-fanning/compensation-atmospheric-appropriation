## This script visualises per capita values of monetary reparations to overshooters in a net zero world

#Read in datafiles from previous scripts (if needed)
#myData21 <- read_csv("./myData/16_myDataCprice_v4.csv")

#overshoot5 <- read_csv("./myData/16_myDataOvershootGDPreparations_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double()))

#bindReparations <- read_csv("./myData/16_debtorsAndCreditors_v5.csv")

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#--------------------------------------------------------------------------------------
#---------------------------------------------------------------------
#			Prep cumulative reparations calcs...
#----------------------------------------------------------------------
#get cumulative 2020-2050 population data
popData <- myData21 %>%
	select(country, iso3c, date, population) %>% 
	filter(date >= 2020) %>%
	group_by(country, iso3c) %>%
	mutate(cumPop = cumsum(ifelse(is.na(population), 0, population)) + population*0) %>%
	ungroup() %>%
	filter(date == 2050) %>%
	select(-date, -population)

#join with reparations data
bindReparations1 <- left_join(bindReparations, popData, by = c("country", "iso3c"))

rm(popData, bindReparations)

#calculate per capita values
bindReparations2 <- bindReparations1 %>%
	rowwise() %>%
	mutate(cumRepPerCap = cumReparationsAR6 / cumPop,
	  cumRepPerCapLwr = cumReparationsAR6lwr / cumPop,
	  cumRepPerCapUpr = cumReparationsAR6upr / cumPop) %>%
	ungroup()

rm(bindReparations1)

#get overshoot values
co2 <- overshoot5 %>%
	filter(scenario == "NetZero", date == 2050) %>%
	select(startDate, country, iso3c, overshoot15C)

bindReparations3 <- left_join(bindReparations2, co2, by=c("startDate", "country", "iso3c"))

rm(bindReparations2, co2)


bindReparations4 <- bindReparations3 %>%
	rowwise() %>%
	mutate(group15C = ifelse(group15C == "Rest of global South overshoot", "Global South overshoot", group15C)) %>%
	ungroup() 

#colours <- c("Global South overshoot"="#fc9272", "China overshoot"="#fcbba1", "Rest of global North"="#ef3b2c", 
#		"EU and UK"="#cb181d", "USA"="#99000d", 
#		"Global North undershoot"="#fee8c8", "Rest of global South"="#41ab5d", 
#		"China"="#78c679", "Sub-Sah. Africa"="#238443", "India"="#005a32") ##addd8e

colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e


chartData <- bindReparations4 %>%
	filter(startDate == 1960)
#write_csv(chartData, "./Figures/Figure 6/figure6_chartData_R2.csv")

#---------------------------------------------------------------------
#			VISUALISE PER CAPITA UNDERSHOOTERS FIGURE (FIGURE 6A)
#----------------------------------------------------------------------

under <- chartData %>%
  filter(type == "Within fair shares")

#1.5C main
panelA <- ggplot() +
  geom_vline(xintercept = 1, colour = "#bdbdbd", size=0.5, linetype="dashed") +
  geom_point(data = under, aes(x=overshoot15C, y=cumRepPerCap, 
    size=cumPop, fill=group15C), shape = 21, colour = "black", alpha=0.7) +  
  geom_text_repel(data=under, aes(x=overshoot15C, y=cumRepPerCap, label=country,
		hjust=0), size=2.5, max.overlaps=10, show.legend=F) +
  facet_wrap(~type, scales="free") +
  scale_size(range = c(2,15)) +  #range = c(0.5,10)
  scale_y_continuous(limits = c(0,8000), breaks = seq(0,8000,1000), labels = scales::comma) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, 0.25), expand=c(0,0.05)) + #
  scale_fill_manual(values=colours, limits=c("USA", "EU and UK", "Rest of global North", "Global South overshoot", 
    "India", "Sub-Sah. Africa", "Rest of global South", "China")) +
  labs(x="Cumulative CO2 emissions in 2050 with respect to 1.5ºC shares, net zero scenario (1 = equal share)",
    y="Annual compensation owed per capita, net zero scenario (constant 2010 $)") +
	theme_chart_SMALLM + 
	theme(legend.position = "none",
		panel.border = element_blank(),
		panel.grid=element_blank(),
		#plot.margin = unit(c(1.5,2.5,0.5,0.5), "lines"),
		#legend.title=element_blank(),
		#legend.position= c(0.7, 0.85),
		#legend.direction="horizontal",
		#legend.text = element_text(size=6, colour = "#636363"),
		#legend.key.size = unit(0.5, "line"),
		#legend.key = element_blank(),
		#legend.background=element_blank(),
		#legend.box.background = element_rect(colour = "#636363"),
		axis.line = element_line(colour = "#636363", size = 0.7),
		axis.title = element_text(size=7, family="sans", color="#636363"),
		axis.text = element_text(size=6, colour = "#636363"),
		axis.ticks = element_line(colour = "#636363")) +
	guides(fill = guide_legend(ncol=1), size = "none")

#---------------------------------------------------------------------
#			VISUALISE PER CAPITA OVERSHOOTERS FIGURE (FIGURE 6B)
#----------------------------------------------------------------------

over <- chartData %>%
  filter(type == "Overemitters") %>%
  mutate(cumRepPerCap = abs(cumRepPerCap))

#1.5C main
panelB <- ggplot() +
  geom_vline(xintercept = 1, colour = "#bdbdbd", size=0.5, linetype="dashed") +
  geom_point(data = over, aes(x=overshoot15C, y=cumRepPerCap, 
    size=cumPop, fill=group15C), shape = 21, colour = "black", alpha=0.7) +  
  geom_text_repel(data=over, aes(x=overshoot15C, y=cumRepPerCap, label=country,
		hjust=0), size=2.5, max.overlaps=10, show.legend=F) +
  facet_wrap(~type) +
  scale_size(range = c(2,8)) +  #range = c(0.5,10)
  scale_y_continuous(limits = c(0,8000), breaks = seq(0,8000,1000), labels = scales::comma) +
  scale_x_continuous(limits = c(1,6), breaks = seq(1,6,1)) + #, expand=c(0,0.14)
  scale_fill_manual(values=colours, limits=c("USA", "EU and UK", "Rest of global North", "Global South overshoot", 
    "India", "Sub-Sah. Africa", "Rest of global South", "China")) +
  labs(x="Cumulative CO2 emissions in 2050 with respect to 1.5ºC shares, net zero scenario (1 = equal share)",
    y="Annual compensation due per capita, net zero scenario (constant 2010 $)") +
	theme_chart_SMALLM + 
	theme(legend.position = "none",
		panel.border = element_blank(),
		panel.grid=element_blank(),
		#plot.margin = unit(c(1.5,2.5,0.5,0.5), "lines"),
		#legend.title=element_blank(),
		#legend.position= c(0.3, 0.12),
		#legend.direction="horizontal",
		#legend.text = element_text(size=6, colour = "#636363"),
		#legend.key.size = unit(0.5, "line"),
		#legend.key = element_blank(),
		#legend.background=element_blank(),
		#legend.box.background = element_rect(colour = "#636363"),
		axis.line = element_line(colour = "#636363", size = 0.7),
		axis.title = element_text(size=7, family="sans", color="#636363"),
		axis.text = element_text(size=6, colour = "#636363"),
		axis.ticks = element_line(colour = "#636363")) +
	guides(fill = guide_legend(ncol=1), size = "none")


#-------------------------------------------------------------------------------------
#	COMBINE UNDERSHOOTING AND OVERSHOOTING COMPENSATION PER CAPITA PLOTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
panelA <- panelA + theme(axis.title.x =element_blank())
panelB <- panelB + theme(axis.title.x =element_blank())

figure <- ggarrange(panelA, panelB, 
  labels = c("a", "b"), ncol = 2, font.label=list(size=9))
	
annotate_figure(figure,
	bottom = text_grob("Cumulative CO2 emissions in 2050 with respect to 1.5ºC fair shares, net zero scenario (1 = fair share)",
	  color="#636363", size=7))


#ggsave("./Figures/Figure6_overshootVScompensation_v5.png", width = 183, height = 100, units = "mm", device = "png")





#---------------------------------------------------------------------
#			WRITE CHART DATA TO FILE
#----------------------------------------------------------------------
#write_csv(bindReparations4, "./myData/17_debtorsAndCreditors-perCap_v5.csv")

