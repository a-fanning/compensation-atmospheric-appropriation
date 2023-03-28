## This script visualises bar charts of mean annual values of monetary reparations in a net zero world

#Read in datafiles from previous scripts (if needed)
#myData21 <- read_csv("./myData/16_myDataCprice_v4.csv")

#overshoot5 <- read_csv("./myData/16_myDataOvershootGDPreparations_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double()))

#bindReparations4 <- read_csv("./myData/17_debtorsAndCreditors-perCap_v5.csv")

#co2gdp4 <- read_csv("./myData/15_cumulativeCO2-GDP-POP_v4.csv", col_types=list(col_double(), col_character(), col_character(),
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_character()))

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#--------------------------------------------------------------------------------------
#---------------------------------------------------------------------
#			Prep data...
#----------------------------------------------------------------------
#calculate mean annual values
bindReparations5 <- bindReparations4 %>%
	rowwise() %>%
	mutate(avgReparations = cumReparationsAR6/31,
	  avgReparationsLwr = cumReparationsAR6lwr/31,
	  avgReparationsUpr = cumReparationsAR6upr/31) %>%
	ungroup() %>%
	group_by(startDate) %>%
	arrange(avgReparations, .by_group=T) %>%
	ungroup()

rm(bindReparations4)

#add GDP
gdp2018 <- co2gdp4 %>%
	select(startDate, country, iso3c, date, gdp) %>%
	filter(startDate == 1960, date == 2018, iso3c != "WLD") %>%
	select(-date, -startDate)


bindReparations6 <- left_join(bindReparations5, gdp2018, by=c("country", "iso3c")) %>%
	rowwise() %>%
	mutate(avgRepGDPshare = (avgReparations / gdp)*100,
	  avgRepGDPshareLwr = (avgReparationsLwr / gdp)*100,
	  avgRepGDPshareUpr = (avgReparationsUpr / gdp)*100) %>%
	ungroup()

rm(bindReparations5)

#calculate by region
regional <- bindReparations6 %>%
	drop_na(gdp) %>%
	group_by(startDate, group15C) %>%
	summarise(avgReparations = sum(avgReparations),
	  avgReparationsLwr = sum(avgReparationsLwr),
	  avgReparationsUpr = sum(avgReparationsUpr),
	  gdp2018 = sum(gdp)) %>%
	ungroup() %>%
	rowwise() %>%
	mutate(avgRepGDPshare = (avgReparations/gdp2018)*100,
	  avgRepGDPshareLwr = (avgReparationsLwr/gdp2018)*100,
	  avgRepGDPshareUpr = (avgReparationsUpr/gdp2018)*100) %>%
	ungroup() %>%
	group_by(startDate) %>%
	arrange(avgRepGDPshare, .by_group=T) %>%
	ungroup() %>%
	rowwise() %>%
	mutate(type = ifelse(avgRepGDPshare <= 0, "Overemitters", "Within fair shares")) %>%
	ungroup()

#colours
colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e

#---------------------------------------------------------------------------------------------------------------------
#Average annual compensation per region, 2020–2050 (FIGURE 5A - Undershooters bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
under <- regional %>%
  filter(startDate == 1960, type == "Within fair shares")

panelA <- ggplot(data = under) +
	geom_col(aes(x=reorder(group15C, -avgRepGDPshare, sum), y=avgRepGDPshare, fill=group15C)) + #, width=0.6
	geom_errorbar(aes(x=reorder(group15C, avgRepGDPshare, sum), ymin=avgRepGDPshareLwr, ymax=avgRepGDPshareUpr),
	  width=0.2, colour="black", alpha=0.7, size=0.8) +
	geom_hline(yintercept = 0, colour = "#bdbdbd", size=0.5, linetype="dashed") +
	facet_wrap(~type) +
	scale_fill_manual(values=colours) +
	#geom_text(aes(label = labelNum1, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	scale_y_continuous(limits=c(0,180), breaks=seq(0,175,25), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	#coord_flip() +
	labs(x="Overshooters and Undershooters", y="Annual compensation owed per region, net zero scenario (% of GDP)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#---------------------------------------------------------------------------------------------------------------------
#Average annual compensation per region, 2020–2050 (FIGURE 5B - Overshooters bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
over <- regional %>%
  filter(startDate == 1960, type == "Overemitters") %>%
  mutate(avgRepGDPshare = abs(avgRepGDPshare),
	avgRepGDPshareLwr = abs(avgRepGDPshareLwr),
	avgRepGDPshareUpr = abs(avgRepGDPshareUpr))

panelB <- ggplot(data = over) +
	geom_col(aes(x=reorder(group15C, -avgRepGDPshare, sum), y=avgRepGDPshare, fill=group15C)) + #, width=0.6
	geom_errorbar(aes(x=reorder(group15C, avgRepGDPshare, sum), ymin=avgRepGDPshareLwr, ymax=avgRepGDPshareUpr),
	  width=0.2, colour="black", alpha=0.7, size=0.8) +
	geom_hline(yintercept = 0, colour = "#bdbdbd", size=0.5, linetype="dashed") +
	facet_wrap(~type) +
	scale_fill_manual(values=colours) +
	#geom_text(aes(label = labelNum1, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	scale_y_continuous(limits=c(0,180), breaks=seq(0,175,25), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	#coord_flip() +
	labs(x="Overshooters and Undershooters", y="Annual compensation due per region, net zero scenario (% of GDP)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#-------------------------------------------------------------------------------------
#	COMBINE UNDERSHOOTING AND OVERSHOOTING COMPENSATION PER REGION PLOTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
panelA <- panelA + theme(axis.title.x =element_blank())
panelB <- panelB + theme(axis.title.x =element_blank())

ggarrange(panelB, panelA, 
  labels = c("a", "b"), ncol = 2, font.label=list(size=9))
	

#ggsave("./Figures/Figure5-reparationsShareGDP_v5.pdf", width = 130, height = 70, units = "mm", device = "pdf")



#---------------------------------------------------------------------------------------------------------------------
#Mean annual reparations per region, (FIGURE S6 - 1850-2050 analysis period bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#Average annual compensation per region, 2020–2050 (FIGURE S6A - Undershooters bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
under <- regional %>%
  filter(startDate == 1850, type == "Within fair shares")

panelA <- ggplot(data = under) +
	geom_col(aes(x=reorder(group15C, -avgRepGDPshare, sum), y=avgRepGDPshare, fill=group15C)) + #, width=0.6
	geom_errorbar(aes(x=reorder(group15C, avgRepGDPshare, sum), ymin=avgRepGDPshareLwr, ymax=avgRepGDPshareUpr),
	  width=0.2, colour="black", alpha=0.7, size=0.8) +
	geom_hline(yintercept = 0, colour = "#bdbdbd", size=0.5, linetype="dashed") +
	facet_wrap(~type) +
	scale_fill_manual(values=colours) +
	#geom_text(aes(label = labelNum1, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	scale_y_continuous(limits=c(0,185), breaks=seq(0,175,25), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	#coord_flip() +
	labs(x="Overshooters and Undershooters", y="Annual compensation owed per region, net zero scenario (% of GDP)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#---------------------------------------------------------------------------------------------------------------------
#Average annual compensation per region, 2020–2050 (FIGURE S6B - Overshooters bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
over <- regional %>%
  filter(startDate == 1850, type == "Overemitters") %>%
  mutate(avgRepGDPshare = abs(avgRepGDPshare),
	avgRepGDPshareLwr = abs(avgRepGDPshareLwr),
	avgRepGDPshareUpr = abs(avgRepGDPshareUpr))

panelB <- ggplot(data = over) +
	geom_col(aes(x=reorder(group15C, -avgRepGDPshare, sum), y=avgRepGDPshare, fill=group15C)) + #, width=0.6
	geom_errorbar(aes(x=reorder(group15C, avgRepGDPshare, sum), ymin=avgRepGDPshareLwr, ymax=avgRepGDPshareUpr),
	  width=0.2, colour="black", alpha=0.7, size=0.8) +
	geom_hline(yintercept = 0, colour = "#bdbdbd", size=0.5, linetype="dashed") +
	facet_wrap(~type) +
	scale_fill_manual(values=colours) +
	#geom_text(aes(label = labelNum1, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.9) + #colour=colText, 
	scale_y_continuous(limits=c(0,185), breaks=seq(0,175,25), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	#coord_flip() +
	labs(x="Overshooters and Undershooters", y="Annual compensation due per region, net zero scenario (% of GDP)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#-------------------------------------------------------------------------------------
#	COMBINE UNDERSHOOTING AND OVERSHOOTING COMPENSATION PER REGION PLOTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
panelA <- panelA + theme(axis.title.x =element_blank())
panelB <- panelB + theme(axis.title.x =element_blank())

ggarrange(panelB, panelA, 
  labels = c("a", "b"), ncol = 2, font.label=list(size=9))
	

#ggsave("./Figures/FigureS6-reparationsShareGDP_v5.pdf", width = 130, height = 70, units = "mm", device = "pdf")

#---------------------------------------------------------------------------------------------------------------------
#Mean annual reparations per region, (FIGURE S7 - 1992-2050 analysis period bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#Average annual compensation per region, 2020–2050 (FIGURE S7A - Undershooters bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
under <- regional %>%
  filter(startDate == 1992, type == "Within fair shares")

panelA <- ggplot(data = under) +
	geom_col(aes(x=reorder(group15C, -avgRepGDPshare, sum), y=avgRepGDPshare, fill=group15C)) + #, width=0.6
	geom_errorbar(aes(x=reorder(group15C, avgRepGDPshare, sum), ymin=avgRepGDPshareLwr, ymax=avgRepGDPshareUpr),
	  width=0.2, colour="black", alpha=0.7, size=0.8) +
	geom_hline(yintercept = 0, colour = "#bdbdbd", size=0.5, linetype="dashed") +
	facet_wrap(~type) +
	scale_fill_manual(values=colours) +
	#geom_text(aes(label = labelNum1, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	scale_y_continuous(limits=c(0,125), breaks=seq(0,125,25), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	#coord_flip() +
	labs(x="Overshooters and Undershooters", y="Annual compensation owed per region, net zero scenario (% of GDP)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#---------------------------------------------------------------------------------------------------------------------
#Average annual compensation per region, 2020–2050 (FIGURE S7B - Overshooters bar chart with range)
#---------------------------------------------------------------------------------------------------------------------
over <- regional %>%
  filter(startDate == 1992, type == "Overemitters") %>%
  mutate(avgRepGDPshare = abs(avgRepGDPshare),
	avgRepGDPshareLwr = abs(avgRepGDPshareLwr),
	avgRepGDPshareUpr = abs(avgRepGDPshareUpr))

panelB <- ggplot(data = over) +
	geom_col(aes(x=reorder(group15C, -avgRepGDPshare, sum), y=avgRepGDPshare, fill=group15C)) + #, width=0.6
	geom_errorbar(aes(x=reorder(group15C, avgRepGDPshare, sum), ymin=avgRepGDPshareLwr, ymax=avgRepGDPshareUpr),
	  width=0.2, colour="black", alpha=0.7, size=0.8) +
	geom_hline(yintercept = 0, colour = "#bdbdbd", size=0.5, linetype="dashed") +
	facet_wrap(~type) +
	scale_fill_manual(values=colours) +
	#geom_text(aes(label = labelNum1, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.9) + #colour=colText, 
	scale_y_continuous(limits=c(0,125), breaks=seq(0,125,25), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	#coord_flip() +
	labs(x="Overshooters and Undershooters", y="Annual compensation due per region, net zero scenario (% of GDP)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#-------------------------------------------------------------------------------------
#	COMBINE UNDERSHOOTING AND OVERSHOOTING COMPENSATION PER REGION PLOTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
panelA <- panelA + theme(axis.title.x =element_blank())
panelB <- panelB + theme(axis.title.x =element_blank())

ggarrange(panelB, panelA, 
  labels = c("a", "b"), ncol = 2, font.label=list(size=9))
	

#ggsave("./Figures/FigureS7-reparationsShareGDP_v5.pdf", width = 130, height = 70, units = "mm", device = "pdf")



#---------------------------------------------------------------------
#			WRITE TO FILE
#----------------------------------------------------------------------
#write_csv(bindReparations6, "./myData/18_cumulativeReparations-PerCap-GDPshare_v5.csv")
#write_csv(regional, "./Figures/18_reparationsShareGDPbyRegion_v5.csv")

rm(bindReparations3, bindReparations6, chartData)

