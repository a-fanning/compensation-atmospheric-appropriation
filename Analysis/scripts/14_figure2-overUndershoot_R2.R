## This script produces final visualisations of cumulative overshoot by country group with respect to the 1.5C boundary,
##starting in 1960 (main) and 1850 (SI).

#Read in datafiles from previous scripts (if needed)
#myData20 <- read_csv("./myData/07_myDataCO2convergenceWithGroups_v3.csv")

#overshoot2 <- read_csv("./myData/12_myDataOvershootGDPnormalised_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#---------------------------------------------------------------------
#			PREP DEBTOR/CREDITOR DATA BY COUNTRY
#----------------------------------------------------------------------
rank <- overshoot2 %>%
	filter(iso3c != "WLD") %>%
	left_join(countryRegion, by="iso3c") %>%
	select(startDate, scenario, country, iso3c, date, northAndRegions, northSouth, eu, cumCO2,
	  fairShare350, fairShare15C, fairShare2C) %>%
	rowwise() %>%
	mutate(overshoot15C = cumCO2 - fairShare15C,
	  debtCred15C = ifelse(overshoot15C > 0, "debtor", "creditor")) %>%
	ungroup()

wldDebtCred <- rank %>%
	select(startDate, scenario, country, iso3c, date, overshoot15C, debtCred15C) %>%
	rowwise() %>%
	mutate(totalDebt15C = ifelse(debtCred15C == "debtor", overshoot15C, NA),
	  totalCred15C = ifelse(debtCred15C == "creditor", overshoot15C, NA)) %>%
	ungroup() %>%
	group_by(startDate, scenario, date) %>%
	summarise(totalDebt15C = sum(totalDebt15C, na.rm=T),
	  totalCred15C = sum(totalCred15C, na.rm=T)) %>%
	ungroup()

rank1 <- left_join(rank, wldDebtCred, by=c("startDate", "scenario", "date"))

rm(rank, wldDebtCred)

rank2 <- rank1 %>%
	rowwise() %>%
	mutate(debtShare15C = ifelse(debtCred15C == "debtor", (overshoot15C/totalDebt15C)*100, NA),
	  credShare15C = ifelse(debtCred15C == "creditor", (overshoot15C/totalCred15C)*100, NA)) %>%
	ungroup() %>% 
	group_by(startDate, scenario, date) %>%
	arrange(-credShare15C, .by_group=T) %>%
	ungroup()

#Tag by country group
rank3 <- rank2 %>%
	rowwise() %>%
	mutate(debtGroup15C = ifelse(iso3c == "USA", "USA",
		ifelse(eu == "EU28", "EU and UK",
		ifelse(debtCred15C == "debtor" && northSouth == "NRT", "Rest of global North",
		ifelse(debtCred15C == "debtor" && iso3c == "CHN", "China overshoot",
		ifelse(debtCred15C == "debtor" && northSouth == "STH", "Rest of global South overshoot", NA))))),
	  credGroup15C = ifelse(iso3c == "IND", "India",
		ifelse(debtCred15C == "creditor" && iso3c == "CHN", "China",
		ifelse(debtCred15C == "creditor" && northSouth == "NRT", "Global North undershoot",
		ifelse(debtCred15C == "creditor" && northAndRegions == "SSF", "Sub-Sah. Africa",
		ifelse(debtCred15C == "creditor" && northSouth == "STH", "Rest of global South", NA)))))) %>%
	ungroup()

rm(rank1, rank2)

#-----------------------------------------------------------------------------------------------------
#			WRITE TO FILE: COUNTRY-LEVEL OVERSHOOT IN 2019 AND 2050 BAU/NET ZERO 
#-----------------------------------------------------------------------------------------------------
countryRank2019 <- rank3 %>%
	filter(date == 2019)

countryRank2050bau <- rank3 %>%
	filter(date == 2050, scenario == "BAU") %>%
	select(-fairShare350, -fairShare2C)

countryRank2050netZ <- rank3 %>%
	filter(date == 2050, scenario == "NetZero") %>%
	select(-fairShare350, -fairShare2C)

#write_csv(countryRank2019, "./Figures/14_countryOvershootShare-2019_v3.csv")
#write_csv(countryRank2050bau, "./Figures/14_countryOvershootShare-2050bau_v3.csv")
#write_csv(countryRank2050netZ, "./Figures/14_countryOvershootShare-2050netZero_v3.csv")

rm(countryRank2019, countryRank2050bau, countryRank2050netZ)


#WRITE FULL DATASET TO FILE
#write_csv(rank3, "./myData/14_countryOvershootShare-1850-2050_v4.csv")

#-----------------------------------------------------------------------------------------------------
#			PREP DEBTOR / CREDITOR DATA BY COUNTRY GROUP  
#-----------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
debtors15C <- rank3 %>%
	filter(debtCred15C == "debtor") %>%
	select(startDate, scenario, country, iso3c, date, overshoot = overshoot15C, region = debtGroup15C) %>%
	group_by(startDate, scenario, date, region) %>%
	summarise(overshoot = sum(overshoot, na.rm=T)) %>%
	ungroup() %>%
	add_column(grp = "1.5C") %>%
	group_by(startDate, scenario, date) %>%
	arrange(-overshoot, .by_group=T) %>%
	ungroup()

creditors15C <- rank3 %>%
	filter(debtCred15C == "creditor") %>%
	select(startDate, scenario, country, iso3c, date, overshoot = overshoot15C, region = credGroup15C) %>%
	group_by(startDate, scenario, date, region) %>%
	summarise(overshoot = sum(overshoot, na.rm=T)) %>%
	ungroup() %>%
	add_column(grp = "1.5C") %>%
	group_by(startDate, scenario, date) %>%
	arrange(overshoot, .by_group=T) %>%
	ungroup()

barData <- rbind(debtors15C, creditors15C)

rm(creditors15C, debtors15C)

#-----------------------------------------------------------------------------------------------------
#			WRITE TO FILE: COUNTRY GROUPS OVERSHOOT IN 2019 AND 2050 BAU/NET ZERO 
#-----------------------------------------------------------------------------------------------------
countryRank2019 <- barData %>%
	filter(date == 2019)

countryRank2050bau <- barData %>%
	filter(date == 2050, scenario == "BAU")

countryRank2050netZ <- barData %>%
	filter(date == 2050, scenario == "NetZero")

#write_csv(countryRank2019, "./Figures/14_countryGroupOvershoot-2019_v3.csv")
#write_csv(countryRank2050bau, "./Figures/14_countryGroupOvershoot-2050bau_v3.csv")
#write_csv(countryRank2050netZ, "./Figures/14_countryGroupOvershoot-2050netZero_v3.csv")

rm(countryRank2019, countryRank2050bau, countryRank2050netZ)

#WRITE COUNTRY GROUP FULL DATASET TO FILE
#write_csv(barData, "./Figures/14_countryGroupOvershootShare-1850-2050_v4.csv")



#------------------------------------------------------------------------------------------------------
#		VISUALISE FIGURE 3 1960–2019
#------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#		OVERSHOOT / UNDERSHOOT PLOT WITH BAU AND NET ZERO BAR CHARTS IN 2050
#----------------------------------------------------------------------------------------------------
chartData <- barData %>%
	filter(startDate == 1960, scenario == "Historical", grp=="1.5C") %>%
	mutate(region = ifelse(region == "Rest of global South overshoot", "Global South overshoot", region),
	region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))

colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e


histArea <- chartData %>% 
  ggplot( aes(x=date, y = overshoot/1000000, fill=region)) + 
  geom_area() +
  geom_hline(yintercept=0, size=1.2) +
  facet_wrap(~ scenario) +
  scale_y_continuous(limits=c(-1800,1700), breaks=seq(-1800,1700,300), expand=c(0,0)) +
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960,2020,10), expand=c(0,0)) +
  scale_fill_manual(values = colours, limits=c("USA", "EU and UK", "Rest of global North", "Global South overshoot", "Global North undershoot",  
	  "India", "China", "Sub-Sah. Africa", "Rest of global South")) + # "China overshoot",
  labs(x="Year", y="Cumulative overshoot or undershoot of 1.5 °C fair share (Gt CO2)") +
  theme_chart_SMALLM +
  theme(#plot.margin = unit(c(0.5,6.5,0.5,0.5), "lines"),
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.title=element_blank(),
	  legend.position= c(0.5, 0.87),
	  legend.direction="horizontal",
	  legend.text = element_text(size=6, colour = "#636363"),
	  legend.key.size = unit(0.5, "line"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363")) +
  guides(fill = guide_legend(ncol=2))


#----------------------------------------------------------------------------------------------------
#		2050 BAU BAR CHART
#----------------------------------------------------------------------------------------------------
chartData <- barData %>% 
	filter(startDate == 1960, date == 2050, grp == "1.5C", scenario == "BAU") %>%
	rowwise() %>%
	mutate(labelFac = ifelse(scenario=="BAU", "Business-as-usual", "Net Zero"),
	  labelFac2 = "2050",
	  colText = ifelse(region == "Rest of global South overshoot", "dark", 
		ifelse(region == "Rest of global South", "dark", "light"))) %>%
	ungroup() %>%
	mutate(region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Rest of global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))

colours <- c("Rest of global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e

bauBar <- ggplot(chartData, aes(labelFac2, overshoot/1000000, group=region)) +
	geom_col(aes(fill=region)) +
	facet_wrap(~labelFac) +
	scale_fill_manual(values=colours) +
	geom_hline(yintercept=0, size=1.2) +
	geom_text(aes(label = region, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	#geom_text_repel(aes(colour=colText, label = region, hjust=0.5), position = position_stack(vjust = 0.5), size=2,
	#  max.overlaps=Inf, point.size=NA, min.segment.length=0, show.legend=F) +
  	#scale_colour_manual(values = c("#000000", "#ffffff")) +
	scale_y_continuous(limits=c(-1800,1700), breaks=seq(-1800,1700,300), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair share (Gt CO2)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#----------------------------------------------------------------------------------------------------
#		2050 NET ZERO BAR CHART
#----------------------------------------------------------------------------------------------------
chartData <- barData %>% 
	filter(startDate == 1960, date == 2050, grp == "1.5C", scenario == "NetZero") %>%
	rowwise() %>%
	mutate(labelFac = ifelse(scenario=="BAU", "Business-as-usual", "Net Zero"),
	  labelFac2 = "2050") %>%
	ungroup() %>%
	mutate(region = ifelse(region == "Rest of global South overshoot", "Global South overshoot", region),
	region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))


colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e

#Net zero in 2050 barChart
netZeroBar <- ggplot(chartData, aes(labelFac2, overshoot/1000000, group=region)) +
	geom_col(aes(fill=region)) +
	facet_wrap(~labelFac) +
	scale_fill_manual(values=colours) +
	geom_hline(yintercept=0, size=1.2) +
	geom_text(aes(label = region, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	#geom_text_repel(aes(colour=colText, label = region, hjust=0.5), position = position_stack(vjust = 0.5), size=2,
	#  max.overlaps=Inf, point.size=NA, min.segment.length=0, show.legend=F) +
  	#scale_colour_manual(values = c("#000000", "#ffffff")) +
	scale_y_continuous(limits=c(-1800,1700), breaks=seq(-1800,1700,300), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair share (Gt CO2)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))


#-------------------------------------------------------------------------------------
#		COMBINE AREA AND BAR CHARTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
histArea <- histArea + theme(axis.title=element_blank())
bauBar <- bauBar + theme(axis.title=element_blank())
netZeroBar <- netZeroBar + theme(axis.title=element_blank())


figure <- ggarrange(histArea, bauBar, netZeroBar, labels = c("a", "b", "c"), ncol = 3, widths=c(2,1,1), font.label=list(size=9))
	
annotate_figure(figure,
	left = text_grob("Cumulative overshoot or undershoot of 1.5 ºC fair share (Gt CO2)", color="#636363", 
	rot=90, size=7),
	bottom = text_grob("Year", color="#636363", size=7))

#ggsave("./Figures/Figure2_overshootUndershoot_v4.pdf", width = 183, height = 100, units = "mm", device = "pdf")


#------------------------------------------------------------------------------------------------------
#		VISUALISE FIGURE S3 1850–2050
#------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#		OVERSHOOT / UNDERSHOOT PLOT WITH BAU AND NET ZERO BAR CHARTS IN 2050
#----------------------------------------------------------------------------------------------------
chartData <- barData %>%
	filter(startDate == 1850, scenario == "Historical", grp=="1.5C") %>%
	mutate(region = ifelse(region == "Rest of global South overshoot", "Global South overshoot", region),
	region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))


colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e

#----------------------------------------------------------------------------------------------------
histArea <- ggplot(chartData, aes(x=date, y = overshoot/1000000, fill=region)) + 
  geom_area() +
  geom_hline(yintercept=0, size=1.2) +
  facet_wrap(~ scenario) +
  scale_y_continuous(limits=c(-2100,1700), breaks=seq(-2100,1700,300), expand=c(0,0)) +
  scale_x_continuous(limits=c(1850, 2020), breaks=seq(1850,2020,50), expand=c(0,0)) +
  scale_fill_manual(values = colours, limits=c("USA", "EU and UK", "Rest of global North", "Global South overshoot", "Global North undershoot",  
	  "India", "China", "Sub-Sah. Africa", "Rest of global South")) + # "China overshoot",
  labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair shares (Gt CO2)") +
  theme_chart_SMALLM +
  theme(#plot.margin = unit(c(0.5,6.5,0.5,0.5), "lines"),
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.title=element_blank(),
	  legend.position= c(0.5, 0.87),
	  legend.direction="horizontal",
	  legend.text = element_text(size=6, colour = "#636363"),
	  legend.key.size = unit(0.5, "line"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363")) +
  guides(fill = guide_legend(ncol=2))


#----------------------------------------------------------------------------------------------------
#		2050 BAU BAR CHART
#----------------------------------------------------------------------------------------------------
chartData <- barData %>% 
	filter(startDate == 1850, date == 2050, grp == "1.5C", scenario == "BAU") %>%
	rowwise() %>%
	mutate(labelFac = ifelse(scenario=="BAU", "Business-as-usual", "Net Zero"),
	  labelFac2 = "2050") %>%
	ungroup() %>%
	mutate(region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Rest of global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))

colours <- c("Rest of global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e



#BAU in 2050 barChart
bauBar <- ggplot(chartData, aes(labelFac2, overshoot/1000000, group=region)) +
	geom_col(aes(fill=region)) +
	facet_wrap(~labelFac) +
	scale_fill_manual(values=colours) +
	geom_hline(yintercept=0, size=1.2) +
	geom_text(aes(label = region, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	#geom_text_repel(aes(colour=colText, label = region, hjust=0.5), position = position_stack(vjust = 0.5), size=2,
	#  max.overlaps=Inf, point.size=NA, min.segment.length=0, show.legend=F) +
  	#scale_colour_manual(values = c("#000000", "#ffffff")) +
	scale_y_continuous(limits=c(-2100,1700), breaks=seq(-2100,1700,300), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair share (Gt CO2)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))


#----------------------------------------------------------------------------------------------------
#		2050 NET ZERO BAR CHART
#----------------------------------------------------------------------------------------------------
chartData <- barData %>% 
	filter(startDate == 1850, date == 2050, grp == "1.5C", scenario == "NetZero", region != "Global North undershoot") %>% #
	rowwise() %>%
	mutate(labelFac = ifelse(scenario=="BAU", "Business-as-usual", "Net Zero"),
	  labelFac2 = "2050") %>%
	ungroup() %>%
	mutate(region = ifelse(region == "Rest of global South overshoot", "Global South overshoot", region),
	region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))


colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e



#Net zero in 2050 barChart
netZeroBar <- ggplot(chartData, aes(labelFac2, overshoot/1000000, group=region)) +
	geom_col(aes(fill=region)) +
	facet_wrap(~labelFac) +
	scale_fill_manual(values=colours) +
	geom_hline(yintercept=0, size=1.2) +
	geom_text(aes(label = region, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.9) + #colour=colText, 
	#geom_text_repel(aes(colour=colText, label = region, hjust=0.5), position = position_stack(vjust = 0.5), size=2,
	#  max.overlaps=Inf, point.size=NA, min.segment.length=0, show.legend=F) +
  	#scale_colour_manual(values = c("#000000", "#ffffff")) +
	scale_y_continuous(limits=c(-2100,1700), breaks=seq(-2100,1700,300), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair share (Gt CO2)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))


#-------------------------------------------------------------------------------------
#		COMBINE AREA AND BAR CHARTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
histArea <- histArea + theme(axis.title=element_blank())
bauBar <- bauBar + theme(axis.title=element_blank())
netZeroBar <- netZeroBar + theme(axis.title=element_blank())


figure <- ggarrange(histArea, bauBar, netZeroBar, labels = c("a", "b", "c"), ncol = 3, widths=c(2.2,1,1), font.label=list(size=9))
	
annotate_figure(figure,
	left = text_grob("Cumulative overshoot or undershoot of 1.5 ºC fair share (Gt CO2)", color="#636363", 
	rot=90, size=7),
	bottom = text_grob("Year", color="#636363", size=7))

#ggsave("./Figures/FigureS3_overshootUndershoot_v4.pdf", width = 183, height = 100, units = "mm", device = "pdf")




#------------------------------------------------------------------------------------------------------
#		VISUALISE FIGURE S4 1992–2050
#------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#		OVERSHOOT / UNDERSHOOT PLOT WITH BAU AND NET ZERO BAR CHARTS IN 2050
#----------------------------------------------------------------------------------------------------
chartData <- barData %>%
	filter(startDate == 1992, scenario == "Historical", grp=="1.5C") %>%
	mutate(region = ifelse(region == "Rest of global South overshoot", "Global South overshoot", region),
	region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))


colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e


#----------------------------------------------------------------------------------------------------
#		1992-2019 OVERSHOOT / UNDERSHOOT PLOT WITH BAU AND NET ZERO BAR CHARTS IN 2050
#----------------------------------------------------------------------------------------------------
histArea <- ggplot(chartData, aes(x=date, y = overshoot/1000000, fill=region)) + 
  geom_area() +
  geom_hline(yintercept=0, size=1.2) +
  facet_wrap(~ scenario) +
  scale_y_continuous(limits=c(-1250,1500), breaks=seq(-1200,1500,300), expand=c(0,0)) +
  scale_x_continuous(limits=c(1992, 2020), breaks=seq(1990,2020,10), expand=c(0,0)) +
  scale_fill_manual(values = colours, limits=c("USA", "EU and UK", "Rest of global North", "Global South overshoot", "Global North undershoot",  
	  "India", "China", "Sub-Sah. Africa", "Rest of global South")) + # "China overshoot",
  labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair shares (Gt CO2)") +
  theme_chart_SMALLM +
  theme(#plot.margin = unit(c(0.5,6.5,0.5,0.5), "lines"),
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.title=element_blank(),
	  legend.position= c(0.5, 0.87),
	  legend.direction="horizontal",
	  legend.text = element_text(size=6, colour = "#636363"),
	  legend.key.size = unit(0.5, "line"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363")) +
  guides(fill = guide_legend(ncol=2))

#----------------------------------------------------------------------------------------------------
#		2050 BAU BAR CHART
#----------------------------------------------------------------------------------------------------
chartData <- barData %>% 
	filter(startDate == 1992, date == 2050, grp == "1.5C", scenario == "BAU") %>%
	rowwise() %>%
	mutate(labelFac = ifelse(scenario=="BAU", "Business-as-usual", "Net Zero"),
	  labelFac2 = "2050") %>%
	ungroup() %>%
	mutate(region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Rest of global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))

colours <- c("Rest of global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e


#BAU in 2050 barChart
bauBar <- ggplot(chartData, aes(labelFac2, overshoot/1000000, group=region)) +
	geom_col(aes(fill=region)) +
	facet_wrap(~labelFac) +
	scale_fill_manual(values=colours) +
	geom_hline(yintercept=0, size=1.2) +
	geom_text(aes(label = region, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	#geom_text_repel(aes(colour=colText, label = region, hjust=0.5), position = position_stack(vjust = 0.5), size=2,
	#  max.overlaps=Inf, point.size=NA, min.segment.length=0, show.legend=F) +
  	#scale_colour_manual(values = c("#000000", "#ffffff")) +
	scale_y_continuous(limits=c(-1250,1500), breaks=seq(-1200,1500,300), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair share (Gt CO2)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))


#----------------------------------------------------------------------------------------------------
#		2050 NET ZERO BAR CHART
#----------------------------------------------------------------------------------------------------
chartData <- barData %>% 
	filter(startDate == 1992, date == 2050, grp == "1.5C", scenario == "NetZero") %>% #, region != "Global North undershoot"
	rowwise() %>%
	mutate(labelFac = ifelse(scenario=="BAU", "Business-as-usual", "Net Zero"),
	  labelFac2 = "2050") %>%
	ungroup() %>%
	mutate(region = ifelse(region == "Rest of global South overshoot", "Global South overshoot", region),
	region = factor(region, levels = c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot",
	  "Rest of global South", "Sub-Sah. Africa", "China", "India", "Global North undershoot")),
		grp = factor(grp, levels = c("1.5C")))


colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e

#Net zero in 2050 barChart
netZeroBar <- ggplot(chartData, aes(labelFac2, overshoot/1000000, group=region)) +
	geom_col(aes(fill=region)) +
	facet_wrap(~labelFac) +
	scale_fill_manual(values=colours) +
	geom_hline(yintercept=0, size=1.2) +
	geom_text(aes(label = region, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	#geom_text_repel(aes(colour=colText, label = region, hjust=0.5), position = position_stack(vjust = 0.5), size=2,
	#  max.overlaps=Inf, point.size=NA, min.segment.length=0, show.legend=F) +
  	#scale_colour_manual(values = c("#000000", "#ffffff")) +
	scale_y_continuous(limits=c(-1250,1500), breaks=seq(-1200,1500,300), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.5)) +
	labs(x="Year", y="Cumulative overshoot or undershoot of 1.5C fair share (Gt CO2)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  #panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))


#-------------------------------------------------------------------------------------
#		COMBINE AREA AND BAR CHARTS INTO ONE FIGURE.
#-------------------------------------------------------------------------------------
histArea <- histArea + theme(axis.title=element_blank())
bauBar <- bauBar + theme(axis.title=element_blank())
netZeroBar <- netZeroBar + theme(axis.title=element_blank())


figure <- ggarrange(histArea, bauBar, netZeroBar, labels = c("a", "b", "c"), ncol = 3, widths=c(2.2,1,1), font.label=list(size=9))
	
annotate_figure(figure,
	left = text_grob("Cumulative overshoot or undershoot of 1.5 ºC fair share (Gt CO2)", color="#636363", 
	rot=90, size=7),
	bottom = text_grob("Year", color="#636363", size=7))

#ggsave("./Figures/FigureS4_overshootUndershoot_v4.png", width = 183, height = 100, units = "mm", device = "png")


rm(barData, bauBar, colours, countries, figure, histArea, netZeroBar)

