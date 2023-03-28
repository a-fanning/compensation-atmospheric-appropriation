## This script visualises reparation flows from overshooters to undershooters, under the net zero by 2050 scenario

#Read in datafiles from previous scripts (if needed)
#myData20 <- read_csv("./myData/07_myDataCO2convergenceWithGroups_v3.csv")

#overshoot2 <- read_csv("./myData/12_myDataOvershootGDPnormalised_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#co2gdp4 <- read_csv("./myData/15_cumulativeCO2-GDP-POP_v4.csv", col_types=list(col_double(), col_character(), col_character(),
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_character()))

#rank3 <- read_csv("./myData/14_countryOvershootShare-1850-2050_v4.csv")


#---------------------------------------------------------------------
#			PREP DATA
#----------------------------------------------------------------------

#read in new AR6 carbon price data.
mac <- read_csv("./CleanData/AR6-carbonPrices_1p5c_v1.csv")

myData21 <- left_join(myData20, mac, by=c("date"))
rm(myData20)

overshoot3 <- left_join(overshoot2, mac, by=c("date"))
rm(overshoot2, mac)

#Calculate linear overshoot slope, 2020-2050
linCO2 <- myData21 %>%
	filter(date == 2050) %>%
	select(country, iso3c, cumNetZeroEnd_1850 = cumCO2_net01850, cumNetZeroEnd_1960 = cumCO2_net01960,
		cumNetZeroEnd_1992 = cumCO2_net01992, fairShare15C_1850, fairShare15C_1960, fairShare15C_1992) %>%
	rowwise() %>%
	mutate(cumOvershoot15C_1850 = fairShare15C_1850 - cumNetZeroEnd_1850,
		cumOvershoot15C_1960 = fairShare15C_1960 - cumNetZeroEnd_1960,
		cumOvershoot15C_1992 = fairShare15C_1992 - cumNetZeroEnd_1992,
		annOvershoot15C_1850 = ifelse(cumOvershoot15C_1850 < 0, cumOvershoot15C_1850/31, NA),
		annOvershoot15C_1960 = ifelse(cumOvershoot15C_1960 < 0, cumOvershoot15C_1960/31, NA),
		annOvershoot15C_1992 = ifelse(cumOvershoot15C_1992 < 0, cumOvershoot15C_1992/31, NA)) %>%
	ungroup() %>%
	select(-fairShare15C_1850, -fairShare15C_1960, -fairShare15C_1992, 
	  -cumOvershoot15C_1850, -cumOvershoot15C_1960, -cumOvershoot15C_1992, 
		-cumNetZeroEnd_1850, -cumNetZeroEnd_1960, -cumNetZeroEnd_1992)

reparations <- myData21 %>%
	filter(date >= 2020) %>%
	left_join(linCO2, by=c("country", "iso3c")) %>%
	rowwise() %>%
	mutate(reparations15C_1850 = ifelse(is.na(annOvershoot15C_1850), NA,
			abs(annOvershoot15C_1850*(AR6Cprice*1000))),
		reparations15C_1850_lwr = ifelse(is.na(annOvershoot15C_1850), NA,
			abs(annOvershoot15C_1850*(AR6Cprice_lwr*1000))),
		reparations15C_1850_upr = ifelse(is.na(annOvershoot15C_1850), NA,
			abs(annOvershoot15C_1850*(AR6Cprice_upr*1000))),
		reparations15C_1960 = ifelse(is.na(annOvershoot15C_1960), NA,
			abs(annOvershoot15C_1960*(AR6Cprice*1000))),
		reparations15C_1960_lwr = ifelse(is.na(annOvershoot15C_1960), NA,
			abs(annOvershoot15C_1960*(AR6Cprice_lwr*1000))),
		reparations15C_1960_upr = ifelse(is.na(annOvershoot15C_1960), NA,
			abs(annOvershoot15C_1960*(AR6Cprice_upr*1000))),
		reparations15C_1992 = ifelse(is.na(annOvershoot15C_1992), NA,
			abs(annOvershoot15C_1992*(AR6Cprice*1000))),
		reparations15C_1992_lwr = ifelse(is.na(annOvershoot15C_1992), NA,
			abs(annOvershoot15C_1992*(AR6Cprice_lwr*1000))),
		reparations15C_1992_upr = ifelse(is.na(annOvershoot15C_1992), NA,
			abs(annOvershoot15C_1992*(AR6Cprice_upr*1000)))) %>%
	ungroup() %>%
	select(country, iso3c, date, reparations15C_1850, reparations15C_1850_lwr, reparations15C_1850_upr, 
		reparations15C_1960, reparations15C_1960_lwr, reparations15C_1960_upr,
		reparations15C_1992, reparations15C_1992_lwr, reparations15C_1992_upr)

rm(linCO2)


#divide by startDate and scenario
#1850
rep1850 <- reparations %>%
	select(country, iso3c, date, reparationsAR6 = reparations15C_1850, reparationsAR6lwr = reparations15C_1850_lwr,
	  reparationsAR6upr = reparations15C_1850_upr) %>%
	add_column(startDate = 1850, scenario = "NetZero") %>%
	relocate(startDate, scenario)

#1960
rep1960 <- reparations %>%
	select(country, iso3c, date, reparationsAR6 = reparations15C_1960, reparationsAR6lwr = reparations15C_1960_lwr,
	  reparationsAR6upr = reparations15C_1960_upr) %>%
	add_column(startDate = 1960, scenario = "NetZero") %>%
	relocate(startDate, scenario)

#1992
rep1992 <- reparations %>%
	select(country, iso3c, date, reparationsAR6 = reparations15C_1992, reparationsAR6lwr = reparations15C_1992_lwr,
	  reparationsAR6upr = reparations15C_1992_upr) %>%
	add_column(startDate = 1992, scenario = "NetZero") %>%
	relocate(startDate, scenario)

rep <- rbind(rep1992, rep1960, rep1850)
rm(rep1850, rep1960, rep1992)

#Join with overshoot3
overshoot4 <- left_join(overshoot3, rep, by=c("startDate", "scenario", "country", "iso3c", "date"))

rm(overshoot3, reparations, rep)

#------------------------------------------------------------------------------------------------------------
#Calculate cumulative reparations
#------------------------------------------------------------------------------------------------------------
cumRep <- overshoot4 %>%
	filter(scenario %in% c("NetZero")) %>%
	group_by(startDate, scenario, country, iso3c) %>%
	mutate(cumReparationsAR6 = cumsum(ifelse(is.na(reparationsAR6), 0, reparationsAR6)) + reparationsAR6*0,
	  cumReparationsAR6lwr = cumsum(ifelse(is.na(reparationsAR6lwr), 0, reparationsAR6lwr)) + reparationsAR6lwr*0,
	  cumReparationsAR6upr = cumsum(ifelse(is.na(reparationsAR6upr), 0, reparationsAR6upr)) + reparationsAR6upr*0) %>%
	ungroup() %>%
	select(startDate, scenario, country, iso3c, date, cumReparationsAR6, cumReparationsAR6lwr, cumReparationsAR6upr)

overshoot5 <- left_join(overshoot4, cumRep, by=c("startDate", "scenario", "country", "iso3c", "date"))
rm(overshoot4)

#add cumulative reparations to rank3
rank4 <- left_join(rank3, cumRep, by=c("startDate", "scenario", "country", "iso3c", "date"))

rm(rank3)

#------------------------------------------------------------------------------------------------------------
#					write to file
#------------------------------------------------------------------------------------------------------------
#write_csv(overshoot5, "./myData/16_myDataOvershootGDPreparations_v4.csv")
#write_csv(myData21, "./myData/16_myDataCprice_v4.csv")
#write_csv(rank4, "./myData/16_countryOvershootShareReparations-1850-2050_v4.csv")
#------------------------------------------------------------------------------------------------------------
#					PREP SUMMARY GROUPS
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#Get overshooting 2050 cumulative reparations countries

debtors <- rank4 %>%
	filter(scenario == "NetZero", date == 2050) %>%
	select(startDate, debtCountry = country, debtIso3c = iso3c, debtNorthRegions = northAndRegions, 
	  debtNorthSouth = northSouth, cumReparationsAR6, cumReparationsAR6lwr, cumReparationsAR6upr) %>%
	drop_na(cumReparationsAR6)

debtGroup <- rank4 %>%
	filter(scenario == "NetZero", date == 2050) %>%
	select(startDate, debtCountry = country, debtIso3c = iso3c, debtShare15C, debtGroup15C) %>% 
	drop_na(debtGroup15C)

debtors1 <- left_join(debtors, debtGroup, by=c("startDate", "debtCountry", "debtIso3c"))

rm(debtors, debtGroup)

#------------------------------------------------------------------------------------------------------------------
#Get undershooting 2050 cumulative country shares

creditors <- rank4 %>%
	filter(scenario == "NetZero", date == 2050) %>%
	select(startDate, credCountry = country, credIso3c = iso3c, credNorthRegions = northAndRegions,
	  credNorthSouth = northSouth, credShare15C, credGroup15C) %>%
	drop_na(credShare15C)

reparations <- left_join(debtors1, creditors, by="startDate")

#------------------------------------------------------------------------------------------------------------------
#Distribute each debtor country reparations by creditor shares
reparations1 <- reparations %>%
	mutate(credShare15C = credShare15C / 100) %>%
	rowwise() %>%
	mutate(cumCompensation = cumReparationsAR6 * credShare15C,
	  cumCompensationLwr = cumReparationsAR6lwr * credShare15C,
	  cumCompensationUpr = cumReparationsAR6upr * credShare15C) %>%
	ungroup()

#Get summary list of creditor country compensation and write to file
creditors1 <- reparations1 %>%
	group_by(startDate, credCountry, credIso3c) %>%
	summarise(credShare15C = mean(credShare15C, na.rm=T),
	  cumCompensation = sum(cumCompensation, na.rm=T),
  	  cumCompensationLwr = sum(cumCompensationLwr, na.rm=T),
	  cumCompensationUpr = sum(cumCompensationUpr, na.rm=T)) %>%
	ungroup() %>%
	group_by(startDate) %>%
	arrange(-cumCompensation, .by_group=T) %>%
	ungroup()

credCountryGrp <- reparations1 %>%
	select(startDate, country = credCountry, iso3c = credIso3c, group15C = credGroup15C) %>%
	count(startDate, country, iso3c, group15C) %>%
	select(-n) %>%
	add_column(type = "Undershooters")

#------------------------------------------------------------------------------------------------------------------
#put cumulative creditors and debtors into single tibble (and add GDP)
debtors2 <- debtors1 %>%
	select(startDate, country = debtCountry, iso3c = debtIso3c, cumReparationsAR6, cumReparationsAR6lwr, cumReparationsAR6upr,
	  share = debtShare15C, group15C = debtGroup15C) %>%
	add_column(type = "Overshooters") %>%
	rowwise() %>%
	mutate(cumReparationsAR6 = cumReparationsAR6*-1, 
	  cumReparationsAR6lwr = cumReparationsAR6lwr*-1, 
	  cumReparationsAR6upr = cumReparationsAR6upr*-1) %>%
	ungroup() %>%
	relocate(group15C, type, .after=iso3c) %>%
	group_by(startDate) %>%
	arrange(cumReparationsAR6, .by_group=T) %>%
	ungroup()

creditors2 <- creditors1 %>%
	select(startDate, country = credCountry, iso3c = credIso3c, cumReparationsAR6 = cumCompensation,
	  cumReparationsAR6lwr = cumCompensationLwr, cumReparationsAR6upr = cumCompensationUpr, share = credShare15C) %>%
	left_join(.,credCountryGrp, by=c("startDate", "country", "iso3c")) %>%
	relocate(group15C, type, .after=iso3c)

bindReparations <- rbind(debtors2, creditors2)

#------------------------------------------------------------------------------------------------------------------
#write_csv(bindReparations, "./myData/16_debtorsAndCreditors_v4.csv")
#write_csv(reparations1, "./myData/16_reparations_v4.csv")
#------------------------------------------------------------------------------------------------------------------

#Put into groups for visualisation
debtorGrp <- debtors1 %>%
	group_by(startDate, debtGroup15C) %>%
	summarise(cumReparationsAR6 = sum(cumReparationsAR6, na.rm=T),
	  cumReparationsAR6lwr = sum(cumReparationsAR6lwr, na.rm=T),
	  cumReparationsAR6upr = sum(cumReparationsAR6upr, na.rm=T),
	  debtShare15C = sum(debtShare15C)) %>%
	ungroup()

creditorGrp <- creditors %>%
	group_by(startDate, credGroup15C) %>%
	summarise(credShare15C = sum(credShare15C, na.rm=T)) %>%
	ungroup()

reparationsGrp <- left_join(debtorGrp, creditorGrp, by="startDate")


reparationsGrp1 <- reparationsGrp %>%
	rowwise() %>%
	mutate(cumCompensation = cumReparationsAR6 * (credShare15C/100),
	  cumCompensationLwr = cumReparationsAR6lwr * (credShare15C/100),
	  cumCompensationUpr = cumReparationsAR6upr * (credShare15C/100)) %>%
	ungroup()

rm(debtorGrp, creditors, creditors1, creditorGrp, debtors1, reparations, reparationsGrp)

#------------------------------------------------------------------------------------------------------------------
#				PREP FOR VISUALISING BAR CHARTS OF REPARATION FLOWS
#------------------------------------------------------------------------------------------------------------------

#colours <- c("Global South overshoot"="#fc9272", "China overshoot"="#fcbba1", "Rest of global North"="#ef3b2c", 
#		"EU and UK"="#cb181d", "USA"="#99000d", 
#		"Global North undershoot"="#fee8c8", "Rest of global South"="#41ab5d", 
#		"China"="#78c679", "Sub-Sah. Africa"="#238443", "India"="#005a32") ##addd8e

colours <- c("Global South overshoot"="#fcbba1", "China overshoot"="#fc9272", "Rest of global North"="#ef3b2c", 
		"EU and UK"="#cb181d", "USA"="#99000d", 
		"Global North undershoot"="#fee8c8", "Rest of global South"="#78c679", 
		"China"="#238443", "Sub-Sah. Africa"="#41ab5d", "India"="#005a32") ##addd8e


reparationsGrp2 <- reparationsGrp1 %>% 
	filter(credGroup15C != "Global North undershoot") %>% #remove 0.04% global North undershoot from 1850
	mutate(debtGroup15C = ifelse(debtGroup15C == "Rest of global South overshoot", "Global South overshoot",
		debtGroup15C),
	  debtGroup15C = factor(debtGroup15C, levels=c("USA", "EU and UK", "Rest of global North", "China overshoot", "Global South overshoot")),
	  credGroup15C = factor(credGroup15C, levels=c("India", "China", "Sub-Sah. Africa", "Rest of global South")))

debtShare <- reparationsGrp2 %>%
	select(startDate, group15C = debtGroup15C, cumReparations = cumReparationsAR6, cumReparationsLwr = cumReparationsAR6lwr, 
	  cumReparationsUpr = cumReparationsAR6upr, share=debtShare15C) %>%
	group_by(startDate, group15C) %>%
	summarise(cumReparations = mean(cumReparations),
	  cumReparationsLwr = mean(cumReparationsLwr),
	  cumReparationsUpr = mean(cumReparationsUpr),
	  share = mean(share)) %>%
	ungroup() %>%
	add_column(type="Overemitters") %>%
	group_by(startDate) %>%
	arrange(-cumReparations, .by_group=T) %>%
	ungroup()

credShare <- reparationsGrp2 %>%
	select(startDate, group15C = credGroup15C, cumReparations = cumCompensation, cumReparationsLwr = cumCompensationLwr, 
	  cumReparationsUpr = cumCompensationUpr, share=credShare15C) %>%
	group_by(startDate, group15C) %>%
	summarise(cumReparations = sum(cumReparations),
	  cumReparationsLwr = sum(cumReparationsLwr),
	  cumReparationsUpr = sum(cumReparationsUpr),
	  share = mean(share)) %>%
	ungroup() %>%
	add_column(type="Within fair shares") %>%
	group_by(startDate) %>%
	arrange(-cumReparations, .by_group=T) %>%
	ungroup()


shareData <- rbind(debtShare, credShare)

shareData1 <- shareData %>%
	rowwise() %>%
	mutate(labelNum = paste0(group15C, "\n($", round(cumReparations/1000000000000, digits=0)," trillion)"),
	  labelNum1 = paste0(group15C, "\n(", round(share, digits=0), "%)"),
	  startLabel = ifelse(startDate == 1992, "1992–2050", ifelse(startDate == 1960, "1960–2050", "1850–2050")),
	  startLabel = factor(startLabel, levels =c("1992–2050", "1960–2050", "1850–2050"))) %>%
	ungroup() %>%
	group_by(startDate) %>%
	mutate(type = factor(type, levels=c("Overemitters", "Within fair shares"))) %>%
	ungroup()

#------------------------------------------------------------------------------------------------------------------
#				VISUALISING BAR CHARTS OF REPARATION FLOWS (FIGURE 4)
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#Cumulative reparation flows in 1960-2050 barChart, absolute y-axis
ggplot(data=shareData1 %>% filter(startDate == 1960), aes(x=type, y=cumReparations/1000000000000, group=group15C)) +
	geom_col(aes(fill=group15C), width=0.4) +
	#facet_wrap(~ startDate) +
	scale_fill_manual(values=colours) +
	geom_text(aes(label = labelNum, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.25)) +
	labs(x="Overshooters and Undershooters", y="Cumulative compensation in 2050, net zero scenario (constant 2010 trillion $)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  panel.border = element_blank(),
	  panel.background = element_blank(),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=6, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#ggsave("./Figures/Figure4-reparationFlows_v5.pdf", width = 89, height = 80, units = "mm", device = "pdf")


#------------------------------------------------------------------------------------------------------------------
#				VISUALISING BAR CHARTS OF REPARATION FLOWS - 1850 and 1992(FIGURE S6)
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#Cumulative reparation flows in 1960-2050 barChart, absolute y-axis
ggplot(data=shareData1 %>% filter(startDate != 1960), aes(x=type, y=cumReparations/1000000000000, group=group15C)) +
	geom_col(aes(fill=group15C), width=0.4) +
	facet_wrap(~ startLabel) +
	scale_fill_manual(values=colours) +
	geom_text(aes(label = labelNum, hjust=0.5), colour="#ffffff", position = position_stack(vjust = 0.5), size=1.8) + #colour=colText, 
	scale_y_continuous(limits=c(0,250), breaks=seq(0,250,50), expand=c(0,0)) +
	scale_x_discrete(expand = c(0,0.25)) +
	labs(x="Overshooters and Undershooters", y="Cumulative compensation, net zero scenario (constant 2010 trillion $)") +
	theme_chart_SMALLM +
	theme(legend.position="none",
	  panel.grid = element_blank(),
	  #panel.border = element_blank(),
	  #panel.background = element_blank(),
	  panel.spacing.x = unit(1.2, "lines"),
	  legend.text = element_text(size=6, colour = "#636363"),
	  axis.line = element_line(colour = "#636363", size = 0.7),
	  axis.title = element_text(size=7, family="sans", color="#636363"),
	  axis.title.x = element_blank(),
	  axis.text = element_text(size=6, colour = "#636363"),
	  axis.ticks = element_line(colour = "#636363"))

#ggsave("./Figures/FigureS5-reparationFlows_1992and1850_v5.png", width = 183, height = 80, units = "mm", device = "png")




#------------------------------------------------------------------------------------------------------------------
#write group data to file
#write_csv(reparationsGrp2, "./myData/16_ReparationsCompensationByGroup_v4.csv")
#write_csv(shareData1, "./Figures/16_barChartData_v4.csv")
#------------------------------------------------------------------------------------------------------------------

rm(reparationsGrp1, shareData)

