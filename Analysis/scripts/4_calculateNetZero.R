## This script calculates country-level net zero pathways

#Read in datafile from previous script (if needed), and drop Estonia and Trinidad
#myData11 <- read_csv("./myData/03_CO2-BAU_v1.csv") %>%
	filter(!iso3c %in% c("EST", "TTO"))
#--------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
# Calculate CO2 zero by 2050
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#Calculate country-specific per capita mitigation rates needed to reach 0.04 t per capita (min 2020 value) by 2050 
rCO2 <- myData11 %>%
	filter(date == 2020) %>%
	select(country, iso3c, population, CO2_bau) %>%
	add_column(t = 30) %>%
	rowwise() %>%
	mutate(CO2_bauPerCap = (CO2_bau*1000) / population,
	  end = 0.1,
	  start = ifelse(CO2_bauPerCap <= 0.1, 0.1, CO2_bauPerCap),
	  rCO2 = log(end/start)/t) %>%
	ungroup() %>%
	select(-CO2_bau, -t, -end, -population, -CO2_bauPerCap)


#write_csv(rCO2, "./myData/4_netZeroMitigationRates_v3.csv")

#Look at bar chart of mitigation rates
ggsave("netZeroMitigationRates_v3.png",
	ggplot(rCO2, aes(x=reorder(factor(country), -rCO2, sum), y=rCO2)) +
  	  geom_col(width=0.5, col="darkred") +
  	  coord_flip() +
  	  labs(x ="Country", y="Net zero by 2050 mitigation rate (%)") +
  	  theme_chart_SMALLM + 
	  theme(axis.text = element_text(size=6, family="sans", color="#666666")),
width = 150, height = 300, dpi = 300, units="mm", device="png")

#Calculate country-level net zero series
CO2_net0 <- myData11 %>%
	select(-CO2_lwr66, -CO2_upr66) %>%
	filter(date >= 2019) %>%
	left_join(rCO2, by=c("country", "iso3c")) %>%
	rowwise() %>%
	mutate(CO2perCap = (CO2*1000) / population,
	  CO2_bauPerCap = (CO2_bau*1000) / population,
	  t = date - 2020,
	  CO2_net0perCap = ifelse(date < 2020, CO2_bauPerCap,
			start * exp(t*rCO2)),
	  CO2_net0 = (CO2_net0perCap * population)/1000) %>%
	ungroup() %>%
	select(country, iso3c, date, CO2_net0)

#merge
myData12 <- left_join(myData11, CO2_net0, by = c("country", "iso3c", "date"))

# Look at countries
#look at countries
countries <- myData12 %>%
	filter(iso3c %in% c("BGD", "BRA", "CRI", "IND", "USA", "GBR", "CHN", "NGA", "KAZ", "UZB", "WLD"))

ggplot(data = countries, aes(x = date)) + 
	#geom_vline(xintercept = 1988, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_vline(xintercept = 2029, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_line(aes(y = fairShare_350ppm/1000000), size=1.2, colour = "#387f30") +
	#geom_line(aes(y = fairShare_15C/1000000), size=1.2, colour = "#a1d99b") +
	geom_line(aes(y = CO2/1000), size=1.2, colour = "black") +
	geom_ribbon(aes(ymin = CO2_lwr66/1000, ymax = CO2_upr66/1000), alpha=0.3, fill="#ff4e4e") +
	geom_line(aes(y = CO2_bau/1000), size=1.2, colour = "red") +
	geom_line(aes(y = CO2_net0/1000), size=1.2, colour = "#0000a5") +
	#geom_line(aes(y = cumCO2data_grn/1000000), size=1.2, colour = "#a5a500") +
	facet_wrap(~ country, scales="free") +
	labs(x="Year", y="Carbon dioxide emissions (Mt CO2)") +
	theme_chart_SMALLM


#ggsave("bau-net0_CountrySelection.png")

rm(CO2_net0, countries, countryList, myData11, rCO2)
#---------------------------------------------------------------------
#		WRITE TO FILE
#--------------------------------------------------------------------
#write_csv(myData12, "./myData/04_CO2-BAU-Net0_v3.csv")


