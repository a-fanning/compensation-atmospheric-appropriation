## This script calculates national convergence pathways from 
# 1850 and 1960 for 350 ppm, 1.5C and 2C

#Read in datafile from previous script (if needed)
#myData18 <- read_csv("./myData/06_myDataCO2andFairShares_v3.csv")
#--------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
# Calculate national overshoot from 1850, 1960 and 1992 for each country (and world)
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#figure out linear overshoot
linCO2 <- myData18 %>%
	filter(date == 2020) %>%
	select(country, iso3c, cumStart_1850 = cumCO2_bau1850, cumStart_1960 = cumCO2_bau1960,
	  cumStart_1992 = cumCO2_bau1992, fairShare350_1850, fairShare350_1960,
	  fairShare15C_1850, fairShare15C_1960, fairShare15C_1992,
	  fairShare2C_1850, fairShare2C_1960, fairShare2C_1992) %>%
	rowwise() %>%
	mutate(cumOvershoot350_1850 = fairShare350_1850 - cumStart_1850,
		cumOvershoot15C_1850 = fairShare15C_1850 - cumStart_1850,
		cumOvershoot2C_1850 = fairShare2C_1850 - cumStart_1850,
		cumOvershoot350_1960 = fairShare350_1960 - cumStart_1960,
		cumOvershoot15C_1960 = fairShare15C_1960 - cumStart_1960,
		cumOvershoot2C_1960 = fairShare2C_1960 - cumStart_1960,
		cumOvershoot15C_1992 = fairShare15C_1992 - cumStart_1992,
		cumOvershoot2C_1992 = fairShare2C_1992 - cumStart_1992,
		linOvershoot350_1850 = ifelse(cumOvershoot350_1850 < 0, cumOvershoot350_1850/30, NA),
		linOvershoot15C_1850 = ifelse(cumOvershoot15C_1850 < 0, cumOvershoot15C_1850/30, NA),
		linOvershoot2C_1850 = ifelse(cumOvershoot2C_1850 < 0, cumOvershoot2C_1850/30, NA),
		linOvershoot350_1960 = ifelse(cumOvershoot350_1960 < 0, cumOvershoot350_1960/30, NA),
		linOvershoot15C_1960 = ifelse(cumOvershoot15C_1960 < 0, cumOvershoot15C_1960/30, NA),
		linOvershoot2C_1960 = ifelse(cumOvershoot2C_1960 < 0, cumOvershoot2C_1960/30, NA),
		linOvershoot15C_1992 = ifelse(cumOvershoot15C_1992 < 0, cumOvershoot15C_1992/30, NA),
		linOvershoot2C_1992 = ifelse(cumOvershoot2C_1992 < 0, cumOvershoot2C_1992/30, NA)) %>%
	ungroup() %>%
	select(-fairShare350_1850, -fairShare350_1960, -fairShare15C_1850, -fairShare15C_1960,
		-fairShare15C_1992, -fairShare2C_1850, -fairShare2C_1960, , -fairShare2C_1992, 
		-cumOvershoot350_1850, -cumOvershoot350_1960, -cumOvershoot15C_1850, -cumOvershoot15C_1960, 
		-cumOvershoot15C_1992, -cumOvershoot2C_1850, -cumOvershoot2C_1960, -cumOvershoot2C_1992)

CO2_neg <- myData18 %>%
	filter(date >= 2020) %>%
	left_join(linCO2, by=c("country", "iso3c")) %>%
	rowwise() %>%
	mutate(t = date - 2020,
		cumNeg350_1850 = ifelse(is.na(linOvershoot350_1850), NA,
			linOvershoot350_1850*t + cumStart_1850),
		cumNeg15C_1850 = ifelse(is.na(linOvershoot15C_1850), NA,
			linOvershoot15C_1850*t + cumStart_1850),
		cumNeg2C_1850 = ifelse(is.na(linOvershoot2C_1850), NA,
			linOvershoot2C_1850*t + cumStart_1850),
		cumNeg350_1960 = ifelse(is.na(linOvershoot350_1960), NA,
			linOvershoot350_1960*t + cumStart_1960),
		cumNeg15C_1960 = ifelse(is.na(linOvershoot15C_1960), NA,
			linOvershoot15C_1960*t + cumStart_1960),
		cumNeg2C_1960 = ifelse(is.na(linOvershoot2C_1960), NA,
			linOvershoot2C_1960*t + cumStart_1960),
		cumNeg15C_1992 = ifelse(is.na(linOvershoot15C_1992), NA,
			linOvershoot15C_1992*t + cumStart_1992),
		cumNeg2C_1992 = ifelse(is.na(linOvershoot2C_1992), NA,
			linOvershoot2C_1992*t + cumStart_1992)) %>%
	ungroup() %>%
	select(country, iso3c, date, cumNeg350_1850, cumNeg350_1960, 
	  cumNeg15C_1850, cumNeg15C_1960, cumNeg15C_1992,
	  cumNeg2C_1850, cumNeg2C_1960, cumNeg2C_1992)

myData19 <- left_join(myData18, CO2_neg, by=c("country", "iso3c", "date"))


#-------------------------------------------------------------------------------------------------
#look at countries
countries <- myData19 %>%
	filter(iso3c %in% c("BGD", "BRA", "CRI", "IND", "USA", "GBR", "CHN", "KAZ", "UZB", "NGA", "WLD"))

ggsave("bau-net0-cnvrg_CountrySelection_1850.png",
ggplot(data = countries, aes(x = date)) + 
	#geom_vline(xintercept = 1988, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_vline(xintercept = 2029, colour = "grey", size = 0.8, linetype="dashed") +
	geom_line(aes(y = fairShare350_1850/1000), size=1.2, colour = "#387f30") +
	geom_line(aes(y = fairShare15C_1850/1000), size=1.2, colour = "#a1d99b") +
	geom_line(aes(y = fairShare2C_1850/1000), size=1.2, colour = "#de970b") +
	geom_line(aes(y = cumCO21850/1000), size=1.2, colour = "black") +
	geom_ribbon(aes(ymin = cumCO2_lwr661850/1000, ymax = cumCO2_upr661850/1000), alpha=0.3, fill="#ff4e4e") +
	geom_line(aes(y = cumCO2_bau1850/1000), size=1.2, colour = "red") +
	geom_line(aes(y = cumCO2_net01850/1000), size=1.2, colour = "#0000a5") +
	geom_line(aes(y = cumNeg350_1850/1000), size=1.2, colour = "#a5a500") +
	geom_line(aes(y = cumNeg15C_1850/1000), size=1.2, colour = "#a5a500") +
	geom_line(aes(y = cumNeg2C_1850/1000), size=1.2, colour = "#a5a500") +
	facet_wrap(~ country, scales="free") +
	labs(x="Year", y="Carbon dioxide emissions (Mt CO2)") +
	theme_chart_SMALLM,
width = 150, height = 100, dpi = 300, units="mm", device="png")

ggsave("bau-net0-cnvrg_CountrySelection_1960.png",
ggplot(data = countries %>% filter(date >=1960), aes(x = date)) + 
	#geom_vline(xintercept = 1988, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_vline(xintercept = 2029, colour = "grey", size = 0.8, linetype="dashed") +
	geom_line(aes(y = fairShare350_1960/1000), size=1.2, colour = "#387f30") +
	geom_line(aes(y = fairShare15C_1960/1000), size=1.2, colour = "#a1d99b") +
	geom_line(aes(y = fairShare2C_1960/1000), size=1.2, colour = "#de970b") +
	geom_line(aes(y = cumCO21960/1000), size=1.2, colour = "black") +
	geom_ribbon(aes(ymin = cumCO2_lwr661960/1000, ymax = cumCO2_upr661960/1000), alpha=0.3, fill="#ff4e4e") +
	geom_line(aes(y = cumCO2_bau1960/1000), size=1.2, colour = "red") +
	geom_line(aes(y = cumCO2_net01960/1000), size=1.2, colour = "#0000a5") +
	geom_line(aes(y = cumNeg350_1960/1000), size=1.2, colour = "#a5a500") +
	geom_line(aes(y = cumNeg15C_1960/1000), size=1.2, colour = "#a5a500") +
	geom_line(aes(y = cumNeg2C_1960/1000), size=1.2, colour = "#a5a500") +
	facet_wrap(~ country, scales="free") +
	labs(x="Year", y="Carbon dioxide emissions (Mt CO2)") +
	theme_chart_SMALLM,
width = 150, height = 100, dpi = 300, units="mm", device="png")


#-------------------------------------------------------------------
# 		ADD COUNTRY NORTH-SOUTH GROUPINGS
#-------------------------------------------------------------------
countryGroups <- read_csv("./cleanData/country_region.csv") %>%
	select(iso3c = iso, northAndRegions = nrt_sth_all, northSouth = nrt_sth, annex1)

myData20 <- left_join(myData19, countryGroups, by="iso3c")

#write_csv(myData20, "./myData/07_myDataCO2convergenceWithGroups_v3.csv")

rm(CO2_neg, countries, countryGroups, linCO2, myData18, myData19)

	