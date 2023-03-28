## This script calculates cumulative global boundaries from 1850 and 1960 for 350 ppm
# 1.5C and 2C

#Read in datafile from previous script (if needed)
#myData12 <- read_csv("./myData/04_CO2-BAU-Net0_v3.csv")
#--------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
# Calculate cumulative CO2 from 1850 and from 1960 for each country (and world)
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
myData13 <- myData12 %>%
	group_by(country, iso3c) %>%
	mutate(cumCO21850 = cumsum(ifelse(is.na(CO2), 0, CO2)) + CO2*0) %>%
	ungroup()

cum1960 <- myData13 %>%
	filter(date >= 1960) %>%
	select(country, iso3c, date, CO2) %>%
	group_by(country, iso3c) %>%
	mutate(cumCO21960 = cumsum(ifelse(is.na(CO2), 0, CO2)) + CO2*0) %>%
	ungroup() %>%
	select(-CO2)

myData14 <- left_join(myData13, cum1960, by=c("country", "iso3c", "date"))

cum1992 <- myData13 %>%
	filter(date >= 1992) %>%
	select(country, iso3c, date, CO2) %>%
	group_by(country, iso3c) %>%
	mutate(cumCO21992 = cumsum(ifelse(is.na(CO2), 0, CO2)) + CO2*0) %>%
	ungroup() %>%
	select(-CO2)

myData15 <- left_join(myData14, cum1992, by=c("country", "iso3c", "date"))


rm(myData12, myData13, myData14, cum1960, cum1992)
#-------------------------------------------------------------------------------------
#Calculate cumulative bau and net zero values
cumCO2 <- myData15 %>%
	filter(date >= 2019) %>%
	select(-population, -CO2) %>%
	rowwise() %>%
	mutate(CO2_net01850 = ifelse(date==2019, cumCO21850, CO2_net0),
		CO2_net01960 = ifelse(date==2019, cumCO21960, CO2_net0),
		CO2_net01992 = ifelse(date==2019, cumCO21992, CO2_net0),
		CO2_bau1850 = ifelse(date==2019, cumCO21850, CO2_bau),
		CO2_bau1960 = ifelse(date==2019, cumCO21960, CO2_bau),
		CO2_bau1992 = ifelse(date==2019, cumCO21992, CO2_bau),
		CO2_lwr661850 = ifelse(date==2019, cumCO21850, CO2_lwr66),
		CO2_lwr661960 = ifelse(date==2019, cumCO21960, CO2_lwr66),
		CO2_lwr661992 = ifelse(date==2019, cumCO21992, CO2_lwr66),
		CO2_upr661850 = ifelse(date==2019, cumCO21850, CO2_upr66),
		CO2_upr661960 = ifelse(date==2019, cumCO21960, CO2_upr66),
		CO2_upr661992 = ifelse(date==2019, cumCO21992, CO2_upr66)) %>%
	ungroup() %>%
	group_by(country, iso3c) %>%
	mutate(cumCO2_net01850 = cumsum(ifelse(is.na(CO2_net01850), 0, CO2_net01850)) + CO2_net01850*0,
		cumCO2_net01960 = cumsum(ifelse(is.na(CO2_net01960), 0, CO2_net01960)) + CO2_net01960*0,
		cumCO2_net01992 = cumsum(ifelse(is.na(CO2_net01992), 0, CO2_net01992)) + CO2_net01992*0,
		cumCO2_bau1850 = cumsum(ifelse(is.na(CO2_bau1850), 0, CO2_bau1850)) + CO2_bau1850*0,
		cumCO2_lwr661850 = cumsum(ifelse(is.na(CO2_lwr661850), 0, CO2_lwr661850)) + CO2_lwr661850*0,
		cumCO2_upr661850 = cumsum(ifelse(is.na(CO2_upr661850), 0, CO2_upr661850)) + CO2_upr661850*0,
		cumCO2_bau1960 = cumsum(ifelse(is.na(CO2_bau1960), 0, CO2_bau1960)) + CO2_bau1960*0,
		cumCO2_lwr661960 = cumsum(ifelse(is.na(CO2_lwr661960), 0, CO2_lwr661960)) + CO2_lwr661960*0,
		cumCO2_upr661960 = cumsum(ifelse(is.na(CO2_upr661960), 0, CO2_upr661960)) + CO2_upr661960*0,
		cumCO2_bau1992 = cumsum(ifelse(is.na(CO2_bau1992), 0, CO2_bau1992)) + CO2_bau1992*0,
		cumCO2_lwr661992 = cumsum(ifelse(is.na(CO2_lwr661992), 0, CO2_lwr661992)) + CO2_lwr661992*0,
		cumCO2_upr661992 = cumsum(ifelse(is.na(CO2_upr661992), 0, CO2_upr661992)) + CO2_upr661992*0) %>%
	ungroup() %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2_bau1850, cumCO2_lwr661850, cumCO2_upr661850, cumCO2_net01850, 
		cumCO2_bau1960, cumCO2_lwr661960, cumCO2_upr661960, cumCO2_net01960,
		cumCO2_bau1992, cumCO2_lwr661992, cumCO2_upr661992, cumCO2_net01992)

myData16 <- left_join(myData15, cumCO2, by=c("country", "iso3c", "date"))

rm(myData15, cumCO2)

#-------------------------------------------------------------------------------------
#Grab cumulative carbon budgets for 350 ppm boundary, crossed in 1988, for 1850 and 1960

budget350 <- myData16 %>%
	filter(iso3c == "WLD", date == 1988) %>%
	select(budget350_1850 = cumCO21850, budget350_1960 = cumCO21960)

#-------------------------------------------------------------------------------------
#Grab cumulative carbon budgets for 1.5C boundary for 1850, 1960, and 1992 equivalent to 360 Gt more than
#2020 values, which was crossed in 2030 according to our BAU.

budget15C <- myData16 %>%
	filter(iso3c == "WLD", date == 2030) %>%
	select(country, date, budget15C_1850 = cumCO2_bau1850, budget15C_1960 = cumCO2_bau1960,
	  budget15C_1992 = cumCO2_bau1992)
 
#-------------------------------------------------------------------------------------
#Do the same for 2C boundary, equivalent to 1035 Gt more than 2020, which would be crossed in 2045,
# judging from our BAU

budget2C <- myData16 %>%
	filter(iso3c == "WLD", date == 2045) %>%
	select(budget2C_1850 = cumCO2_bau1850, budget2C_1960 = cumCO2_bau1960,
	  budget2C_1992 = cumCO2_bau1992)

#Bind the boundaries into a single tibble
budgets <- cbind(budget350, budget15C, budget2C)

myData17 <- myData16 %>%
	add_column(budget350_1850 = budgets$budget350_1850,
		budget350_1960 = budgets$budget350_1960,
		budget15C_1850 = budgets$budget15C_1850,
		budget15C_1960 = budgets$budget15C_1960,
		budget15C_1992 = budgets$budget15C_1992,
		budget2C_1850 = budgets$budget2C_1850,
		budget2C_1960 = budgets$budget2C_1960,
		budget2C_1992 = budgets$budget2C_1992)

rm(budgets, budget2C, budget15C, budget350, myData16)

#look at world
#look at countries
countries <- myData17 %>%
	filter(iso3c %in% c("WLD"), date >= 1992)

ggplot(data = countries, aes(x = date)) + 
	#geom_vline(xintercept = 1988, colour = "grey", size = 0.8, linetype="dashed") +
	geom_vline(xintercept = 2030, colour = "grey", size = 0.8, linetype="dashed") +
	geom_vline(xintercept = 2045, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_line(aes(y = budget350_1992/1000000), size=1.2, colour = "#387f30") +
	geom_line(aes(y = budget15C_1992/1000000), size=1.2, colour = "#a1d99b") +
	geom_line(aes(y = budget2C_1992/1000000), size=1.2, colour = "#de970b") +
	geom_line(aes(y = cumCO21992/1000000), size=1.2, colour = "black") +
	geom_ribbon(aes(ymin=cumCO2_lwr661992/1000000, ymax=cumCO2_upr661992/1000000), alpha=0.3, fill="#ff4e4e") +
	geom_line(aes(y = cumCO2_bau1992/1000000), size=1.2, colour = "red") +
	geom_line(aes(y = cumCO2_net01992/1000000), size=1.2, colour = "#0000a5") +
	#geom_line(aes(y = cumCO2data_grn/1000000), size=1.2, colour = "#a5a500") +
	facet_wrap(~ country, scales="free") +
	labs(x="Year", y="Carbon dioxide emissions (Gt CO2)") +
	theme_chart_SMALLM


#ggsave("bau-net0-boundaries_global1992.png")

rm(countries)
#---------------------------------------------------------------------
#		WRITE TO FILE
#--------------------------------------------------------------------
#write_csv(myData17, "./myData/05_cumCO2andBoundaries_v3.csv")



